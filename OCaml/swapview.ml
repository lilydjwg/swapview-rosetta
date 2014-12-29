let format = "%5s %9s %s" ^^ "\n"
let totalFmt = "Total: %8s" ^^ "\n"

(* from http://stackoverflow.com/a/5775024/296473 *)
let readfile filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; []
  with End_of_file ->
    (* FIXME: other exceptions lead to fd leakage here *)
    close_in chan;
  List.rev !lines

let filesize n =
  let units = "KMGTP" in
  let rec liftUnit n u =
    let maxLevel = String.length units in
    if n > 1100. && u < maxLevel
    then liftUnit (n /. 1024.) (u+1)
    else (n, u) in
  let (m, level) = liftUnit (float n) 0 in
  if level <> 0
  then
    let u = units.[level-1] in
    Printf.sprintf "%.1f%ciB" m u
  else (string_of_int n) ^ "B"

let swapused pid =
  let lines = readfile ("/proc/" ^ pid ^ "/smaps") in
  let swap_amount cur line =
    if Str.string_partial_match (Str.regexp "^Swap:[^0-9]+\\([0-9]+\\)") line 0
    then cur + int_of_string (Str.matched_group 1 line)
    else cur in
  let inkB = List.fold_left swap_amount 0 lines in
  inkB * 1024

let get_cmd pid =
  let f = "/proc/" ^ pid ^ "/cmdline" in
  try
    Str.global_replace (Str.regexp "\000") " " (List.hd (readfile f))
  with Sys_error _ ->
    ""

let _ =
  let fs = Array.to_list (Sys.readdir "/proc") in
  let pids =
    List.filter (fun x -> Str.string_match (Str.regexp "[0-9]+") x 0) fs in
  let swapused_or_0 pid =
    try
      swapused pid
    with Sys_error _ ->
      0 in
  let data = List.map (fun pid -> (pid, swapused_or_0 pid)) pids in
  let sorted = List.sort (fun (_,a) (_,b) -> compare a b) data in
  let total = ref 0 in
  Printf.printf format "PID" "SWAP" "COMMAND";
  List.iter (fun (a, b) ->
    if b <> 0 then
      Printf.printf format a (filesize b) (get_cmd a);
      total := b + !total
  ) sorted;
  Printf.printf totalFmt (filesize !total)
