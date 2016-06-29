let inline (|>) x f = f x

let format = "%5s %9s %s" ^^ "\n"
let totalFmt = "Total: %8s" ^^ "\n"

let is_digit str =
  let rec is_digit' i =
    if i < 0
    then true
    else
      let code = Char.code str.[i] in
      if code < 48 || code > 57
      then false
      else is_digit' (i - 1) in
  is_digit' ((String.length str) - 1)

let starts_with str target =
  let rec starts_with' i =
    if i < 0
    then true
    else
      if str.[i] != target.[i]
      then false
      else starts_with' (i - 1) in
  starts_with' ((String.length target) - 1)

let parse_swap_line line =
  try
    let pos = String.index line ' ' in
    String.sub line 0 pos
  with Not_found ->
    "0"

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
  !lines

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
  let swap_amount cur line =
    if starts_with line "Swap:"
    then
      let size = String.sub line 5 ((String.length line) - 5)
                 |> String.trim
                 |> parse_swap_line
                 |> int_of_string in
      cur + size
    else cur in
  let inkB = readfile ("/proc/" ^ pid ^ "/smaps")
             |> List.fold_left swap_amount 0 in
  inkB * 1024

let get_cmd pid =
  let f = "/proc/" ^ pid ^ "/cmdline" in
  try
    String.map (fun x -> if x == '\000' then ' ' else x) (List.hd (readfile f))
  with Sys_error _ ->
    ""

let _ =
  let swapused_or_0 pid =
    try
      swapused pid
    with Sys_error _ ->
      0 in
  let sorted = Sys.readdir "/proc"
               |> Array.fold_left (fun acc x ->
                   if is_digit x
                   then (x, swapused_or_0 x) :: acc
                   else acc
                 ) []
               |> List.sort (fun (_, a) (_, b) -> compare a b) in
  let total = ref 0 in
  Printf.printf format "PID" "SWAP" "COMMAND";
  List.iter (fun (a, b) ->
    if b <> 0 then
      Printf.printf format a (filesize b) (get_cmd a);
      total := b + !total
  ) sorted;
  Printf.printf totalFmt (filesize !total)
