type swap_t = (string * int * string) (*pid, size, comm*)

let is_pid (file:string) : bool =
    try ignore (int_of_string file); true
    with _ -> false

let filesize (size:int) : string =
    let rec aux = function
        | (size, []) when size < 1100. -> Printf.sprintf "%.0fB" size
        | (size, []) -> aux (size /. 1024., ["KiB"; "MiB"; "GiB"; "TiB"])
        | (size, h :: []) -> Printf.sprintf "%.1f%s" size h
        | (size, h :: _) when size < 1100. -> Printf.sprintf "%.1f%s" size h
        | (size, _ :: t) -> aux (size /. 1024., t)
    in aux (float_of_int size, [])

let read_dir (dir:string) : string list =
    Array.to_list (Sys.readdir dir)

let read_file (filename:string) : string list =
    try
        let ic = open_in filename in
        let rec loop acc =
            try
                let line = input_line ic in
                loop (line :: acc)
            with _ -> close_in ic; acc
        in
        List.rev (loop [])
    with _ -> []

let chop_null (s:string) : string =
    let len = String.length s in
    let ss = if (len <> 0) && (s.[len - 1] = '\000')
        then String.sub s 0 (len - 1)
        else s
    in
    String.map (function '\000' -> ' ' | x -> x) ss

let get_comm_for (pid:string) : string =
    match read_file ("/proc/" ^ pid ^ "/cmdline") with
        | h :: _ -> chop_null h
        | _ -> ""

let get_swap_for (pid:string) : swap_t =
    match read_file ("/proc/" ^ pid ^ "/smaps") with
        | [] -> (pid, 0, "")
        | lines ->
            List.filter (fun line -> String.sub line 0 5 = "Swap:") lines
            |> List.map (fun line ->
                let len = (String.rindex line ' ') - 5 in
                String.sub line 5 len
                |> String.trim
                |> int_of_string)
            |> List.fold_left (fun acc x -> acc + x) 0
            |> fun swap -> (pid, swap * 1024, get_comm_for pid)

let get_swaps () : swap_t list =
    read_dir "/proc"
    |> List.filter is_pid
    |> List.map get_swap_for
    |> List.filter (fun (_, s, _) -> s <> 0)
    |> List.sort (fun (_,a,_) (_,b,_) -> compare a b)

let main =
    let print' = Printf.printf "%7s %9s %s\n" in
    let print_swap (pid, swap, comm) = print' pid (filesize swap) comm in
    let print_total total = Printf.printf "Total: %8s\n" (filesize total) in

    let swaps = get_swaps () in
    let total = List.fold_left (fun acc (_, x, _) -> acc + x) 0 swaps in

    print' "PID" "SWAP" "COMMAND";
    List.iter print_swap swaps;
    print_total total
