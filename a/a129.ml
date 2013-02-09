(* Read first line input and return numbers *)
let read_nums str =
  let nstrings = Str.split (Str.regexp " ") str in
  List.map (fun x -> int_of_string x) nstrings
in
ignore (read_line ());
let p = read_line () in
let pck = read_nums p in
let methods_count packs =
  let count = ref 0 in
  let list_sum list =
    let sum = ref 0 in
    List.iter (fun x -> sum := !sum + x) list;
    !sum
  in
  let lsum = list_sum packs in
  let even = lsum mod 2 = 0 in
  let comparefun' e =
    match e with
	true -> fun x -> x mod 2 = 0
      | false -> fun x -> x mod 2 <> 0
  in
  let compare = comparefun' even in
  List.iter (fun x -> if compare x then
      count := !count + 1
  ) packs;
  !count
in
print_int (methods_count pck)
