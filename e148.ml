(*let rxNumbers = Str.regexp "[0-9]+"*)

type shelve =

(* Read first line input and return numbers *)
let read_nums str =
  let nstrings = Str.split (Str.regexp "[\\s]+") str in
  List.map (fun x -> int_of_string x) nstrings

let nm =
  let lst = read_nums (read_line ()) in
  (List.nth lst 0, List.nth lst 1)
