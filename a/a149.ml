let minNum lst target =
  let rec f' acc l cur =
    match l with
      | [] -> (-1)
      | h :: t -> let c = acc + h in
                  if c >= target then cur + 1
		  else f' c t (cur + 1)
  in
  match target with
    | 0 -> 0
    | _ -> f' 0 lst 0
in
let cmp x y =
  if x = y then 0
  else begin
    if x > y then 1 else -1
  end
in
let read_nums str =
  let nstrings = Str.split (Str.regexp " ") str in
  List.map (fun x -> int_of_string x) nstrings
in
let k = read_int () in
let grows_i = List.rev (List.sort cmp (read_nums (read_line ()))) in
print_int (minNum grows_i k)
