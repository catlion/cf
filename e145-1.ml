let num =
	| N4
	| N7

let vertice = {
	len: int;
	n4: int;
	n7: int;
	n47: int;
	n74:int;
}

let read_nums str delim =
  let nstrings = Str.split (Str.regexp delim) str in
  Array.of_list (List.map (fun x -> int_of_string x) nstrings)

let main =
  let s1 = read_nums (read_line ()) " " in (* First line, n = number of  *)
  let n, m = s1.(0), s1.(1) in
  let happy_nums = read_nums (read_line ()) "" in
  let commands = read_cmds m in
  let switch_params str =
    let l, r = Scanf.sscanf str "switch %d %d" (fun i1 i2 -> i1, i2) in
    (l, r)
  in
  let rec docmd = function
    | [] -> ()
    | h :: t ->
      match h with
	  "count" -> count happy_nums; docmd t
	| _ -> let p = switch_params h in
	       switch happy_nums (fst p) (snd p);
	       docmd t
  in
  docmd commands;
  ()
