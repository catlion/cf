let switch arr l r =
  for i = l - 1 to r - 1 do
    arr.(i) <- arr.(i) lxor 3
  done;
  (*for i = 0 to (Array.length arr) - 1 do
    print_int arr.(i)
  done;
  print_endline "";*)
  ()

let rec print_l = function
  | [] -> print_endline " ..."
  | h :: t -> print_int h; print_l t

let count arr =
  let alen = Array.length arr in
  let max = ref 0 in
  let curstart = ref 1 in
  let debug = ref [] in
  while (alen - !curstart) >= !max do
    let tmp = ref 1 in
    let curidx = ref (!curstart - 1) in
    let cdebug = ref [] in
    for i = !curstart to (alen - 1) do
      if arr.(i) >= arr.(!curidx) then
	begin
	  tmp := !tmp + 1;
	  curidx := i;
	  cdebug := i :: !cdebug;
	end
      else
	()
    done;
    if !tmp > !max then
      begin
	max := !tmp;
	debug := !cdebug;
      end;
    curstart := !curstart + 1;
  done;
  print_l (List.rev !debug);
  print_endline (string_of_int !max)

let read_cmds count =
  let rec read (lst: string list) remain =
    match remain with
	0 -> List.rev lst
      | _ -> let str = read_line () in
	     read (str :: lst) (remain - 1)
  in
  read [] count

let read_nums str delim =
  let nstrings = Str.split (Str.regexp delim) str in
  Array.of_list (List.map (fun x -> int_of_string x) nstrings)

let main =
  let s1 = read_nums (read_line ()) " " in
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
