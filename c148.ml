(* Print result to stdout *)
let rec print_list lst =
  match lst with
    | [] -> ()
    | h :: [] -> print_int h
    | h :: t -> Printf.printf "%s " (string_of_int h); print_list t
in
(* Take "Ogo!" b times, then "Ah!" a times *)
let make_ah_ogo a b =
  (* Sum of list *)
  let rec lsum lst acc =
    match lst with
	[] -> acc
      | h :: tail -> lsum tail (acc + h)
  in
  (* Ogo! *)
  let rec ogo lst b =
    let cur_sum = lsum lst 0 in
    match b with
	0 -> lst
      | _ -> (*Printf.printf "Cur ogo sum:%d\n" cur_sum;*) ogo (cur_sum + 1 :: lst) (b - 1)
  in
  (* Ah! *)
  let rec ah lst a =
    match a with
	0 -> lst
      | _ -> match lst with
	  [] -> ah [1] (a-1)
	  | x :: [] -> ah (x :: x :: []) a
	  | h :: t -> (*Printf.printf "Incr ah %d\n" h;*) ah (h + 1 :: h :: t) (a - 1)
  in
  (* Ogo! list *)
  let logo = ogo [1] b in
  (* Ogo! + Ah! list *)
  ah logo a
in
(* Create resulting list *)
let create inp =
  (* Get n, a, b numbers from stdin *)
  let parse_input x =
    let rx = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
    if Str.string_match rx x 0 then
      let i ii = int_of_string (Str.matched_group ii x) in
      Some (i 1, i 2, i 3)
    else
      None
  in
  let n, a, b = match parse_input inp with
      None -> raise Not_found
    | Some (x1, x2, x3) -> x1, x2, x3
  in
  (*Printf.printf "n=%d a=%d b=%d\n" n a b;*)
  let result = make_ah_ogo a b in
  (* Function to fill the list to make it appropriate length=n *)
  let create_remain todo =
    let rec rem list todo =
      match todo with
	  x when x <= 0 -> list
	| _ -> rem (1 :: list) (todo - 1)
    in
    rem [] todo
  in
  let len_remain = n - (List.length result) in
  (*Printf.printf "Remain to %d length: %d; done %d\n" n len_remain (List.length result);*)
  let result = (List.rev result) @ (create_remain len_remain) in
  let len = List.length result in
  (* Check if list is not longer than "n" *)
  if len <= n then
    result
  else begin
    [-1]
  end
in
let init_data = read_line () in
let init_list = create init_data in
let result_list = init_list in
print_list result_list
