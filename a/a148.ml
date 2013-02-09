let calc k l m n d =
	let total = ref 0 in
	for i = 1 to d do
		if (i mod k) = 0 || (i mod l) = 0 || (i mod m) = 0 || (i mod n) = 0 then
			total := !total + 1
	done;
	!total
in
let k = read_int() in
let l = read_int() in
let m = read_int() in
let n = read_int() in
let d = read_int() in
print_int (calc k l m n d)
