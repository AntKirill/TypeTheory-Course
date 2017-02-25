open Hw1;;

let out s a = (print_string s; print_int a; print_newline());;

let peano_arifmetics_test() =    
	let a = int_of_string(Sys.argv.(1)) in
	let b = int_of_string(Sys.argv.(2)) in
	let parceda = peano_of_int a in
	let parcedb = peano_of_int b in
	let ans_inc = int_of_peano (inc parceda) in
	let ans_add = int_of_peano(add parceda parcedb) in
	let ans_sub = int_of_peano(sub parceda parcedb) in
	let ans_mul = int_of_peano(mul parceda parcedb) in
	let ans_deg = int_of_peano(power parceda parcedb) in
	out "inc of first arg is: " ans_inc;
	out "sum of args is: " ans_add;
	out "sub second from first is: " ans_sub;
	out "mult of args is: " ans_mul;
	out "first agr powered second is: " ans_deg;;

let rec print_list x = match x with
						[] -> print_string "";
					|	hd :: tl -> (print_int hd; print_string " "; print_list tl);;


let rev_list_test() = 
	let l = -1 :: 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: [] in 
	let a = rev l in
	print_string "the reverse of ";
	print_list l;
	print_string "is ";
	print_list a;
	print_newline();;

let sort_list_test() = 
	let l = 5 :: -4 :: 1 :: 20 :: 3 :: -2123 :: 5 :: 20 :: [] in
	let a = merge_sort l in
	print_string "the result of sort of the list:\n";
	print_list l;
	print_newline();
	print_string "is:\n";
	print_list a;
	print_newline();;

sort_list_test();;

(*
print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));;
*)