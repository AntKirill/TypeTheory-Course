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
	| [] -> print_string "";
	| hd :: tl -> (print_int hd; print_string " "; print_list tl);;

let rev_list_test() = 
	let l = [-1; 0; 1; 2; 3; 4; 5] in 
	let a = rev l in
	print_string "the reverse of ";
	print_list l;
	print_string "is ";
	print_list a;
	print_newline();;

let sort_list_test() = 
	let l = [1; 0; -2017; 2017; 7; 3; 14; 2; 71] in
	let a = merge_sort l in
	print_string "the result of sort of the list:\n";
	print_list l;
	print_newline();
	print_string "is:\n";
	print_list a;
	print_newline();;

let lambda_test() =
	let go s = 
		let lam = lambda_of_string s in
		let str = string_of_lambda lam in
		print_string str;
		print_newline() in
	go "((((((((\\x.\\y.\\x.y)(\\x.x)(\\x.x)(\\a.b)y )))))))";
	go "(\\x.((\\x.x)(\\a.b)(\\x.\\y.\\x.y)(\\x.x)(\\x.(\\x.\\y.\\x.y)(\\x.x)(\\x.x)(\\a.b)y)(\\a.b)y))y";
	go "(\\x.x)   (\\a.b)     y";
	go "(\\x.x)                                       (\\a.b)";
	go "\\x.\\y.x";
	go "x y a b c d e f g h type theory lambda lambda uraaa z";
	go "x \\x.bbb ur a a a a \\x.bbb";
	go "(\\x.x x) \\y.y y";
	go "(\\x.y y) (a b c d) ((\\x.x x) \\y.y y)";;

lambda_test();;

(* print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x"));; *)
