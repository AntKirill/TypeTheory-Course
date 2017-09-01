open Hw1_reduction;;
open Hw1;;

let test_free_vars () = 
	let test_all f = 
		let tests = [
			"\\x.y";
			"\\x.x";
			"(\\x.x x) \\y.y y";
			"(\\x.x x) \\y.y y z";
			"((((((((\\x.\\y.\\x.y)(\\x.x)(\\x.x)(\\a.y)b)))))))";
			"(\\x.x)\\y.x";
			"(\\x.x)(\\z.y)"
		] in
		let rec ta l = match l with
			| hd :: tl -> f hd; ta tl
			| [] -> print_string "Done\n"; print_newline()
		in
		ta tests
	in
	let print_list l =
		let rec pl l = 
			match l with
				| head :: tail -> print_string head; print_string ","; pl tail
				| [] -> print_char '}'; print_newline()
		in
		print_char '{';
		pl l
	in
	let go str =
		let a = free_vars (lambda_of_string str) in
		print_list a
	in
	test_all go
;;

let test_free_to_subst () = 
	let go term l str =
		if Hw1_reduction.free_to_subst (lambda_of_string term) (lambda_of_string l) str then print_string "True\n"
		else print_string "False\n"
	in
	go "y" "\\x.y" "y";
	go "x" "\\x.y" "y";
	go "x" "\\x.y" "z";
	go "x" "\\x.x" "x";
	go "x" "\\z.x" "x";
	go "x" "(\\x.x) \\z.x" "x";
	go "\\x.x" "\\x.x" "x";
	go "\\y.x" "\\x.x" "x";
	go "\\x.\\x.\\y.y" "\\x.x" "x";
	go "\\x.\\x.\\y.y" "\\x.x" "zzzzzzzzz";
	go "p" "\\x.y" "x"
;;

let test_is_alpha_equivalent () = 
	let go s1 s2 = 
		if Hw1_reduction.is_alpha_equivalent (lambda_of_string s1) (lambda_of_string s2) then print_string "True\n"
		else print_string "False\n"
	in
	go "x" "y";
	go "y" "y";
	go "abc" "abc";
	go "abc" "abcd";
	go "\\x.x" "\\x.x";
	go "\\x.x" "\\y.y";
	go "\\x.x" "x x";
	go "\\x.y" "\\x.z";
	go "(\\x.y)" "(\\xxx.y)";
	go "(\\x.y) (\\xx.y)" "(\\xxx.y) (\\xx.y)";
	go "(\\x.y) (\\xx.y) (\\xxx.yyy)" "(\\xxx.y) (\\xx.y) (\\x.yyy)"
;;

let tests_for_reduction_1 = [
	"x";	
	"\\x.y";
	"\\x.x";
	"(\\x.x)(\\z.y)";
	"(\\x.(\\x.x) (\\x.x)) (\\y.z)";
	"(\\f.\\x.f (f (f (f x)))) (\\f.\\x.f (f (f (f x))))";
	"(\\f.\\x.f (f (f (f (f x))))) (\\f.\\x.f (f (f (f (f x)))))";
	"(\\x.x x x) (\\x.x) (\\x.x)";
	"(\\x.x x) \\y.y y";
	"(\\x.x x) \\y.y y z";
	"((((((((\\x.\\y.\\x.y)(\\x.x)(\\x.x)(\\a.y)b)))))))";
	"(\\x.x)\\y.x";
	"x";
	"\\x.x";
	"(\\x.x) y";
	"(\\x.\\y.x) y";
	"x x x x x x x xxxxxxxxx";
	"(\\x.x x) \\x.x x";
	"(\\a.b) x";
	"\\y.(\\a.b) x";
	"(\\x.\\y.(\\a.b) x) y";
	"(\\x.\\y.(\\a.\\x.a) x) y";
	"(\\x.y) (\\xx.y) (\\xxx.yyy)";
	"((\\x.(x) (x))) ((\\x.(x) (x)))"
];;

let tests_for_reduction_2 = [
	"(\\x.x x x) (\\x.x) (\\x.x)";
	"(\\x.x x x) ((\\x.x) (\\x.x))";
	"(\\f.\\x.f (f (f (f x)))) (\\f.\\x.f (f (f (f x))))";
	"(\\f.\\x.f (f (f (f (f x))))) (\\f.\\x.f (f (f (f (f x)))))";	
	"(\\x.x x) (\\x.x x)";
];;

let test_reduction tests f = 
	let apply f t =
		f t;
		print_string " for test: ";
		print_string t;
		print_newline()
	in
	let rec ta l = match l with
		| hd :: tl -> apply f hd; ta tl
		| [] -> print_string "Done\n"; print_newline()
	in
	ta tests;;

let test_is_normal_form () = 
	let go str = 
		let a = lambda_of_string str in
		if Hw1_reduction.is_normal_form a then print_string "True"
		else print_string "False"
	in
	test_reduction tests_for_reduction_1 go;;

let test_normal_beta_reduction () = 
	let go str = 
		let a = lambda_of_string str in
		let reduced_l = Hw1_reduction.normal_beta_reduction a in
		let str2 = string_of_lambda reduced_l in print_string str2
	in
	test_reduction tests_for_reduction_1 go;;

let test_try_to_subst () = 
	let go str_m str_n var_name = 
		let g () = 
			let f new_lambda = 
				let str_lambda = string_of_lambda new_lambda in 
				print_string "substituted OK, ";
				print_string str_lambda;
			in
			let m = lambda_of_string str_m in
			let n = lambda_of_string str_n in
			let (substituted, new_lambda) = Hw1_reduction.try_to_subst m n var_name in
			if substituted = false then print_string "NOT substituted "
			else f new_lambda
		in
		g ();
		print_string " For test: ";
		print_string (str_m ^ ", " ^ str_n ^ ", " ^ var_name);
		print_newline ()
	in
	go "x" "y" "y";
	go "x" "\\y.y" "y";
	go "\\x.x" "\\y.y" "y";
	go "\\x.x" "\\x.y" "y";
	go "\\x.x" "((\\a.a) (\\a.b)) (\\c.d)" "a";
	go "\\x.x" "((\\a.a) (\\a.b)) (\\c.d)" "b";
	go "\\x.x" "((\\a.a) (\\a.b)) (\\c.d)" "d";
	go "\\x.x" "((\\a.d) (\\a.d)) (\\c.d)" "d";
	go "\\x.x" "((\\a.a) (\\a.b)) (\\c.d)" "c";
	go "x" "\\x.x" "x";
	go "(\\x.x) x" "((\\x.a) (\\a.b)) (\\c.d)" "a";
	go "y" "x x" "x";
	go "(\\x.x) (\\x.x)" "x x x" "x"
;;

let test_reduce_to_normal_form () = 
	let go str = 
		let a = lambda_of_string str in
		let reduced_l = Hw1_reduction.reduce_to_normal_form a in
		let is_done = is_normal_form reduced_l in
		let str2 = string_of_lambda reduced_l in 
		match is_done with
			| true -> print_string str2
			| false -> print_string "NOT NORMAL FORM!!! "; print_string str2
	in
	test_reduction tests_for_reduction_2 go;;

(* test_normal_beta_reduction ();; *)
(* test_try_to_subst ();; *)
test_reduce_to_normal_form();;
