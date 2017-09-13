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
	go "(\\x.y) (\\xx.y) (\\xxx.yyy)" "(\\xxx.y) (\\xx.y) (\\x.yyy)";;

let tests_for_reduction_1 = [
	"x";	
	"\\x.y";
	"\\x.x";
	"(\\x.x)(\\z.y)";
	"(\\x.(\\x.x) (\\x.x)) (\\y.z)";
	"(\\f.\\x.f (f (f (f x)))) (\\f.\\x.f (f (f (f x))))";
	"(\\f.\\x.f (f (f (f (f x))))) (\\f.\\x.f (f (f (f (f x)))))";
	"(\\x.x x x) (\\x.x) (\\x.x)"
(* 	"(\\x.x x) \\y.y y";
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
	"((\\x.(x) (x))) ((\\x.(x) (x)))" *)
];;

let tests_for_reduction_2 = [
	"x";	
	"\\x.y";
	"\\x.x";
	"(\\x.x)(\\z.y)";
	"(\\x.(\\x.x) (\\x.x)) (\\y.z)";
	"(\\f.\\x.f (f (f (f x)))) (\\f.\\x.f (f (f (f x))))";
	"(\\f.\\x.f (f (f (f (f x))))) (\\f.\\x.f (f (f (f (f x)))))";
	"(\\x.x x x) (\\x.x) (\\x.x)";
	"((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\l13.((l13 (\\l14.(\\l15.(l14 (l14 l15))))) (\\l14.(\\l15.(l14 (l14 (l14 l15))))))) (\\l13.(\\l14.(((l0 (\\l15.(\\l16.(\\l17.(((l1 (l10 l16)) (l12 l17)) (((l1 (l10 l17)) ((l15 (l11 l16)) (\\l18.(\\l19.(l18 l19))))) ((l15 (l11 l16)) ((l15 l16) (l11 l17))))))))) l13) l14))))) (\\l12.(\\l13.(\\l14.((l12 l13) (l13 l14))))))) (\\l11.(\\l12.(\\l13.(((l11 (\\l14.(\\l15.(l15 (l14 l12))))) (\\l14.l13)) (\\l14.l14))))))) (\\l10.((l10 (\\l11.l3)) l2)))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\l13.(((l1 l12) l13) (((l1 l13) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))";
	"\\a.\\b.a (a (a (a (a (a (a (a (a b))))))))"
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
test_reduce_to_normal_form();;
(* test_is_alpha_equivalent();; *)
