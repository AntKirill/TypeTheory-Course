open Hw1_reduction;;
open Hw1;;

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
;;

let test_free_vars () = 
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
	go "\\x.\\x.\\y.y" "\\x.x" "zzzzzzzzz"
;;

test_free_to_subst();;
