open Hw2_unify;;
open List;;

let term_to_string a = 
	let rec dfs x = 
		match x with
			| Var (var_name) -> var_name
			| Fun (fun_name, arguments) -> "(" ^ fun_name ^ 
			(List.fold_left (fun prv term ->  prv ^ " " ^ (dfs term)) "" arguments) ^ ")"
	in
	dfs a;;


(* (F x) *)
let term1 = Fun ("F", [Var "x"]);;

(* (F (f a b c) (f e d f)) *)
let term2 = Fun ("F", [(Fun ("f", [(Var "a"); (Var "b"); (Var "c")])); (Fun ("f", [(Var "e"); (Var "d"); (Var "c")]))]);;

(* (F (f a b c) (f e d f) (G (G (g (H (h a b c) (h e d c)) b c)) (g e d f)) ) *)
let term3 = Fun ("F", [Fun("f", [Var "a"; Var "b"; Var "c"]); Fun("f", [Var "e"; Var "d"; Var "c"]); 
Fun ("G", [Fun("G", [
Fun ("g", [Fun ("H", [Fun("h", [Var "a"; Var "b"; Var "c"]);Fun("h", [Var "e"; Var "d"; Var "c"])]); Var "b"; Var "c"])
]) ;Fun ("g", [Var "e"; Var "d"; Var "f"])])
]);;

let test_system_to_eq () = 
	let go a = 
		let (ans_left, ans_right) = Hw2_unify.system_to_equation a in
		let (ans_left_str, ans_right_str) = (term_to_string ans_left, term_to_string ans_right) in
		print_string ans_left_str;
		print_string " = ";
		print_string ans_right_str;
		print_newline()
	in
	let system1 = [(term1, term2)] in
	let system2 = [(Var "x", Var "y")] in
	let system3 = [(term1, term2); (Var "x", Var "y"); (term2, term3); (term1, term3)] in
	go system1;
	go system2;
	go system3;;

let f () =
	let go aterm = 
		let str = term_to_string aterm in
		print_string str;
		print_newline ()
	in
	go (Var "x");
	go term1;
	go term2;
	go term3;;

(* f();; *)

test_system_to_eq ();;