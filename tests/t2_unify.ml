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

(* (F (f a b c) (f e d c)) *)
let term2 = Fun ("F", [(Fun ("f", [(Var "a"); (Var "b"); (Var "c")])); (Fun ("f", [(Var "e"); (Var "d"); (Var "c")]))]);;

(* (F (f a b c) (f e d f) (G (G (g (H (h a b c) (h e d c)) b c)) (g e d f)) ) *)
let term3 = Fun ("F", [Fun("f", [Var "a"; Var "b"; Var "c"]); Fun("f", [Var "e"; Var "d"; Var "c"]); 
Fun ("G", [Fun("G", [
Fun ("g", [Fun ("H", [Fun("h", [Var "a"; Var "b"; Var "c"]);Fun("h", [Var "e"; Var "d"; Var "c"])]); Var "b"; Var "c"])
]) ;Fun ("g", [Var "e"; Var "d"; Var "f"])])
]);;

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

let test_apply_substitution () =
	let go subst a = 
		let rec print_subst s = 
			match s with 
				| hd :: tl -> 
					let (x, y) = hd in
					print_string (x ^ " -> " ^ (term_to_string y) ^ ", "); 
					print_subst tl
				| [] -> print_string ";"
		in	
		let new_term = Hw2_unify.apply_substitution subst a in
		let str_a = term_to_string a in
		let str = term_to_string new_term in
		print_string str;
		print_string " for test: ";
		print_string (str_a ^ " with subst: ");
		print_subst subst;
		print_newline ()
	in
	let subst1 = [("x", term1)] in
	go subst1 term1;
	let subst2 = [("a", term1); ("c", term1)] in
	go subst2 term2
;;

let test_check_solution () =
	let go subst system = 
		let ans = Hw2_unify.check_solution subst system in
		match ans with
			| true -> print_string "True\n"
			| false -> print_string "False\n"
	in
	let system1 = [(term1, (Var "y"))] in
	let sol1 = [("y", term1)] in
	let sol2 = [("y", term2)] in
	go sol1 system1;
	go sol2 system1;
	let term1 = Fun ("F", [Var "x"; Var "y"]) in
	let sol = [("y", Fun ("f", [Var "e"; Var "d"; Var "c"])); ("x", Fun ("f", [Var "a"; Var "b"; Var "c"]))] in
	let system2 = [(term1, term2)] in
	go sol system2;;

(* f();; *)
(* test_system_to_eq ();; *)
(* test_apply_substitution ();; *)
test_check_solution ();;