open Hw1
open Hw1_reduction
open Hw2_unify

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

let rec to_aterm stype = 
	match stype with
	| S_Arrow(a, b) -> Hw2_unify.Fun("aw", [(to_aterm a); (to_aterm b)])
	| S_Elem a -> Hw2_unify.Var a
;;

let rec from_aterm term = 
	match term with
	| Hw2_unify.Fun(name, [left; right]) -> S_Arrow (from_aterm left, from_aterm right)
	| Hw2_unify.Var a -> S_Elem a
	| _ -> S_Elem "omg"
;;

module StringMap = Map.Make(String)

let infer_simp_type l =
	let have_done = ref false in 
	let ans = ref ([], S_Elem "bag") in 
	let impl l = 
		let fresh_type_counter = ref 0 in
		let generate_fresh_type () = 
			fresh_type_counter := !fresh_type_counter + 1;
			"σ" ^ string_of_int !fresh_type_counter 
		in
		let rec make_system lam name_to_type = 
			match lam with
			| Hw1.Var a -> ([], StringMap.find a name_to_type)
			| Hw1.App(a, b) -> 
				let (s_left, t_left) = make_system a name_to_type in
				let (s_right, t_right) = make_system b name_to_type in
				let new_type = S_Elem(generate_fresh_type ()) in
				(List.append s_left (List.append s_right ([(t_left, S_Arrow(t_right, new_type))])), new_type)
			| Hw1.Abs(a, b) -> 
				let add_types = StringMap.add a (S_Elem (generate_fresh_type ())) name_to_type in
				let system, tp = make_system b add_types in
				(system, S_Arrow(StringMap.find a add_types, tp)) 
		in
		if !have_done then !ans
		else 
		let rec give_types lst name_to_type =
		 	match lst with
			| [] -> name_to_type
			| (hd :: tl) -> give_types tl (StringMap.add hd (S_Elem(generate_fresh_type ())) name_to_type) 
		in
		let new_ans = make_system l (give_types (free_vars l) StringMap.empty) in 
		ans := new_ans;
		have_done := true;
		new_ans
	in
	let make_system l = 
		let s, t = impl l in s
	in
	let solve_system system =
		Hw2_unify.solve_system system 
	in
	let get_type l =
		let s, t = impl l in t
	in
	let apply subst t =
		match subst with
		| None -> failwith "Impossible"
		| Some subst -> Hw2_unify.apply_substitution subst t
	in
	let type_system = make_system l in
	let aterm_system = List.map (fun (lhs, rhs) -> (to_aterm lhs, to_aterm rhs)) type_system in 
	let aterm_solution = solve_system aterm_system in 
	let to_type_solution subst = 
		match subst with
		| None -> None
		| Some x -> Some (List.map (fun (name, rhs) -> (name, from_aterm rhs)) x)
	in
	let type_solution = to_type_solution aterm_solution in 
	match type_solution with 
	| None -> None
	| Some type_solution ->
		let dep_type = get_type l in
		let dep_aterm = to_aterm dep_type in 
		let aterm = apply aterm_solution dep_aterm in 
		let l_type = from_aterm aterm in 
		Some (type_solution, l_type)
;;


type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let algorithm_w hm_lam = failwith "Not implemented yet";;