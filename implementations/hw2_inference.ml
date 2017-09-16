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

module StringSet = Set.Make(String);;
exception SmthWentWrong of string;; 

let algorithm_w hm_lam = 	
	let fresh_name_counter = ref 0 in
	let generate_fresh_name () = 
		fresh_name_counter := !fresh_name_counter + 1;
		"ß" ^ (string_of_int !fresh_name_counter )
	in
	let rec to_aterm hmt = 
		match hmt with
	  	| HM_Arrow(x, y) -> Hw2_unify.Fun ("aw",  [(to_aterm x); (to_aterm y)])
		| HM_Elem v -> Hw2_unify.Var v
	  	| _ -> failwith "Smth went wrong" 
	in	 
	let rec from_aterm term = 
		match term with
	  	| Hw2_unify.Fun(name, [x; y]) -> HM_Arrow (from_aterm x, from_aterm y)
		| Hw2_unify.Var v -> HM_Elem v
	  	| _ -> failwith "Smth went wrong" 
	in


	let rec algorithm_w_impl hmlmb types = 

		let rec try_to_subst subst hmt set_of_vars = 
			match hmt with
		  	| HM_ForAll (x, y) -> HM_ForAll(x, try_to_subst subst y (StringSet.add x set_of_vars))
		  	| HM_Arrow (x, y) -> HM_Arrow(try_to_subst subst x set_of_vars, try_to_subst subst y set_of_vars)
			| HM_Elem x -> 
				if StringSet.mem x set_of_vars then hmt
				else if StringMap.mem x subst then StringMap.find x subst
				else hmt
		in
		let combine_substs f1 f2 = 
			StringMap.fold 
				(fun x y m -> 
				if StringMap.mem x m then m else StringMap.add x y m) f1
				(StringMap.fold (fun x y m -> StringMap.add x (try_to_subst f1 y StringSet.empty) m)
				f2 StringMap.empty) 
		in
		let infer_subst_types subst tps = 
			StringMap.fold (fun x y m -> StringMap.add x (try_to_subst subst y StringSet.empty) m)
			tps StringMap.empty
		in

		let var_case v =
			let rec del hmt = 
				match hmt with
				| HM_ForAll(x, y) -> try_to_subst (StringMap.add x (HM_Elem(generate_fresh_name ())) StringMap.empty)
					(del y) StringSet.empty
			  	| _ -> hmt 
			in
			if StringMap.mem v types
			then (del (StringMap.find v types), StringMap.empty)
			else raise (SmthWentWrong "Any type")
		in
		let abs_case x y =
	  		let fresh_type = HM_Elem (generate_fresh_name ()) in
			let (hmt, types) = algorithm_w_impl y (StringMap.add x fresh_type (StringMap.remove x types)) in
			(HM_Arrow(try_to_subst types fresh_type StringSet.empty, hmt), types)
		in
		let app_case lhs rhs =
		 	let (hmt_left, types_left) = algorithm_w_impl lhs types in
			let (hmt_right, types_right) = algorithm_w_impl rhs (infer_subst_types types_left types) in
			let fresh_type = HM_Elem (generate_fresh_name ()) in
			match solve_system ([((to_aterm (try_to_subst types_right hmt_left StringSet.empty)),
				(to_aterm (HM_Arrow(hmt_right, fresh_type))))]) with
		  	| Some x -> 	
		  		let finale_types = combine_substs 
				(List.fold_left (fun mp (s, term) -> StringMap.add s (from_aterm term) mp)
				StringMap.empty x) (combine_substs types_right types_left) in
				(try_to_subst finale_types fresh_type StringSet.empty, finale_types)
			| None -> raise (SmthWentWrong "Incompatible system")
		in
		let let_case x y z =
			let _add hmt types = 
				let rec unblock hmt blocked = 
					match hmt with
					| HM_Elem v -> if StringSet.mem v blocked then StringSet.empty
						else StringSet.singleton v
				  	| HM_Arrow (x, y) -> StringSet.union (unblock x blocked) (unblock y blocked)
				  	| HM_ForAll (x, y) -> unblock y (StringSet.add x blocked)
				in
				let get_known_types tps = 
					 StringMap.fold (fun extra x s -> 
					 	StringSet.union (unblock x StringSet.empty) s) tps StringSet.empty
				in
				let known_types = get_known_types types in
				StringSet.fold (fun x y -> HM_ForAll(x, y)) 
				(StringSet.fold (fun x y -> 
					match StringSet.mem x known_types with
					| false -> StringSet.add x y
					| true -> y)
				(unblock hmt StringSet.empty) StringSet.empty) hmt 
			in	
	  		let (hmt1, types1) = algorithm_w_impl y types in 
			let fresh_types = infer_subst_types types1 types in 
			let (hmt2, types2) = algorithm_w_impl z (StringMap.add x (_add hmt1 fresh_types)
			(StringMap.remove x fresh_types)) in (hmt2, combine_substs types2 types1)
		in

		match hmlmb with
		| HM_Var v -> var_case v
	  	| HM_Abs (x, y) -> abs_case x y
	  	| HM_App (lhs, rhs) -> app_case lhs rhs
	  	| HM_Let (x, y, z) -> let_case x y z
	in


	let types = StringSet.fold (fun x map -> StringMap.add x (HM_Elem (generate_fresh_name ())) map)
	((fun x -> 
		let rec f hmlmb set = 
			match hmlmb with
			| HM_Var v -> if StringSet.mem v set then StringSet.empty else StringSet.singleton v
		  	| HM_Abs (x, y) -> f y (StringSet.add x set)
		  	| HM_App (lhs, rhs) -> StringSet.union (f lhs set) (f rhs set)
		  	| HM_Let (x, y, z) -> StringSet.union (f y set) (f z (StringSet.add x set))
		in f x StringSet.empty) hm_lam) StringMap.empty in
	try let (hmt, map) = algorithm_w_impl hm_lam types in Some (StringMap.bindings map, hmt) with
	(SmthWentWrong sww) -> None;;