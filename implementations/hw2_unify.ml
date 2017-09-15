type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let system_to_equation x = 
	let make_fun_name lhs_list rhs_list = 
		let name_from_list l =
			let rec get_all_functions_names t = 
				match t with
					| Var x -> ""
					| Fun (name, l) -> name ^ (List.fold_left f "" l)
			and f all_prev_names term = 
				all_prev_names ^ (get_all_functions_names term)
			in
			List.fold_left f "" l
		in
		(name_from_list lhs_list) ^ (name_from_list rhs_list)
	in
	let (lhs_list, rhs_list) = List.split x in
	let fresh_name = make_fun_name lhs_list rhs_list in
	let fresh_name = "super_fresh_name_" ^ fresh_name in
	(Fun(fresh_name, lhs_list), Fun(fresh_name, rhs_list));;

let apply_substitution subst term = 
	let rec find subst x = 
		match subst with
			| hd :: tl -> 
				let (str, y) = hd in
				if str = x then y
				else find tl x
			| [] -> Var x
	in
	let rec apply subst term = 
		match term with 
			| Fun (name, l) -> Fun (name, (List.map (fun x -> apply subst x) l))
			| Var x -> find subst x
	in
	apply subst term;;

let is_equal eq = 
	let (lhs, rhs) = eq in
	let rec dfs lhs rhs = 
		match (lhs, rhs) with 
			| (Var x, Var y) -> x = y
			| (Fun (name1, l1), Fun (name2, l2)) -> (name1 = name2) && (List.length l1 = List.length l2) &&
				let l = List.combine l1 l2 in 
				List.for_all (fun x -> let (a, b) = x in (dfs a b)) l
			| _ -> false
	in 
	dfs lhs rhs;;

let check_solution subst system = List.for_all 
	(fun x ->
	let (a, b) = x in
	let (lhs, rhs) = (apply_substitution subst a, apply_substitution subst b) in 
	is_equal (lhs,rhs)) system;;

exception Incompatible;;
exception SomethingWentWrong;;


let solve_system system =
	let one_step_robinson system = 
		let is_step_done = ref false in

		(* term = x, term is not var ==> x = term *)
		let rule1 system =
			let check_compatible var_name term =
				let rec check x term = 
					match term with
					| Fun (name, l) -> List.for_all (fun t -> check x t) l 
					| Var v -> if x = v then false else true
				in let ok = (check var_name term) in
				if (not ok) then raise Incompatible
			in
			List.map (fun x -> match x with 
				| (Fun (a, b), Var v) -> is_step_done := true; check_compatible v (Fun(a, b)); (Var v, Fun (a, b)) 
				| (Var x, t) -> check_compatible x t; (Var x, t)
				| other -> other) system
		in

		(* (f Q1 Q2 .. Qn) = (f P1 P2 .. Pn) ==> [Q1 = P1; Q2 = P2; ..; Qn = Pn] *)
		let rule2 system =
			let rec open_good_terms new_system old_system =
				match old_system with
				| hd :: tl ->
					let is_good eq =
						match eq with
						| (Fun (name1, l1), Fun (name2, l2)) -> 
							if (name1 = name2) && (List.length l1 = List.length l2) then true 
							else raise Incompatible
						| _ -> false
					in
					let open_good eq = 
						is_step_done := true;
						match eq with 
						| (Fun (name1, l1), Fun (name2, l2)) -> List.combine l1 l2
						| _ -> raise SomethingWentWrong
					in
					if (is_good hd) then open_good_terms (List.append new_system (open_good hd)) tl
					else open_good_terms (hd :: new_system) tl
				| [] -> new_system
			in
			open_good_terms [] system
		in

		(* x = Q, P = T, x enties in P or T ==> P[x := Q] = T[x := Q]; x = Q *)
		let rule3 system = 
			if (!is_step_done) then system else
			let try_to_subst y term var_name = 
				let rec impl y term var_name = 
					match term with
					| Fun (name, l) -> Fun (name, (List.map (fun t -> impl y t var_name) l))
					| Var x -> 
						let check x = 
							if x = var_name then (is_step_done := true; y) else (Var x) in 
						check x
				in impl y term var_name
			in
			let rec loop2 new_system var_name term system =
				match system with 
				| hd :: tl -> 
					let is_same var_name term eq = 
						let (lhs, rhs) = eq in 
						let v = (Var var_name) in
						match (v, lhs) with
							| (Var a, Var b) -> (a = b) && (is_equal (rhs, term))
							| _ -> false
					in
					if (is_same var_name term hd) then loop2 (hd :: new_system) var_name term tl
					else let (lhs, rhs) = hd in
						let new_left_term = try_to_subst term lhs var_name in
						let new_right_term = try_to_subst term rhs var_name in 
						let new_hd = (new_left_term, new_right_term) in
						loop2 (new_hd :: new_system) var_name term tl
				| [] -> new_system
			in
			let rec loop1 l = 
				match l with
				| hd :: tl -> 
					let is_good eq = 
						match eq with
						| (Var x, t) -> true
						| _ -> false
					in
					if (is_good hd) then 
						let new_system = loop2 []
						((fun eq -> match eq with (Var x, t) -> x | _ -> raise SomethingWentWrong) hd)
						((fun eq -> match eq with (Var x, t) -> t | _ -> raise SomethingWentWrong) hd) system in
						if !is_step_done then new_system
						else loop1 tl
					else loop1 tl
				| [] -> system
			in
			loop1 system
		in

		(* delete Q = Q *)
		let rule4 system =
			let rec del_useless new_system system = 
				match system with
				| hd :: tl -> 
					if (is_equal hd) then (is_step_done := true; del_useless new_system tl)
					else del_useless (hd :: new_system) tl
				| [] -> new_system
			in
			del_useless [] system
		in

		let rules = [rule1; rule2; rule4; rule3] in 
		let apply functions obj = 
			let new_system = List.fold_left (fun x f -> f x) obj rules in 
			(!is_step_done, new_system)
		in
		apply rules system
	in
	let rec solve system = 
		let (is_step_done, new_system) = one_step_robinson system in
		match is_step_done with
		| true -> solve new_system
		| false -> List.map (fun x -> match x with (Var v, term) -> (v, term) | _ -> raise SomethingWentWrong) system
	in 
	try Some (solve system) with _ -> None
;;
