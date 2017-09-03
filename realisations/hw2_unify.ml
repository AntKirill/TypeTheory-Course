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

let check_solution subst system = 
	let is_equal eq = 
		let (lhs, rhs) = eq in
		let (lhs, rhs) = (apply_substitution subst lhs, apply_substitution subst rhs) in 
		let rec dfs lhs rhs = 
			match (lhs, rhs) with 
				| (Var x, Var y) -> x = y
				| (Fun (name1, l1), Fun (name2, l2)) -> (name1 = name2) && (List.length l1 = List.length l2) &&
					let l = List.combine l1 l2 in 
					List.for_all (fun x -> let (a, b) = x in (dfs a b)) l
				| _ -> false
		in 
		dfs lhs rhs
	in
	List.for_all is_equal system;;
	
let solve_system x = failwith "Not implemented";;
