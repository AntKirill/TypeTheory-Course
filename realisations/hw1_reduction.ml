open Hw1;;

module StringSet = Set.Make (
struct
	type t = string
	let compare = String.compare
end);;

module StringMap = Map.Make (
struct
	type t = string
	let compare = String.compare
end);;

(* Вернуть список имён свободных переменных *)
let free_vars l = 
	let rec set_of_free_vars l = 
		match l with
			| Var x -> StringSet.add x StringSet.empty
			| Abs (x, n) -> StringSet.remove x (set_of_free_vars n)
			| App (m, n) -> StringSet.union (set_of_free_vars m) (set_of_free_vars n)
	in
	let s = set_of_free_vars l in
	StringSet.elements s;;

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst term l var_name = 
	let fv = StringSet.of_list (free_vars term) in
	let rec dfs l bl = 
		(*  1 - нашли вершину с правильным именем, свободную для подстановки
			0 - ни у одной вершины в поддереве не совпало имя с var_name
			-1 - нашил вершину с правильным именем, но она не свободна для подстановки *)
		let ret i1 i2 = 
			if (i1 = 1 || i2 = 1) then 1 
			else if (i1 = 0 && i2 = 0) then 0
			else -1
		in
		let check_var x bl = 
			let intersects s1 s2 = 
				let s_inter = StringSet.inter s1 s2 in not (StringSet.is_empty s_inter)
			in
			if (x <> var_name) then 0
			else if (intersects fv bl) then -1
			else 1
		in
		match l with 
			| App (m, n) -> ret (dfs m bl) (dfs n bl)
			| Var x -> check_var x bl
			| Abs (x, n) -> dfs n (StringSet.add x bl)
	in
	if (dfs l StringSet.empty <> -1) then true
	else false;;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent a b = 
	let is_same var1 var2 mp1 mp2 = 
		let get_mapped x mp =
			if (StringMap.mem x mp) then StringMap.find x mp
			else x
		in
		(get_mapped var1 mp1) = (get_mapped var2 mp2)
	in
	let update_with_fresh_var var new_var mp =
		if (StringMap.mem var mp) then mp
		else StringMap.add var new_var mp
	in
	let rec dfs l1 l2 mp1 mp2 =  
		match (l1, l2) with
			| (Var var1, Var var2) -> is_same var1 var2 mp1 mp2
			| (App (p1, q1), App (p2, q2)) -> (dfs p1 p2 mp1 mp2) && (dfs q1 q2 mp1 mp2)
			| (Abs (var1, p1), Abs (var2, p2)) -> 
			dfs p1 p2 (update_with_fresh_var var1 ("_"^var1) mp1) (update_with_fresh_var var2 ("_"^var1) mp2)
			| _ -> false

	in
	dfs a b StringMap.empty StringMap.empty;;

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form p = 
	let rec inf p = 
		let test_parts m n = 
			match m with
				| Abs(x, p) -> if (free_to_subst n p x) then false else inf p
				| _ -> (inf m) && (inf n)
		in
		match p with
			| App(m, n) -> test_parts m n
			| Abs(x, m) -> inf m
			| _ -> true
	in inf p;;

(* Проверить возможноесть подстановки. Параметры:
что подставляем, где подставляем, вместо чего подставляем*)
(* (\\var_name.l) term *)
let try_to_subst term l var_name = 
	let fv = StringSet.of_list (free_vars term) in
	let subst_state = ref (-2) in
	let rec dfs l bl = 
		(*  1 - нашли вершину с правильным именем, свободную для подстановки
			0 - ни у одной вершины в поддереве не совпало имя с var_name
			-1 - нашил вершину с правильным именем, но она не свободна для подстановки *)
		let update_subst_state i1 i2 = 
			if (i1 = 1 || i2 = 1) then subst_state := 1 
			else if (i1 = 0 && i2 = 0) then subst_state := 0
			else subst_state := -1
		in
		let check_var x bl = 
			let intersects s1 s2 = 
				let s_inter = StringSet.inter s1 s2 in not (StringSet.is_empty s_inter)
			in
			if (x <> var_name) then (subst_state := 0; Var x)
			else if (intersects fv bl) then (subst_state := -1; Var x)
			else (subst_state := 1; term)
		in
		match l with 
			| App (m, n) -> 
				let new_left_term = dfs m bl in
				let done_left = !subst_state in 
				let new_right_term = dfs n bl in
				let done_right = !subst_state in
				update_subst_state done_left done_right;
				App (new_left_term, new_right_term)
			| Var x -> check_var x bl
			| Abs (x, n) -> Abs (x, (dfs n (StringSet.add x bl)))
	in
	let new_term = dfs l StringSet.empty in
	match !subst_state with 
		| -1 -> (false, new_term)
		| 0 -> (true, l)
		| 1 -> (true, new_term)
		| _ -> (false, new_term)
;;

let _normal_beta_reduction_impl a = 
	let is_step_done = ref false in
	let rec dfs term = 
		let _app left right = 
			(* (\\x.p) m *)
			let part_case left right = 
				let new_left = dfs left in 
				if !is_step_done then App(new_left, right)
				else let new_right = dfs right in
				App (new_left, new_right)
			in
			let redex_case x p m = 
				let (is_done, new_term) = try_to_subst m p x in
				if is_done then (is_step_done := true; new_term)
				else part_case p m 
			in
			match left with Abs (x, p) -> redex_case x p right
				| _ -> part_case left right
		in
		match term with 
			| App (m, n) -> _app m n
			| Abs (x, m) -> Abs (x, (dfs m))
			| t -> t
	in
	let new_term = dfs a in (!is_step_done, new_term)	
;;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction a = 
	let (is_step_done, new_term) = _normal_beta_reduction_impl a in
	new_term
;;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let reduce_to_normal_form a = 
	let rec do_step a = 
		let (is_step_done, new_term) = _normal_beta_reduction_impl a in
		if is_step_done then do_step new_term
		else a
	in
	do_step a
;;