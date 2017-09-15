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


let fresh_var_counter = ref 0;;

type lamRef = VarRef of string
	| AbsRef of (string * lamRef ref)
	| AppRef of (lamRef ref * lamRef ref);;

let rec to_lamRef lam = 
	match lam with
	| Var a -> ref (VarRef a)
	| App (a, b) -> ref (AppRef (to_lamRef a, to_lamRef b))
	| Abs (a, b) -> ref (AbsRef (a, to_lamRef b));;

let rec from_lamRef lr = 
	match !lr with
	| VarRef a -> Var a
	| AppRef (a, b) -> App (from_lamRef a, from_lamRef b)
	| AbsRef (a, b) -> Abs (a, from_lamRef b);;

let generate_fresh_var () = 
	fresh_var_counter := !fresh_var_counter + 1;
	("fresh$" ^ string_of_int !fresh_var_counter);;

let rec to_alpha_eq l map = 
	match l with
	| Var a -> if StringMap.mem a map then Var (StringMap.find a map) else l
	| App (a, b) -> App(to_alpha_eq a map, to_alpha_eq b map)
	| Abs (a, b) -> 
		let new_var = generate_fresh_var () in
		Abs(new_var, to_alpha_eq b (StringMap.add a new_var map));;

let _normal_beta_reduction_impl lr = 
	let rec try_to_subst term lr var = 
		match !lr with
		| VarRef a -> if a = var then lr := !term
	  	| AppRef (a, b) -> try_to_subst term a var; try_to_subst term b var
	  	| AbsRef (a, b) -> if a <> var then try_to_subst term b var
	in
	let rec reduction_impl lr = 
		let app_case a b = 
			match !a with					
			| AbsRef (x, y) ->
				let fresh_var = generate_fresh_var () in
				lr := !(to_lamRef (to_alpha_eq (from_lamRef y)
				(StringMap.singleton x fresh_var)));
				try_to_subst b lr fresh_var;
				Some lr
			| _ -> (
				match reduction_impl a with Some x -> Some lr | None -> (
					match reduction_impl b with
					| Some x -> Some lr
					| None -> None
				))
		in
		match !lr with
		| VarRef a -> None
		| AppRef (a, b) -> app_case a b
		| AbsRef (a, b) -> (
			match reduction_impl b with
			| Some x -> Some lr
			| None -> None
		)
	in reduction_impl lr;;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction l = 
	let lr = to_lamRef (to_alpha_eq l StringMap.empty) in
	let new_lr = _normal_beta_reduction_impl lr in 
	fresh_var_counter := 0;
	match new_lr with
	| Some x -> from_lamRef x
	| None -> l;;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let rec reduce_to_normal_form l = 
	let lr = to_lamRef (to_alpha_eq l StringMap.empty) in	
	let rec do_step r = 
		let new_ref = _normal_beta_reduction_impl r in
		match new_ref with
		| Some x -> do_step r
	  	| None -> r
	in
	fresh_var_counter := 0;
	let new_lambda = from_lamRef (do_step lr) in 
	new_lambda;;
