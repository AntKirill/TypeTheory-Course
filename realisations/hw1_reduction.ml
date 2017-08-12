open Hw1;;

module StringSet = Set.Make (
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
	else false
;;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent a b = failwith "Not implemented yet";;

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form a = failwith "Not implemented yet";;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction a = failwith "Not implemented yet";;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let reduce_to_normal_form a = failwith "Not implemented yet";;