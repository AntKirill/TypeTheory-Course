open Hw1;;

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let free_to_subst a b c = failwith "Not implemented yet";;

(* Вернуть список имён свободных переменных *)
let free_vars a = failwith "Not implemented yet";;

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form a = failwith "Not implemented yet";;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let is_alpha_equivalent a b = failwith "Not implemented yet";;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction a = failwith "Not implemented yet";;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let reduce_to_normal_form a = failwith "Not implemented yet";;