type peano = Z | S of peano;; (* типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = if x = 0 then Z else S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = S (x);;
let rec add x y = match y with
				Z -> x
				| S c -> S (add x c)
			;;
let rec mul x y = match y with
				Z -> Z
				| S c -> add (mul x c) x;;
let rec sub a b = match (a, b) with
				(S c , Z) -> a
				| (S c, S d) -> sub c d
				| (Z, Z) -> Z
				| _ -> failwith "second argument is greater"
			;;
let rec power x y = match y with
				Z -> S Z
				| S c -> mul (power x c) x;;
                     
let rec revl cur l = match l with 
						[] -> cur
					|	hd :: tl -> revl (hd :: cur) tl
				;;

let rev x = (revl [] x);;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;