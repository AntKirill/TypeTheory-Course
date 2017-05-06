type peano = Z | S of peano;; (* òèïû íåîáõîäèìî êîïèðîâàòü â ðåàëèçàöèþ *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = if x = 0 then Z else S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
	| Z -> 0
	| S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = match y with
	| Z -> x
	| S c -> S (add x c);;

let rec mul x y = match y with
	| Z -> Z
	| S c -> add (mul x c) x;;

	let rec sub a b = match (a, b) with
	| (S c , Z) -> a
	| (S c, S d) -> sub c d
	| (Z, Z) -> Z
	| _ -> failwith "second argument is greater";;

let rec power x y = match y with
	| Z -> S Z
	| S c -> mul (power x c) x;;
                     
let rec revl cur l = match l with 
	| [] -> cur
	| hd :: tl -> revl (hd :: cur) tl;;

let rev x =
	let rec revl cur l = match l with
		| [] -> cur
		| hd :: tl -> revl (hd :: cur) tl in

	revl [] x;;

let merge_sort x = 
	let rec merge_sort_rec a left right = 
		
		let rec merge1 a ans = match a with 
			| [] -> ans
			| ha :: ta -> merge1 ta (ha :: ans)
		in

		let rec merge0 a b ans = match (a, b) with
			| ([], []) -> ans
			| (ha :: ta, []) -> merge1 a ans
			| ([], hb :: tb) -> merge1 b ans
			| (ha :: ta, hb :: tb) -> merge0 
				(if ha < hb then ta else a)
				(if ha < hb then b else tb) 
				(if ha < hb then (ha :: ans) else (hb :: ans))
		in

		let merge a b = merge0 a b [] in 

		if (left + 1) >= right then (List.nth a left) :: [] else 
		let mid = (left + right) / 2 in
		let sortedleft = merge_sort_rec a left mid in
		let sortedright = merge_sort_rec a mid right in
		rev (merge sortedleft sortedright)

	in merge_sort_rec x 0 (List.length x);;
                     



let rec string_of_lambda x = match x with
	| Var (a) -> a
	| Abs (a, expr) -> ("(\\"^a^"."^(string_of_lambda expr)^")")
	| App (exprl, exprr) -> 
		let strl = string_of_lambda exprl in
		let strr = string_of_lambda exprr in "("^strl^") ("^strr^")";;

let lambda_of_string x = 
	let rec parse s =
		let module CharS = Set.Make(
		struct 
		    type t = char 
		    let compare = compare
		end) in 
		let alphabet = 
			CharS.of_list ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';
                's';'t';'u';'v';'w';'x';'y';'z';'0';'1';'2'; '3';'4';'5';'6';'7';'8';'9'] in
		let pos = ref 0 in
		let has_next () = if (!pos < String.length s - 1) then true else false in
		let next () = if has_next() then pos := (!pos + 1) in
		let rec ignore_whitespace () = if ((s.[!pos] = ' ') && (has_next())) then (next (); ignore_whitespace()) in
		let get () = (ignore_whitespace(); s.[!pos]) in
		let eat x = if get() <> x then failwith "unexpected symbol" else next() in
		let get_symbol () = 
			let has_el x = if (x = get()) then true else false in
			let rec gs x = if (CharS.exists has_el alphabet)
				then (
					x :=  (!x)^(String.make 1 (get()));
					next();
				 	(gs x)) else x in
			!(gs (ref "")) in
		let rec parse_lambda() =
			let rec get_lambda x = 
				match x with
					| '(' -> (eat '(' ; let l = parse_lambda() in (eat ')'; l))
					| '\\' -> (eat '\\'; parse_abs())
					| _ -> parse_ident()

			and parse_app l1 = App(l1, get_lambda (get ())) 
			and parse_ident() = let x = get_symbol() in Var(x)
			and parse_abs() = 
				let x = get_symbol() in 
				eat '.';
				let l = get_lambda(get ()) in
				Abs(x, l) in
			let lambda = (get_lambda (get() )) in
			let reflambda = ref lambda in
			while ( has_next() && (get() <> ')')) do
				reflambda := parse_app (!reflambda);
			done;
			!reflambda in

		parse_lambda() in
	parse (x^";");;