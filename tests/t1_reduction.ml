open Hw1_reduction;;
open Hw1;;

print_string "Hello, World!\n";;

let a = free_vars (lambda_of_string "(\\a.b)");;
