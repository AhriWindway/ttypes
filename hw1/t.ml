open Hw1;;
let tList = [1;2;5;4;3];;

let rec print_list = function 
    | [] -> ()
    | e::l -> print_int e ; print_string " " ; print_list l;;
    
print_string "\nCheck peano of 3: ";;
print_int (int_of_peano (S (S (S (Z)))));;
print_string "\nCheck 3 of peano: ";;
print_int (int_of_peano (peano_of_int 3));;

print_string "\nCheck inc(4++): ";;
print_int (int_of_peano (inc (peano_of_int 4)));;

print_string "\nCheck add(4 + 3): ";;
print_int (int_of_peano (add (peano_of_int 4) (peano_of_int 3)));;

print_string "\nCheck mul(4 * 3): ";;
print_int (int_of_peano (mul (peano_of_int 4) (peano_of_int 3)));;

print_string "\nCheck power(3 ^ 4): ";;
print_int (int_of_peano (power (peano_of_int 3) (peano_of_int 4)));;

print_string "\nCheck sub(5 - 2): ";;
print_int (int_of_peano (sub (peano_of_int 5) (peano_of_int 2)));;

print_string "\nReverse list: ";;
print_list tList;;
print_string " -> ";;
print_list (rev tList);;

print_string "\nMerge sorting list: ";;
print_list tList;;
print_string " -> ";;
print_list (merge_sort tList);;

print_string "Parsing: \n";;
print_string (string_of_lambda (lambda_of_string "(x)(y)"));;
print_string "\n";
print_string (string_of_lambda (lambda_of_string "(x)"));
print_string "\n";
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); 
print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xf yf")); print_string "\n";;