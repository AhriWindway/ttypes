open Hw1;;
open T;;

let string_of_peano x = string_of_int (int_of_peano x);;

let rec gen_peano ?(acc = Z) n =
  if n != 0
  then gen_peano ~acc:(S acc) (n - 1)
  else acc;;

let rec string_of_list string_of_elem l =
  match l with
  | [] -> ""
  | x::xs -> (string_of_elem x) ^ "; " ^ (string_of_list string_of_elem xs);;

let string_of_list_of_int = string_of_list string_of_int;;
  
print_endline ("HOMEWORK 1"); 
test "int_of_peano"
  ~tester: int_of_peano
  ~string_of: string_of_int
  (Stream.of_list [
      { input = (gen_peano 0); output = 0 };
      { input = (gen_peano 1); output = 1 };
      { input = (gen_peano 2); output = 2 };
      { input = (gen_peano 3); output = 3 };
      { input = (gen_peano 1000); output = 1000 };
    ]);;  
    
test "peano_of_int"
  ~tester: peano_of_int
  ~string_of: string_of_peano
  (Stream.of_list [
      { input = 0; output = (gen_peano 0) };
      { input = 1; output = (gen_peano 1) };
      { input = 2; output = (gen_peano 2) };
      { input = 3; output = (gen_peano 3) };
      { input = 1000; output = gen_peano 1000 };
    ]);;    
    
test "inc"
  ~tester: (fun a -> int_of_peano (inc (peano_of_int a)))
  ~string_of: string_of_int
  (Stream.of_list [
      { input = 0; output = 1 };
      { input = 1; output = 2};
      { input = 2; output = 3 };
      { input = 3; output = 4 };
      { input = 1001; output = 1002 };
    ]);;        
    
test "add"
  ~tester: (fun (a, b) -> int_of_peano (add (peano_of_int a) (peano_of_int b)))
  ~string_of: string_of_int
  (Stream.of_list [
      { input = (0, 0); output = 0 };
      { input = (2, 5); output = 7};
      { input = (5, 11); output = 16 };
      { input = (3, 0); output = 3 };
      { input = (499, 501); output = 1000 };
    ]);;        
        
test "sub"
  ~tester: (fun (x, y) -> int_of_peano (sub (peano_of_int x) (peano_of_int y)))
  ~string_of: string_of_int
  (Stream.of_list [
      { input = (0, 0); output = 0 };
      { input = (2, 5); output = 0 };
      { input = (11, 11); output = 0 };
      { input = (3, 0); output = 3 };
      { input = (1000, 500); output = 500 };
    ]);;

test "mul"
  ~tester: (fun (x, y) -> int_of_peano (mul (peano_of_int x) (peano_of_int y)))
  ~string_of: string_of_int
  (Stream.of_list [
      { input = (0, 0); output = 0 };
      { input = (0, 10); output = 0 };
      { input = (10, 0); output = 0 };
      { input = (10, 11); output = 110 };
      { input = (100, 100); output = 10000 };
    ]);;

test "power"
  ~tester: (fun (x, y) -> int_of_peano (power (peano_of_int x) (peano_of_int y)))
  ~string_of: string_of_int
  (Stream.of_list [
      { input = (0, 10); output = 0 };
      { input = (10, 0); output = 1 };
      { input = (2, 1); output = 2 };
      { input = (2, 10); output = 1024 };
      { input = (100, 2); output = 10000 };
    ]);;
    
test "rev"
  ~tester: rev
  ~string_of: string_of_list_of_int
  (Stream.of_list [
      { input = []; output = [] };
      { input = [1]; output = [1] };
      { input = [1;2]; output = [2;1] };
      { input = [7;6;5;4;3;2;1]; output = [1;2;3;4;5;6;7] };
    ]);;    
    
test "merge_sort"
  ~tester: merge_sort
  ~string_of: string_of_list_of_int
  (Stream.of_list [
      { input = []; output = [] };
      { input = [1]; output = [1] };
      { input = [1;2]; output = [1;2] };
      { input = [7;2;5;4;3;6;1]; output = [1;2;3;4;5;6;7] };
    ]);;    
    
test "lambda_of_string"
  ~tester: lambda_of_string
  ~string_of: string_of_lambda
  (Stream.of_list [
      { input = "x"; output = Var "x" };
      { input = "x y"; output = App (Var "x", Var "y") };
      { input = "\\x.x"; output = Abs ("x", Var "x") };
      { input = "\\x.x y"; output = Abs ("x", App (Var "x", Var "y")) };
      { input = "((\\x.((((x)) (y)))))"; output = Abs ("x", App (Var "x", Var "y")) };
    ]);;

test "string_of_lambda"
  ~tester: string_of_lambda
  ~string_of: (fun x -> x)
  (Stream.of_list [
      { input = lambda_of_string "(x)"; output = "x" };
      { input = lambda_of_string "(x) (y)"; output = "x y" };
      { input = lambda_of_string "\\x.x"; output = "\\x.x" };
      { input = lambda_of_string "\\x.\\y.x y"; output = "\\x.\\y.x y" };
      { input = lambda_of_string "(((((((\\y1.y1)))))))"; output = "\\y1.y1" };
      { input = lambda_of_string "((z))(\\x.\\y.((x y)))"; output = "z (\\x.\\y.x y)" };
      { input = lambda_of_string "\\x.\\y.(x f) (y f)"; output = "\\x.\\y.x f (y f)" };
      { input = lambda_of_string "((\\z.(z(y z)))(z x)z)"; output = "(\\z.z (y z)) (z x) z" };
      { input = lambda_of_string "\\f.(\\x.f(x x))(\\x.f(x x))"; output = "\\f.(\\x.f (x x)) (\\x.f (x x))" };
      { input = lambda_of_string "\\n.(\\p.p (\\x.\\y.x)) (n (\\p.\\f.f ((\\p.p (\\x.\\y.y)) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p (\\x.\\y.y)) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))"; output = "\\n.(\\p.p (\\x.\\y.x)) (n (\\p.\\f.f ((\\p.p (\\x.\\y.y)) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p (\\x.\\y.y)) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))" };
    ]);;    
    