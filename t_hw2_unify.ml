open T;;
open Hw2_unify;;

let rec string_of_term x =
  match x with
  | Var v -> v
  | Fun (f, t) -> f ^ "(" ^ (String.concat ", " (List.map string_of_term t)) ^ ")";;

test "string_of_term"
  ~tester: string_of_term
  ~string_of: (fun x -> x)
  (Stream.of_list [
      { input = Fun ("f", [Var "x"; Var "y"; Fun ("z", [Var "abc"; Var "def"]); Var "x"]); output = "f(x, y, z(abc, def), x)" };
    ]);;

test "term_of_string"
  ~tester: term_of_string
  ~string_of: string_of_term
  (Stream.of_list [
      { input = "f(x,(y),z(abc,def),x)"; output = Fun ("f", [Var "x"; Var "y"; Fun ("z", [Var "abc"; Var "def"]); Var "x"]) };
    ]);;  
    
let sym1 = [
  (Fun ("x", [Var "p1"]), Fun ("x", [Var "p2"]));
  (Fun ("y", [Var "p3"]), Fun ("y", [Var "p4"]));
  (Fun ("z", [Var "p5"]), Fun ("z", [Var "p6"]));
];;

let sol1 = [
  ("p1", Var "p2");
  ("p3", Var "p4");
  ("p5", Var "p6");
];;

let sym2 = [
  (Fun("x", [Var "p1"]), Fun("x", [Var "p2"]));
  (Fun("m", [Var "p1"]), Fun("y", [Var "p4"]));
  (Fun("z", [Var "p5"]), Fun("z", [Var "p6"]));
];;

let sym3 = [
  (Fun("x", [Var "p1"]), Fun("x", [Var "p2"]));
  (Fun("y", [Var "p1"]), Fun("y", [Var "p4"]));
  (Fun("z", [Var "p1"]), Fun("z", [Var "p6"]));
];;

let sol3 = [
  ("p1", Var "p6");
  ("p2", Var "p6");
  ("p4", Var "p6");
];; 

let sym4 = [
  (Fun("a", [Var "tx"; Fun("a", [Var "ty"; Fun("a", [Var "tz";Var "t2"])])]), Fun("a", [Fun("a", [Var "ta"; Fun("a", [Var "tb"; Var "ta"])]); Var "t1"]));
  (Var("ty"), Fun("a", [Var "tz"; Var "t4"]));
  (Var("tx"), Fun("a", [Var "tz"; Var "t3"]));
  (Var("t3"), Fun("a", [Var "t4"; Var "t2"]));
];;

let sol4 = [
  ("t1", term_of_string "a(a(ta, tb), a(ta, ta))");
  ("ty", term_of_string "a(ta, tb)");
  ("tx", term_of_string "a(ta, a(tb, ta))");
  ("t3", term_of_string "a(tb, ta)");
  ("tz", term_of_string "ta");
  ("t4", term_of_string "tb");
  ("t2", term_of_string "ta");
];; 

let sym5 = [
  (Fun ("x", [Var "p1"]), Fun ("x", [Var "p2"]));
  (Fun ("y", [Var "p2"]), Fun ("y", [Var "p4"]));
  (Fun ("z", [Var "p5"]), Fun ("z", [Var "p6"]));
];;

let sol5 = [
  ("p1", Var "p4");
  ("p2", Var "p4");
  ("p5", Var "p6");
];;

let sym6 = [
  (Fun ("x", [Var "p1"]), Fun ("x", [Var "p1"]));
];;

let sol6 = [];;

let string_of_solution solution =
  match solution with
  | Some s -> String.concat "; " (List.map (fun (v, t) -> v ^ " = " ^ (string_of_term t)) s)
  | None -> "No solution";;
    
test "solve_system"
  ~tester: solve_system
  ~string_of: string_of_solution
  (Stream.of_list [
      { input = sym1; output = Some sol1 };
      { input = sym2; output = None };
      { input = sym3; output = Some sol3 };
      { input = sym4; output = Some sol4 };
      { input = sym5; output = Some sol5 };
      { input = sym6; output = Some sol6 };
    ]);;    
    