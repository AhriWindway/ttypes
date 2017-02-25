open Genlex;;

type peano = Z | S of peano;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with
    | 0 -> Z
    | s -> S(peano_of_int (s - 1));;

let rec int_of_peano p = match p with
    |  Z -> 0
    |  S x -> 1 + int_of_peano x;;

let inc x = S (x);;
let rec add x y = match y with
    | Z -> x
    | S ys -> S (add x ys);;
    
let rec mul x y = match y with
    | Z -> Z
    | S ys -> add (mul x ys) x;;
    
let rec sub x y = match (x, y) with
    | x, Z -> x
    | Z, y -> Z
    | S xs, S ys -> sub xs ys;;
    
let rec power x y = match (x, y) with
    | x, Z -> S(Z)
    | Z, y -> Z
    | x, S ys -> mul x (power x ys);;

let rev x = 
    let rec rev_help xs = function
      | [] -> xs
      | head::tail -> rev_help (head::xs) tail in
    rev_help [] x;;
 
let rec merge = function
    | list, []
    | [], list -> list
    | h1::t1, h2::t2 ->
        if h1 <= h2 then
          h1 :: merge (t1, h2::t2)
        else
          h2 :: merge (h1::t1, t2);;
          
let rec halve x = match x with
    | [] -> [], []
    | [_] as t1 -> t1, []
    | head::tail ->
        let t1, t2 = halve tail in
          head::t2, t1;;

let rec merge_sort x = match x with
    | [] -> []
    | [_] -> x
    | x ->
        let l1, l2 = halve x in
          merge (merge_sort l1, merge_sort l2);;
          

let rec string_of_lambda x = match x with
    | Var xs -> xs
    | App (a, b) -> "(" ^ string_of_lambda a ^ " " ^ string_of_lambda b ^ ")"
    | Abs (s, e) -> "(\\" ^ s ^ "." ^ string_of_lambda e ^ ")";;
    
   
let lexer = make_lexer ["\\";".";"(";")";";"];;


let parse tokens = 
    let next() = Stream.next tokens in
    let peek() = Stream.peek tokens in
    let eat x = if (next() <> Kwd x) then failwith "Unexpected token" in
    let rec parse_lambda() = match (next()) with
        | Kwd "(" -> 
            let lambda = parse_lambda() in
            eat ")";
            parse_app lambda;
        | Kwd "\\" -> parse_app (parse_abs());
        | Ident s -> parse_app (Var s);
        | _ -> failwith "Unexpected token"
    and parse_abs() =
        match (next()) with
            |Ident s -> 
                eat ".";
                Abs(s, parse_lambda());
            | _ -> failwith "Unexpected token"
    and parse_app t = 
        match (peek()) with
            | None -> failwith "Error";
            | Some l -> match l with
                | Kwd ")" -> t
        				| Kwd ";" -> t
        				| _ -> App(t, parse_lambda()) in
    parse_lambda();; 
    
let lambda_of_string x = parse(lexer(Stream.of_string (x^";")));;    