open Genlex;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

(* Printing lambda *)
let rec string_of_lambda x =
    match x with
    | Var l1 -> l1
    | App (a,b) -> "(" ^ string_of_lambda a ^ " " ^ string_of_lambda b ^ ")"
    | Abs (s,e) -> "(\\" ^ s ^ "." ^ string_of_lambda e ^ ")";; 
    
(*Lambda parser*)
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
    
(*Check two lambdas on alpha equality*)

let rec subst_var newv lambda oldv = 
    match lambda with
    | Var(v) -> if v = oldv then Var(newv) else Var(v);
    | App(l1, l2) -> App(subst_var newv l1 oldv, subst_var newv l2 oldv);
    | Abs(v, l) -> if v = oldv then Abs(newv, subst_var newv l oldv) else Abs(v, subst_var newv l oldv);;

let rec is_alpha_equivalent a b =
    match (a, b) with 
    | (Var(v1), Var(v2)) -> v1 = v2;
    | (App(fl1, fl2), App(sl1, sl2)) -> is_alpha_equivalent fl1 sl1 && is_alpha_equivalent fl2 sl2
    | (Abs(v1, l1), Abs(v2, l2)) -> 
            let newv = "subst"^ string_of_int (Random.int(10000)) in
            is_alpha_equivalent (subst_var newv l1 v1) (subst_var newv l2 v2); 
    | _ -> false;;


