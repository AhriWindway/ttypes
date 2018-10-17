open Genlex;;

type algebraic_term =
  | Var of string
  | Fun of string * (algebraic_term list);;

(* Parser *)
let term_of_string str =
  let stream = Stream.of_string (str ^ "$") in
  let tokens = make_lexer [","; "("; ")"; "$"] stream in
  let next() = Stream.next tokens in
  let peek() = Stream.peek tokens in
  let eat c err value = if (next() <> Kwd c) then failwith err else value in
  
  let rec parse_term() =
    match next() with
    | Ident name -> fun_or_var name
    | Kwd "("    -> let t = parse_term() in eat ")" "Parenthesis not closed!" t
    | _ -> failwith "Unexpected symbol"
    
  and fun_or_var name = 
    match peek() with
    | Some (Kwd "(") -> let _ = next() in parse_fun name []
    | Some (Kwd _ )  -> Var name
    | _ -> failwith "Unexpected end of string"
    
  and parse_fun name args =
    let args = (parse_term()) :: args in
    match next() with 
      | Kwd "," -> parse_fun name args
      | Kwd ")" -> Fun (name, List.rev args)
      | _ -> failwith "Unexpected symbol"
  in parse_term();;

  
module SS = Set.Make (String);;
module SM = Map.Make(String);;
  
exception NoSolution of string;;  
let fresh_name = Stream.from (fun i -> Some ("fresh_" ^ string_of_int i));;

(** Returns one equation equivalent to given system of equations *)
let system_to_equation sys =
  let new_name = Stream.next fresh_name in
  let (left, right) = List.split sys in
  (Fun (new_name, left), Fun (new_name, right));;
  
(** Returns a term after applying given substitution *)
let apply_substitution substitution_list term =
  let rec apply_helper substitution term =
    let (key, new_term) = substitution in
    match term with
    | Var v            -> if (key = v) then new_term else term
    | Fun (name, args) -> Fun (name, List.map (apply_helper substitution) args)
  in
    List.fold_right apply_helper substitution_list term;;
    
(** Returns True if a substitution is a solution of a given system *)
let check_solution substitution_list sys = 
  let (l, r) = system_to_equation sys in
  let substitute = apply_substitution substitution_list in
  substitute l = substitute r;;
  
(** Returns a solution of a given system or None, if there is no solution *)
let solve_system system =
  let rec contains var term 
    = match term with 
    | Var a         -> a = var
    | Fun (a, args) -> List.exists (contains var) args
  in let equation_substitute (l, r)
    = match l with
    | Var a -> (a, r)
    | _ -> raise (NoSolution "Equation is not resolved")
  in let rec solve_helper system result
    = match system with 
    | [] -> []
    | _ when 
      SS.cardinal result = List.length system -> system
    | eq :: eqs ->
      match eq with
      | (l, r) when 
        l = r -> solve_helper eqs result
      | (Var a, r) -> 
        if contains a r  
        then raise (NoSolution "Variable at both sides of equation")
        else 
          let nres = SS.add a result
          in let substitution = apply_substitution [(a, r)]
          in let tail = List.map (fun (l, r) -> (substitution l, substitution r)) eqs 
          in solve_helper (tail @ [eq]) nres
      | (l, Var a) -> solve_helper (eqs @ [(Var a, l)]) result
      | (Fun (fa, aargs), Fun (fb, bargs)) when (fa = fb) && List.length aargs = List.length bargs ->
        let new_eqs = List.combine aargs bargs in
        solve_helper (eqs @ new_eqs) result
      | (Fun _, Fun _) -> raise (NoSolution "Equations with different functions")
  in try
    let res_system = solve_helper system SS.empty
    in let solution = List.map equation_substitute res_system
    in Some solution
  with NoSolution _ -> None