open Hw1;;
open Genlex;;

module SS = Set.Make (String);;
module SM = Map.Make (String);;

(* Returns a set of free variables in `lambda` *)
let rec free_var_set lambda
  = match lambda with 
    | Var x        -> SS.singleton x
    | App (l1, l2) -> SS.union (free_var_set l1) (free_var_set l2)
    | Abs (x, l)   -> SS.remove x (free_var_set l);;
   
(* Returns a list of free variables in `lambda` *)   
let free_vars l = SS.elements (free_var_set l);;

(* Returns true if `key` is free in `lambda` *)
let has_free key l = List.mem key (free_vars l);;

let substitite src l key
  = let 
      nothing_to_replace l = not (has_free key l)
    in let
      not_bounding l = not (SS.mem l (free_var_set src))
    in let 
      rec substitite_helper l
        = match l with
          | Var a        -> if (a = key) then src else l
          | App (l1, l2) -> App (substitite_helper l1, substitite_helper l2)
          | Abs (a, l)   -> if (nothing_to_replace l)
                            then l
                            else if (not_bounding a)
                                 then Abs (a, substitite_helper l)
                                 else failwith (a^" is not free for substitution")
    in substitite_helper l;;
                                 
(* Returns true if `src` is free for substitution *)
(* in `dest` instead of `key`, false otherwise    *)
let free_to_subst src l key
  = try
      let _ = substitite src l key in true
    with _ -> false;;

(* Returns true if `lambda` expression is in normal form *)
let rec is_normal_form l
  = match l with
  | Var _               -> true
  | Abs (x, y)          -> is_normal_form y
  | App (Abs (x, y), t) -> not (free_to_subst t y x)
  | App (x, y)          -> is_normal_form x && is_normal_form y;;

let fresh_name = Stream.from (fun i -> Some ("var_" ^ string_of_int i));;

let is_alpha_equivalent lambda1 lambda2 =
  let check_vars v1 v2 map1 map2 =
    if (SM.mem v1 map1) && (SM.mem v2 map2) && (SM.find v1 map1 = v2) && (SM.find v2 map2 = v1) then true
    else if (not (SM.mem v1 map1)) && (not (SM.mem v2 map2)) && v1 = v2 then true
    else false
  in
  let rec is_alpha_eq lambda1 lambda2 map1 map2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2) -> check_vars v1 v2 map1 map2
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_eq x1 x2 map1 map2 && is_alpha_eq y1 y2 map1 map2)
    | (Abs (x1, y1), Abs (x2, y2)) -> is_alpha_eq y1 y2 (SM.add x1 x2 map1) (SM.add x2 x1 map2)
    | _ -> false
  in is_alpha_eq lambda1 lambda2 SM.empty SM.empty;;

type lambda_imp = 
  | Var_imp of string
  | Abs_imp of (string * lambda_imp ref)
  | App_imp of (lambda_imp ref * lambda_imp ref);;

let rec imp_of_lambda lambda
  = match lambda with
  | Var v     -> ref (Var_imp v)
  | App(x, y) -> ref (App_imp (imp_of_lambda x, imp_of_lambda y))
  | Abs(x, l) -> ref (Abs_imp (x, imp_of_lambda l));;
  
let rec lambda_of_imp lambda_imp
  = match !lambda_imp with
  | Var_imp v -> Var v
  | App_imp (x, y) -> App (lambda_of_imp x, lambda_of_imp y)
  | Abs_imp (x, l) -> Abs (x, lambda_of_imp l);;
  
let rec reduction_step lambda_imp =
  let rec to_alpha_equivalent lambda_imp map
    = match !lambda_imp with
    | Var_imp v      -> 
      if SM.mem v map
      then ref (Var_imp (SM.find v map))
      else lambda_imp
    | App_imp (x, y) -> ref (App_imp (to_alpha_equivalent x map, to_alpha_equivalent y map))
    | Abs_imp (x, l) -> 
      let new_var = Stream.next fresh_name in
      ref (Abs_imp (new_var, to_alpha_equivalent l (SM.add x new_var map)))
  in 
  let rec try_to_substitute src dest key
      = match !dest with
      | Var_imp v      -> if v = key then dest := !src
      | Abs_imp (x, l) -> if x <> key then try_to_substitute src l key
      | App_imp (x, y) ->
        try_to_substitute src x key;
        try_to_substitute src y key
  in
  let app_reduction a b 
    = match !a with
    | Abs_imp (x, y) ->
      let new_var = Stream.next fresh_name in
      lambda_imp := !(to_alpha_equivalent y (SM.singleton x new_var));
      try_to_substitute b lambda_imp new_var;
      Some lambda_imp
    | _ ->
      match reduction_step a with
      | Some _ -> Some lambda_imp
      | None   ->
        match reduction_step b with
        | Some _ -> Some lambda_imp
        | None   -> None
  in 
    match !lambda_imp with  
    | Var_imp v      -> None
    | App_imp (x, y) -> app_reduction x y
    | Abs_imp (x, l) -> 
      match reduction_step l with
      | Some _ -> Some lambda_imp
      | None   -> None;;
  
(* Do one beta-reduction step for `lambda` using normal reduction order *)
let normal_beta_reduction lambda
  = match reduction_step (imp_of_lambda lambda) with
  | Some x -> lambda_of_imp x
  | None   -> lambda;;

(* Beta-reduce `lambda` to its normal form using normal reduction order *)
let reduce_to_normal_form lambda
  = let rec reduce_helper lambda_imp
    = match reduction_step lambda_imp with
    | Some x -> reduce_helper x
    | None   -> lambda_imp
  in 
    lambda_of_imp (reduce_helper (imp_of_lambda lambda));;

