open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;

type simp_type = S_Elem  of string | S_Arrow  of simp_type * simp_type
type hm_lambda = HM_Var  of string | HM_Abs   of string * hm_lambda  | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type   = HM_Elem of string | HM_Arrow of hm_type * hm_type   | HM_ForAll of string * hm_type

module MS = Map.Make(String);;
module SS = Set.Make(String);;

exception NoSolution of string;;

let fresh_var  = Stream.from (fun i -> Some ("var" ^ string_of_int i));;
let fresh_type = Stream.from (fun i -> Some ("type" ^ string_of_int i));;

let rec term_of_simp_type t 
  = match t with
  | S_Elem a       -> Var a
  | S_Arrow (a, b) -> Fun ("->", [ (term_of_simp_type a); (term_of_simp_type b) ]);;
  
let rec simp_type_of_term t
  = match t with 
  | Var v -> S_Elem v
  | Fun (name, [l;r]) -> S_Arrow (simp_type_of_term l, simp_type_of_term r)
  | _ -> failwith "Term is not representing a simple type!";;
  
let rec get_system lambda types
  = match (lambda : lambda) with
  | Var v -> ([], MS.find v types)
  | App (l1, l2) ->
    let (system1, t1) = get_system l1 types in
    let (system2, t2) = get_system l2 types in
    let new_type = S_Elem (Stream.next fresh_type) in
    (system1 @ system2 @ [(t1, S_Arrow(t2, new_type))], new_type)
  | Abs (v, l) ->
    let new_type = S_Elem (Stream.next fresh_type) in
    let new_map = MS.add v new_type types in
    let (system1, t1) = get_system l new_map in
    (system1, S_Arrow(MS.find v new_map, t1))
  
let infer_simp_type lambda =
  let add_type_to_map map t = MS.add t (S_Elem (Stream.next fresh_type)) map in
  let types = List.fold_left add_type_to_map MS.empty (free_vars lambda) in
  let (system, t) = get_system lambda types in
  match solve_system (List.map (fun (a, b) -> (term_of_simp_type a, term_of_simp_type b)) system) with
  | None          -> None
  | Some solution ->
    let type_term = apply_substitution solution (term_of_simp_type t) in
    let type_list = List.map (fun (a, b) -> (a, simp_type_of_term b)) in
    Some (type_list solution, simp_type_of_term type_term);;
  
let rec term_of_hm_type hm_type
  = match hm_type with
  | HM_Elem a       -> Var a
  | HM_Arrow (a, b) -> Fun ("->", [(term_of_hm_type a); (term_of_hm_type b)])
  | _ -> failwith "Can't be represented as a term";;
  
let rec hm_type_of_term term
  = match term with
  | Var a             -> HM_Elem a
  | Fun (name, [l;r]) -> HM_Arrow (hm_type_of_term l, hm_type_of_term r)
  | _ -> failwith "Term is not representing a simple type";;
  
let rec free_vars hm_lambda
  = match hm_lambda with
  | HM_Var a         -> SS.singleton a
  | HM_App (a, b)    -> SS.union (free_vars a) (free_vars b)
  | HM_Abs (a, b)    -> SS.remove a (free_vars b)
  | HM_Let (a, b, c) -> SS.union (free_vars b) (SS.remove a (free_vars c));;
  
let rec free_types hm_type
  = match hm_type with
  | HM_Elem a        -> SS.singleton a
  | HM_Arrow (a, b)  -> SS.union (free_types a) (free_types b)
  | HM_ForAll (a, b) -> SS.remove a (free_types b);;
  
let rec type_subst hm_type sub
  = match hm_type with
  | HM_Elem a        -> if MS.mem a sub then MS.find a sub else hm_type
  | HM_Arrow (a, b)  -> HM_Arrow (type_subst a sub, type_subst b sub)
  | HM_ForAll (a, b) -> HM_ForAll (a, type_subst b (MS.remove a sub));;
  
let rec env_subst sub env = MS.map (fun a -> type_subst a sub) env;;

let compose_subst s1 s2 =
  let tmp_sub = MS.map (fun a -> type_subst a s1) s2 in
  let merger key va vb
    = match (va, vb) with
    | (None, None)   -> None
    | (Some v, None) -> Some v
    | (_, Some v)    -> Some v
  in MS.merge merger s1 tmp_sub;;

let generalize env hm_type =
  let union_types _ v = SS.union (free_types v) in
  let env_free_types = MS.fold union_types env SS.empty in
  let hm_free_types = free_types hm_type in
  let new_forall_vars = SS.diff hm_free_types env_free_types in
  let add_forall var hm_type = HM_ForAll (var, hm_type) in
  SS.fold add_forall new_forall_vars hm_type;;

let rec propogate hm_type
  = match hm_type with
    | HM_ForAll (a, b) ->
      let sub = MS.singleton a (HM_Elem (Stream.next fresh_var))
      in type_subst (propogate b) sub
    | _ -> hm_type;;
  
let algorithm_w hm_lambda =
  let new_var() = Stream.next fresh_var in
  let rec w_helper type_env hm_lambda
    = match hm_lambda with
    | HM_Var a when MS.mem a type_env -> (MS.empty, propogate (MS.find a type_env))
    | HM_Var a -> raise (NoSolution "Free variable found.")
    | HM_Abs (a, b) ->
      let new_type = HM_Elem (new_var()) in
      let type_env = MS.add a new_type (MS.remove a type_env) in
      let (system, t) = w_helper type_env b in
      (system, HM_Arrow (type_subst new_type system, t))
    | HM_Let (a, b, c) ->
      let (system1, t1) = w_helper type_env b in
      let a_type = generalize (env_subst system1 type_env) t1 in
      let type_env = env_subst system1 (MS.remove a type_env) in
      let type_env = MS.add a a_type type_env in
      let (system2, t2) = w_helper type_env c in
      (compose_subst system2 system1, t2)
    | HM_App (a, b) ->
      (let (system1, t1) = w_helper type_env a in
       let (system2, t2) = w_helper (env_subst system1 type_env) b in
       let new_type = HM_Elem (new_var()) in
       let eq = (term_of_hm_type (type_subst t1 system2), term_of_hm_type (HM_Arrow (t2, new_type))) in
       match solve_system [eq] with
       | None -> raise (NoSolution "No solution")
       | Some answer ->
         let add_subst (str, term) = MS.add str (hm_type_of_term term) in
         let v = List.fold_right add_subst answer MS.empty in
         let unifier = compose_subst v (compose_subst system2 system1) in
         (unifier, type_subst new_type unifier))
  in 
  let unique a = MS.add a (HM_Elem (new_var())) in
  let type_env = SS.fold unique (free_vars hm_lambda) MS.empty in
  try
    let (unifier, hm_type) = w_helper type_env hm_lambda in
    Some (MS.bindings unifier, hm_type)
  with (NoSolution e) -> None;;
  