open Hw1;;

(* Returns a set of free variables in `lambda` *)
val free_var_set         : lambda -> Set.Make (String).t
(* Returns a list of free variables in `lambda` *)
val free_vars            : lambda -> Set.Make (String).elt list
(* Returns true if `key` is free in `lambda` *)
val has_free             : string -> lambda -> bool
(* Returns true if `lambda` expression is in normal form *)
val is_normal_form: lambda -> bool
(* Check on alpha equivalence *)
val is_alpha_equivalent  : lambda -> lambda -> bool
(* Returns true if `src` is free for substitution *)
(* in `dest` instead of `key`, false otherwise    *)
val free_to_subst        : lambda -> lambda -> string -> bool
(* Do one beta-reduction step for `lambda` using normal reduction order *)
val normal_beta_reduction: lambda -> lambda
(* Beta-reduce `lambda` to its normal form using normal reduction order *)
val reduce_to_normal_form: lambda -> lambda