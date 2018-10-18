open Hw1
open Hw2_unify

(** Simple type is a type name or arrow operator *)
type simp_type =
  | S_Elem of string                    (** Type: a       *)
  | S_Arrow of simp_type * simp_type    (** Arrow: a -> a *)

(** Returns a list of variable types and a type of simple lambda expression *)
val infer_simp_type : lambda -> ((string * simp_type) list * simp_type) option

(** Hindley-Milner lambda is a variable, abstraction, application or let *)
type hm_lambda =
  | HM_Var of string                            (** Variable: x          *)
  | HM_Abs of string * hm_lambda                (** Abstraction: λx.f    *)
  | HM_App of hm_lambda * hm_lambda             (** Application: f x     *)
  | HM_Let of string * hm_lambda * hm_lambda    (** Let: let x = t in t' *)

(** Hindley-Milner type is a type name, arrow operator or forall quantifier *)
type hm_type =
  | HM_Elem of string                (** Type: a          *)
  | HM_Arrow of hm_type * hm_type    (** Arrow: a -> a    *)
  | HM_ForAll of string * hm_type    (** Quantifier: ∀t.t *)

  
val term_of_hm_type : hm_type -> algebraic_term  
(** Returns a list of variable types and a type of Hindley-Milner lambda *)
val algorithm_w : hm_lambda -> ((string * hm_type) list * hm_type) option