(** The unification problem in first-order logic can be expressed as follows:
    Given two terms containing some variables, find, if it exists, the simplest
    substitution (i.e., an assignment of some term to every variable) which
    makes the two terms equal. The resulting substitution is called
    the most general unifier. *)

(** Algebraic term is a variable or a function of some other terms *)
type algebraic_term =
  | Var of string                            (** Variable: x          *)
  | Fun of string * (algebraic_term list)    (** Function: f(x, y, z) *)

(* These types might also be used for convenience:
   
   type substitution = string * algebraic_term
   type equation = algebraic_term * algebraic_term
   type unifier = substitution list
   type system = equation list
*)

(* Parse input string to algebraic term *)
val term_of_string: string -> algebraic_term

(** Returns one equation equivalent to given system of equations *)
val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)

(** Returns a term after applying given substitution *)
val apply_substitution: (string * algebraic_term) list -> algebraic_term -> algebraic_term

(** Returns True if a substitution is a solution of a given system *)
val check_solution: (string * algebraic_term) list -> (algebraic_term * algebraic_term) list -> bool

(** Returns a solution of a given system or None, if there is no solution *)
val solve_system: (algebraic_term * algebraic_term) list -> (string * algebraic_term) list option