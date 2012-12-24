(** Representation for Datalog terms and rules *)

(* ----------------------------------------------------------------------
 * Terms and rules
 * ---------------------------------------------------------------------- *)

type term = int array
  (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
      array is the predicate, then arguments follow *)

type rule = term array
  (** A datalog rule, i.e. head :- body_1, ..., body_n *)

type subst = int Utils.IHashtbl.t
  (** A substitution is a map from (negative) ints to (positive) ints *)

val is_var : int -> bool
  (** A variable is a negative int *)

val is_ground : term -> bool
  (** Is the term ground (a fact)? *)

val arity : term -> int
  (** Number of subterms of the term. Ex for p(a,b,c) it returns 3 *)

val vars : ?start:int -> ?stop:int -> term array -> Utils.ISet.t
  (** Set of variables of the terms in array[start...stop]. Start and
      stop are inclusive (default is the whole array). *)

val subst_term : subst -> term -> term
  (** Apply substitution to the term *)

val subst_rule : subst -> rule -> rule
  (** Apply substitution to the rule *)

val check_safe : rule -> bool
  (** A datalog rule is safe iff all variables in its head also occur in its body *)

val pp_term : (int -> string) -> Format.formatter -> term -> unit
  (** Pretty print the term, using the given mapping from symbols to strings *)

val pp_rule : (int -> string) -> Format.formatter -> rule -> unit
  (** Pretty print the rule, using the given mapping from symbols to strings *)

(* ----------------------------------------------------------------------
 * Generalization index on terms
 * ---------------------------------------------------------------------- *)
