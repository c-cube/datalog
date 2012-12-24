(** Representation for Datalog terms and rules *)

(* ----------------------------------------------------------------------
 * Terms and rules
 * ---------------------------------------------------------------------- *)

type term = int array
  (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
      array is the predicate, then arguments follow *)

type rule = term array
  (** A datalog rule, i.e. head :- body_1, ..., body_n *)

val is_var : int -> bool
  (** A variable is a negative int *)

val vars : ?start:int -> ?stop:int -> term array -> Utils.ISet.t
  (** Set of variables of the terms in array[start...stop]. Start and
      stop are inclusive (default is the whole array). *)

val check_safe : rule -> bool
  (** A datalog rule is safe iff all variables in its head also occur in its body *)

(* ----------------------------------------------------------------------
 * Generalization index on terms
 * ---------------------------------------------------------------------- *)
