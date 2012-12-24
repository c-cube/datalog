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

val eq_term : term -> term -> bool
  (** Are the terms equal? *)

val hash_term : term -> int
  (** Hash the term *)

val vars : ?start:int -> ?stop:int -> term array -> Utils.ISet.t
  (** Set of variables of the terms in array[start...stop]. Start and
      stop are inclusive (default is the whole array). *)

val subst_term : subst -> term -> term
  (** Apply substitution to the term *)

val subst_rule : subst -> rule -> rule
  (** Apply substitution to the rule *)

val check_safe : rule -> bool
  (** A datalog rule is safe iff all variables in its head also occur in its body *)

val eq_rule : rule -> rule -> bool
  (** Check whether rules are (syntactically) equal *)

val hash_rule : rule -> int
  (** Hash the rule *)

val pp_term : ?to_s:(int -> string) -> Format.formatter -> term -> unit
  (** Pretty print the term, using the given mapping from symbols to strings *)

val pp_rule : ?to_s:(int -> string) -> Format.formatter -> rule -> unit
  (** Pretty print the rule, using the given mapping from symbols to strings *)

(* ----------------------------------------------------------------------
 * Generalization/Specialization index on terms
 * ---------------------------------------------------------------------- *)

(** Type for an indexing structure on terms *)
module type Index =
  sig
    type t
      (** A term index *)

    type elt
      (** A value indexed by a term *)

    val create : unit -> t
      (** Create a new index *)

    val add : t -> term -> elt -> unit
      (** Add an element indexed by the term *)

    val clear : t -> unit
      (** Reset to empty index *)

    val retrieve_generalizations : ('a -> elt -> subst -> 'a) -> 'a -> t -> term -> 'a
      (** Fold on generalizations of given ground term (with transient substitution) *)

    val retrieve_specializations : ('a -> elt -> subst -> 'a) -> 'a -> t -> term -> 'a
      (** Fold on ground specifications of given term (with transient substitution) *)

    val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
      (** Fold on all indexed elements *)

    val is_empty : t -> bool
      (** Is the index empty? *)

    val size : t -> int
      (** Number of indexed elements (linear) *)
  end

module Make(H : Hashtbl.HashedType) : Index with type elt = H.t
  (** Create an Index module for the given type of elements. The implementation
      is based on perfect discrimination trees. *)
