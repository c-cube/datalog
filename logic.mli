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

val is_fact : rule -> bool
  (** A fact is a ground rule with empty body *)

val compare_rule : rule -> rule -> int
  (** Lexicographic comparison of rules *)

val eq_rule : rule -> rule -> bool
  (** Check whether rules are (syntactically) equal *)

val hash_rule : rule -> int
  (** Hash the rule *)

val remove_first : rule -> rule
  (** Rule without its first body term *)

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

    module DataSet : Set.S with type elt = elt
      (** Set of indexed elements *)

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

module Make(X : Set.OrderedType) : Index with type elt = X.t
  (** Create an Index module for the given type of elements. The implementation
      is based on perfect discrimination trees. *)

(* ----------------------------------------------------------------------
 * The datalog bipartite resolution algorithm
 * ---------------------------------------------------------------------- *)

type db
  (** A database of facts and rules, with incremental fixpoint computation *)

val db_create : unit -> db
  (** Create a DB *)

val db_mem : db -> rule -> bool
  (** Is the rule member of the DB? *)

val db_add : db -> rule -> unit
  (** Add the rule/fact to the DB, updating fixpoint *)

val db_size : db -> int
  (** Size of the DB *)

val db_fold : ('a -> rule -> 'a) -> 'a -> db -> 'a
  (** Fold on all rules in the current DB (including fixpoint) *)
