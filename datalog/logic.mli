(** Representation for Datalog terms and rules, and the main algorithm *)

(** Module type for logic *)
module type S = sig

  (* ----------------------------------------------------------------------
   * Terms and rules
   * ---------------------------------------------------------------------- *)

  type symbol
    (** Abstract type of symbols *)

  type term
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type rule
    (** A datalog rule, i.e. head :- body_1, ..., body_n. [n] must be <= 31. *)

  type subst
    (** A substitution maps variables to symbols *)

  val mk_term : symbol -> [`Var of int | `Symbol of symbol] list -> term
    (** Helper to build a term. Arguments are either variables or symbols; if they
        variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

  val mk_term_s : string -> [`Var of int | `Symbol of string] list -> term
    (** Same as [mk_term], but converts strings to symbols on-the-fly *)

  val open_term : term -> symbol * [`Var of int | `Symbol of symbol] list
    (** Deconstruct a term *)

  val mk_rule : term -> (term * bool) list -> rule
    (** Create a rule from a conclusion and a list of body premises with their sign. *)

  val open_rule : rule -> term * (term * bool) list
    (** Deconstruct a rule *)

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

  val subst_term : subst -> term -> term
    (** Apply substitution to the term *)

  val subst_rule : subst -> rule -> rule
    (** Apply substitution to the rule *)

  val check_safe : rule -> bool
    (** A datalog rule is safe iff all variables in its head also occur in
        positive literals of its body *)

  val is_fact : rule -> bool
    (** A fact is a ground rule with empty body *)

  val compare_rule : rule -> rule -> int
    (** Lexicographic comparison of rules *)

  val eq_rule : rule -> rule -> bool
    (** Check whether rules are (syntactically) equal *)

  val hash_rule : rule -> int
    (** Hash the rule *)

  val pp_term : Format.formatter -> term -> unit
    (** Pretty print the term *)

  val pp_rule : Format.formatter -> rule -> unit
    (** Pretty print the rule *)

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
    (** Add the rule/fact to the DB as an axiom, updating fixpoint *)

  val db_remove : db -> rule -> unit
    (** Remove the rule/fact from the DB as an axiom. It may remove consequences
        of the rule. *)

  val db_match : db -> term -> (term -> subst -> unit) -> unit
    (** match the given term with facts of the DB, calling the handler on
        each fact that match (with the corresponding substitution) *)

  val db_size : db -> int
    (** Size of the DB *)

  val db_fold : ('a -> rule -> 'a) -> 'a -> db -> 'a
    (** Fold on all rules in the current DB (including fixpoint) *)

  val db_subscribe : db -> symbol -> (term -> bool -> unit) -> unit
    (** [db_subscribe db symbol handler] causes [handler] to be called with
        any added/removed fact that has head symbol [symbol] from now on.
        the bool indicates whether the fact is added (true) or removed (false). *)

  val db_explain : db -> term -> term list
    (** Explain the given fact by returning a list of facts that imply it
        under the current rules. *)

  val db_premises : db -> term -> rule * term list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the term), plus the rule that has been used. *)
end

(** Signature for a symbol type. It must be hashable, comparable and
    in bijection with strings *)
module type SymbolType = sig
  include Hashtbl.HashedType
  val to_string : t -> string
  val of_string : string -> t
end

(** Build a Datalog module *)
module Make(Symbol : SymbolType) : S with type symbol = Symbol.t

(** Default term base, where symbols are just strings *)
module Default : S with type symbol = string
