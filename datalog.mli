(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Interface of Datalog} *)

(** {2 Logic types: literals and clauses, substitutions and unification} *)
module Logic : sig
  (** Signature for a symbol type. It must be hashable, comparable and
      in bijection with strings *)
  module type SymbolType = sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val to_string : t -> string
    val of_string : string -> t
  end

  module type S = sig
    (** {2 Literals and clauses} *)

    module Symbol : SymbolType

    type symbol = Symbol.t
      (** Abstract type of symbols *)

    type literal = private
      | Var of int
      | Apply of symbol * literal array
      (** A datalog atom, i.e. pred(arg_1, ..., arg_n). Arguments can
          themselves be literals *)

    type clause = private
      | Clause of literal * literal list
      (** A datalog clause, i.e. head :- body_1, ..., body_n *)

    val mk_apply : symbol -> literal list -> literal
    val mk_apply_a : symbol -> literal array -> literal
    val mk_const : symbol -> literal
    val mk_var : int -> literal

    type subst
      (** A substitution maps variables to symbols *)

    type 'a bind = ('a * int)
      (** A context in which to interpret variables in a literal or clause.
          The context is an offset that is implicitely applied to variables *)

    (** {3 Constructors and destructors} *)

    val mk_clause : literal -> literal list -> clause
      (** Create a clause from a conclusion and a list of premises *)

    val is_var : literal -> bool
      (** A variable is a negative int *)

    val vars : literal -> literal Sequence.t
      (** Iterate on variables of the literal *)

    val is_ground : literal -> bool
      (** Is the literal ground (a fact)? *)

    val arity : literal -> int
      (** Number of subliterals of the literal. Ex for p(a,b,c) it returns 3 *)

    (** {3 Comparisons} *)

    val eq_literal : literal -> literal -> bool
      (** Are the literals equal? *)

    val hash_literal : literal -> int
      (** Hash the literal *)

    val conclusion : clause -> literal
      (** Conclusion of the clause *)

    val body : clause -> literal Sequence.t
      (** Body of the clause *)

    val all_lits : clause -> literal Sequence.t
      (** All the literals of the clause *)

    val check_safe : clause -> bool
      (** A datalog clause is safe iff all variables in its head also occur in its body *)

    val is_fact : clause -> bool
      (** A fact is a ground clause with empty body *)

    val eq_clause : clause -> clause -> bool
      (** Check whether clauses are (syntactically) equal *)

    val hash_clause : clause -> int
      (** Hash the clause *)

    (** {3 Unification, matching and substitutions} *)

    exception UnifFailure

    val empty_subst : subst
      (** The empty substitution *)

    (* TODO external API to build substitutions *)

    val lit_offset : literal -> int
      (** Offset to avoid collisions with the given lit *)

    val offset : clause -> int
      (** Offset to avoid collisions with the given clause *)

    val matching : ?subst:subst -> literal bind -> literal bind -> subst
      (** [matching pattern l] matches [pattern] against [l]; variables in [l]
          cannot be bound. Raise UnifFailure if they do not match. *)

    val unify : ?subst:subst -> literal bind -> literal bind -> subst
      (** [unify l1 l2] tries to unify [l1] with [l2].
           Raise UnifFailure if they do not match. *)

    val alpha_equiv : ?subst:subst -> literal bind -> literal bind -> subst
      (** If the literals are alpha equivalent, return the corresponding renaming *)

    val subst_literal : subst -> literal bind -> literal
      (** Apply substitution to the literal *)

    val subst_clause : subst -> clause bind -> clause
      (** Apply substitution to the clause *)

    val remove_first_subst : subst -> clause bind -> clause
      (** Remove first body element of the clause, after substitution *)

    (** {3 Pretty-printing} *)

    val pp_literal : Format.formatter -> literal -> unit
      (** Pretty print the literal *)

    val pp_clause : Format.formatter -> clause -> unit
      (** Pretty print the clause *)

    val pp_subst : Format.formatter -> subst -> unit
      (** Pretty print the substitution *)

    (** {3 Utils} *)

    module LitMutHashtbl : Hashtbl.S with type key = literal

    module ClauseHashtbl : FHashtbl.S with type key = clause

    module ClauseMutHashtbl : Hashtbl.S with type key = clause
  end

  (** Build a Datalog module *)
  module Make(Symbol : SymbolType) : S with module Symbol = Symbol

  module DefaultSymbol : SymbolType with type t = string

  (** Default literal base, where symbols are just strings.
      No locking. *)
  module Default : S with module Symbol = DefaultSymbol
end

(** {2 Term indexing} *)
module Index : sig
  module type S = sig
    type 'a t

    module Logic : Logic.S

    type literal = Logic.literal
    type subst = Logic.subst

    val empty : unit -> 'a t
      (** Empty index. *)

    val is_empty : _ t -> bool
      (** Is the index empty? *)

    val map : 'a t -> literal -> ('a option -> 'a option) -> 'a t
      (** Maps the value associated to this literal (modulo alpha-renaming)
          to a value. None indicates that the literal is not present, or
          that the literal is to be removed. *)

    val retrieve_generalizations : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                                   'a t Logic.bind -> literal Logic.bind -> 'b
      (** Fold on generalizations of given literal *)

    val retrieve_specializations : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                                   'a t Logic.bind -> literal Logic.bind -> 'b
      (** Fold on specializations of given literal *)

    val retrieve_unify : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                         'a t Logic.bind -> literal Logic.bind -> 'b
      (** Fold on content that is unifiable with given literal *)

    val retrieve_renaming : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                         'a t Logic.bind -> literal Logic.bind -> 'b
      (** Fold on elements that are alpha-equivalent to given literal *)

    val fold : ('b -> literal -> 'a -> 'b) -> 'b -> 'a t -> 'b
      (** Fold on all indexed elements *)

    val size : _ t -> int
      (** Number of indexed elements (linear time) *)
  end

  module Make(L : Logic.S) : S with module Logic = L
end

(** {2 A Datalog database, i.e. a set of clauses closed under the application
       of non-unit clauses} *)
module DB : sig
  module type S = sig
    module Logic : Logic.S

    type literal = Logic.literal

    type clause = Logic.clause

    type t
      (** The type for a database of facts and clauses, with
          incremental fixpoint computation *)

    exception UnsafeClause

    type explanation =
      | Axiom
      | Resolution of clause * literal
      (** Explanation for a clause or fact *)

    type result =
      | NewFact of literal
      | NewGoal of literal
      | NewRule of clause

    type action =
      | AddClause of clause * explanation
      | AddFact of literal * explanation   (* shortcut *)
      | AddGoal of literal

    type handler = result -> action list
      (** A handler is a piece of external code that knows how to interpret
          some results *)

    val empty : unit -> t
      (** Empty database *)

    val copy : t -> t
      (** Get a (shallow) that can be used for backtracking without modifying
          the given DB. This is quite cheap. *)

    val add_handler : t -> handler -> unit
      (** All results of the DB will be given to the handler, from now on.
          Handlers too are copied by [copy]. *)

    val mem : t -> clause -> bool
      (** Is the clause member of the DB? *)

    val propagate : t -> result Sequence.t
      (** Compute the fixpoint of the current Database's state *)

    (** The modification functions ({!add}, {!add_fact}, {!add_goal}...) modify
        the database, and then update the fixpoint. They return a list of
        {b results}, ie the new information that has been discovered during
        propagation. *)

    val add : t -> clause -> result Sequence.t
      (** Add the clause/fact to the DB as an axiom, updating fixpoint.
          It returns the list of deduced new results.
          UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

    val add_fact : t -> literal -> result Sequence.t
      (** Add a fact (ground unit clause) *)

    val add_goal : t -> literal -> result Sequence.t
      (** Add a goal to the DB. The goal is used to trigger backward chaining
          (calling goal handlers that could help solve the goal) *)

    val add_seq : t -> clause Sequence.t -> result Sequence.t
      (** Add a whole sequence of clauses, in batch. *)

    val add_action : t -> action -> result Sequence.t
      (** Add an action to perform *)
    
    val add_actions : t -> action Sequence.t -> result Sequence.t
      (** Add a finite set of actions *)

    val raw_add : t -> action -> unit
      (** Add the action but does not propagate yet. The user needs to call
          {! propagate} by herself *)

    val match_with : t -> literal ->
                    (literal Logic.bind -> Logic.subst -> unit) -> unit
      (** match the given literal with facts of the DB, calling the handler on
          each fact that match (with the corresponding substitution) *)

    val size : t -> int
      (** Size of the DB *)

    val fold : ('a -> clause -> 'a) -> 'a -> t -> 'a
      (** Fold on all clauses in the current DB (including fixpoint) *)

    val goal_mem : t -> literal -> bool
      (** Is the given literal a goal? *)
      (* XXX: should it be true if the lit is subsumed by a goal? *)

    val goals : t -> literal Sequence.t
      (** Iterate on all current goals *)

    val support : t -> literal -> literal list
      (** Explain the given fact by returning a set of facts that imply it
          under the current clauses, or raise Not_found *)

    val premises : t -> literal -> clause * literal list
      (** Immediate premises of the fact (ie the facts that resolved with
          a clause to give the literal), plus the clause that has been used. *)

    val explanations : t -> clause -> explanation Sequence.t
      (** Get all the explanations that explain why this clause is true *)
  end
  
  module Make(L : Logic.S) : S with module Logic = L

  val version : string
end

(** {2 Parser for Datalog files (syntax is a subset of prolog)} *)
module Parser : sig
  type token
  
  val parse_literal :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.literal
  
  val parse_clause :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.clause
  
  val parse_file :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.clause list
end

(** {2 Lexer for parsing Datalog files} *)
module Lexer : sig
  val token : Lexing.lexbuf -> Parser.token
end
