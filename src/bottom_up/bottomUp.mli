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

(** {1 Main Datalog module} *)

(** Datalog is a fragment of first-order logic. It can be viewed as
    relational algebra with deduction/recursion (through {b Horn clauses}).

    A Datalog database contains {b facts} and {b clauses} (or rules). A fact
    is simply a relational atom, like [mother(mary, john)]. A Horn clause 
    is a deduction rule, for instance [grandmother(X,Y) :- mother(X,Z), parent(Z,Y)] 
    and [parent(X,Y) :- mother(X,Y)].

    The {! Datalog_cli} module provides a simple client that demonstrates most
    of the features of Datalog. Some basic examples can be found in the
    {i tests/} directory.

    {!indexlist}
*)

(** {2 Universal type} *)

(** This module is present to allow the user to extend explanations
    with her own types. *)
module Univ : sig
  (** This is largely inspired by {{: https://ocaml.janestreet.com/?q=node/18}this thread} *)

  type t (** The universal type *)

  type 'a embedding 
    (** Conversion between the universal type and 'a *)

  val embed : unit -> 'a embedding
    (** Create a new embedding. Values packed by a given embedding can
        only be unpacked by the same embedding. *)

  val pack : 'a embedding -> 'a -> t
  val unpack : 'a embedding -> t -> 'a option
  val compatible : 'a embedding -> t -> bool
end

(** {2 Main module type} *)

(** Main module, that exposes datatypes for logic literals and clauses,
    functions to manipulate them, and functions to compute the fixpoint
    of a set of clauses *)

module type S = sig
  (** {2 Literals and clauses} *)

  type symbol
    (** Abstract type of symbols (individual objects) *)

  type term = private
    | Var of int
    | Const of symbol
    (** Individual object *)

  val mk_var : int -> term
  val mk_const : symbol -> term

  type literal
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type clause
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  type soft_lit = symbol * term list
  type soft_clause = soft_lit * soft_lit list

  (** {3 Constructors and destructors} *)

  val mk_literal : symbol -> term list -> literal
    (** Helper to build a literal. Arguments are either variables or constants *)

  val of_soft_lit : soft_lit -> literal

  val open_literal : literal -> soft_lit
    (** Deconstruct a literal *)

  val mk_clause : literal -> literal list -> clause
    (** Create a clause from a conclusion and a list of premises *)

  val of_soft_clause : soft_clause -> clause

  val open_clause : clause -> soft_clause
    (** Deconstruct a clause *)

  val is_ground : literal -> bool
    (** Is the literal ground (a fact)? *)

  val arity : literal -> int
    (** Number of subliterals of the literal. Ex for p(a,b,c) it returns 3 *)

  (** {3 Comparisons} *)

  val eq_term : term -> term -> bool

  val eq_literal : literal -> literal -> bool
    (** Are the literals equal? *)

  val hash_literal : literal -> int
    (** Hash the literal *)

  val check_safe : clause -> bool
    (** A datalog clause is safe iff all variables in its head also occur in its body *)

  val is_fact : clause -> bool
    (** A fact is a ground clause with empty body *)

  val eq_clause : clause -> clause -> bool
    (** Check whether clauses are (syntactically) equal *)

  val hash_clause : clause -> int
    (** Hash the clause *)

  (** {3 Pretty-printing} *)

  val pp_term : Format.formatter -> term -> unit

  val pp_literal : Format.formatter -> literal -> unit
    (** Pretty print the literal *)

  val pp_clause : Format.formatter -> clause -> unit
    (** Pretty print the clause *)

  (** {2 Higher level API} *)

  (** This part of the API can be used to avoid building variables
      yourself. Calling [quantify3 f] with call [f] with 3 distinct
      variables, and [f] can use those variables to, for instance,
      build a clause *)

  val quantify1 : (term -> 'a) -> 'a
  val quantify2 : (term -> term -> 'a) -> 'a
  val quantify3 : (term -> term -> term -> 'a) -> 'a
  val quantify4 : (term -> term -> term -> term -> 'a) -> 'a
  val quantifyn : int -> (term list -> 'a) -> 'a

  (** {2 The Datalog unit resolution algorithm} *)

  exception UnsafeClause

  type db
    (** A database of facts and clauses, with incremental fixpoint computation *)

  type explanation =
    | Axiom
    | Resolution of clause * literal
    | ExtExplanation of string * Univ.t
    (** Explanation for a clause or fact. It is extensible through universal types. *)

  val db_create : unit -> db
    (** Create a DB *)

  val db_copy : db -> db
    (** Deep copy of the DB *)

  val db_mem : db -> clause -> bool
    (** Is the clause member of the DB? *)

  val db_add : ?expl:explanation -> db -> clause -> unit
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val db_add_fact : ?expl:explanation -> db -> literal -> unit
    (** Add a fact (ground unit clause) *)

  val db_goal : db -> literal -> unit
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val db_match : db -> literal -> (literal -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match *)

  val db_query : db -> literal -> int list -> (symbol list -> unit) -> unit
    (** Like {!db_match}, but the additional int list is used to select
        bindings of variables in the literal. Their bindings, in the same
        order, are given to the callback. *)

  val db_size : db -> int
    (** Size of the DB *)

  val db_fold : ('a -> clause -> 'a) -> 'a -> db -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

  type fact_handler = literal -> unit
  type goal_handler = literal -> unit

  val db_subscribe_fact : db -> symbol -> fact_handler -> unit
  val db_subscribe_all_facts : db -> fact_handler -> unit
  val db_subscribe_goal : db -> goal_handler -> unit

  type user_fun = soft_lit -> soft_lit

  val db_add_fun : db -> symbol -> user_fun -> unit
    (** Add a function to be called on new literals. Only one function per
        symbol can be registered. *)

  val db_goals : db -> (literal -> unit) -> unit
    (** Iterate on all current goals *)

  val db_explain : db -> literal -> literal list
    (** Explain the given fact by returning a list of facts that imply it
        under the current clauses, or raise Not_found *)

  val db_premises : db -> literal -> clause * literal list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the literal), plus the clause that has been used. *)

  val db_explanations : db -> clause -> explanation list
    (** Get all the explanations that explain why this clause is true *)

  (** {2 Querying} *)

  module Query : sig
    type set
      (** mutable set of term lists *)

    val ask : db -> ?neg:literal list -> int array -> literal list -> set
      (** Given a list of variables, and a list of literals that contain those
          variables, return a set. Each element of the set is an instantiation
          of the variables such that all instantiated literals are facts of
          the [db]. [neg] is an optional list of literals that must be false
          for an instantiation to be an answer.
          This is lazy, and will only be evaluated upon calls to {! iter},
          {! to_list} or other similar functions. The answers will be cached
          in the set and readily available thereafter. *)

    val iter : set -> (term array -> unit) -> unit
      (** Evaluate the set by iterating on it *)

    val to_list : set -> term array list
      (** Convert to a list *)

    val cardinal : set -> int
      (** Number of elements of the set *)

    val pp_plan : Format.formatter -> set -> unit
      (** Print query plan *)
  end
end

(** {2 Signature for a symbol type} *)

(** A symbol must be hashable, comparable and printable. *)
module type SymbolType = sig
  include Hashtbl.HashedType
  val to_string : t -> string
end

(** {2 Hashconsing of symbols} *)

module Hashcons(S : SymbolType) : sig
  include SymbolType with type t = S.t

  val make : S.t -> S.t
    (** Hashcons the symbol *)
end

(** {2 Main functor} *)

(** Build a Datalog module. This allows to specialize Datalog for a user-defined
  type of {b atoms}. Strings are a good default, but more complicated types
  can be useful.  *)
module Make(Symbol : SymbolType) : S with type symbol = Symbol.t

