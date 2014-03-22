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

(** {2 Main module type} *)

(** Main module, that exposes datatypes for logic literals and clauses,
    functions to manipulate them, and functions to compute the fixpoint
    of a set of clauses *)

module type S = sig
  module Base : Base.S
  type const = Base.Const.t

  (** {2 Higher level API} *)

  (** This part of the API can be used to avoid building variables
      yourself. Calling [quantify3 f] with call [f] with 3 distinct
      variables, and [f] can use those variables to, for instance,
      build a clause *)

  val quantify1 : (Base.T.t -> 'a) -> 'a
  val quantify2 : (Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantify3 : (Base.T.t -> Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantify4 : (Base.T.t -> Base.T.t -> Base.T.t -> Base.T.t -> 'a) -> 'a
  val quantifyn : int -> (Base.T.t list -> 'a) -> 'a

  (** {2 The Datalog unit resolution algorithm} *)

  type t
  (** A database of facts and clauses, with incremental fixpoint computation *)

  type explanation =
    | Axiom
    | Resolution of Base.C.t * Base.Lit.t
    (** Explanation for a clause or fact. *)

  val create : unit -> t
    (** Create a DB *)

  val copy : t -> t
    (** Deep copy of the DB *)

  val mem : t -> Base.C.t -> bool
    (** Is the clause member of the DB? *)

  val add : ?expl:explanation -> t -> Base.C.t -> t
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val add_fact : ?expl:explanation -> t -> Base.Lit.t  -> t
    (** Add a fact (ground unit clause) *)

  val goal : t -> Base.Lit.t -> t
    (** Add a goal to the DB. The goal is used to trigger backward chaining
        (calling goal handlers that could help solve the goal) *)

  val match_ : t -> Base.Lit.t -> (Base.Lit.t -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match *)

  val query : t -> Base.Lit.t -> int list -> (const list -> unit) -> unit
    (** Like {!db_match}, but the additional int list is used to select
        bindings of variables in the literal. Their bindings, in the same
        order, are given to the callback. *)

  val size : t -> int
    (** Size of the DB *)

  val fold : ('a -> Base.C.t -> 'a) -> 'a -> t -> 'a
    (** Fold on all clauses in the current DB (including fixpoint) *)

  type fact_handler = Base.Lit.t -> unit
  type goal_handler = Base.Lit.t -> unit

  val subscribe_fact : t -> const -> fact_handler -> unit
  val subscribe_all_facts : t -> fact_handler -> unit
  val subscribe_goal : t -> goal_handler -> unit

  type user_fun = Base.Lit.t -> Base.Lit.t

  val add_fun : t -> const -> user_fun -> t
    (** Add a function to be called on new literals. Only one function per
        symbol can be registered. *)

  val goals : t -> (Base.Lit.t -> unit) -> unit
    (** Iterate on all current goals *)

  val explain : t -> Base.Lit.t -> Base.Lit.t list
    (** Explain the given fact by returning a list of facts that imply it
        under the current clauses, or raise Not_found *)

  val premises : t -> Base.Lit.t -> Base.C.t * Base.Lit.t list
    (** Immediate premises of the fact (ie the facts that resolved with
        a clause to give the literal), plus the clause that has been used. *)

  val explanations : t -> Base.C.t -> explanation list
    (** Get all the explanations that explain why this clause is true *)

  (** {2 Querying} *)

  module Query : sig
    type set
      (** mutable set of term lists *)

    val ask : t -> ?neg:Base.Lit.t list -> int array -> Base.Lit.t list -> set
      (** Given a list of variables, and a list of literals that contain those
          variables, return a set. Each element of the set is an instantiation
          of the variables such that all instantiated literals are facts of
          the [db]. [neg] is an optional list of literals that must be false
          for an instantiation to be an answer.
          This is lazy, and will only be evaluated upon calls to {! iter},
          {! to_list} or other similar functions. The answers will be cached
          in the set and readily available thereafter. *)

    val iter : set -> (Base.T.t array -> unit) -> unit
      (** Evaluate the set by iterating on it *)

    val to_list : set -> Base.T.t array list
      (** Convert to a list *)

    val cardinal : set -> int
      (** Number of elements of the set *)

    val pp_plan : Format.formatter -> set -> unit
      (** Print query plan *)
  end
end

(** {2 Main functor} *)

module Make(Base : Base.S) : S with module Base = Base

