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

(** {1 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries
    with non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics"
*)

exception NonStratifiedProgram

(** {2 DB} *)

module type S = sig
  module Base : Base.S
  type const = Base.Const.t
  type scope = Base.scope

  (** A DB stores facts and clauses, that constitute a logic program.
      Facts and clauses can only be added.

      Non-stratified programs will be rejected with NonStratifiedProgram.  *)

  type t
    (** A database is a repository for Datalog clauses. *)

  type interpreter = Base.T.t -> Base.C.t list
    (** Interpreted predicate. It takes terms which have a given
        symbol as head, and return a list of (safe) clauses that
        have the same symbol as head, and should unify with the
        query term. *)

  val create : ?parent:t -> unit -> t

  val copy : t -> t

  val add_fact : t -> Base.T.t -> t
  val add_facts : t -> Base.T.t list -> t

  val add_clause : t -> Base.C.t -> t
  val add_clauses : t -> Base.C.t list -> t

  val interpret : ?help:string -> t -> const -> interpreter -> t
    (** Add an interpreter for the given constant. Goals that start with
        this constant will be given to all registered interpreters, all
        of which can add new clauses. The returned clauses must
        have the constant as head symbol. *)

  val interpret_list : t -> (const * string * interpreter) list -> t
    (** Add several interpreters, with their documentation *)

  val is_interpreted : t -> const -> bool
    (** Is the constant interpreted by some OCaml code? *)

  val add_builtin : t -> Base.Const.t -> Base.BuiltinFun.t -> t
    (** Add a builtin fun *)

  val builtin_funs : t -> Base.BuiltinFun.map

  val eval : t -> Base.T.t -> Base.T.t
    (** Evaluate the given term at root *)

  val help : t -> string list
    (** Help messages for interpreted predicates *)

  val num_facts : t -> int
  val num_clauses : t -> int
  val size : t -> int

  val find_facts : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                   (Base.T.t -> Base.Subst.t -> unit) -> unit
    (** find facts unifying with the given term, and give them
        along with the unifier, to the callback *)

  val find_clauses_head : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                          (Base.C.t -> Base.Subst.t -> unit) -> unit
    (** find clauses whose head unifies with the given term,
        and give them along with the unifier, to the callback *)

  val find_interpretation : ?oc:bool -> t -> scope -> Base.T.t -> scope ->
                            (Base.C.t -> Base.Subst.t -> unit) -> unit
    (** Given an interpreted goal, try all interpreters on it,
        and match the query against their heads. Returns clauses
        whose head unifies with the goal, along with the substitution. *)

  (** {2 Query} *)

  val ask : ?oc:bool -> ?with_rules:Base.C.t list -> ?with_facts:Base.T.t list ->
            t -> Base.T.t -> Base.T.t list
    (** Returns the answers to a query in a given DB.
        Additional facts and rules can be added in a local scope.
        @param oc enable occur-check in unification (default [false]) *)

  val ask_lits : ?oc:bool -> ?with_rules:Base.C.t list -> ?with_facts:Base.T.t list ->
                 t -> Base.T.t list -> Base.Lit.t list -> Base.T.t list
    (** Extension of {! ask}, where the query ranges over the list of
        variables (the term list), all of which must be bound in
        the list of literals that form a constraint.

        [ask_lits db vars lits] queries over variables [vars] with
        the constraints given by [lits]. 

        Conceptually, the query adds a clause (v1, ..., vn) :- lits, which
        should respect the same safety constraint as other clauses.

        @return a list of answers, each of which is a list of terms that
          map to the given list of variables. *)
end

(** {2 Generic implementation} *)

module Make(B : Base.S) : S with module Base = B

module Default : sig
  include S with module Base = Base.Default

  val default_interpreters : (const * string * interpreter) list
    (** List of default interpreters for some symbols, mostly
        infix predicates *)

  val builtin : (const * Base.BuiltinFun.t) list
    (** Default builtin functions *)

  val setup_default : t -> t
    (** Load the default interpreters and builtin functions into the DB *)
end
