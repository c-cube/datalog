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

(** {1 A Datalog database, i.e. a set of clauses closed under the application
       of non-unit clauses} *)

module type S = sig
  module Logic : Logic.S

  type literal = Logic.literal

  type clause = Logic.clause

  type context = Logic.context

  type t
    (** The type for a database of facts and clauses, with
        incremental fixpoint computation *)

  exception UnsafeClause

  type explanation =
    | Axiom
    | HyperResolution of clause * literal list
    (** Explanation for a clause or fact *)

  type result =
    | NewFact of literal * explanation
    | NewClause of clause * explanation
    | NewGoal of literal

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

  val match_with : t -> context -> literal -> context -> (literal -> unit) -> unit
    (** match the given literal with facts of the DB, calling the handler on
        each fact that match (with its variables bound in its context) *)

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
