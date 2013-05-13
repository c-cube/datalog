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

(** {2 Interface file} *)

(** Main module, that exposes datatypes for logic literals and clauses,
    functions to manipulate them, and functions to compute the fixpoint
    of a set of clauses *)
module Logic : sig
  module type S = sig
    (** {2 Literals and clauses} *)

    type symbol
      (** Abstract type of symbols *)

    type literal
      (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
          array is the predicate, then arguments follow *)

    type clause
      (** A datalog clause, i.e. head :- body_1, ..., body_n *)

    type soft_lit = symbol * [`Var of int | `Symbol of symbol] list
    type soft_clause = soft_lit * soft_lit list

    type subst
      (** A substitution maps variables to symbols *)

    type 'a bind = ('a * int)
      (** A context in which to interpret variables in a literal or clause.
          The context is an offset that is implicitely applied to variables *)

    (** {3 Constructors and destructors} *)

    val mk_literal : symbol -> [< `Var of int | `Symbol of symbol] list -> literal
      (** Helper to build a literal. Arguments are either variables or symbols; if they
          variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

    val of_soft_lit : soft_lit -> literal

    val mk_literal_s : string -> [< `Var of int | `Symbol of string] list -> literal
      (** Same as [mk_literal], but converts strings to symbols on-the-fly *)

    val open_literal : literal -> soft_lit
      (** Deconstruct a literal *)

    val mk_clause : literal -> literal list -> clause
      (** Create a clause from a conclusion and a list of premises *)

    val of_soft_clause : soft_clause -> clause

    val open_clause : clause -> soft_clause
      (** Deconstruct a clause *)

    val is_var : int -> bool
      (** A variable is a negative int *)

    val is_ground : literal -> bool
      (** Is the literal ground (a fact)? *)

    val arity : literal -> int
      (** Number of subliterals of the literal. Ex for p(a,b,c) it returns 3 *)

    (** {3 Comparisons} *)

    val eq_literal : literal -> literal -> bool
      (** Are the literals equal? *)

    val hash_literal : literal -> int
      (** Hash the literal *)

    val compare_literal : literal -> literal -> int
      (** Arbitrary comparison of literals (lexicographic) *)

    val check_safe : clause -> bool
      (** A datalog clause is safe iff all variables in its head also occur in its body *)

    val is_fact : clause -> bool
      (** A fact is a ground clause with empty body *)

    val compare_clause : clause -> clause -> int
      (** Lexicographic comparison of clauses *)

    val eq_clause : clause -> clause -> bool
      (** Check whether clauses are (syntactically) equal *)

    val hash_clause : clause -> int
      (** Hash the clause *)

    (** {3 Unification, matching and substitutions} *)

    exception UnifFailure

    val empty_subst : subst
      (** The empty substitution *)

    (* TODO external API to build substitutions *)

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

    (** {3 Pretty-printing} *)

    val pp_literal : Format.formatter -> literal -> unit
      (** Pretty print the literal *)

    val pp_clause : Format.formatter -> clause -> unit
      (** Pretty print the clause *)

    val pp_subst : Format.formatter -> subst -> unit
      (** Pretty print the substitution *)

    (** {2 The Datalog unit resolution algorithm} *)

    exception UnsafeClause

    type db
      (** A database of facts and clauses, with incremental fixpoint computation *)

    type explanation =
      | Axiom
      | Resolution of clause * literal
      (** Explanation for a clause or fact *)

    val db_create : unit -> db
      (** Create a DB *)

    val db_mem : db -> clause -> bool
      (** Is the clause member of the DB? *)

    val db_add : db -> clause -> unit
      (** Add the clause/fact to the DB as an axiom, updating fixpoint.
          UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

    val db_add_fact : db -> literal -> unit
      (** Add a fact (ground unit clause) *)

    val db_goal : db -> literal -> unit
      (** Add a goal to the DB. The goal is used to trigger backward chaining
          (calling goal handlers that could help solve the goal) *)

    val db_match : db -> literal -> (literal bind -> subst -> unit) -> unit
      (** match the given literal with facts of the DB, calling the handler on
          each fact that match (with the corresponding substitution) *)

    val db_size : db -> int
      (** Size of the DB *)

    val db_fold : ('a -> clause -> 'a) -> 'a -> db -> 'a
      (** Fold on all clauses in the current DB (including fixpoint) *)

    type fact_handler = literal -> unit
    type goal_handler = literal -> unit

    val db_subscribe_fact : db -> symbol -> fact_handler -> unit
    val db_subscribe_goal : db -> goal_handler -> unit

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

  (** Default literal base, where symbols are just strings.
      No locking. *)
  module Default : S with type symbol = string
end

(** Parser for Datalog files (syntax is a subset of prolog) *)
module Parser : sig
  type token
  val parse_literal :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.literal
  val parse_clause :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.clause
  val parse_file :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.clause list
end

(** Lexer for parsing Datalog files *)
module Lexer : sig
  val token : Lexing.lexbuf -> Parser.token
end
