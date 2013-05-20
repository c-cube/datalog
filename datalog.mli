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
    | Query of int      (* for internal use *)
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
    (** Helper to build a literal. Arguments are either variables or symbols; if they
        variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

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

  (* TODO a copy operator (even if expensive) *)

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

  (** {2 Earley resolution} *)

  module Earley : sig
    type query
      (** Environment for running queries *)

    type db
      (** Contains user-provided clauses, as context for queries *)

    type explanation =
      | Program
      | Instantiation of clause
      | Reduction of clause * literal
      (** explanations for Earley resolution *)

    val db_create : unit -> db
      (** Fresh db *)

    val iter_queries : db -> (query -> unit) -> unit
      (** Iterate on the active queries *)

    val ask : ?within:clause list -> db -> literal list -> int list ->
              (term list -> unit) -> query
      (** New query that runs against the given [db]. It will transmit
          the instances of the given list of variables (int list) that
          satisfy the list of literals, to the handler. [within] contains
          additional clauses for the processing of the query. *)

    val register : query -> (term list -> unit) -> unit
      (** Register another callback to the query *)

    val del_query : query -> unit
      (** Terminate the query. It will no longer receive updates from its [db] *)

    val db_add : db -> clause -> unit
      (** Add a clause to the environment (will update attached queries) *)
  end
end

(** Signature for a symbol type. It must be hashable, comparable and printable *)
module type SymbolType = sig
  include Hashtbl.HashedType
  val to_string : t -> string
end

(** Hashconsing of symbols *)
module Hashcons(S : SymbolType) : sig
  include SymbolType with type t = S.t

  val make : S.t -> S.t
    (** Hashcons the symbol *)
end

(** Build a Datalog module *)
module Make(Symbol : SymbolType) : S with type symbol = Symbol.t

(** Parser for Datalog files (syntax is a subset of prolog) *)
module Parser : sig
  type token
  val parse_literal :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.literal
  val parse_literals :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.literal list
  val parse_clause :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.clause
  val parse_file :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.file
end

(** Lexer for parsing Datalog files *)
module Lexer : sig
  val token : Lexing.lexbuf -> Parser.token

  val print_location : Lexing.lexbuf -> string
end

(** Symbols are just hashconsed strings *)
module StringSymbol : SymbolType with type t = string

(** Default literal base, where symbols are just strings.
    No locking. *)
module Default : sig
  include S with type symbol = string

  type vartbl = {
    mutable vartbl_count : int;
    vartbl_tbl : (string,int) Hashtbl.t;
  }

  val mk_vartbl : unit -> vartbl

  val literal_of_ast : ?tbl:vartbl -> Ast.literal -> literal

  val clause_of_ast : Ast.clause -> clause
end

val version : string
  (** Version of the library *)
