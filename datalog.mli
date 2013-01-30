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

(** Main module, that exposes datatypes for logic terms and rules,
    functions to manipulate them, and functions to compute the fixpoint
    of a set of rules *)
module Logic : sig
  module type S = sig
    (** {2 Terms and rules} *)

    type symbol
      (** Abstract type of symbols *)

    type term
      (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
          array is the predicate, then arguments follow *)

    type rule
      (** A datalog rule, i.e. head :- body_1, ..., body_n *)

    type subst
      (** A substitution maps variables to symbols *)

    (** {3 Constructors and destructors} *)

    val mk_term : symbol -> [`Var of int | `Symbol of symbol] list -> term
      (** Helper to build a term. Arguments are either variables or symbols; if they
          variables indexes *must* be negative (otherwise it will raise Invalid_argument *)

    val mk_term_s : string -> [`Var of int | `Symbol of string] list -> term
      (** Same as [mk_term], but converts strings to symbols on-the-fly *)

    val open_term : term -> symbol * [`Var of int | `Symbol of symbol] list
      (** Deconstruct a term *)

    val mk_rule : term -> term list -> rule
      (** Create a rule from a conclusion and a list of premises *)

    val open_rule : rule -> term * term list
      (** Deconstruct a rule *)

    val is_var : int -> bool
      (** A variable is a negative int *)

    val is_ground : term -> bool
      (** Is the term ground (a fact)? *)

    val arity : term -> int
      (** Number of subterms of the term. Ex for p(a,b,c) it returns 3 *)

    (** {3 Comparisons} *)

    val eq_term : term -> term -> bool
      (** Are the terms equal? *)

    val hash_term : term -> int
      (** Hash the term *)

    val compare_term : term -> term -> int
      (** Arbitrary comparison of terms (lexicographic) *)

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

    (** {3 Comparisons} *)

    val subst_term : subst -> term -> term
      (** Apply substitution to the term *)

    val subst_rule : subst -> rule -> rule
      (** Apply substitution to the rule *)

    (** {3 Pretty-printing} *)

    val pp_term : Format.formatter -> term -> unit
      (** Pretty print the term *)

    val pp_rule : Format.formatter -> rule -> unit
      (** Pretty print the rule *)

    val pp_subst : Format.formatter -> subst -> unit
      (** Pretty print the substitution *)

    (** {2 The Datalog unit resolution algorithm} *)

    type db
      (** A database of facts and rules, with incremental fixpoint computation *)

    val db_create : unit -> db
      (** Create a DB *)

    val db_mem : db -> rule -> bool
      (** Is the rule member of the DB? *)

    val db_add : db -> rule -> unit
      (** Add the rule/fact to the DB as an axiom, updating fixpoint *)

    val db_match : db -> term -> (term -> subst -> unit) -> unit
      (** match the given term with facts of the DB, calling the handler on
          each fact that match (with the corresponding substitution) *)

    val db_size : db -> int
      (** Size of the DB *)

    val db_fold : ('a -> rule -> 'a) -> 'a -> db -> 'a
      (** Fold on all rules in the current DB (including fixpoint) *)

    val db_subscribe : db -> symbol -> (term -> unit) -> unit
      (** [db_subscribe db symbol handler] causes [handler] to be called with
          any new fact that has head symbol [symbol] from now on *)

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
end

(** Parser for Datalog files (syntax is a subset of prolog) *)
module Parser : sig
  type token
  val parse_term :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.term
  val parse_rule :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.rule
  val parse_file :
    (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic.Default.rule list
end

(** Lexer for parsing Datalog files *)
module Lexer : sig
  val token : Lexing.lexbuf -> Parser.token
end
