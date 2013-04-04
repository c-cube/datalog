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

(** {1 Logic types: literals and clauses, substitutions and unification} *)

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

  module T : Logic_terms.S with type symbol = symbol and type payload = unit

  type literal = T.t
    (** An algebraic term, can be arbitrary *)

  type clause = private
    | Clause of literal * literal list
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  type context = T.context

  (** {3 Constructors and destructors} *)

  val mk_clause : literal -> literal list -> clause
    (** Create a clause from a conclusion and a list of premises *)

  (** {3 Comparisons} *)

  val conclusion : clause -> literal
    (** Conclusion of the clause *)

  val body : clause -> literal Sequence.t
    (** Body of the clause *)

  val vars : literal -> literal Sequence.t
    (** Variables of the given set of terms *)

  val all_lits : clause -> literal Sequence.t
    (** All the literals of the clause *)

  val check_safe : clause -> bool
    (** A datalog clause is safe iff all variables in its head also occur in its body *)

  val is_fact : clause -> bool
    (** A fact is a ground clause with empty body *)

  val eq_clause : clause -> clause -> bool
    (** Check whether clauses are (syntactically) equal *)

  val compare_clause : clause -> clause -> int
    (** Arbitrary comparison *)

  val hash_clause : clause -> int
    (** Hash the clause *)

  (** {3 Unification, matching and substitutions} *)

  val apply_clause : clause -> context -> clause
    (** Eval the clause in the given context *)

  val remove_first : clause -> context -> clause
    (** Remove first body element of the clause, after substitution *)

  (** {3 Pretty-printing} *)

  val pp_clause : Format.formatter -> clause -> unit
    (** Pretty print the clause *)

  (** {2 Utils} *)

  module LitMutHashtbl : Hashtbl.S with type key = literal

  module ClauseHashtbl : PersistentHashtbl.S with type key = clause

  module ClauseMutHashtbl : Hashtbl.S with type key = clause
end

(** Build a Datalog module *)
module Make(Symbol : SymbolType) : S with module Symbol = Symbol

module DefaultSymbol : SymbolType with type t = string

(** Default literal base, where symbols are just strings.
    No locking. *)
module Default : S with module Symbol = DefaultSymbol
