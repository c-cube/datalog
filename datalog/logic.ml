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

(** {2 Logic types: literals and clauses, substitutions and unification} *)
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

module Make(Symbol : SymbolType) = struct
  (** {2 Literals and clauses} *)
  module Symbol = struct
    include Symbol
    let pp fmt s = Format.pp_print_string fmt (Symbol.to_string s)
  end

  module Payload = struct
    type t = unit
    let default = ()
  end

  module T = Logic_terms.Term(Symbol)(Payload)

  type symbol = Symbol.t
    (** Abstract type of symbols *)

  type literal = T.t

  type clause =
    | Clause of literal * literal list
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  (** {3 Constructors and destructors} *)

  type context = T.context

  (** Create a clause from a conclusion and a list of premises *)
  let mk_clause head premises = Clause (head, premises)

  (** {3 Comparisons} *)

  let conclusion clause =
    match clause with
    | Clause (head, _) -> head

  let body clause =
    match clause with
    | Clause (_, body) -> Sequence.of_list body

  let vars lit =
    Sequence.from_iter
      (fun k -> T.iter_vars k lit)

  let all_lits clause = match clause with
    | Clause (head, body) ->
      Sequence.from_iter (fun k ->
        k head; List.iter k body)

  (** A datalog clause is safe iff all variables in its head also occur
      in its body *)
  let check_safe clause =
    Sequence.for_all
      (fun v -> Sequence.exists
        (fun v' -> v == v')
        (Sequence.flatMap vars (body clause)))
      (vars (conclusion clause))

  (** A fact is a ground clause with empty body *)
  let is_fact clause =
    Sequence.is_empty (body clause) && T.ground (conclusion clause)

  (** Syntactic equality of clauses *)
  let eq_clause c1 c2 = match c1, c2 with
    | Clause (concl1, body1), Clause (concl2, body2) ->
      T.eq concl1 concl2 &&
      try List.for_all2 T.eq body1 body2 with Invalid_argument _ -> false

  let compare_clause c1 c2 =
    let rec cmp c1 c2 = match c1, c2 with
    | Clause (concl1, body1), Clause (concl2, body2) ->
      let c = T.compare concl1 concl2 in
      if c <> 0 then c
      else cmp_list body1 body2 
    and cmp_list l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | t1::l1', t2::l2' ->
      let c = T.compare t1 t2 in 
      if c <> 0 then c else cmp_list l1' l2'
    in cmp c1 c2

  (** Hash the clause *)
  let hash_clause c = match c with
    | Clause (head, body) ->
      List.fold_left
        (fun h lit -> (T.hash lit) * 65599 + h)
        (T.hash head) body

  (** {3 Unification, matching and substitutions} *)

  (** Apply substitution to the clause. *)
  let apply_clause clause ctx =
    (* use a consistent renaming for all literals *)
    let renaming = T.mk_renaming () in
    match clause with
    | Clause (h, body) ->
      let h' = T.apply ~renaming h ctx in
      let body' = List.map (fun t -> T.apply ~renaming t ctx) body in
      mk_clause h' body'

  (** Remove first body element of the clause, after substitution *)
  let remove_first clause ctx =
    let renaming = T.mk_renaming () in
    match clause with
    | Clause (_, []) -> raise (Invalid_argument "remove_first_subst")
    | Clause (h, _::body) ->
      let h' = T.apply ~renaming h ctx in
      let body' = List.map (fun t -> T.apply ~renaming t ctx) body in
      mk_clause h' body'

  (** {3 Pretty-printing} *)

  let pp_clause formatter clause =
    match clause with
    | Clause (head, []) ->
      Format.fprintf formatter "%a." T.pp head
    | Clause (head, body) ->
      Format.fprintf formatter "%a :-@ %a." T.pp head
        (Sequence.pp_seq ~sep:", " T.pp) (Sequence.of_list body)

  (** {2 Utils} *)

  module LitMutHashtbl = T.Table

  (** Functional Hashtable on clauses *)
  module ClauseHashtbl = PersistentHashtbl.Make(struct
    type t = clause
    let equal = eq_clause
    let hash = hash_clause
  end)

  (** Mutable hashtable on clauses *)
  module ClauseMutHashtbl = Hashtbl.Make(struct
    type t = clause
    let equal = eq_clause
    let hash = hash_clause
  end)
end

module DefaultSymbol = struct
  type t = string
  let to_string s = s
  let of_string s = s
  let equal s1 s2 = String.compare s1 s2 = 0
  let hash s = Hashtbl.hash s
end

(** Default literal base, where symbols are just strings *)
module Default = Make(DefaultSymbol)
