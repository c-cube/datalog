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

  (** {2 Utils} *)

  module ClauseHashtbl : FHashtbl.S with type key = clause

  module ClauseMutHashtbl : Hashtbl.S with type key = clause
end

module Make(Symbol : SymbolType) = struct
  (** {2 Literals and clauses} *)
  module Symbol = Symbol

  type symbol = Symbol.t
    (** Abstract type of symbols *)

  type literal =
    | Var of int
    | Apply of symbol * literal array
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). Arguments can
        themselves be literals *)

  let rec equal_lit l1 l2 = match l1, l2 with
    | Var i, Var j -> i = j
    | Apply (s1, args1), Apply (s2, args2) when Symbol.equal s1 s2 ->
      equal_args args1 args2 0
    | _ -> false
  and equal_args l1 l2 i =
    if Array.length l1 <> Array.length l2 then false
    else if i = Array.length l1 then true
    else equal_lit l1.(i) l2.(i) && equal_args l1 l2 (i+1)

  let rec hash_lit lit = match lit with
    | Var i -> i
    | Apply (s, args) ->
      Array.fold_left (fun h arg -> (hash_lit arg) * 65599 + h) (Symbol.hash s) args

  (** Hashconsing on literals *)
  module Hashcons : Weak.S with type data = literal = Weak.Make(struct
    type t = literal
    let equal l1 l2 = equal_lit l1 l2
    let hash l = hash_lit l
  end)

  type clause =
    | Clause of literal * literal list
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  let table = Hashcons.create 5003 (* hashconsing for literals *)

  (** {3 Constructors and destructors} *)

  let mk_apply_a head args =
    Hashcons.merge table (Apply (head, args))

  let mk_apply head args =
    let args = Array.of_list args in
    mk_apply_a head args

  let mk_const s =
    mk_apply_a s [||]

  let mk_var i =
    Hashcons.merge table (Var i)

  type subst =
    | SubstEmpty
    | SubstBind of (literal * int * literal * int * subst)
    (** A substitution is a map from variables with context
        to literals with context *)

  type 'a bind = ('a * int)
    (** A context in which to interpret variables in a literal or clause.
        The context is an offset that is implicitely applied to variables *)

  (** Create a clause from a conclusion and a list of premises *)
  let mk_clause head premises = Clause (head, premises)

  let is_var x =
    match x with
    | Var _ -> true
    | Apply _ -> false

  (** Iterate on variables of the literal *)
  let vars lit =
    let rec vars k lit = 
      match lit with
      | Var _ -> k lit
      | Apply (_, args) -> Array.iter (fun arg -> vars k arg) args
    in
    Sequence.from_iter (fun k -> vars k lit)

  (** Is the literal ground (a fact)? *)
  let is_ground t = Sequence.is_empty (vars t)

  (** Number of subterms of the literal. Ex for p(a,b,c) it returns 3 *)
  let arity t = match t with
    | Var _ -> 0
    | Apply (_, args) -> Array.length args

  (** {3 Comparisons} *)

  (** Are the literals equal? *)
  let eq_literal t1 t2 = t1 == t2

  (** Hash the literal *)
  let hash_literal lit = hash_lit lit

  let conclusion clause =
    match clause with
    | Clause (head, _) -> head

  let body clause =
    match clause with
    | Clause (_, body) -> Sequence.of_list body

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
    Sequence.is_empty (body clause) && is_ground (conclusion clause)

  (** Syntactic equality of clauses *)
  let eq_clause c1 c2 = match c1, c2 with
    | Clause (concl1, body1), Clause (concl2, body2) ->
      eq_literal concl1 concl2 &&
      try List.for_all2 eq_literal body1 body2 with Invalid_argument _ -> false

  (** Hash the clause *)
  let hash_clause c = match c with
    | Clause (head, body) ->
      List.fold_left
        (fun h lit -> (hash_lit lit) * 65599 + h)
        (hash_lit head) body

  (** {3 Unification, matching and substitutions} *)

  exception UnifFailure

  let empty_subst = SubstEmpty

  let is_empty_subst = function | SubstEmpty -> true | _ -> false

  let lit_offset lit =
    let open Sequence.Infix in
    vars lit
      |> Sequence.map (function | Var i -> i | Apply _ -> assert false)
      |> fun seq -> Sequence.max ~lt:(fun x y -> x < y) seq 0

  (** Offset to avoid collisions with the given clause *)
  let offset clause =
    let open Sequence.Infix in
    let offset =
      all_lits clause
      |> Sequence.flatMap vars
      |> Sequence.map (function | Var i -> i | Apply _ -> assert false)
      |> fun seq -> Sequence.max ~lt:(fun x y -> x < y) seq 0 in
    offset + 1

  (** Find the binding for [var] in context [offset] *)
  let rec get_var subst var offset =
    match subst with
    | SubstBind (t1, o1, t2, o2, subst') ->
      if t1 == var && o1 = offset
        then get_var subst t2 o2
        else get_var subst' var offset
    | SubstEmpty -> (var, offset)

  (** Bind [v] to [t], with offsets *)
  let bind_subst subst v o_v t o_t =
    assert (is_var v);
    if v = t && o_v = o_t
      then subst
      else SubstBind (v, o_v, t, o_t, subst)

  let subst_to_seq subst =
    let rec iter k subst = match subst with
    | SubstEmpty -> ()
    | SubstBind (x, o_x, y, o_y, subst') ->
      k (x, o_x, y, o_y); iter k subst'
    in
    Sequence.from_iter (fun k -> iter k subst)

  (** Checks whether [v,o_v] occurs in [subst t,o_t] *)
  let rec occur_check subst v o_v t o_t =
    match t with
    | Apply (_, args) ->
      Sequence.exists (fun a -> occur_check subst v o_v a o_t)
        (Sequence.of_array args)
    | Var _ when v == t && o_v = o_t -> true
    | Var _ ->
      let t', o_t' = get_var subst t o_t in
      if t == t' && o_t = o_t'
        then false (* t not bound, and not equal to v *)
        else occur_check subst v o_v t' o_t' (* apply subst *)

  (** [matching pattern l] matches [pattern] against [l]; variables in [l]
      cannot be bound. Raise UnifFailure if they do not match. *)
  let matching ?(subst=empty_subst) (l1, o1) (l2, o2) =
    let rec matching subst l1 o1 l2 o2 =
      let l1, o1 = if is_var l1 then get_var subst l1 o1 else l1, o1 in
      let l2, o2 = if is_var l2 then get_var subst l2 o2 else l2, o2 in
      match l1, l2 with
      | _ when l1 == l2 && o1 = o2 -> subst
      | Var i, _ ->
        if occur_check subst l1 o1 l2 o2
          then raise UnifFailure
          else bind_subst subst l1 o1 l2 o2
      | Apply _, Var _ -> raise UnifFailure
      | Apply (s1, args1), Apply (s2, args2) ->
        if Symbol.equal s1 s2 && Array.length args1 = Array.length args2
          then match_array subst args1 o1 args2 o2 0
          else raise UnifFailure
    and match_array subst args1 o1 args2 o2 i =
      if i = Array.length args1
        then subst
        else
          let subst' = matching subst args1.(i) o1 args2.(i) o2 in
          match_array subst' args1 o1 args2 o2 (i+1)
    in matching subst l1 o1 l2 o2

  (** [unify l1 l2] tries to unify [l1] with [l2].
       Raise UnifFailure if they do not match. *)
  let unify ?(subst=empty_subst) (l1, o1) (l2, o2) =
    let rec unif subst l1 o1 l2 o2 =
      let l1, o1 = if is_var l1 then get_var subst l1 o1 else l1, o1 in
      let l2, o2 = if is_var l2 then get_var subst l2 o2 else l2, o2 in
      match l1, l2 with
      | _ when l1 == l2 && o1 = o2 -> subst
      | Var _, _ ->
        if occur_check subst l1 o1 l2 o2
          then raise UnifFailure
          else bind_subst subst l1 o1 l2 o2
      | Apply _, Var _ ->
        if occur_check subst l2 o2 l1 o1
          then raise UnifFailure
          else bind_subst subst l2 o2 l1 o1
      | Apply (s1, args1), Apply (s2, args2) ->
        if Symbol.equal s1 s2 && Array.length args1 = Array.length args2
          then unif_array subst args1 o1 args2 o2 0
          else raise UnifFailure
    and unif_array subst args1 o1 args2 o2 i =
      if i = Array.length args1
        then subst
        else
          let subst' = unif subst args1.(i) o1 args2.(i) o2 in
          unif_array subst' args1 o1 args2 o2 (i+1)
    in unif subst l1 o1 l2 o2

  (** If the literals are alpha equivalent, return the corresponding renaming *)
  let alpha_equiv ?(subst=empty_subst) (l1,o1) (l2,o2) =
    let rec unif subst l1 o1 l2 o2 =
      let l1, o1 = if is_var l1 then get_var subst l1 o1 else l1, o1 in
      let l2, o2 = if is_var l2 then get_var subst l2 o2 else l2, o2 in
      match l1, l2 with
      | _ when l1 == l2 && o1 = o2 -> subst
      | Var _, Var _ ->
        if occur_check subst l1 o1 l2 o2
          then raise UnifFailure
          else bind_subst subst l1 o1 l2 o2
      | Apply _, Var _
      | Var _, Apply _ -> raise UnifFailure
      | Apply (s1, args1), Apply (s2, args2) ->
        if Symbol.equal s1 s2 && Array.length args1 = Array.length args2
          then unif_array subst args1 o1 args2 o2 0
          else raise UnifFailure
    and unif_array subst args1 o1 args2 o2 i =
      if i = Array.length args1
        then subst
        else
          let subst' = unif subst args1.(i) o1 args2.(i) o2 in
          unif_array subst' args1 o1 args2 o2 (i+1)
    in unif subst l1 o1 l2 o2

  (** shift literal by offset *)
  let rec shift_lit lit offset =
    if offset = 0 then lit
    else match lit with
      | Var i -> mk_var (i+offset)
      | Apply (s, args) ->
        mk_apply_a s (Array.map (fun lit -> shift_lit lit offset) args)

  let shift_clause c offset =
    if offset = 0 then c
    else match c with
      | Clause (head, body) ->
        let body' = List.map (fun lit -> shift_lit lit offset) body in
        mk_clause (shift_lit head offset) body'

  (** Apply substitution to the literal *)
  let rec subst_literal subst (lit,offset) =
    if is_ground lit || (is_empty_subst subst && offset = 0) then lit
    else if is_empty_subst subst then shift_lit lit offset
    else match lit with
      | Var _ ->
        let lit', offset' = get_var subst lit offset in
        if lit == lit' && offset = offset'
          then lit
          else subst_literal subst (lit', offset')
      | Apply (s, args) ->
        let args' = Array.map (fun lit -> subst_literal subst (lit, offset)) args in
        mk_apply_a s args'

  (** Apply substitution to the clause. *)
  let subst_clause subst (clause,offset) =
    let open Sequence.Infix in
    if is_empty_subst subst && offset = 0 then clause
    else if is_empty_subst subst then shift_clause clause offset
    else match clause with
      | Clause (head, body) ->
        let head' = subst_literal subst (head, offset) in
        let body' = List.map (fun lit -> subst_literal subst (lit,offset)) body
        in mk_clause head' body'

  (** Remove first body element of the clause, after substitution *)
  let remove_first_subst subst (clause,offset) =
    match clause with
    | Clause (_, []) -> raise (Invalid_argument "remove_first_subst")
    | Clause (head, _::body) ->
      let head' = subst_literal subst (head, offset) in
      let body' = List.map (fun lit -> subst_literal subst (lit,offset)) body
      in mk_clause head' body'

  (** {3 Pretty-printing} *)

  let rec pp_literal formatter t =
    match t with
    | Var i -> Format.fprintf formatter "X%d" i
    | Apply (s, [||]) ->
      Format.fprintf formatter "%s" (Symbol.to_string s)
    | Apply (s, args) ->
      Format.fprintf formatter "%s(%a)" (Symbol.to_string s)
        (Sequence.pp_seq ~sep:", " pp_literal) (Sequence.of_array args)

  let pp_clause formatter clause =
    match clause with
    | Clause (head, []) ->
      Format.fprintf formatter "%a." pp_literal head
    | Clause (head, body) ->
      Format.fprintf formatter "%a :-@ %a." pp_literal head
        (Sequence.pp_seq ~sep:", " pp_literal) (Sequence.of_list body)

  let pp_subst formatter subst =
    let pp_4 formatter (x,o_x,y,o_y) =
      Format.fprintf formatter "%a[%d] -> %a[%d]@;"
        pp_literal x o_x pp_literal y o_y
    in
    Format.fprintf formatter "@[{%a}@]"
      (Sequence.pp_seq pp_4) (subst_to_seq subst)

  (** {2 Utils} *)

  (** Functional Hashtable on clauses *)
  module ClauseHashtbl = FHashtbl.Tree(struct
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
