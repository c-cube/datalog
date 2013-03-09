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

(** {1 Term indexing} *)

(** {2 Interface of a term index} *)
module type S = sig
  type 'a t

  module Logic : Logic.S

  type literal = Logic.literal
  type subst = Logic.subst

  val empty : unit -> 'a t
    (** Empty index. *)

  val is_empty : _ t -> bool
    (** Is the index empty? *)

  val add : 'a t -> literal -> 'a -> 'a t
    (** Add a value, indexed by the literal, to the index *)

  val remove : ?eq:('a -> 'a -> bool) -> 'a t -> literal -> 'a -> 'a t
    (** Remove a value indexed by some literal *)

  val retrieve_generalizations : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                                 'a t Logic.bind -> literal Logic.bind -> 'b
    (** Fold on generalizations of given literal *)

  val retrieve_specializations : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                                 'a t Logic.bind -> literal Logic.bind -> 'b
    (** Fold on specializations of given literal *)

  val retrieve_unify : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                       'a t Logic.bind -> literal Logic.bind -> 'b
    (** Fold on content that is unifiable with given literal *)

  val retrieve_renaming : ('b -> literal -> 'a -> subst -> 'b) -> 'b ->
                       'a t Logic.bind -> literal Logic.bind -> 'b
    (** Fold on elements that are alpha-equivalent to given literal *)

  val fold : ('b -> literal -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** Fold on all indexed elements *)

  val size : _ t -> int
    (** Number of indexed elements (linear time) *)
end

(** {2 Implementation using discrimination trees} *)
module Make(L : Logic.S) : S with module Logic = L = struct

  type literal = L.literal
  type subst = L.subst
  type symbol = L.Symbol.t

  (** A set of literal+indexed data *)
  module DataSet = struct
    type 'a t = (literal * 'a) list

    let add set lit data : 'a t = (lit, data) :: set

    let remove ?(eq=(=)) set lit data =
      List.filter (fun (lit', data') ->
        not (lit == lit' && eq data data')) set

    let fold f acc set =
      List.fold_left (fun acc (lit, data) -> f acc lit data) acc set
  end

  (** Elements of a flat-literal (labels edges of the discrimination tree) *)
  type flat_char =
    | FlatVar
    | FlatSymbol of symbol * int  (* symbol+arity *)
  (** Flat literal *)
  and flat_lit = flat_char array

  (** Get length of the literal *)
  let rec lit_len lit =
    match lit with
    | L.Var _ -> 1
    | L.Apply (_, args) ->
      Array.fold_left (fun acc lit -> acc + lit_len lit) 1 args

  module LitCache = Cache.Replacing(struct
    type t = literal
    let equal = L.eq_literal
    let hash = L.hash_literal
  end)

  (** Build flat-literal from a literal *)
  let flatten_lit =
    (* recursive flattening *)
    let rec flatten a lit i =
      match lit with
      | L.Var _ ->
        a.(i) <- FlatVar; i+1
      | L.Apply (s, args) ->
        a.(i) <- FlatSymbol (s, Array.length args);
        Array.fold_left (fun i lit' -> flatten a lit' i) (i+1) args
    in
    (* function that flattens a literal (allocates, etc.) *)
    let flatten lit =
      let len = lit_len lit in
      let a = Array.make len FlatVar in
      ignore (flatten a lit 0);
      a
    in
    (* caching *)
    let cache = LitCache.create 256 in
    fun lit -> LitCache.with_cache cache flatten lit

  let eq_flat_char f1 f2 = match f1, f2 with
    | FlatVar, FlatVar -> true
    | FlatSymbol (s1, ar1), FlatSymbol (s2, ar2) ->
      ar1 = ar2 && L.Symbol.equal s1 s2
    | _ -> false

  let hash_flat_char f = match f with
    | FlatVar -> 13
    | FlatSymbol (s, arity) -> arity lxor L.Symbol.hash s

  (** Persistent hashtable on flat characters *)
  module FlatHashtbl = FHashtbl.Tree(struct
    type t = flat_char
    let equal = eq_flat_char
    let hash = hash_flat_char
  end)

  (** Non-perfect discrimination tree *)
  type 'a t =
    | Leaf of 'a DataSet.t        (** leaf, with data *)
    | Node of 'a t FlatHashtbl.t  (** subtries *)

  (** Empty trie *)
  let empty () = Node (FlatHashtbl.empty 6)

  (** Add the element indexed by the literal *)
  let add t literal data =
    let flat = flatten_lit literal in
    (* recursive insertion (index i in flat-lit) *)
    let rec insert t i =
      match t with
      | Leaf set ->
        assert (i = Array.length flat);
        Leaf (DataSet.add set literal data)
      | Node h ->
        let subtrie =
          try FlatHashtbl.find h flat.(i)
          with Not_found -> empty ()  (* create subtrie *)
        in
        (* insert in subtrie *)
        let subtrie' = insert subtrie (i+1) in
        FlatHashtbl.replace h flat.(i) subtrie'
    in
    insert t 0

  (** Remove (lit,data) from the trie *)
  let remove ?(eq=(=)) t literal data =
    let flat = flatten_lit literal in
    (* recursive deletion *)
    let rec remove t i =
      match t with
      | Leaf set ->
        assert (i = Array.length flat);
        Leaf (DataSet.remove ~eq set literal data)
      | Node h ->
        try
          let subtrie = FlatHashtbl.find h flat.(i) in
          (* remove in subtrie *)
          let subtrie' = remove subtrie (i+1) in
          FlatHashtbl.replace h flat.(i) subtrie'
        with Not_found -> t  (* not present *)
    in
    remove t 0

  (** Fold on generalizations of given ground literal *)
  let retrieve_generalizations k acc (t,o_t) (literal,o_lit) =
    let flat = flatten_lit literal in
    (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
    let rec search t i acc = match t, i with
    | Node (set, _), i when i = len ->
      DataSet.fold
        (fun (lit',elt) acc ->
          try
            let subst = matching (lit',o_t) (literal,o_lit) in
            k acc lit' elt subst
          with UnifFailure -> acc)
        !set acc
    | Node (_, subtries), i ->
      if is_var literal.(i)
        then try_with subtries acc (-1) i
        else
          let acc' = try_with subtries acc (-1) i in
          try_with subtries acc' literal.(i) i
    (* try to search in the subtree annotated with given symbol/var *)
    and try_with subtries acc sym i =
      try let t' = Utils.IHashtbl.find subtries sym in
          search t' (i+1) acc
      with Not_found -> acc
    in
    search t 0 acc

  (** Fold on specializations of given literal *)
  let retrieve_specializations k acc (t,o_t) (literal,o_lit) =
    let len = Array.length literal in
    (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
    let rec search t i acc = match t, i with
    | Node (set, _), i when i = len ->
      DataSet.fold
        (fun (lit',elt) acc ->
          try
            let subst = matching (literal,o_lit) (lit',o_t) in
            k acc lit' elt subst
          with UnifFailure -> acc)
        !set acc
    | Node (_, subtries), i ->
      if is_var literal.(i)
        then  (* fold on all subtries *)
          Utils.IHashtbl.fold
            (fun _ subtrie acc -> search subtrie (i+1) acc)
            subtries acc
        else try_with subtries acc literal.(i) i
    (* try to search in the subtree annotated with given symbol/var *)
    and try_with subtries acc sym i =
      try let t' = Utils.IHashtbl.find subtries sym in
          search t' (i+1) acc
      with Not_found -> acc
    in
    search t 0 acc

  (** Fold on content that is unifiable with given literal *)
  let retrieve_unify k acc (t,o_t) (literal, o_lit) =
    let len = Array.length literal in
    (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
    let rec search t i acc = match t, i with
    | Node (set, _), i when i = len ->
      DataSet.fold
        (fun (lit',elt) acc ->
          try
            let subst = unify (literal,o_lit) (lit',o_t) in
            k acc lit' elt subst
          with UnifFailure -> acc)
        !set acc
    | Node (_, subtries), i ->
      if is_var literal.(i)
        then  (* fold on all subtries *)
          Utils.IHashtbl.fold
            (fun _ subtrie acc -> search subtrie (i+1) acc)
            subtries acc
        else (* try both subtrie with same symbol, and subtrie with variable *)
          let acc' = try_with subtries acc literal.(i) i in
          try_with subtries acc' (-1) i
    (* try to search in the subtree annotated with given symbol/var *)
    and try_with subtries acc sym i =
      try let t' = Utils.IHashtbl.find subtries sym in
          search t' (i+1) acc
      with Not_found -> acc
    in
    search t 0 acc

  (** Fold on content that is unifiable with given literal *)
  let retrieve_renaming k acc (t,o_t) (literal, o_lit) =
    let len = Array.length literal in
    (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
    let rec search t i acc = match t, i with
    | Node (set, _), i when i = len ->
      DataSet.fold
        (fun (lit',elt) acc ->
          try
            let subst = alpha_equiv (literal,o_lit) (lit',o_t) in
            k acc lit' elt subst
          with UnifFailure -> acc)
        !set acc
    | Node (_, subtries), i ->
      let sym = if is_var literal.(i) then (-1) else literal.(i) in
      try let t' = Utils.IHashtbl.find subtries sym in
          search t' (i+1) acc
      with Not_found -> acc
    in
    search t 0 acc

  (** Fold on all indexed elements *)
  let rec fold k acc t = match t with
    | Node (set, subtries) ->
      (* fold on elements at this point *)
      let acc = DataSet.fold
        (fun (lit,elt) acc -> k acc lit elt)
        !set acc in
      (* fold on subtries *)
      Utils.IHashtbl.fold
        (fun _ subtrie acc -> fold k acc subtrie)
        subtries acc

  (** Check whether the property is true for all subtries *)
  let for_all p subtries =
    try
      Utils.IHashtbl.iter
        (fun _ t' -> if not (p t') then raise Exit)
      subtries;
      true
    with Exit -> false

  (** Check whether there are no elements in the index *)
  let rec is_empty t = match t with
    | Node (set, subtries) ->
      DataSet.cardinal !set = 0 && for_all is_empty subtries

  (** Number of elements *)
  let size t = fold (fun i _ _ -> i + 1) 0 t
end
