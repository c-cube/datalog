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

  val map : 'a t -> literal -> ('a option -> 'a option) -> 'a t * 'a option
    (** Maps the value associated to this literal (modulo alpha-renaming)
        to a value. None indicates that the literal is not present, or
        that the literal is to be removed. The old value is also returned. *)

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
  module Logic = L

  type literal = L.literal
  type subst = L.subst
  type symbol = L.Symbol.t

  (** A set of literal+indexed data. *)
  module DataSet = struct
    type 'a t = (literal * 'a) list

    let empty = []

    let is_empty = function
      | [] -> true
      | _ -> false

    let rec map set lit f =
      match set with
      | [] ->
        begin match f None with
        | None -> [] (* do not insert *)
        | Some x -> [lit, x]  (* insertion *)
        end
      | (lit', data)::set' ->
        begin try
          ignore (L.alpha_equiv (lit,0) (lit',1));
          begin match f (Some data) with
          | None -> set'  (* remove *)
          | Some data' -> (lit', data') :: set'  (* replace *)
          end
        with L.UnifFailure ->
          (* keep this pair *)
          (lit', data) :: map set' lit f
        end

    let flat_map set f =
      let rec loop acc set = match set with
      | [] -> acc
      | (lit, x)::set' ->
        (* transform value associated to the literal *)
        let xs = List.map (fun y -> lit, y) (f x) in
        loop (xs @ acc) set'
      in loop [] set

    let fold f acc set =
      List.fold_left (fun acc (lit, data) -> f acc lit data) acc set
  end

  (** Elements of a flat-literal (labels edges of the discrimination tree) *)
  type flat_char =
    | FlatVar
    | FlatEnd (* end of literal *)
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
      let len = lit_len lit + 1 in
      let a = Array.make len FlatEnd in
      ignore (flatten a lit 0);
      assert (a.(len) = FlatEnd);
      a
    in
    (* caching *)
    let cache = LitCache.create 256 in
    fun lit -> LitCache.with_cache cache flatten lit

  let eq_flat_char f1 f2 = match f1, f2 with
    | FlatVar, FlatVar -> true
    | FlatEnd, FlatEnd -> true
    | FlatSymbol (s1, ar1), FlatSymbol (s2, ar2) ->
      ar1 = ar2 && L.Symbol.equal s1 s2
    | _ -> false

  let hash_flat_char f = match f with
    | FlatVar -> 13
    | FlatEnd -> 17
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

  (** Check whether there are no elements in the index *)
  let is_empty t = match t with
    | Node h -> FlatHashtbl.size h = 0
    | Leaf set -> DataSet.is_empty set

  (** Maps the value associated to this literal (modulo alpha-renaming)
      to a value. None indicates that the literal is not present, or
      that the literal is to be removed *)
  let map t lit f =
    let flat = flatten_lit lit in
    (* recursive lookup (index i in flat-lit) *)
    let rec recurse t i =
      match t with
      | Leaf set ->
        assert (i = Array.length flat);
        (* map set to a new set' (may add/remove literal) *)
        let set' = DataSet.map set lit f in
        Leaf set'
      | Node h ->
        let subtrie =
          (try FlatHashtbl.find h flat.(i)
          with Not_found ->
            if flat.(i) = FlatEnd
              then Leaf DataSet.empty (* create leaf *)
              else empty ())  (* create subtrie *)
        in
        (* insert in subtrie *)
        let subtrie' = recurse subtrie (i+1) in
        if is_empty subtrie'
          then Node (FlatHashtbl.remove h flat.(i))
          else Node (FlatHashtbl.replace h flat.(i) subtrie')
    in
    recurse t 0

  (** Fold on generalizations of given literal *)
  let retrieve_generalizations k acc (t,o_t) (literal,o_lit) =
    let flat = flatten_lit literal in
    (* fold over the tree *)
    let rec fold acc t i =
      match t, flat.(i) with
      | Leaf set, FlatEnd ->
        DataSet.fold
          (fun acc lit data -> 
            try
              let subst = L.matching (lit,o_t) (literal,o_lit) in
              k acc lit data subst
            with L.UnifFailure -> acc)
          acc set
      | Node h, FlatVar ->
        (try fold acc (FlatHashtbl.find h FlatVar) (i+1) with Not_found -> acc)
      | Node h, ((FlatSymbol _) as flat_char) ->
        (* follow both '*' and s *)
        let acc' = (try fold acc (FlatHashtbl.find h flat_char) (i+1) with Not_found -> acc) in
        (try fold acc' (FlatHashtbl.find h FlatVar) (i+1) with Not_found -> acc')
      | Leaf _, _ -> assert false
      | Node _, FlatEnd -> assert false
    in
    fold acc t 0

  (** Fold on specializations of given literal *)
  let retrieve_specializations k acc (t,o_t) (literal,o_lit) =
    let flat = flatten_lit literal in
    (* fold over the tree *)
    let rec fold acc t i =
      match t, flat.(i) with
      | Leaf set, FlatEnd ->
        DataSet.fold
          (fun acc lit data -> 
            try
              let subst = L.matching (literal,o_lit) (lit,o_t)in
              k acc lit data subst
            with L.UnifFailure -> acc)
          acc set
      | Node h, FlatVar ->
        (* follow every path *)
        FlatHashtbl.fold (fun acc _ t' -> fold acc t' (i+1)) acc h
      | Node h, ((FlatSymbol _) as flat_char) ->
        (* follow symbol *)
        (try fold acc (FlatHashtbl.find h flat_char) (i+1) with Not_found -> acc)
      | Leaf _, _ -> assert false
      | Node _, FlatEnd -> assert false
    in
    fold acc t 0

  (** Fold on content that is unifiable with given literal *)
  let retrieve_unify k acc (t,o_t) (literal, o_lit) =
    let flat = flatten_lit literal in
    (* fold over the tree *)
    let rec fold acc t i =
      match t, flat.(i) with
      | Leaf set, FlatEnd ->
        DataSet.fold
          (fun acc lit data -> 
            try
              let subst = L.unify (literal,o_lit) (lit,o_t)in
              k acc lit data subst
            with L.UnifFailure -> acc)
          acc set
      | Node h, FlatVar ->
        (* follow every path *)
        FlatHashtbl.fold (fun acc _ t' -> fold acc t' (i+1)) acc h
      | Node h, ((FlatSymbol _) as flat_char) ->
        (* follow symbol and '*' *)
        let acc' = (try fold acc (FlatHashtbl.find h flat_char) (i+1) with Not_found -> acc) in
        (try fold acc' (FlatHashtbl.find h FlatVar) (i+1) with Not_found -> acc')
      | Leaf _, _ -> assert false
      | Node _, FlatEnd -> assert false
    in
    fold acc t 0

  (** Fold on content that is unifiable with given literal *)
  let retrieve_renaming k acc (t,o_t) (literal, o_lit) =
    let flat = flatten_lit literal in
    (* fold over the tree *)
    let rec fold acc t i =
      match t, flat.(i) with
      | Leaf set, FlatEnd ->
        DataSet.fold
          (fun acc lit data -> 
            try
              let subst = L.alpha_equiv (literal,o_lit) (lit,o_t)in
              k acc lit data subst
            with L.UnifFailure -> acc)
          acc set
      | Node h, FlatVar ->
        (* follow '*' *)
        (try fold acc (FlatHashtbl.find h FlatVar) (i+1) with Not_found -> acc)
      | Node h, ((FlatSymbol _) as flat_char) ->
        (* follow symbol *)
        (try fold acc (FlatHashtbl.find h flat_char) (i+1) with Not_found -> acc)
      | Leaf _, _ -> assert false
      | Node _, FlatEnd -> assert false
    in
    fold acc t 0

  (** Fold on all indexed elements *)
  let rec fold k acc t = match t with
    | Leaf set ->
      DataSet.fold
        (fun acc lit data -> k acc lit data)
        acc set
    | Node h ->
      FlatHashtbl.fold (fun acc _ t' -> fold k acc t') acc h

  (** Number of elements *)
  let size t = fold (fun i _ _ -> i + 1) 0 t
end
