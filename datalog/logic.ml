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

(** Representation for Datalog literals and clauses, and the main algorithm *)

(** Module type for logic *)
module type S = sig
  (** {2 Literals and clauses} *)

  type symbol
    (** Abstract type of symbols *)

  type literal =
    | Var of int
    | Apply of symbol * literal array
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). Arguments can
        themselves be literals *)

  type clause
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

  val open_clause : clause -> literal * literal list
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

module Make(Symbol : SymbolType) : S with type symbol = Symbol.t = struct
  (* ----------------------------------------------------------------------
   * Literals and clauses
   * ---------------------------------------------------------------------- *)

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
  module Hashcons = Weak.Make(struct
    type t = literal
    let equal l1 l2 = equal_lit l1 l2
    let hash l = hash_lit l
  end)

  type clause = literal array
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  let table = Hashcons.create 5003

  let mk_apply_a head args =
    Hashcons.find table (Apply (head, args))

  let mk_apply head args =
    let args = Array.of_list args in
    mk_apply_a head args

  type subst =
    | SubstEmpty
    | SubstBind of (literal * int * literal * int * subst)
    (** A substitution is a map from variables with context
        to literals with context *)

  type 'a bind = ('a * int)
    (** A context in which to interpret variables in a literal or clause.
        The context is an offset that is implicitely applied to variables *)

  (** Create a clause from a conclusion and a list of premises *)
  let mk_clause head premises = Array.of_list (head :: premises)

  (** Deconstruct a clause *)
  let open_clause clause =
    clause.(0), Array.to_list (Array.sub clause 1 (Array.length clause -1))

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
  let arity t =
    match t with
    | Var _ -> 0
    | Apply (_, args) -> Array.length args

  (** Are the literals equal? *)
  let eq_literal t1 t2 = t1 == t2

  (** Hash the literal *)
  let hash_literal lit = hash_lit lit

  let body clause =
    Sequence.array_slice clause 1 (Array.length clause - 1)

  (** A datalog clause is safe iff all variables in its head also occur
      in its body *)
  let check_safe clause =
    Sequence.for_all
      (fun v -> Sequence.exists
        (fun v' -> v == v')
        (Sequence.flatMap vars (body clause)))
      (vars clause.(0))

  (** A fact is a ground clause with empty body *)
  let is_fact clause =
    Array.length clause = 1 && is_ground clause.(0)

  (** Syntactic equality of clauses *)
  let eq_clause c1 c2 =
    if Array.length c1 <> Array.length c2 then
      Array.length c1 - Array.length c2
    else
      let rec lexico i =
        if i = Array.length c1 then true
        else eq_literal c1.(i) c2.(i) && lexico (i+1) in
      lexico 0

  (** Hash the clause *)
  let hash_clause c =
    Array.fold_left
      (fun h lit -> (hash_lit lit) * 65599 + h)
      0 c

  (** {3 Unification, matching and substitutions} *)

  exception UnifFailure

  let empty_subst = SubstEmpty

  let is_empty_subst = function | SubstEmpty -> true | _ -> false

  (** Offset to avoid collisions with the given clause *)
  let offset clause =
    let offset = Sequence.max
      ~lt:(fun v1 v2 -> match v1, v2 with
            | Var i, Var j -> i < j
            | _ -> assert false)
      (Sequence.flatMap vars clause) in
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
    if l1.(0) <> l2.(0) || Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec match_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = get_var subst l1.(i) o1
        and t2, o2' = get_var subst l2.(i) o2 in
        let subst' = match_pair subst t1 o1' t2 o2' in
        match_pairs subst' (i+1)
      and match_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | _ when t1 = t2 && (t1 >= 0 || o1' = o2') ->
          subst (* same symbol or variable *)
        | _ when t1 >= 0 && t2 >= 0 ->
          raise UnifFailure (* incompatible symbols *)
        | _ when t1 < 0 ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _ ->
          assert (t2 < 0 && t1 >= 0);
          raise UnifFailure (* cannot bind t2 *)
      in match_pairs subst 1

  (** [unify l1 l2] tries to unify [l1] with [l2].
       Raise UnifFailure if they do not match. *)
  let unify ?(subst=empty_subst) (l1, o1) (l2, o2) =
    if l1.(0) <> l2.(0) || Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec unif_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = get_var subst l1.(i) o1
        and t2, o2' = get_var subst l2.(i) o2 in
        let subst' = unif_pair subst t1 o1' t2 o2' in
        unif_pairs subst' (i+1)
      and unif_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | _ when t1 = t2 && (t1 >= 0 || o1' = o2') ->
          subst (* same symbol or variable *)
        | _ when t1 >= 0 && t2 >= 0 ->
          raise UnifFailure (* incompatible symbols *)
        | _ when t1 < 0 ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _ when t2 < 0 ->
          bind_subst subst t2 o2' t1 o1' (* bind var *)
        | _ -> assert false
      in unif_pairs subst 1

  (** If the literals are alpha equivalent, return the corresponding renaming *)
  let alpha_equiv ?(subst=empty_subst) (l1,o1) (l2,o2) =
    if l1.(0) <> l2.(0) || Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec unif_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = get_var subst l1.(i) o1
        and t2, o2' = get_var subst l2.(i) o2 in
        let subst' = unif_pair subst t1 o1' t2 o2' in
        unif_pairs subst' (i+1)
      and unif_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | _ when t1 = t2 && (t1 >= 0 || o1' = o2') ->
          subst (* same symbol or variable *)
        | _ when t1 < 0 && t2 < 0 ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _ ->
          raise UnifFailure (* incompatible symbols or vars *)
      in unif_pairs subst 1

  (** shift literal by offset *)
  let shift_lit lit offset =
    if offset = 0 then lit
    else Array.map (fun t -> if is_var t then t+offset else t) lit

  let shift_clause c offset =
    if offset = 0 then c
    else Array.map (fun lit -> shift_lit lit offset) c

  (** Apply substitution to the literal *)
  let subst_literal subst (lit,offset) =
    if is_ground lit || (is_empty_subst subst && offset = 0) then lit
    else if is_empty_subst subst then shift_lit lit offset
    else Array.map
      (fun t ->
        let t', o_t' = get_var subst t offset in
        (* shift [t'] if [t'] is a var *)
        if is_var t' then t' + o_t' else t')
      lit

  (** Apply substitution to the clause. TODO remove duplicate literals afterward *)
  let subst_clause subst (clause,offset) =
    if is_empty_subst subst && offset = 0 then clause
    else if is_empty_subst subst then shift_clause clause offset
    else Array.map
      (fun lit -> subst_literal subst (lit,offset))
      clause

  (** Remove first body element of the clause, after substitution *)
  let remove_first_subst subst (clause,offset) =
    assert (Array.length clause > 1);
    let a = Array.make (Array.length clause - 1) [||] in
    a.(0) <- subst_literal subst (clause.(0),offset);
    for i = 1 to Array.length clause - 2 do
      a.(i) <- subst_literal subst (clause.(i+1),offset);
    done;
    a

  (** {3 Pretty-printing} *)

  let pp_literal formatter t =
    (* symbol index (int) to string *)
    let to_s s = Symbol.to_string (i_to_s s) in
    if arity t = 0
      then Format.fprintf formatter "%s" (to_s t.(0))
      else begin
        Format.fprintf formatter "%s(" (to_s t.(0));
        for i = 1 to Array.length t - 1 do
          (if i > 1 then Format.fprintf formatter ", ");
          if is_var t.(i)
            then Format.fprintf formatter "X%d" (abs t.(i))
            else Format.fprintf formatter "%s" (to_s t.(i));
        done;
        Format.fprintf formatter ")";
      end

  let pp_clause formatter clause =
    if Array.length clause = 1
      then Format.fprintf formatter "%a." pp_literal clause.(0)
      else begin
        Format.fprintf formatter "%a :-@ "  pp_literal clause.(0);
        for i = 1 to Array.length clause - 1 do
          (if i > 1 then Format.fprintf formatter ",@ ");
          Format.fprintf formatter "%a" pp_literal clause.(i);
        done;
        Format.fprintf formatter ".";
      end

  let pp_subst formatter subst =
    let to_s s = Symbol.to_string (i_to_s s) in
    Format.fprintf formatter "@[{";
    let first = ref true in
    let rec iter subst = match subst with
    | SubstEmpty -> ()
    | SubstBind (v,o_v,t,o_t,subst') ->
      (if !first then first := false else Format.fprintf formatter ", ");
      Format.fprintf formatter "X%d[%d] -> %s[%d]@;" (abs v) o_v (to_s t) o_t;
      iter subst'
    in
    iter subst;
    Format.fprintf formatter "}@]";

  (* ----------------------------------------------------------------------
   * Generalization/Specialization index on literals
   * ---------------------------------------------------------------------- *)

  (** Hashtable on literals *)
  module LitHashtbl = Hashtbl.Make(
    struct
      type t = literal
      let equal = eq_literal
      let hash = Utils.hash_ints
    end)

  (** Type for an indexing structure on literals *)
  module type Index =
    sig
      type t
        (** A literal index *)

      type elt
        (** A value indexed by a literal *)

      module DataSet : Set.S with type elt = literal * elt
        (** Set of indexed elements *)

      val create : unit -> t
        (** Create a new index *)

      val add : t -> literal -> elt -> unit
        (** Add an element indexed by the literal *)

      val clear : t -> unit
        (** Reset to empty index *)

      val retrieve_generalizations : ('a -> literal -> elt -> subst -> 'a) -> 'a ->
                                     t bind -> literal bind -> 'a
        (** Fold on generalizations of given literal *)

      val retrieve_specializations : ('a -> literal -> elt -> subst -> 'a) -> 'a ->
                                     t bind -> literal bind -> 'a
        (** Fold on specializations of given literal *)

      val retrieve_unify : ('a -> literal -> elt -> subst -> 'a) -> 'a ->
                           t bind -> literal bind -> 'a
        (** Fold on content that is unifiable with given literal *)

      val retrieve_renaming : ('a -> literal -> elt -> subst -> 'a) -> 'a ->
                              t bind -> literal bind -> 'a
        (** Fold on elements that are alpha-equivalent to given literal *)

      val fold : ('a -> literal -> elt -> 'a) -> 'a -> t -> 'a
        (** Fold on all indexed elements *)

      val is_empty : t -> bool
        (** Is the index empty? *)

      val size : t -> int
        (** Number of indexed elements (linear time) *)
    end

  (** Create an Index module for the given type of elements. The implementation
      is based on non-perfect discrimination trees. *)
  module Make(X : Set.OrderedType) : Index with type elt = X.t =
    struct

      (** A set of literal+indexed data *)
      module DataSet = Set.Make(struct
        type t = literal * X.t
        let compare (lit1, data1) (lit2, data2) =
          let cmp = compare_literal lit1 lit2 in
          if cmp <> 0 then cmp else X.compare data1 data2
        end)

      (** The literal index. It is a trie with, at each node, a hashset
          of elements, plus a map symbol/var -> subtrie.
          All variables are mapped to [-1], because the tree is non-perfect.
          This makes matching slower, but consumes less memory. *)
      type t =
      | Node of DataSet.t ref * t Utils.IHashtbl.t

      (** Indexed elements *)
      type elt = X.t

      (** Create a new index *)
      let create () = Node (ref DataSet.empty, Utils.IHashtbl.create 2)

      (** Add the element indexed by the literal *)
      let add t literal elt =
        let len = Array.length literal in
        (* index in subtrie [t], with a cursor at literal[i]. *)
        let rec add t i = match t, i with
        | Node (set, subtries), i when i = len ->
          set := DataSet.add (literal,elt) !set (* insert in leaf *)
        | Node (_, subtries), i ->
          let term = if is_var literal.(i) then -1 else literal.(i) in
          try
            let subtrie = Utils.IHashtbl.find subtries term in
            add subtrie (i+1)
          with Not_found ->
            (* create a new subtrie for the i-th argument of literal, then recurse *)
            let subtrie = Node (ref DataSet.empty, Utils.IHashtbl.create 2) in
            Utils.IHashtbl.add subtries term subtrie;
            add subtrie (i+1)
        in
        add t 0

      (** Reset to empty index *)
      let clear t = match t with
        | Node (set, subtries) ->
          set := DataSet.empty;
          Utils.IHashtbl.clear subtries

      (** Fold on generalizations of given ground literal *)
      let retrieve_generalizations k acc (t,o_t) (literal,o_lit) =
        let len = Array.length literal in
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

  (* ----------------------------------------------------------------------
   * The datalog bipartite resolution algorithm
   * ---------------------------------------------------------------------- *)

  exception UnsafeClause

  module ClausesIndex = Make(struct
    type t = clause
    let compare = compare_clause
  end)

  module GoalIndex = Make(struct
    type t = unit
    let compare a b = 0
  end)

  (** Hashtable on clauses *)
  module ClauseHashtbl = Hashtbl.Make(
    struct
      type t = clause
      let equal = eq_clause
      let hash = hash_clause
    end)

  (** Explanation for a clause or fact *)
  type explanation =
    | Axiom
    | Resolution of clause * literal

  type fact_handler = literal -> unit
  type goal_handler = literal -> unit

  type queue_item =
    [ `AddClause of clause * explanation
    | `AddGoal of literal
    ]

  (** A database of facts and clauses, with incremental fixpoint computation *)
  type db = {
    db_all : explanation ClauseHashtbl.t;             (** maps all clauses to their explanations *)
    db_facts : ClausesIndex.t;                        (** index on facts *)
    db_goals : GoalIndex.t;                           (** set of goals *)
    db_selected : ClausesIndex.t;                     (** index on clauses' selected premises *)
    db_heads : ClausesIndex.t;                        (** index on clauses' heads *)
    db_fact_handlers : (int, fact_handler) Hashtbl.t; (** map symbol -> fact handlers *)
    mutable db_goal_handlers : goal_handler list;     (** goal handlers *)
    db_queue : queue_item Queue.t;                    (** queue of items to process *)
  }

  (** Create a DB *)
  let db_create () =
    { db_all = ClauseHashtbl.create 17;
      db_facts = ClausesIndex.create ();
      db_goals = GoalIndex.create ();
      db_selected = ClausesIndex.create ();
      db_heads = ClausesIndex.create ();
      db_fact_handlers = Hashtbl.create 3;
      db_goal_handlers = [];
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let db_mem db clause =
    assert (check_safe clause);
    ClauseHashtbl.mem db.db_all clause

  let add_clause db clause explanation =
    (* check if clause already present; in any case add the explanation *)
    let already_present = db_mem db clause in
    ClauseHashtbl.add db.db_all clause explanation;
    if already_present then ()
    (* generate new clauses by resolution *)
    else if is_fact clause then begin
      ClausesIndex.add db.db_facts clause.(0) clause;
      (* call handler for this fact, if any *)
      let handlers = Hashtbl.find_all db.db_fact_handlers clause.(0).(0) in
      List.iter (fun h ->
        try h clause.(0)
        with e ->
          Format.eprintf "Datalog: exception while calling handler for %d@."
            clause.(0).(0);
          raise e)
        handlers;
      (* insertion of a fact: resolution with all clauses whose
         first body literal matches the fact. No offset is needed, because
         the fact is ground. *)
      ClausesIndex.retrieve_generalizations
        (fun () _ clause' subst ->
          (* subst(clause'.(1)) = clause.(0) , remove the first element of the
             body of subst(clause'), that makes a new clause *)
          let clause'' = remove_first_subst subst (clause',0) in
          let explanation = Resolution (clause', clause.(0)) in
          Queue.push (`AddClause (clause'', explanation)) db.db_queue)
        () (db.db_selected,0) (clause.(0),0)
    end else begin
      assert (Array.length clause > 1);
      (* check if some goal unifies with head of clause *)
      let offset = offset clause in
      GoalIndex.retrieve_unify
        (fun () goal () subst ->
          (* subst(goal) = subst(clause.(0)), so subst(clause.(1)) is a goal *)
          let new_goal = subst_literal subst (clause.(1),0) in
          Queue.push (`AddGoal new_goal) db.db_queue)
        () (db.db_goals,offset) (clause.(0),0);
      (* add to index *)
      ClausesIndex.add db.db_selected clause.(1) clause;
      ClausesIndex.add db.db_heads clause.(0) clause;
      (* insertion of a non_unit clause: resolution with all facts that match the
         first body literal of the clause *)
      ClausesIndex.retrieve_specializations
        (fun () fact _ subst ->
          (* subst(clause.body.(0)) = fact, remove this first literal *)
          let clause' = remove_first_subst subst (clause,0) in
          let explanation = Resolution (clause, fact) in
          Queue.push (`AddClause (clause', explanation)) db.db_queue)
        () (db.db_facts,offset) (clause.(1),0)
    end

  let add_goal db lit =
    try
      let offset = offset [|lit|] in
      GoalIndex.retrieve_renaming
        (fun () _ _ _ -> raise Exit)
        () (db.db_goals,offset) (lit,0);
      (* goal is not present! call handlers and add it *)
      List.iter (fun h -> h lit) db.db_goal_handlers;
      GoalIndex.add db.db_goals lit ();
      (* find clauses that may help solving this goal *)
      ClausesIndex.retrieve_unify
        (fun () head clause subst ->
          (* subst(clause.(0)) = subst(lit), so subst(clause.(1)) is a new goal *)
          let new_goal = subst_literal subst (clause.(1),offset) in
          Queue.push (`AddGoal new_goal) db.db_queue)
        () (db.db_heads,offset) (lit,0)
    with Exit ->
      (* goal already present (in an alpha-equivalent form) *)
      ()

  (** Push the item in the queue, and process items in the queue
      if no other function call is already doing it *)
  let process_items db item =
    let empty = Queue.is_empty db.db_queue in
    Queue.push item db.db_queue;
    (* how to process one queue item *)
    let process_item item = 
      match item with
      | `AddClause (c, explanation) -> add_clause db c explanation
      | `AddGoal goal -> add_goal db goal
    in
    (* if the queue was not empty, that means that another call
        below in the stack is already processing items. We only
        need to do it if it is not the case *)
    if empty then begin
      while not (Queue.is_empty db.db_queue) do
        let item = Queue.pop db.db_queue in
        process_item item
      done
    end

  (** Add the clause/fact to the DB, updating fixpoint *)
  let db_add db clause =
    (if not (check_safe clause) then raise UnsafeClause);
    process_items db (`AddClause (clause, Axiom))

  (** Add a fact (ground unit clause) *)
  let db_add_fact db lit =
    (if not (is_ground lit) then raise UnsafeClause);
    process_items db (`AddClause ([|lit|], Axiom))

  (** Add a goal to the DB. The goal is used to trigger backward chaining
      (calling goal handlers that could help solve the goal) *)
  let db_goal db lit =
    process_items db (`AddGoal lit)

  (** match the given literal with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    ClausesIndex.retrieve_specializations
      (fun () fact _ subst -> handler (fact,0) subst)
      () (db.db_facts,0) (pattern,1)

  (** Size of the DB *)
  let db_size db =
    ClausesIndex.size db.db_facts + ClausesIndex.size db.db_selected

  (** Fold on all clauses in the current DB (including fixpoint) *)
  let db_fold k acc db =
    ClauseHashtbl.fold
      (fun clause _ acc -> k acc clause)
      db.db_all acc

  let db_subscribe_fact db symbol handler =
    let i = s_to_i symbol in
    Hashtbl.add db.db_fact_handlers i handler

  let db_subscribe_goal db handler =
    db.db_goal_handlers <- handler :: db.db_goal_handlers

  (** Iterate on all current goals *)
  let db_goals db k =
    GoalIndex.fold
      (fun () goal () -> k goal)
      () db.db_goals

  (** Explain the given fact by returning a list of facts that imply it
      under the current clauses. *)
  let db_explain db fact =
    let module LitSet = Set.Make(struct type t = literal let compare = compare_literal end) in
    let explored = ref ClausesIndex.DataSet.empty
    and set = ref LitSet.empty in
    (* recursively collect explanations *)
    let rec search clause =
      let elt = clause.(0), clause in
      if ClausesIndex.DataSet.mem elt !explored then ()
      else begin
        explored := ClausesIndex.DataSet.add elt !explored;
        let explanation = ClauseHashtbl.find db.db_all clause in
        match explanation with
        | Axiom when is_fact clause -> set := LitSet.add clause.(0) !set
        | Axiom -> ()
        | Resolution (clause, fact) -> begin
          search clause;
          search [|fact|]
        end
      end
    in
    (* once the set is collected, convert it to list *)
    search [|fact|];
    LitSet.elements !set

  (** Immediate premises of the fact (ie the facts that resolved with
      a clause to give the literal), plus the clause that has been used. *)
  let db_premises db fact =
    let rec search acc clause =
      let explanation = ClauseHashtbl.find db.db_all clause in
      match explanation with
      | Axiom -> clause, acc  (* no premises *)
      | Resolution (clause, fact) -> let acc = fact :: acc in search acc clause
    in
    search [] [|fact|]

  (** Get all the explanations that explain why this clause is true *)
  let db_explanations db clause =
    ClauseHashtbl.find_all db.db_all clause
end

(** Default literal base, where symbols are just strings *)
module Default = Make(
  struct
    type t = string
    let to_string s = s
    let of_string s = s
    let equal s1 s2 = String.compare s1 s2 = 0
    let hash s = Hashtbl.hash s
  end)

let version = "0.3.1"
