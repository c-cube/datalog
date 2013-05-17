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

(** Module type for logic *)
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

  val db_query : db -> literal -> int list -> (term list -> unit) -> unit
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

(** Build a Datalog module *)
module Make(Symbol : SymbolType) : S with type symbol = Symbol.t = struct
  type symbol = Symbol.t

  module SymbolHashtbl = Hashtbl.Make(Symbol)

  type term =
    | Var of int
    | Const of symbol
    | Query of int      (* for internal use *)
    (** Individual object *)

  let mk_var i = Var i
  let mk_const s = Const s
  let mk_query i = Query i

  type literal = term array
    (** A datalog atom, i.e. pred(arg_1, ..., arg_n). The first element of the
        array is the predicate, then arguments follow *)

  type clause = literal array
    (** A datalog clause, i.e. head :- body_1, ..., body_n *)

  type soft_lit = symbol * term list
  type soft_clause = soft_lit * soft_lit list

  (* TODO: proper variable banks *)

  type subst =
    | SubstEmpty
    | SubstBind of (int * int * term * int * subst)
    (** A substitution is a map from (negative) ints (variables) with context
        to ints with context (variable or symbol) *)

  type 'a bind = ('a * int)
    (** A context in which to interpret variables in a literal or clause.
        The context is an offset that is implicitely applied to variables *)

  (** Helper to build a literal. *)
  let mk_literal head args = Array.of_list (Const head :: args)

  let of_soft_lit sl = match sl with
    | hd, args -> mk_literal hd args

  (** Deconstruct a literal *)
  let open_literal literal =
    match Array.to_list literal with
    | Const x :: args -> x, args
    | _ -> assert false

  (** Create a clause from a conclusion and a list of premises *)
  let mk_clause head premises = Array.of_list (head :: premises)

  let of_soft_clause sc = match sc with
    | concl, premises ->
      let concl = of_soft_lit concl in
      let premises = List.map of_soft_lit premises in
      mk_clause concl premises

  (** Deconstruct a clause *)
  let open_clause clause =
    let head = clause.(0) in
    let head = open_literal head in
    let body = Array.to_list (Array.sub clause 1 (Array.length clause - 1)) in
    let body = List.map open_literal body in
    head, body

  let is_var t = match t with
    | Var _ -> true
    | Const _
    | Query _ -> false

  (** Is the literal ground (a fact)? *)
  let is_ground t =
    assert (not (is_var t.(0)));
    let rec check t i =
      if i = Array.length t then true
      else (not (is_var t.(i))) && check t (i+1)
    in
    check t 1

  (** Number of subterms of the literal. Ex for p(a,b,c) it returns 3 *)
  let arity t = Array.length t - 1

  let eq_term t1 t2 = match t1, t2 with
    | Var i, Var j
    | Query i, Query j -> i = j
    | Const s1, Const s2 -> Symbol.equal s1 s2
    | _ -> false

  (** Are the literals equal? *)
  let eq_literal t1 t2 =
    let rec check t1 t2 i =
      if i = Array.length t1
        then true
        else eq_term t1.(i) t2.(i) && check t1 t2 (i+1)
    in
    Array.length t1 = Array.length t2 && check t1 t2 0

  let hash_term t = match t with
    | Var i
    | Query i -> i
    | Const s -> Symbol.hash s

  (** Hash the literal *)
  let hash_literal t =
    let hash_term h t = match t with
    | Var i
    | Query i -> h * 65599 + i
    | Const s -> h * 65599 + Symbol.hash s
    in
    abs (Array.fold_left hash_term 13 t)

  (** A datalog clause is safe iff all variables in its head also occur in its body *)
  let check_safe clause =
    let rec check_head i =
      if i = Array.length clause.(0) then true
      else
        let t = clause.(0).(i) in
        if is_var t
          then check_body t 1 && check_head (i+1)
          else check_head (i+1)
    and check_body var j =
      if j = Array.length clause then false
        else check_body_literal var clause.(j) 1 || check_body var (j+1)
    and check_body_literal var literal k =
      if k = Array.length literal then false
      else if eq_term literal.(k) var then true
      else check_body_literal var literal (k+1)
    in
    check_head 1

  (** A fact is a ground clause with empty body *)
  let is_fact clause =
    Array.length clause = 1 && is_ground clause.(0)

  (** Check whether clauses are (syntactically) equal *)
  let eq_clause r1 r2 =
    let rec check t1 t2 i =
      if i = Array.length r1
        then true
        else eq_literal r1.(i) r2.(i) && check r1 r2 (i+1)
    in
    Array.length r1 = Array.length r2 && check r1 r2 0

  (** Hash the clause *)
  let hash_clause r =
    let h = ref 17 in
    for i = 0 to Array.length r - 1 do
      h := (!h + 65536) * hash_literal r.(i);
    done;
    abs !h

  (** {3 Unification, matching and substitutions} *)

  exception UnifFailure

  let empty_subst = SubstEmpty

  let is_empty_subst = function | SubstEmpty -> true | _ -> false

  (** Offset to avoid collisions with the given clause *)
  let offset clause =
    (* explore literals of the clause, looking for the lowest var *)
    let rec fold_lit terms offset i = 
      if i = Array.length terms then offset
      else
        let offset = match terms.(i) with
        | Const _
        | Query _ -> offset
        | Var i -> max i offset
        in fold_lit terms offset (i+1)
    and fold_lits lits offset i =
      if i = Array.length lits then offset
      else fold_lits lits (fold_lit lits.(i) offset 1) (i+1)
    in
    let offset = fold_lits clause 0 0 in
    (* highest var + 1, no collision! *)
    offset + 1

  (** Dereference [var] in context [offset] until it is no longer a
      bound variable *)
  let rec deref subst var offset =
    match subst, var with
    | _, (Const _ | Query _) -> var, offset
    | SubstBind (i, o, t, o_t, _), Var j when i = j && o = offset ->
      deref subst t o_t
    | SubstBind (_, _, _, _, subst'), _ -> deref subst' var offset
    | SubstEmpty, _ -> var, offset

  (** Bind [v] to [t], with offsets *)
  let bind_subst subst v o_v t o_t =
    assert (is_var v);
    if eq_term v t && o_v = o_t
      then subst  (* no-op *)
      else match v with
        | Var i -> SubstBind (i, o_v, t, o_t, subst)
        | Const _
        | Query _ -> assert false

  (** [matching pattern l] matches [pattern] against [l]; variables in [l]
      cannot be bound. Raise UnifFailure if they do not match. *)
  let matching ?(subst=empty_subst) (l1, o1) (l2, o2) =
    if Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec match_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = deref subst l1.(i) o1
        and t2, o2' = deref subst l2.(i) o2 in
        let subst' = match_pair subst t1 o1' t2 o2' in
        match_pairs subst' (i+1)
      and match_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | Const s1, Const s2 ->
          if Symbol.equal s1 s2 then subst else raise UnifFailure
        | Query i, Query j ->
          if i = j then subst else raise UnifFailure
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _, _ -> raise UnifFailure
      in match_pairs subst 0

  (** [unify l1 l2] tries to unify [l1] with [l2].
       Raise UnifFailure if they do not match. *)
  let unify ?(subst=empty_subst) (l1, o1) (l2, o2) =
    if Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec unif_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = deref subst l1.(i) o1
        and t2, o2' = deref subst l2.(i) o2 in
        let subst' = unif_pair subst t1 o1' t2 o2' in
        unif_pairs subst' (i+1)
      and unif_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | Const s1, Const s2 ->
          if Symbol.equal s1 s2 then subst else raise UnifFailure
        | Query i, Query j ->
          if i = j then subst else raise UnifFailure
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _, _ -> 
          bind_subst subst t2 o2' t1 o1' (* bind var *)
      in unif_pairs subst 0

  (** If the literals are alpha equivalent, return the corresponding renaming *)
  let alpha_equiv ?(subst=empty_subst) (l1,o1) (l2,o2) =
    if Array.length l1 <> Array.length l2
    then raise UnifFailure
    else
      let rec unif_pairs subst i =
        if i = Array.length l1 then subst else
        let t1, o1' = deref subst l1.(i) o1
        and t2, o2' = deref subst l2.(i) o2 in
        let subst' = unif_pair subst t1 o1' t2 o2' in
        unif_pairs subst' (i+1)
      and unif_pair subst t1 o1' t2 o2' =
        match t1, t2 with
        | Const s1, Const s2 ->
          if Symbol.equal s1 s2 then subst else raise UnifFailure
        | Query i, Query j ->
          if i = j then subst else raise UnifFailure
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, Var _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | _, _ -> raise UnifFailure
      in unif_pairs subst 0

  (** shift literal by offset *)
  let shift_lit lit offset =
    if offset = 0 then lit
    else Array.map
      (fun t -> match t with
      | Var i -> Var (i+offset)
      | Const _
      | Query _ -> t)
      lit

  let shift_clause c offset =
    if offset = 0 then c
    else Array.map (fun lit -> shift_lit lit offset) c

  (** Apply substitution to the literal *)
  let subst_literal subst (lit,offset) =
    if is_ground lit || (is_empty_subst subst && offset = 0) then lit
    else if is_empty_subst subst then shift_lit lit offset
    else Array.map
      (fun t ->
        let t', o_t' = deref subst t offset in
        (* shift [t'] if [t'] is a var *)
        match t' with
        | Var i -> Var (i + o_t')
        | Const _
        | Query _ -> t')
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

  let pp_term formatter t = match t with
    | Const s -> Format.pp_print_string formatter (Symbol.to_string s)
    | Query i -> Format.fprintf formatter "$query%d" i
    | Var i -> Format.fprintf formatter "X%d" i

  let pp_literal formatter t =
    if arity t = 0
      then pp_term formatter t.(0)
      else begin
        Format.fprintf formatter "%a(" pp_term t.(0);
        for i = 1 to Array.length t - 1 do
          (if i > 1 then Format.fprintf formatter ", ");
          pp_term formatter t.(i)
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

  (** {2 Term index for generalization/specialization retrieval} *)

  (** Hashtable on literals *)
  module LitHashtbl = Hashtbl.Make(struct
    type t = literal
    let equal = eq_literal
    let hash = hash_literal
  end)

  (** Type for an indexing structure on literals *)
  module type Index = sig
    type t
      (** A literal index *)

    type elt
      (** A value indexed by a literal *)

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
  module MakeIndex(X : Hashtbl.HashedType) : Index with type elt = X.t = struct
    type char_ = term

    module TermHashtbl = Hashtbl.Make(struct
      type t = term
      let equal = eq_term
      let hash = hash_term
    end)

    (** A set of literal+indexed data *)
    module DataSet = Hashtbl.Make(struct
      type t = literal * X.t
      let equal (l1,x1) (l2,x2) = eq_literal l1 l2 && X.equal x1 x2
      let hash (l,x) = hash_literal l lxor X.hash x
    end)

    (** The literal index. It is a trie with, at each node, a hashset
        of elements, plus a map symbol/var -> subtrie.
        All variables are mapped to [Var 0], because the tree is non-perfect.
        This makes matching slower, but consumes less memory. *)
    type t =
    | Node of unit DataSet.t * t TermHashtbl.t

    (** Indexed elements *)
    type elt = X.t

    (** Create a new index *)
    let create () = Node (DataSet.create 3, TermHashtbl.create 2)

    let __star = Var 0

    let term_to_char t = match t with
      | Const _
      | Query _ -> t
      | Var _ -> __star  (* any var is the same *)

    (** Add the element indexed by the literal *)
    let add t literal elt =
      let len = Array.length literal in
      (* index in subtrie [t], with a cursor at literal[i]. *)
      let rec add t i = match t, i with
      | Node (set, subtries), i when i = len ->
        DataSet.replace set (literal,elt) () (* insert in leaf *)
      | Node (_, subtries), i ->
        let c = term_to_char literal.(i) in
        try
          let subtrie = TermHashtbl.find subtries c in
          add subtrie (i+1)
        with Not_found ->
          (* create a new subtrie for the i-th argument of literal, then recurse *)
          let subtrie = create () in
          TermHashtbl.add subtries c subtrie;
          add subtrie (i+1)
      in
      add t 0

    (** Reset to empty index *)
    let clear t = match t with
      | Node (set, subtries) ->
        DataSet.clear set;
        TermHashtbl.clear subtries

    (** Fold on generalizations of given ground literal *)
    let retrieve_generalizations k acc (t,o_t) (literal,o_lit) =
      let len = Array.length literal in
      (* search in subtrie [t], with cursor at [i]-th argument of [literal] *)
      let rec search t i acc = match t, i with
      | Node (set, _), i when i = len ->
        DataSet.fold
          (fun (lit',elt) () acc ->
            try
              let subst = matching (lit',o_t) (literal,o_lit) in
              k acc lit' elt subst
            with UnifFailure -> acc)
          set acc
      | Node (_, subtries), i ->
        if is_var literal.(i)
          then try_with subtries acc __star i
          else
            let acc' = try_with subtries acc __star i in
            try_with subtries acc' literal.(i) i
      (* try to search in the subtree annotated with given symbol/var *)
      and try_with subtries acc sym i =
        try let t' = TermHashtbl.find subtries sym in
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
          (fun (lit',elt) () acc ->
            try
              let subst = matching (literal,o_lit) (lit',o_t) in
              k acc lit' elt subst
            with UnifFailure -> acc)
          set acc
      | Node (_, subtries), i ->
        if is_var literal.(i)
          then  (* fold on all subtries *)
            TermHashtbl.fold
              (fun _ subtrie acc -> search subtrie (i+1) acc)
              subtries acc
          else try_with subtries acc literal.(i) i
      (* try to search in the subtree annotated with given symbol/var *)
      and try_with subtries acc sym i =
        try let t' = TermHashtbl.find subtries sym in
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
          (fun (lit',elt) () acc ->
            try
              let subst = unify (literal,o_lit) (lit',o_t) in
              k acc lit' elt subst
            with UnifFailure -> acc)
          set acc
      | Node (_, subtries), i ->
        if is_var literal.(i)
          then  (* fold on all subtries *)
            TermHashtbl.fold
              (fun _ subtrie acc -> search subtrie (i+1) acc)
              subtries acc
          else (* try both subtrie with same symbol, and subtrie with variable *)
            let acc' = try_with subtries acc literal.(i) i in
            try_with subtries acc' __star i
      (* try to search in the subtree annotated with given symbol/var *)
      and try_with subtries acc sym i =
        try let t' = TermHashtbl.find subtries sym in
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
          (fun (lit',elt) () acc ->
            try
              let subst = alpha_equiv (literal,o_lit) (lit',o_t) in
              k acc lit' elt subst
            with UnifFailure -> acc)
          set acc
      | Node (_, subtries), i ->
        let c = term_to_char literal.(i) in
        try let t' = TermHashtbl.find subtries c in
            search t' (i+1) acc
        with Not_found -> acc
      in
      search t 0 acc

    (** Fold on all indexed elements *)
    let rec fold k acc t = match t with
      | Node (set, subtries) ->
        (* fold on elements at this point *)
        let acc = DataSet.fold
          (fun (lit,elt) () acc -> k acc lit elt)
          set acc in
        (* fold on subtries *)
        TermHashtbl.fold
          (fun _ subtrie acc -> fold k acc subtrie)
          subtries acc

    (** Check whether the property is true for all subtries *)
    let for_all p subtries =
      try
        TermHashtbl.iter
          (fun _ t' -> if not (p t') then raise Exit)
        subtries;
        true
      with Exit -> false

    (** Check whether there are no elements in the index *)
    let rec is_empty t = match t with
      | Node (set, subtries) ->
        DataSet.length set = 0 && for_all is_empty subtries

    (** Number of elements *)
    let size t = fold (fun i _ _ -> i + 1) 0 t
  end

  (* ----------------------------------------------------------------------
   * The datalog bipartite resolution algorithm
   * ---------------------------------------------------------------------- *)

  exception UnsafeClause

  module ClausesIndex = MakeIndex(struct
    type t = clause
    let equal = eq_clause
    let hash = hash_clause
  end)

  module GoalIndex = MakeIndex(struct
    type t = unit
    let equal a b = true
    let hash a = 0
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

  type user_fun = soft_lit -> soft_lit

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
    db_fact_handlers : fact_handler SymbolHashtbl.t;  (** map symbol -> fact handlers *)
    mutable db_goal_handlers : goal_handler list;     (** goal handlers *)
    db_funs : user_fun SymbolHashtbl.t;               (** user-defined functions *)
    db_queue : queue_item Queue.t;                    (** queue of items to process *)
  }

  (** Create a DB *)
  let db_create () =
    { db_all = ClauseHashtbl.create 17;
      db_facts = ClausesIndex.create ();
      db_goals = GoalIndex.create ();
      db_selected = ClausesIndex.create ();
      db_heads = ClausesIndex.create ();
      db_fact_handlers = SymbolHashtbl.create 3;
      db_goal_handlers = [];
      db_funs = SymbolHashtbl.create 13;
      db_queue = Queue.create ();
    }

  (** Is the clause member of the DB? *)
  let db_mem db clause =
    assert (check_safe clause);
    ClauseHashtbl.mem db.db_all clause

  (** Apply user-defined functions to the clause *)
  let rewrite_clause db clause =
    (* rewrite the literal using user-defined functions *)
    let rec rewrite_lit db_funs lit =
      match lit.(0) with
      | Var _ -> assert false
      | Query _ -> lit (* do not rewrite *)
      | Const s ->
        let lit' = try
          let f = SymbolHashtbl.find db.db_funs s in
          let lit' = f (open_literal lit) in
          let lit' = of_soft_lit lit' in
          lit'
        with Not_found ->
          lit
      in
      if lit == lit' || eq_literal lit lit'
        then lit'  (* fixpoint *)
        else rewrite_lit db_funs lit'
    in
    (* rewrite every literal *)
    Array.map (fun lit -> rewrite_lit db.db_funs lit) clause

  let add_clause db clause explanation =
    let clause = rewrite_clause db clause in
    (* check if clause already present; in any case add the explanation *)
    let already_present = db_mem db clause in
    ClauseHashtbl.add db.db_all clause explanation;
    if already_present then ()
    (* generate new clauses by resolution *)
    else if is_fact clause then begin
      ClausesIndex.add db.db_facts clause.(0) clause;
      (* call handler for this fact, if any *)
      begin match clause.(0).(0) with
      | Const s ->
        let handlers = SymbolHashtbl.find_all db.db_fact_handlers s in
        List.iter (fun h ->
          try h clause.(0)
          with e ->
            Format.eprintf
              "Datalog: exception while calling handler for %s@."
              (Symbol.to_string s);
            raise e)
          handlers
      | Query _ -> ()
      | Var _ -> assert false
      end;
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
      (fun () fact _ subst -> handler fact)
      () (db.db_facts,0) (pattern,1)

  (** Like {!db_match}, but the additional int list is used to select
      bindings of variables in the literal. Their bindings, in the same
      order, are given to the callback. *)
  let db_query db pattern vars k =
    ClausesIndex.retrieve_specializations
      (fun () lit _ subst ->
        let terms = List.map
          (fun i ->
            let v = mk_var i in
            let t, _ = deref subst v 1 in
            t)
          vars in
        (* yield the list of terms *)
        k terms)
      () (db.db_facts,0) (pattern,1)

  (** Size of the DB *)
  let db_size db =
    ClausesIndex.size db.db_facts + ClausesIndex.size db.db_selected

  (** Fold on all clauses in the current DB (including fixpoint) *)
  let db_fold k acc db =
    ClauseHashtbl.fold
      (fun clause _ acc -> k acc clause)
      db.db_all acc

  let db_add_fun db s f =
    (if SymbolHashtbl.mem db.db_funs s
      then failwith ("function already defined for symbol " ^ Symbol.to_string s));
    SymbolHashtbl.replace db.db_funs s f

  let db_subscribe_fact db symbol handler =
    SymbolHashtbl.add db.db_fact_handlers symbol handler

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
    let explored = ClauseHashtbl.create 5 in
    let set = LitHashtbl.create 5 in
    (* recursively collect explanations *)
    let rec search clause =
      if ClauseHashtbl.mem explored clause then ()
      else begin
        ClauseHashtbl.add explored clause ();
        let explanation = ClauseHashtbl.find db.db_all clause in
        match explanation with
        | Axiom when is_fact clause ->
          LitHashtbl.replace set clause.(0) ();
        | Axiom -> ()
        | Resolution (clause, fact) -> begin
          search clause;
          search [|fact|]
        end
      end
    in
    (* once the set is collected, convert it to list *)
    search [|fact|];
    LitHashtbl.fold (fun lit () acc -> lit :: acc) set []

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

  (** {2 Earley resolution} *)

  module Earley = struct
    (** We roughly follow http://web.cecs.pdx.edu/~harry/earley/earley.htm 

        Two inference rules:
        
        - instantiation:
          A :- B1,...,Bn \in db       C :- D1,...,Dm \in query
          ----------------------------------------------------  A\sigma = D1\sigma
                A\sigma :- B1\sigma,...,Bn\sigma \in query

        - reduction:
          A :- B1,...,Bn \in query    C \in (query union db)
          --------------------------------------------------  B1\sigma = C
                A\sigma :- B2\sigma,...,Bn\sigma

        The query set contains a growing set of clauses, starting from a query
        clause. Clauses are added if they are the result of resolution
        with a unit fact, or if they are the result of instantiating a
        rule of the db so that the conclusion can help solving another
        query clause.
    *)

    type query = {
      q_id : int;                   (** unique ID *)
      q_db : db;
      q_all : explanation ClauseHashtbl.t;   (** maps clauses to their explanations *)
      q_selected : ClausesIndex.t;  (** derived clauses, by their selected lit *)
      q_facts : ClausesIndex.t;     (** derived facts *)
      q_concl : literal;            (** literal dedicated to the query *)
      mutable q_handlers : (term list -> unit) list;
    }
    and db = {
      mutable db_q_count : int;
      db_all : explanation ClauseHashtbl.t;   (** maps clauses to their explanations *)
      db_facts : ClausesIndex.t;              (** index on facts *)
      db_selected : ClausesIndex.t;           (** index on clauses' selected premises *)
      db_queries : (int, query) Hashtbl.t;    (** Queries by their ID *)
    }
    and explanation =
      | Program
      | Instantiation of clause
      | Reduction of clause * literal
      (** explanations for Earley resolution *)

    let db_create () =
      let db = {
        db_q_count = 0;
        db_all = ClauseHashtbl.create 15;
        db_facts = ClausesIndex.create ();
        db_selected = ClausesIndex.create ();
        db_queries = Hashtbl.create 3;
      } in
      db

    let del_query q =
      Hashtbl.remove q.q_db.db_queries q.q_id

    let iter_queries db k =
      Hashtbl.iter (fun _ q -> k q) db.db_queries

    let register q handler =
      q.q_handlers <- handler :: q.q_handlers

    (* add a derived clause *)
    let add_derived_clause ~tasks q c =
      failwith "Datalog.Earley.add_derived_clause: not implemented"

    (* given the fact [c], reduce clauses of [query] with it *)
    let reduce_with_fact ~tasks query c =
      failwith "Datalog.Earley.reduce: not implemented"

    (* use the non-unit clause for instantiation *)
    let instantiate_this_clause ~tasks query c =
      failwith "Datalog.Earley.instantiate: not implemented"

    (* add a clause to the program (static clauses of [db]) *)
    let db_add db c =
      if not (ClauseHashtbl.mem db.db_all c) then begin
        ClauseHashtbl.add db.db_all c Program;
        if is_fact c
          then iter_queries db
            (fun query -> reduce_with_fact ~tasks:(Queue.create ()) query c)
          else iter_queries db
            (fun query -> instantiate_this_clause ~tasks:(Queue.create ()) query c)
        end
    (* TODO figure out how to make this work: schedule tasks to do;
      implement inferences;
      implement subsumption
    *)
    (* TODO explanations *)

    let ask ?(within=[]) db lits vars k =
      let n = db.db_q_count in
      db.db_q_count <- db.db_q_count + 1;
      (* special conclusion and clause *)
      let concl = Array.of_list (Query n :: List.map mk_var vars) in
      let args = lits in
      let clause = mk_clause concl args in
      (* query object *)
      let q = {
        q_id = n;
        q_db = db;
        q_all = ClauseHashtbl.create 15;
        q_selected = ClausesIndex.create ();
        q_facts = ClausesIndex.create ();
        q_concl = concl;
        q_handlers = [k];
      } in
      Hashtbl.add db.db_queries q.q_id q;
      let tasks = Queue.create () in
      (* add bonus clauses *)
      List.iter (fun c -> add_derived_clause ~tasks q c) within;
      (* add the initial clause *)
      add_derived_clause ~tasks q clause;
      q
  end
end

module Hashcons(S : SymbolType) = struct
  type t = S.t

  module W = Weak.Make(struct
    type t = S.t
    let equal x y = S.equal x y
    let hash x = S.hash x
  end)

  let equal x y = x == y

  let hash x = S.hash x

  let to_string x = S.to_string x

  let __table = W.create 1024   (** Weak table *)

  let make x =
    let y = W.merge __table x in
    y
end

module Parser = Parser
module Lexer = Lexer

module StringSymbol = Hashcons(struct
  type t = string
  let equal a b = a = b
  let hash x = Hashtbl.hash x
  let to_string s = s
end)

(** Default literal base, where symbols are just strings *)
module Default = struct
  include Make(StringSymbol)

  type vartbl = {
    mutable vartbl_count : int;
    vartbl_tbl : (string,int) Hashtbl.t;
  }

  let mk_vartbl () =
    { vartbl_count = 0;
      vartbl_tbl = Hashtbl.create 5;
    }

  let getvar ~tbl name =
    try Hashtbl.find tbl.vartbl_tbl name
    with Not_found ->
      let n = tbl.vartbl_count in
      Hashtbl.add tbl.vartbl_tbl name n;
      tbl.vartbl_count <- n + 1;
      n

  let term_of_ast ~tbl ast = match ast with
    | Ast.Const s
    | Ast.Quoted s ->
      mk_const (StringSymbol.make s)
    | Ast.Var x ->
      mk_var (getvar ~tbl x)

  let literal_of_ast ?(tbl=mk_vartbl ()) lit = match lit with
    | Ast.Atom (s, args) ->
      let s = StringSymbol.make s in
      let args = List.map (term_of_ast ~tbl) args in
      mk_literal s args

  let clause_of_ast c = match c with
    | Ast.Clause (a, l) ->
      let tbl = mk_vartbl () in
      let a = literal_of_ast ~tbl a in
      let l = List.map (literal_of_ast ~tbl) l in
      mk_clause a l
end

let version = "0.3.1"
