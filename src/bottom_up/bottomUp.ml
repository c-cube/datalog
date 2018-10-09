
(* this file is part of datalog. See README for the license *)

(** {1 Main Datalog module} *)

(** {2 Universal type} *)

module Univ = struct
  type t = unit -> unit
    (** The universal type *)

  type 'a embedding = {
    pack : 'a -> t;           (** Pack a 'a into a univ value *)
    unpack : t -> 'a option;  (** Try to unpack the univ value into an 'a *)
  } (** Conversion between the universal type and 'a *)

  (** Create a new embedding. Values packed by a given embedding can
      only be unpacked by the same embedding. *)
  let embed () =
    let r = ref None in (* place to store values *)
    let pack a =        (* pack the 'a value into a new univ cell *)
      let o = Some a in
      fun () -> r := o
    in
    let unpack t =      (* try to extract the content of a univ cell *)
      r := None;
      t ();
      let a = !r in
      a
    in
    { pack; unpack; }

  let pack emb x = emb.pack x

  let unpack emb t = emb.unpack t

  let compatible emb t = match unpack emb t with
    | None -> false
    | Some _ -> true
end

(** Module type for logic *)
module type S = sig
  (** {2 Literals and clauses} *)

  type symbol
    (** Abstract type of symbols (individual objects) *)

  type term = private
    | Var of int
    | Const of symbol
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
    (** Helper to build a literal. Arguments are either variables or constants *)

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

  (** {2 Higher level API} *)

  (** This part of the API can be used to avoid building variables
      yourself. Calling [quantify3 f] with call [f] with 3 distinct
      variables, and [f] can use those variables to, for instance,
      build a clause *)

  val quantify1 : (term -> 'a) -> 'a
  val quantify2 : (term -> term -> 'a) -> 'a
  val quantify3 : (term -> term -> term -> 'a) -> 'a
  val quantify4 : (term -> term -> term -> term -> 'a) -> 'a
  val quantifyn : int -> (term list -> 'a) -> 'a

  (** {2 The Datalog unit resolution algorithm} *)

  exception UnsafeClause

  type db
    (** A database of facts and clauses, with incremental fixpoint computation *)

  type explanation =
    | Axiom
    | Resolution of clause * literal
    | ExtExplanation of string * Univ.t
    (** Explanation for a clause or fact. It is extensible through universal types. *)

  val db_create : unit -> db
    (** Create a DB *)

  val db_copy : db -> db
    (** Deep copy of the DB *)

  val db_mem : db -> clause -> bool
    (** Is the clause member of the DB? *)

  val db_add : ?expl:explanation -> db -> clause -> unit
    (** Add the clause/fact to the DB as an axiom, updating fixpoint.
        UnsafeRule will be raised if the rule is not safe (see {!check_safe}) *)

  val db_add_fact : ?expl:explanation -> db -> literal -> unit
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
  val db_subscribe_all_facts : db -> fact_handler -> unit
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

  (** {2 Querying} *)

  module Query : sig
    type set
      (** mutable set of term lists *)

    val ask : db -> ?neg:literal list -> int array -> literal list -> set
      (** Given a list of variables, and a list of literals that contain those
          variables, return a set. Each element of the set is an instantiation
          of the variables such that all instantiated literals are facts of
          the [db]. [neg] is an optional list of literals that must be false
          for an instantiation to be an answer.
          This is lazy, and will only be evaluated upon calls to {! iter},
          {! to_list} or other similar functions. The answers will be cached
          in the set and readily available thereafter. *)

    val iter : set -> (term array -> unit) -> unit
      (** Evaluate the set by iterating on it *)

    val to_list : set -> term array list
      (** Convert to a list *)

    val cardinal : set -> int
      (** Number of elements of the set *)

    val pp_plan : Format.formatter -> set -> unit
      (** Print query plan *)
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
    (** Individual object *)

  let mk_var i = Var i
  let mk_const s = Const s

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
    | Const _ -> false

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
    | Var i, Var j -> i = j
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
    | Var i -> i
    | Const s -> Symbol.hash s

  module TermHashtbl = Hashtbl.Make(struct
    type t = term
    let equal = eq_term
    let hash = hash_term
  end)

  (** Hash the literal *)
  let hash_literal t =
    let hash_term h t = match t with
    | Var i -> h * 65599 + i
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
    let rec check i =
      if i = Array.length r1
        then true
        else eq_literal r1.(i) r2.(i) && check (i+1)
    in
    Array.length r1 = Array.length r2 && check 0

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
        | Const _ -> offset
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
    | _, Const _ -> var, offset
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
        | Const _ -> assert false

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
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | Const _, Var _ -> raise UnifFailure (* cannot bind t2 *)
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
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | Const _, Var _ ->
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
        | Var i, Var j when i = j && o1' = o2' -> subst
        | Var _, Var _ ->
          bind_subst subst t1 o1' t2 o2' (* bind var *)
        | Const _, Var _
        | Var _, Const _ -> raise UnifFailure
      in unif_pairs subst 0

  (** shift literal by offset *)
  let shift_lit lit offset =
    if offset = 0 then lit
    else Array.map
      (fun t -> match t with
      | Var i -> Var (i+offset)
      | Const _ -> t)
      lit

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
        | Const _ -> t')
      lit

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

  (** {2 Higher level API} *)

  let quantify1 f =
    let v1 = mk_var 1 in
    f v1

  let quantify2 f =
    let v1 = mk_var 1 in
    let v2 = mk_var 2 in
    f v1 v2

  let quantify3 f =
    let v1 = mk_var 1 in
    let v2 = mk_var 2 in
    let v3 = mk_var 3 in
    f v1 v2 v3

  let quantify4 f =
    let v1 = mk_var 1 in
    let v2 = mk_var 2 in
    let v3 = mk_var 3 in
    let v4 = mk_var 4 in
    f v1 v2 v3 v4

  let quantifyn n f =
    let rec mk_vars = function
      | 0 -> []
      | n -> mk_var n :: mk_vars (n-1)
    in
    assert(n >= 0);
    f (mk_vars n)

  (** {2 Term index for generalization/specialization retrieval} *)

  (** Hashtable on literals *)
  module LitHashtbl = Hashtbl.Make(struct
    type t = literal
    let equal = eq_literal
    let hash = hash_literal
  end)

  (* TODO: SQL-like indexing for the fact index (hashtable on columns)? *)

  (** Type for an indexing structure on literals *)
  module type Index = sig
    type t
      (** A literal index *)

    type elt
      (** A value indexed by a literal *)

    val create : unit -> t
      (** Create a new index *)

    val copy : t -> t
      (** Deep copy (TODO copy-on-write?) *)

    val add : t -> literal -> elt -> unit
      (** Add an element indexed by the literal *)

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

    val size : t -> int
      (** Number of indexed elements (linear time) *)
  end

  (** Create an Index module for the given type of elements. The implementation
      is based on non-perfect discrimination trees. *)
  module MakeIndex(X : Hashtbl.HashedType) : Index with type elt = X.t = struct
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

    let rec copy t = match t with
      | Node (set, h) ->
        let set' = DataSet.copy set in
        let h' = TermHashtbl.create (TermHashtbl.length h) in
        TermHashtbl.iter
          (fun k t' -> TermHashtbl.add h' k (copy t'))
          h;
        Node (set', h')

    let __star = Var 0

    let term_to_char t = match t with
      | Const _ -> t
      | Var _ -> __star  (* any var is the same *)

    (** Add the element indexed by the literal *)
    let add t literal elt =
      let len = Array.length literal in
      (* index in subtrie [t], with a cursor at literal[i]. *)
      let rec add t i = match t, i with
      | Node (set, _subtries), i when i = len ->
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
    let equal () () = true
    let hash () = 0
  end)

  (** Hashtable on clauses *)
  module ClauseHashtbl = Hashtbl.Make(
    struct
      type t = clause
      let equal = eq_clause
      let hash = hash_clause
    end)

  (** Explanation for a clause or fact. It is extensible through universal types. *)
  type explanation =
    | Axiom
    | Resolution of clause * literal
    | ExtExplanation of string * Univ.t

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
    db_fact_handlers : fact_handler list SymbolHashtbl.t;  (** map symbol -> fact handlers *)
    mutable db_all_facts : fact_handler list;
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
      db_all_facts = [];
      db_fact_handlers = SymbolHashtbl.create 3;
      db_goal_handlers = [];
      db_funs = SymbolHashtbl.create 13;
      db_queue = Queue.create ();
    }

  let db_copy db =
    { db_all = ClauseHashtbl.copy db.db_all;
      db_facts = ClausesIndex.copy db.db_facts;
      db_goals = GoalIndex.copy db.db_goals;
      db_selected= ClausesIndex.copy db.db_selected;
      db_heads = ClausesIndex.copy db.db_heads;
      db_all_facts = db.db_all_facts;
      db_fact_handlers = SymbolHashtbl.copy db.db_fact_handlers;
      db_goal_handlers = db.db_goal_handlers;
      db_funs = SymbolHashtbl.copy db.db_funs;
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
    else if is_fact clause then (
      ClausesIndex.add db.db_facts clause.(0) clause;
      (* call handler for this fact, if any *)
      let s = match clause.(0).(0) with Const s -> s | Var _ -> assert false in
      let call_handler h =
        try h clause.(0)
        with e ->
          Format.eprintf
            "Datalog: exception while calling handler for %s@."
            (Symbol.to_string s);
          raise e
      in
      begin match SymbolHashtbl.find db.db_fact_handlers s with
        | l -> List.iter call_handler l
        | exception Not_found -> ()
      end;
      List.iter call_handler db.db_all_facts;
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
    ) else (
      assert (Array.length clause > 1);
      (* check if some goal unifies with head of clause *)
      let offset = offset clause in
      GoalIndex.retrieve_unify
        (fun () _goal () subst ->
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
    )

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
        (fun () _head clause subst ->
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
  let db_add ?(expl=Axiom) db clause =
    (if not (check_safe clause) then raise UnsafeClause);
    process_items db (`AddClause (clause, expl))

  (** Add a fact (ground unit clause) *)
  let db_add_fact ?(expl=Axiom) db lit =
    (if not (is_ground lit) then raise UnsafeClause);
    process_items db (`AddClause ([|lit|], expl))

  (** Add a goal to the DB. The goal is used to trigger backward chaining
      (calling goal handlers that could help solve the goal) *)
  let db_goal db lit =
    process_items db (`AddGoal lit)

  (** match the given literal with facts of the DB, calling the handler on
      each fact that match (with the corresponding substitution) *)
  let db_match db pattern handler =
    ClausesIndex.retrieve_specializations
      (fun () fact _ _subst -> handler fact)
      () (db.db_facts,0) (pattern,1)

  (** Like {!db_match}, but the additional int list is used to select
      bindings of variables in the literal. Their bindings, in the same
      order, are given to the callback. *)
  let db_query db pattern vars k =
    ClausesIndex.retrieve_specializations
      (fun () _lit _ subst ->
        let terms = List.map
          (fun i ->
            let v = mk_var i in
            let t, _ = deref subst v 1 in
            match t with
            | Var _ -> assert false  (* should be ground *)
            | Const s -> s)
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
    if SymbolHashtbl.mem db.db_funs s then (
      failwith ("function already defined for symbol " ^ Symbol.to_string s)
    );
    SymbolHashtbl.replace db.db_funs s f

  let db_subscribe_fact db symbol handler =
    let l = try SymbolHashtbl.find db.db_fact_handlers symbol with Not_found -> [] in
    SymbolHashtbl.replace db.db_fact_handlers symbol (handler::l)

  let db_subscribe_all_facts db handler =
    db.db_all_facts <- handler :: db.db_all_facts

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
        | ExtExplanation _
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
      | ExtExplanation _
      | Axiom -> clause, acc  (* no premises *)
      | Resolution (clause, fact) -> let acc = fact :: acc in search acc clause
    in
    search [] [|fact|]

  (** Get all the explanations that explain why this clause is true *)
  let db_explanations db clause =
    ClauseHashtbl.find_all db.db_all clause

  (** {2 Querying} *)

  module Query = struct
    module RowTable = Hashtbl.Make(struct
      type t = term array
      let equal = eq_literal
      let hash = hash_literal
    end)

    type set = {
      db : db;
      query : query;
    } (** mutable set of term lists *)
    and query = {
      q_expr : expr;                    (* relational expression *)
      q_vars : int array;               (* variables *)
      mutable q_table : table option;   (* answer table *)
    } (** A query *)
    and expr =
      | Match of literal * int array * int array  (* match with literal, then project *)
      | Join of query * query                 (* join on common variables *)
      | ProjectJoin of int array * query * query  (* join and project immediately *)
      | Project of int array * query            (* project on given columns*)
      | AntiJoin of query * query               (* tuples of q1 that are not joinable with q2 *)
    and table = {
      tbl_vars : int array;             (* vars labelling columns *)
      tbl_rows : unit RowTable.t;       (* set of rows *)
    } (** A relational table; column are labelled with variables *)

    (* union of two sets of variable *)
    let union_vars l1 l2 =
      let l = Array.fold_left
        (fun acc x -> if List.mem x acc then acc else x :: acc)
        (Array.to_list l2) l1
      in
      Array.of_list (List.sort compare l)

    (* variables that are common to [l1] and [l2] *)
    let common_vars l1 l2 =
      let l2 = Array.to_list l2 in
      let l = Array.fold_left
        (fun acc x -> if List.mem x l2 then x :: acc else acc)
        [] l1
      in
      Array.of_list l

    (* build a query from an expression *)
    let mk_query expr =
      let q_vars = match expr with
      | Match (_, vars, _) -> vars
      | Join (q1, q2) ->
        if common_vars q1.q_vars q2.q_vars = [||]
          then Array.append q1.q_vars q2.q_vars
          else union_vars q1.q_vars q2.q_vars
      | ProjectJoin (vars, _, _) -> vars
      | Project (vars, _) -> vars
      | AntiJoin (q1, _q2) -> q1.q_vars
      in
      { q_expr = expr; q_table = None; q_vars; }

    let mk_table vars =
      { tbl_vars = vars; tbl_rows = RowTable.create 27; }

    let add_table tbl row =
      RowTable.replace tbl.tbl_rows row ()

    let iter_table tbl k =
      RowTable.iter (fun row () -> k row) tbl.tbl_rows

    let length_table tbl = RowTable.length tbl.tbl_rows

    (* list of (var, index) for each variables in literal *)
    let vars_index_of_lit lit =
      let vars, indexes, _ = Array.fold_left
        (fun (vars,indexes,idx) t -> match t with
          | Var i when not (List.mem i vars) ->
            i::vars, idx::indexes, idx+1
          | _ -> vars, indexes, idx+1)
        ([], [], 0) lit
      in
      Array.of_list vars, Array.of_list indexes

    (* optimize query (TODO more optimization, e.g. re-balance joins) *)
    let rec optimize q = match q.q_expr with
      | Project (vars, {q_expr=Join(q1,q2);_}) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        mk_query (ProjectJoin (vars, q1, q2))
      | Project (vars, q') ->
        if vars = q'.q_vars
          then optimize q'  (* reord is the identity *)
          else mk_query (Project (vars, optimize q'))
      | ProjectJoin (vars, q1, q2) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        mk_query (ProjectJoin (vars, q1, q2))
      | Join (q1, q2) -> mk_query (Join (optimize q1, optimize q2))
      | AntiJoin (q1, q2) ->
        let q1 = optimize q1 in
        let q2 = optimize q2 in
        if common_vars q1.q_vars q2.q_vars = [||]
          then q1 (* diff is trivial *)
          else (* TODO: try to push antijoins in q1? *)
            mk_query (AntiJoin (optimize q1, optimize q2))
      | Match _ -> q

    (** Given a list of variables, and a list of literals that contain those
        variables, return a set. Each element of the set is an instantiation
        of the variables such that all instantiated literals are facts of
        the [db].
        This is lazy, and will only be evaluated upon calls to {! iter},
        {! to_list} or other similar functions. *)
    let ask db ?(neg=[]) vars lits =
      assert (Array.length vars > 0);
      (* buid query for a given lit *)
      let rec build_query lit =
        let vars, indexes = vars_index_of_lit lit in
        mk_query (Match (lit, vars, indexes))
      (* combine queries. [vars] is the list of variables in [q]. [lits']
          are the remaining constraint literals *)
      and combine_queries q lits = match lits with
        | [] -> q
        | lit::lits' ->
          let q' = build_query lit in
          let q'' = mk_query (Join (q, q')) in
          combine_queries q'' lits'
      in
      let q = match lits with
      | [] -> failwith "Datalog.Query.ask: require at least one literal"
      | lit::lits' ->
        (* initial query, to combine with other queries *)
        let q_lit = build_query lit in
        combine_queries q_lit lits'
      in
      (* negations *)
      let q = match neg with
      | [] -> q
      | lit::lits ->
        let q_neg = build_query lit in
        let q_neg = combine_queries q_neg lits in
        mk_query (AntiJoin (q, q_neg))
      in
      (* project columns *)
      let q = mk_query (Project (vars, q)) in
      (* optimize *)
      let q = optimize q in
      (* return set *)
      { db; query = q; }

    (* select the given indexes of [a] *)
    let select_indexes indexes a =
      Array.map (fun i -> Array.get a i) indexes

    exception Found of int

    (* column indexes of variables in [l]. For each [v] in [vars], finds
        the index of the first occurrence of [v] in [l]. *)
    let find_indexes vars l =
      Array.map (fun v ->
        try
          Array.iteri (fun i v' -> if v = v' then raise (Found i)) l;
          raise Not_found
        with Found i ->
          i)
      vars

    (** Evaluate the query set *)
    let rec eval db query =
      match query.q_table with
      | Some l -> l
      | None ->
        let tbl = match query.q_expr with
        | Match (lit, vars, indexes) ->
          let tbl = mk_table vars in
          (* iterate on literals that match [lit] *)
          db_match db lit
            (fun lit' ->
              let row = project indexes lit' in
              add_table tbl row);
          tbl
        | Project (vars, q) ->
          let tbl = eval db q in
          (* map variables to their index in the input table *)
          let indexes = find_indexes vars tbl.tbl_vars in
          let result = mk_table vars in
          (* project each input tuple *)
          iter_table tbl
            (fun row ->
              let row' = project indexes row in
              add_table result row');
          result
        | ProjectJoin (vars, q1, q2) ->
          eval_join ~vars db q1 q2
        | Join (q1, q2) ->
          eval_join ?vars:None db q1 q2
        | AntiJoin (q1, q2) ->
          let tbl1 = eval db q1 in
          let tbl2 = eval db q2 in
          antijoin tbl1 tbl2
      in
      (* save result before returning it *)
      query.q_table <- Some tbl;
      tbl
    (* special case of joins *)
    and eval_join ?vars db q1 q2 =
      (* evaluate subqueries *)
      let tbl1 = eval db q1 in
      let tbl2 = eval db q2 in
      (* common variables *)
      let common = common_vars tbl1.tbl_vars tbl2.tbl_vars in
      (* project on which vars? *)
      match vars, common with
      | None, [||] -> product tbl1 tbl2
      | Some vars, [||] -> project_product ~vars tbl1 tbl2
      | None, _ ->
        let vars = union_vars tbl1.tbl_vars tbl2.tbl_vars in
        join ~vars common tbl1 tbl2
      | Some vars, _ ->
        join ~vars common tbl1 tbl2
    (* project on the given indexes *)
    and project indexes row =
      Array.map (fun i -> row.(i)) indexes
    (* cartesian product *)
    and product tbl1 tbl2 =
      let vars = Array.append tbl1.tbl_vars tbl2.tbl_vars in
      let tbl = mk_table vars in
      iter_table tbl1
        (fun row1 ->
          iter_table tbl2
            (fun row2 -> let row = Array.append row1 row2 in add_table tbl row));
      tbl
    (* projection + cartesian product *)
    and project_product ~vars tbl1 tbl2 =
      let tbl = mk_table vars in
      let indexes = find_indexes vars (Array.append tbl1.tbl_vars tbl2.tbl_vars) in
      iter_table tbl1
        (fun row1 ->
          iter_table tbl2
            (fun row2 ->
              let row = Array.append row1 row2 in
              let row = project indexes row in  (* project *)
              add_table tbl row));
      tbl
    (* join on the given list of common variables, and project on [vars] *)
    and join ~vars common tbl1 tbl2 =
      let vars1 = tbl1.tbl_vars and vars2 = tbl2.tbl_vars in
      (* bij: var -> index of var in joined columns *)
      let indexes = find_indexes vars (Array.append vars1 vars2) in
      let result = mk_table vars in
      (* index rows of [tbl1] *)
      let idx1 = mk_index tbl1 common in
      (* which column in [tbl2] for variables of [common]? *)
      let common_indexes = find_indexes common vars2 in
      (* join on [tbl2] *)
      iter_table tbl2
        (fun row2 ->
          let join_items = select_indexes common_indexes row2 in
          (* join on [join_items] *)
          let rows1 = try Hashtbl.find idx1 join_items with Not_found -> [] in
          List.iter
            (fun row1 ->
              let row = project indexes (Array.append row1 row2) in
              add_table result row)
            rows1);
      result
    (* Antijoin of tables *)
    and antijoin tbl1 tbl2 =
      (* common variables *)
      let common = common_vars tbl1.tbl_vars tbl2.tbl_vars in
      assert (common <> [||]);
      let common_indexes = find_indexes common tbl1.tbl_vars in
      (* index tbl2, to test rows of tbl1 *)
      let idx2 = mk_index tbl2 common in
      let result = mk_table tbl1.tbl_vars in
      iter_table tbl1
        (fun row ->
          let join_items = select_indexes common_indexes row in
          if Hashtbl.mem idx2 join_items
            then ()  (* drop row *)
            else add_table result row);
      result
    (* Build an index for the given list of variables (in this order). The
        indexed rows do not contain the selected columns. *)
    and mk_index tbl vars =
      let indexes = find_indexes vars tbl.tbl_vars in
      let h = Hashtbl.create 17 in
      iter_table tbl
        (fun row ->
          (* tuple of values to index with *)
          let indexed_items = select_indexes indexes row in
          (* add [row] to the rows indexed by the same items *)
          let rows = try Hashtbl.find h indexed_items with Not_found -> [] in
          Hashtbl.replace h indexed_items (row::rows));
      h

    (** Evaluate the set and iter on it *)
    let iter set k =
      let answers = eval set.db set.query in
      iter_table answers k

    let to_list set =
      let tbl = eval set.db set.query in
      let l = ref [] in
      iter_table tbl (fun row -> l := row :: !l);
      !l

    let cardinal set =
      let tbl = eval set.db set.query in
      length_table tbl

    let pp_array ~sep pp_elt fmt a =
      Array.iteri (fun i x ->
        (if i > 0 then Format.pp_print_string fmt sep);
        pp_elt fmt x)
      a

    let pp_plan formatter set =
      let pp_var fmt i = Format.pp_print_int fmt i in
      let rec pp_q ~top fmt q = match q.q_expr with
      | Match (lit, vars, _) ->
        Format.fprintf fmt "match[%a] %a" (pp_array ~sep:"," pp_var) vars pp_literal lit
      | Join (q1, q2) ->
        (if not top then Format.pp_print_string fmt "(");
        Format.fprintf fmt "%a |><| %a" (pp_q ~top:false) q1 (pp_q ~top:false) q2;
        (if not top then Format.pp_print_string fmt ")");
      | ProjectJoin (vars, q1, q2) ->
        (if not top then Format.pp_print_string fmt "(");
        Format.fprintf fmt "%a |><|_[%a] %a" (pp_q ~top:false) q1
          (pp_array ~sep:"," pp_var) vars (pp_q ~top:false) q2;
        (if not top then Format.pp_print_string fmt ")");
      | AntiJoin (q1, q2) ->
        Format.fprintf fmt "%a |> %a" (pp_q ~top:false) q1 (pp_q ~top:false) q2
      | Project (vars, q) ->
        Format.fprintf fmt "project[%a] %a" (pp_array ~sep:"," pp_var) vars (pp_q ~top:false) q
      in pp_q ~top:true formatter set.query
  end
end

module Hashcons(S : SymbolType) = struct
  type t = S.t

  module W = Weak.Make(struct
    type t = S.t
    let equal x y = S.equal x y
    let hash x = S.hash x
  end)

  let equal (x:S.t) y = x == y

  let hash x = S.hash x

  let to_string x = S.to_string x

  let __table = W.create 1024   (** Weak table *)

  let make x =
    let y = W.merge __table x in
    y
end
