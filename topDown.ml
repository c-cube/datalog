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

(** {6 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries
    with non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics"
*)

module type S = sig
  type const

  (** {2 Terms} *)

  module T : sig
    type t = private
    | Var of int
    | Apply of const * t array

    val mk_var : int -> t
    val mk_const : const -> t
    val mk_apply : const -> t array -> t
    val mk_apply_l : const -> t list -> t

    val eq : t -> t -> bool
    val hash : t -> int

    val ground : t -> bool
    val vars : t -> int list
    val max_var : t -> int    (** max var, or 0 if ground *)
    val head_symbol : t -> const

    val to_string : t -> string
    val pp : out_channel -> t -> unit

    module Tbl : Hashtbl.S with type key = t
  end

  (** {2 Literals} *)

  module Lit : sig
    type t =
    | LitPos of T.t
    | LitNeg of T.t

    val mk_pos : T.t -> t
    val mk_neg : T.t -> t
    val mk : bool -> T.t -> t

    val eq : t -> t -> bool
    val hash : t -> int

    val to_term : t -> T.t
    val fmap : (T.t -> T.t) -> t -> t

    val to_string : t -> string
    val pp : out_channel -> t -> unit
  end

  (** {2 Clauses} *)

  module C : sig
    type t = private {
      head : T.t;
      body : Lit.t list;
    }

    exception Unsafe

    val mk_clause : T.t -> Lit.t list -> t
    val mk_fact : T.t -> t

    val eq : t -> t -> bool
    val hash : t -> int

    val head_symbol : t -> const
    val max_var : t -> int
    val fmap : (T.t -> T.t) -> t -> t

    val to_string : t -> string
    val pp : out_channel -> t -> unit

    module Tbl : Hashtbl.S with type key = t
  end

  (** {2 Substs} *)

  (** This module is used for variable bindings. *)

  module Subst : sig
    type t
    type scope = int
    type renaming

    val empty : t
      (** Empty subst *)
    
    val bind : t -> T.t -> scope -> T.t -> scope -> t
      (** Bind a variable,scope to a term,scope *)

    val deref : t -> T.t -> scope -> T.t * scope
      (** While the term is a variable bound in subst, follow its binding.
          Returns the final term and scope *)

    val create_renaming : unit -> renaming

    val reset_renaming : renaming -> unit

    val rename : renaming:renaming -> T.t -> scope -> T.t
      (** Rename the given variable into a variable that is unique
          within variables known to the given [renaming] *)

    val eval : t -> renaming:renaming -> T.t -> scope -> T.t
      (** Apply the substitution to the term. Free variables are renamed
          using [renaming] *)

    val eval_lit : t -> renaming:renaming -> Lit.t -> scope -> Lit.t

    val eval_lits : t -> renaming:renaming -> Lit.t list -> scope -> Lit.t list

    val eval_clause : t -> renaming:renaming -> C.t -> scope -> C.t
  end

  (** {2 Unification, matching...} *)

  type scope = Subst.scope

  exception UnifFail

  (** For {!unify} and {!match_}, the optional parameter [oc] is used to
      enable or disable occur-check. It is disabled by default. *)

  val unify : ?oc:bool -> ?subst:Subst.t -> T.t -> scope -> T.t -> scope -> Subst.t
    (** Unify the two terms.
        @raise UnifFail if it fails *)

  val match_ : ?oc:bool -> ?subst:Subst.t -> T.t -> scope -> T.t -> scope -> Subst.t
    (** [match_ a sa b sb] matches the pattern [a] in scope [sa] with term
        [b] in scope [sb].
        @raise UnifFail if it fails *)

  val alpha_equiv : ?subst:Subst.t -> T.t -> scope -> T.t -> scope -> Subst.t
    (** Test for alpha equivalence.
        @raise UnifFail if it fails *)

  val are_alpha_equiv : T.t -> T.t -> bool
    (** Special version of [alpha_equiv], using distinct scopes for the two
        terms to test, and discarding the result *)

  val clause_are_alpha_equiv : C.t -> C.t -> bool
    (** Alpha equivalence of clauses. *)

  (** The following hashtables use alpha-equivalence checking instead of
      regular, syntactic equality *)

  module TVariantTbl : Hashtbl.S with type key = T.t
  module CVariantTbl : Hashtbl.S with type key = C.t

  (** {2 DB} *)

  (** A DB stores facts and clauses, that constitute a logic program.
      Facts and clauses can only be added.
      
      TODO: interpreted symbols (with OCaml handlers)
  *)

  module DB : sig
    type t

    val create : unit -> t

    val copy : t -> t

    val add_fact : t -> T.t -> unit
    val add_facts : t -> T.t list -> unit

    val add_clause : t -> C.t -> unit
    val add_clauses : t -> C.t list -> unit
  end

  (** {2 Query} *)

  module Query : sig
    type t

    val ask : ?oc:bool -> DB.t -> T.t -> t
      (** Create a query in a given DB *)

    val answers : t -> T.t list
      (** All answers so far *)
  end
end

module type CONST = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
end

(** {2 Generic implementation} *)

let combine_hash hash i =
  abs (hash * 65599 + i)

(** Hash a list. Each element is hashed using [f]. *)
let rec hash_list f h l = match l with
  | [] -> h
  | x::l' -> hash_list f (combine_hash h (f x)) l'

let _array_forall2 p a1 a2 =
  if Array.length a1 = Array.length a2
    then try
      for i = 0 to Array.length a1 - 1 do
        if not (p a1.(i) a2.(i)) then raise Exit
      done;
      true
    with Exit -> false
    else false

let _array_exists p a =
  try
    for i = 0 to Array.length a - 1 do
      if p a.(i) then raise Exit
    done;
    false
  with Exit -> true

let _array_fold2 f acc a1 a2 =
  if Array.length a1 <> Array.length a2
    then failwith "_array_fold2: arrays must have same length";
  let acc = ref acc in
  for i = 0 to Array.length a1 - 1 do
    acc := f !acc a1.(i) a2.(i)
  done;
  !acc

module Make(Const : CONST) = struct
  type const = Const.t

  module ConstTbl = Hashtbl.Make(Const)
  module ConstWeak = Weak.Make(Const)

  module T = struct
    type t =
    | Var of int
    | Apply of const * t array
    type term = t

    let __const_table = ConstWeak.create 255

    let mk_var i = assert (i>=0); Var i
    let mk_apply const args =
      let const = ConstWeak.merge __const_table const in
      Apply (const, args)
    let mk_apply_l const args = mk_apply const (Array.of_list args)
    let mk_const const = mk_apply const [| |]

    (* equality *)
    let rec eq t1 t2 = match t1, t2 with
    | Var i, Var j -> i = j
    | Apply (c1, l1), Apply (c2, l2) ->
      Array.length l1 = Array.length l2 &&
      Const.equal c1 c2 &&
      _array_forall2 eq l1 l2
    | Var _, Apply _
    | Apply _, Var _ -> false

    (* hash *)
    let rec hash t = match t with
    | Var i -> i
    | Apply (c, [| |]) -> Const.hash c
    | Apply (c, args) ->
      let h = ref (Const.hash c) in
      for i = 0 to Array.length args -1 do
        h := combine_hash !h (hash args.(i))
      done;
      !h

    (* hash invariant by var renaming *)
    let rec hash_novar t = match t with
    | Var _ -> 42
    | Apply(c, args) ->
      let h = ref (Const.hash c) in
      for i = 0 to Array.length args -1 do
        h := combine_hash !h (hash_novar args.(i))
      done;
      !h

    let rec ground t = match t with
    | Var _ -> false
    | Apply (_, [| |]) -> false
    | Apply (_, args) ->
      _ground_arr args 0
    and _ground_arr a i =
      if i = Array.length a
        then true
        else ground a.(i) && _ground_arr a (i+1)

    let vars t =
      let rec _gather acc t = match t with
      | Var i when _var_present acc i -> acc
      | Var i -> i :: acc
      | Apply (_, [| |]) -> acc
      | Apply (_, args) -> Array.fold_left _gather acc args
      and _var_present l i = match l with
      | [] -> false
      | j::l' -> i = j || _var_present l' i
      in
      _gather [] t

    let rec max_var t = match t with
    | Var i -> i
    | Apply (_, args) ->
      Array.fold_left (fun m t' -> max m (max_var t')) 0 args

    let head_symbol t = match t with
    | Var _ -> failwith "variable has no head symbol"
    | Apply(c,_) -> c

    let to_string t =
      let rec pp buf t = match t with
      | Var i -> Printf.bprintf buf "X%d" i
      | Apply (c, [| |]) -> Buffer.add_string buf (Const.to_string c)
      | Apply (c, args) ->
        Printf.bprintf buf "%s(" (Const.to_string c);
        Array.iteri
          (fun i t' ->
            if i > 0 then Buffer.add_string buf ", ";
            pp buf t')
          args;
        Buffer.add_char buf ')'
      in
      let buf = Buffer.create 10 in
      pp buf t;
      Buffer.contents buf

    let pp oc t = output_string oc (to_string t)

    module Tbl = Hashtbl.Make(struct
      type t = term
      let equal = eq
      let hash = hash
    end)
  end

  module Lit = struct
    type t =
    | LitPos of T.t
    | LitNeg of T.t

    let mk_pos t = LitPos t
    let mk_neg t = LitNeg t
    let mk sign t =
      if sign then LitPos t else LitNeg t

    let eq lit1 lit2 = match lit1, lit2 with
    | LitPos t1, LitPos t2
    | LitNeg t1, LitNeg t2 -> T.eq t1 t2
    | _ -> false

    let hash lit = match lit with
    | LitPos t -> T.hash t
    | LitNeg t -> T.hash t + 65599 * 13

    let hash_novar lit = match lit with
    | LitPos t -> T.hash_novar t
    | LitNeg t -> T.hash_novar t + 65599 * 13

    let to_term = function
    | LitPos t
    | LitNeg t -> t

    let fmap f lit = match lit with
    | LitPos t -> LitPos (f t)
    | LitNeg t -> LitNeg (f t)

    let to_string lit = match lit with
    | LitPos t -> T.to_string t
    | LitNeg t -> Printf.sprintf "~%s" (T.to_string t)

    let pp oc lit = output_string oc (to_string lit)
  end

  module C = struct
    type t = {
      head : T.t;
      body : Lit.t list;
    }
    type clause = t

    exception Unsafe

    let _safe_clause head body =
      true  (* TODO *)

    let mk_clause head body =
      if _safe_clause head body
        then {head; body;}
        else raise Unsafe

    let mk_fact head = mk_clause head []

    let eq c1 c2 =
      T.eq c1.head c2.head &&
      List.length c1.body = List.length c2.body &&
      List.for_all2 Lit.eq c1.body c2.body

    let hash c = match c.body with
      | [] -> T.hash c.head
      | _ -> hash_list Lit.hash (T.hash c.head) c.body

    let hash_novar c = match c.body with
      | [] -> T.hash_novar c.head
      | _ -> hash_list Lit.hash_novar (T.hash_novar c.head) c.body

    let head_symbol c = T.head_symbol c.head

    let max_var c =
      List.fold_left
        (fun m lit -> max m (T.max_var (Lit.to_term lit)))
        (T.max_var c.head) c.body

    let fmap f c =
      let head = f c.head in
      let body = List.map (Lit.fmap f) c.body in
      mk_clause head body

    let to_string c = match c.body with
    | [] -> Printf.sprintf "%s." (T.to_string c.head)
    | _ ->
      let buf = Buffer.create 16 in
      Printf.bprintf buf "%s :- " (T.to_string c.head);
      List.iteri
        (fun i lit ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (Lit.to_string lit))
        c.body;
      Buffer.add_char buf '.';
      Buffer.contents buf

    let pp oc c =
      output_string oc (to_string c)

    module Tbl = Hashtbl.Make(struct
      type t = clause
      let equal = eq
      let hash = hash
    end)
  end

  (** {2 Substitutions} *)

  module Subst = struct
    type scope = int
    type t =
      | Nil
      | Bind of int * scope * T.t * scope * t

    type renaming = ((int*int), T.t) Hashtbl.t

    let empty = Nil

    let bind subst v s_v t s_t = match v with
    | T.Var i -> Bind (i, s_v, t, s_t, subst)
    | _ -> failwith "Subst.bind: expected variable"

    let deref subst t scope =
      let rec search subst v i scope = match subst with
      | Nil -> v, scope
      | Bind (i', s_i', t, s_t, _) when i = i' && scope = s_i' ->
        begin match t with
        | T.Var j -> search subst t j s_t
        | _ -> t, s_t
        end
      | Bind (_, _, _, _, subst') -> search subst' v i scope
      in
      match t with
      | T.Var i -> search subst t i scope
      | _ -> t, scope

    let create_renaming () =
      Hashtbl.create 7

    let reset_renaming r = Hashtbl.clear r

    let rename ~renaming v scope = match v with
    | T.Var i ->
      begin try
        let v' = Hashtbl.find renaming (i, scope) in
        v'
      with Not_found ->
        let n = Hashtbl.length renaming in
        let v' = T.mk_var n in
        Hashtbl.add renaming (i, scope) v';
        v'
      end
    | _ -> failwith "Subst.rename: expected variable"

    let rec eval subst ~renaming t scope =
      let t, scope = deref subst t scope in
      match t with
      | T.Var _ -> rename ~renaming t scope  (* free var *)
      | T.Apply (c, [| |]) -> t
      | T.Apply (c, args) ->
        let args' = Array.map
          (fun t' -> eval subst ~renaming t' scope)
          args
        in
        T.mk_apply c args'

    let eval_lit subst ~renaming lit scope =
      match lit with
      | Lit.LitPos t -> Lit.LitPos (eval subst ~renaming t scope)
      | Lit.LitNeg t -> Lit.LitNeg (eval subst ~renaming t scope)

    let eval_lits subst ~renaming lits scope =
      List.map
        (function
        | Lit.LitPos t -> Lit.LitPos (eval subst ~renaming t scope)
        | Lit.LitNeg t -> Lit.LitNeg (eval subst ~renaming t scope))
        lits

    let eval_clause subst ~renaming c scope =
      C.fmap (fun t -> eval subst ~renaming t scope) c
  end

  (** {2 Unification, matching...} *)

  type scope = Subst.scope

  exception UnifFail

  let rec _occur_check subst v sc_v t sc_t = match t with
  | T.Var _ when T.eq v t && sc_v = sc_t -> true
  | T.Var _ -> false
  | T.Apply (_, [| |]) -> false
  | T.Apply (_, args) ->
    _array_exists (fun t' -> _occur_check subst v sc_v t' sc_t) args

  let rec unify ?(oc=false) ?(subst=Subst.empty) t1 sc1 t2 sc2 =
    let t1, sc1 = Subst.deref subst t1 sc1 in
    let t2, sc2 = Subst.deref subst t2 sc2 in
    match t1, t2 with
    | T.Var i, T.Var j when i = j && sc1 = sc2 -> subst
    | T.Var _, _ when not oc || not (_occur_check subst t1 sc1 t2 sc2) ->
      Subst.bind subst t1 sc1 t2 sc2
    | _, T.Var _ when not oc || not (_occur_check subst t2 sc2 t1 sc1) ->
      Subst.bind subst t1 sc1 t2 sc2
    | T.Apply (c1, [| |]), T.Apply (c2, [| |]) when Const.equal c1 c2 -> subst
    | T.Apply (c1, l1), T.Apply (c2, l2)
      when Const.equal c1 c2 && Array.length l1 = Array.length l2 ->
      _array_fold2
        (fun subst t1' t2' -> unify ~oc ~subst t1' sc1 t2' sc2)
        subst l1 l2
    | _, _ -> raise UnifFail

  let rec match_ ?(oc=false) ?(subst=Subst.empty) t1 sc1 t2 sc2 =
    let t1, sc1 = Subst.deref subst t1 sc1 in
    let t2, sc2 = Subst.deref subst t2 sc2 in
    match t1, t2 with
    | T.Var i, T.Var j when i = j && sc1 = sc2 -> subst
    | T.Var _, _ when not oc || not (_occur_check subst t1 sc1 t2 sc2) ->
      Subst.bind subst t1 sc1 t2 sc2
    | T.Apply (c1, [| |]), T.Apply (c2, [| |]) when Const.equal c1 c2 -> subst
    | T.Apply (c1, l1), T.Apply (c2, l2)
      when Const.equal c1 c2 && Array.length l1 = Array.length l2 ->
      _array_fold2
        (fun subst t1' t2' -> match_ ~oc ~subst t1' sc1 t2' sc2)
        subst l1 l2
    | _, _ -> raise UnifFail

  let rec alpha_equiv ?(subst=Subst.empty) t1 sc1 t2 sc2 =
    let t1, sc1 = Subst.deref subst t1 sc1 in
    let t2, sc2 = Subst.deref subst t2 sc2 in
    match t1, t2 with
    | T.Var i, T.Var j when i = j && sc1 = sc2 -> subst
    | T.Apply (c1, [| |]), T.Apply (c2, [| |]) when Const.equal c1 c2 -> subst
    | T.Apply (c1, l1), T.Apply (c2, l2)
      when Const.equal c1 c2 && Array.length l1 = Array.length l2 ->
      _array_fold2
        (fun subst t1' t2' -> alpha_equiv ~subst t1' sc1 t2' sc2)
        subst l1 l2
    | _, _ -> raise UnifFail

  let are_alpha_equiv t1 t2 =
    try
      let _ = alpha_equiv t1 0 t2 1 in
      true
    with UnifFail ->
      false

  let _lit_alpha_equiv ~subst lit1 sc1 lit2 sc2 = match lit1, lit2 with
  | Lit.LitPos t1, Lit.LitPos t2
  | Lit.LitNeg t1, Lit.LitNeg t2 ->
    alpha_equiv ~subst t1 sc1 t2 sc2
  | _ -> raise UnifFail

  let clause_are_alpha_equiv c1 c2 =
    List.length c1.C.body = List.length c2.C.body &&
    try
      let subst = alpha_equiv c1.C.head 0 c2.C.head 1 in
      let _ = List.fold_left2
        (fun subst lit1 lit2 -> _lit_alpha_equiv ~subst lit1 0 lit2 1)
        subst c1.C.body c2.C.body
      in
      true
    with UnifFail ->
      false

  (* hashtable on terms that use alpha-equiv-checking as equality *)
  module TVariantTbl = Hashtbl.Make(struct
    type t = T.t
    let equal = are_alpha_equiv
    let hash = T.hash_novar
  end)

  module CVariantTbl = Hashtbl.Make(struct
    type t = C.t
    let equal = clause_are_alpha_equiv
    let hash = C.hash_novar
  end)

  (** {2 DB} *)
  
  (* TODO interpreted symbols (for given arity) *)
  (* TODO aggregates? *)

  module DB = struct
    type t = {
      rules : unit CVariantTbl.t ConstTbl.t;  (* maps constants to non-fact clauses *)
      facts : unit TVariantTbl.t ConstTbl.t;  (* maps constants to facts *)
    }

    let create () =
      let db = {
        rules = ConstTbl.create 23;
        facts = ConstTbl.create 23;
      } in
      db

    let copy db = failwith "DB.copy: not implemented" (* TODO *)

    let add_fact db t =
      let sym = T.head_symbol t in
      try
        let set = ConstTbl.find db.facts sym in
        TVariantTbl.replace set t ()
      with Not_found ->
        let set = TVariantTbl.create 5 in
        TVariantTbl.add set t ();
        ConstTbl.add db.facts sym set

    let add_facts db l = List.iter (add_fact db) l

    let add_clause db c =
      match c.C.body with
      | [] -> add_fact db c.C.head
      | _::_ ->
        let sym = C.head_symbol c in
        try
          let set = ConstTbl.find db.rules sym in
          CVariantTbl.replace set c ()
        with Not_found ->
          let set= CVariantTbl.create 5 in
          CVariantTbl.add set c ();
          ConstTbl.add db.rules sym set

    let add_clauses db l = List.iter (add_clause db) l
  end

  (** {2 Query} *)

  module Query = struct
    type x_clause = {
      head : T.t;
      delayed : Lit.t list;
      body : Lit.t list;
    } (** CLause with delayed negative literals *)

    module XClauseTbl = Hashtbl.Make(struct
      type t = x_clause
      let hash xc = Hashtbl.hash xc
      let equal xc1 xc2 =
        try T.eq xc1.head xc2.head
          && List.for_all2 Lit.eq xc1.delayed xc2.delayed
          && List.for_all2 Lit.eq xc1.body xc2.body
        with Invalid_argument _ -> false
    end)

    type t = {
      db : DB.t;
      oc : bool;                              (* perform occur-check? *)
      mutable count : int;                    (* global DFS count *)
      forest : goal_entry TVariantTbl.t;      (* forest of goals *)
      goals : stack_cell Stack.t;             (* stack of goals *)
      stack : (unit -> unit) Stack.t;         (* stack of tasks *)
      renaming : Subst.renaming;              (* renaming *)
      mutable top_answers : T.t list;         (* already known answers *)
    } (** A global state for querying *)

    and goal_entry = {
      goal : T.t;
      mutable answers : unit XClauseTbl.t;    (* set of answers *)
      mutable poss : (T.t * x_clause) list;   (* positive waiters *)
      mutable negs : (T.t * x_clause) list;   (* negative waiters *)
      mutable complete : bool;                (* goal evaluation completed? *)
    } (** Root of the proof forest *)

    and stack_cell = {
      sc_goal : T.t;
      dfn : int;                (* depth-first number *)
      mutable poslink : int;    (* lowest stack frame depended on positively *)
      mutable neglink : int;    (* lowest stack frame depended on negatively *)
    }

    (** In a goal entry, [poss] and [negs] are other goals that depend
        on this given goal. IT's the reverse dependency graph.
        When an answer is added to this goal, it's also propagated
        to waiters. *)

    let create ~oc ~db =
      let query = {
        db;
        oc;
        count = 1;
        forest = TVariantTbl.create 127;
        goals = Stack.create ();
        stack = Stack.create ();  (* TODO: more efficient stack (e.g. Vector)? *)
        renaming = Subst.create_renaming ();
        top_answers = [];
      } in
      query

    (* get goal entry for this term *)
    let _get_goal ~query t =
      try
        TVariantTbl.find query.forest t
      with Not_found ->
        let goal_entry = {
          goal = t;
          answers = XClauseTbl.create 7;
          poss = [];
          negs = [];
          complete = false;
        } in
        TVariantTbl.add query.forest t goal_entry;
        goal_entry

    (* push goal [t] on stack, return the stack frame *)
    let _push ~query t =
      let frame = {
        sc_goal = t;
        dfn = query.count;
        poslink = query.count;
        neglink = max_int;
      } in
      query.count <- query.count + 1;
      (* push frame *)
      Stack.push frame query.goals;
      frame

    (* reset and return the renaming *)
    let _get_renaming ~query =
      Subst.reset_renaming query.renaming;
      query.renaming

    (* resolution of [clause] with [lit] *)
    let compute_resolvent ~renaming lit s_lit clause s_clause subst =
      { head = Subst.eval subst ~renaming lit s_lit;
        delayed = [];
        body = Subst.eval_lits subst ~renaming clause.C.body s_clause;
      }

    (* try to resolve fact with clause *)
    let resolve ~query fact clause =
      match clause.body with
      | (Lit.LitPos lit) :: body' ->
        begin try
          let subst = unify fact 0 lit 1 in
          let renaming = _get_renaming ~query in
          Some {
            head=Subst.eval subst ~renaming clause.head 1;
            delayed=Subst.eval_lits subst ~renaming clause.delayed 1;
            body=Subst.eval_lits subst ~renaming body' 1;
          }
        with UnifFail -> None
        end
      | _ -> None


    (* factoring of [clause] w.r.t [subst], by delaying its selected
        literal *)
    let factor_clause ~renaming clause s_clause subst =
      match clause.body with
      | [] -> assert false
      | lit::body' ->
        let eval_lits lits = List.map
          (fun lit -> Subst.eval_lit subst ~renaming lit s_clause) lits
        in
        { head=Subst.eval subst ~renaming clause.head s_clause;
          delayed=eval_lits (lit :: clause.delayed);
          body=eval_lits body';
        }

    (* solve the subgoal [lit] by all possible means. *)
    let rec slg_subgoal ~query ~pos_min ~neg_min goal =
      let c = T.head_symbol goal in
      (* once results of resolution with clauses have been processed,
          we will have to call slg_complete *)
      Stack.push
        (fun () -> slg_complete ~query ~pos_min ~neg_min goal)
        query.stack;
      (* first, match with facts (can give answers directly) *)
      begin try
        let facts = ConstTbl.find query.db.DB.facts c in
        TVariantTbl.iter
          (fun fact () ->
            try
              let subst = unify goal 0 fact 1 in
              let renaming = _get_renaming ~query in
              let answer =
                {head=Subst.eval subst ~renaming goal 0;
                delayed=[]; body=[]; }
              in
              (* process the new answer to the goal *)
              slg_answer ~query ~pos_min ~neg_min goal answer
            with UnifFail -> ())
          facts;
      with Not_found -> ()
      end;
      (* then, resolve with rules *)
      try
        let rules = ConstTbl.find query.db.DB.rules c in
        (* resolve with clauses *)
        CVariantTbl.iter
          (fun clause () ->
            try
              let subst = unify goal 0 clause.C.head 1 in
              let renaming = _get_renaming ~query in
              let clause' = compute_resolvent ~renaming goal 0 clause 1 subst in
              (* eval [slg_newclause goal clause'] recursively, with external stack *)
              Stack.push
                (fun () -> slg_newclause ~query ~pos_min ~neg_min goal clause')
                query.stack
            with UnifFail -> ())
          rules;
      with Not_found -> ()

    (* called when a new clause appears in the forest
        of [goal] *)
    and slg_newclause ~query ~pos_min ~neg_min goal clause =
      assert (clause.delayed <> [] || clause.body <> []);
      match clause.body with
      | [] ->
        (* new fact (or clause with only delayed lits) *)
        slg_answer ~query ~pos_min ~neg_min goal clause
      | (Lit.LitPos subgoal)::_ ->
        (* positive subgoal  *)
        slg_positive ~query ~pos_min ~neg_min goal clause subgoal
      | (Lit.LitNeg neg_subgoal)::_ when T.ground neg_subgoal ->
        (* negative subgoal *)
        slg_negative ~query ~pos_min ~neg_min goal clause neg_subgoal
      | _ -> failwith "slg_newclause with non-ground negative goal"

    (* add an answer [ans] to the given [goal]. If [ans] is new,
      insert it into the list of answers of [goal], and update
      positive and negative dependencies depending on whether
      [ans] has delayed negative lits *)
    and slg_answer ~query ~pos_min ~neg_min goal ans =
      assert (ans.body = []);
      let node = _get_goal ~query goal in
      if not (XClauseTbl.mem node.answers ans) then begin
        (* new answer! *)
        XClauseTbl.add node.answers ans ();
        if ans.delayed = [] then begin
          (* it's a fact, negative dependencies must fail *)
          node.negs <- [];
          (* resolve ans.head with positive dependencies *)
          List.iter
            (fun (goal', clause') ->
              match resolve ~query ans.head clause' with
              | None -> ()
              | Some clause'' ->
                (* resolution succeeded, call slg_newclause recursively *)
                Stack.push
                  (fun () -> slg_newclause ~query ~pos_min ~neg_min goal' clause'')
                  query.stack)
            node.poss
        end else
          (* is there another answer with the same head? *)
          let new_head = try
            XClauseTbl.iter (fun ans' () ->
              if T.eq ans.head ans'.head then raise Exit)
              node.answers;
            true
            with Exit -> false
          in
          if new_head then
            (* factor clauses whose selected literal unifies with ans.head *)
            List.iter
              (fun (goal', clause') ->
                match clause'.body with
                | []
                | (Lit.LitNeg _) :: _ -> ()
                | (Lit.LitPos lit') :: _ ->
                  try
                    let subst = unify ans.head 0 lit' 1 in
                    let renaming = _get_renaming ~query in
                    let clause'' = factor_clause ~renaming clause' 1 subst in
                    (* call slg_newclause for the given factored clause *)
                    Stack.push
                      (fun () -> slg_newclause ~query ~pos_min ~neg_min goal' clause'')
                      query.stack
                  with UnifFail -> ())
              node.poss
      end

    (* positive subgoal *)
    and slg_positive ~query ~pos_min ~neg_min goal clause subgoal =
      if not (TVariantTbl.mem query.forest subgoal)
      then begin
        let sub_pos_min = ref query.count in
        let sub_neg_min = ref max_int in
        (* afterwards, update solutions of node *)
        Stack.push
          (fun () -> update_solutions ~query
            ~pos_min ~neg_min ~sub_pos_min ~sub_neg_min goal clause)
          query.stack;
        (* call slg_subgoal *)
        Stack.push
          (fun () ->
            ignore (_push ~query subgoal);
            slg_subgoal ~query ~pos_min:sub_pos_min ~neg_min:sub_neg_min subgoal)
          query.stack
      end else begin
        let node = _get_goal ~query subgoal in
        (* subgoal not complete? try to complete it *)
        if not node.complete then begin
          node.poss <- (goal, clause) :: node.poss;
          update_lookup ~query node
          end;
        assert false  (* TODO *)
      end

    (* negative subgoal *)
    and slg_negative ~query ~pos_min ~neg_min goal clause neg_subgoal =
      failwith "slg_negative: not implemented"  (* TODO *)

    and update_solutions ~query ~pos_min ~neg_min ~sub_pos_min ~sub_neg_min goal clause =
      assert false  (* TODO *)

    (* mark the [goal] as completed. *)
    and slg_complete ~query ~pos_min ~neg_min goal =
      assert false (* TODO *)

    and update_lookup ~query node =
      assert false (* TODO *)

    let ask ?(oc=false) db lit =
      let query = create ~oc ~db in
      let _ = _push ~query lit in
      let pos_min = ref query.count in
      let neg_min = ref max_int in
      (* recursive search for answers *)
      slg_subgoal ~query ~pos_min ~neg_min lit;
      while not (Stack.is_empty query.stack) do
        let task = Stack.pop query.stack in
        task ()
      done;
      let forest = _get_goal ~query lit in
      (* get fact answers into [query.top_answers] *)
      XClauseTbl.iter
        (fun clause () -> if clause.delayed = []
          then query.top_answers <- clause.head :: query.top_answers)
        forest.answers;
      query

    let answers query = query.top_answers
  end
end

(** {2 Default Implementation with Strings} *)

module Default = struct
  module TD = Make(struct
    type t = string
    let equal a b = a = b
    let hash a = Hashtbl.hash a
    let to_string a = a
  end)
 
  include TD

  module A = DatalogAst

  type name_ctx = (string, T.t) Hashtbl.t

  let rec term_of_ast ~ctx t = match t with
  | A.Const s
  | A.Quoted s -> T.mk_const s
  | A.Var s ->
    begin try
      Hashtbl.find ctx s
    with Not_found ->
      let n = Hashtbl.length ctx in
      let v = T.mk_var n in
      Hashtbl.add ctx s v;
      v
    end
  and _lit_of_ast ~ctx lit = match lit with
  | A.Atom (s, l) ->
    T.mk_apply_l s (List.map (term_of_ast ~ctx) l)
  and lit_of_ast ~ctx lit =
    Lit.mk_pos (_lit_of_ast ~ctx lit)

  let clause_of_ast ?(ctx=Hashtbl.create 3) c = match c with
    | A.Clause (head, body) ->
      let head = _lit_of_ast ~ctx head in
      let body = List.map (lit_of_ast ~ctx) body in
      C.mk_clause head body

  let clauses_of_ast ?ctx l = List.map (clause_of_ast ?ctx) l
end
