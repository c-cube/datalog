
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

(** {1 Base Definitions} *)

(** {2 Signature for symbols} *)

module type CONST = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val of_string : string -> t

  val query : t
    (** Special symbol, that will never occur in any user-defined
        clause or term. For strings, this may be the empty string "". *)
end

(** {2 Terms, Clauses, Substitutions, Indexes} *)

module type S = sig
  module Const : CONST

  type const = Const.t

  val set_debug : bool -> unit

  (** {2 Terms} *)

  module T : sig
    type t = private
    | Var of int
    | Apply of const * t array

    val mk_var : int -> t
    val mk_const : const -> t
    val mk_apply : const -> t array -> t
    val mk_apply_l : const -> t list -> t

    val is_var : t -> bool
    val is_apply : t -> bool
    val is_const : t -> bool

    val eq : t -> t -> bool
    val hash : t -> int
    val hash_novar : t -> int

    val ground : t -> bool
    val vars : t -> int list
    val max_var : t -> int    (** max var, or 0 if ground *)
    val head_symbol : t -> const

    val to_string : t -> string
    val pp : out_channel -> t -> unit
    val fmt : Format.formatter -> t -> unit

    val pp_tuple : out_channel -> t list -> unit

    module Tbl : Hashtbl.S with type key = t
  end

  (** {2 Literals} *)

  module Lit : sig
    type aggregate = {
      left : T.t;
      constructor : const;
      var : T.t;
      guard : T.t;
    } (* aggregate: ag_left = ag_constructor set
        where set is the set of bindings to ag_var
        that satisfy ag_guard *)

    type t =
    | LitPos of T.t
    | LitNeg of T.t
    | LitAggr of aggregate

    val mk_pos : T.t -> t
    val mk_neg : T.t -> t
    val mk : bool -> T.t -> t

    val mk_aggr : left:T.t -> constructor:const -> var:T.t -> guard:T.t -> t

    val eq : t -> t -> bool
    val hash : t -> int
    val hash_novar : t -> int

    val to_term : t -> T.t
    val fmap : (T.t -> T.t) -> t -> t

    val to_string : t -> string
    val pp : out_channel -> t -> unit
    val fmt : Format.formatter -> t -> unit
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

    val set_head : t -> T.t -> t
    val set_body : t -> Lit.t list -> t

    val eq : t -> t -> bool
    val hash : t -> int
    val hash_novar : t -> int

    val head_symbol : t -> const
    val max_var : t -> int
    val fmap : (T.t -> T.t) -> t -> t

    val to_string : t -> string
    val pp : out_channel -> t -> unit
    val fmt : Format.formatter -> t -> unit

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

  module Unif : sig
    exception Fail


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
  end

  (** {2 Special built-in functions}
  The built-in functions are symbols that have a special {b meaning}. The
  meaning is given by a set of OCaml functions that can evaluate applications
  of the function symbol to arguments.

  For instance, [sum] is a special built-in function that tries to add its
  arguments if those are constants.

  {b Note} that a constant will never be interpreted.
  *)

  module BuiltinFun : sig
    type t = T.t -> T.t option

    type map
      (** Map symbols to builtin functions. Every symbol can only have at
          most one built-in function. *)

    val create : unit -> map

    val add : map -> Const.t -> t -> map
      (** Interpret the given constant by the given function. The function
          can assume that any term is it given as a parameter has the
          constant as head. *)

    val add_list : map -> (Const.t * t) list -> map

    val interpreted : map -> Const.t -> bool
      (** Is the constant interpreted by a built-in function? *)

    val eval : map -> T.t -> T.t
      (** Evaluate the term at root *)
  end

  (** The following hashtables use alpha-equivalence checking instead of
      regular, syntactic equality *)

  module TVariantTbl : PersistentHashtbl.S with type key = T.t
  module CVariantTbl : PersistentHashtbl.S with type key = C.t

  (** {2 Index}
  An index is a specialized data structured that is used to efficiently
  store and retrieve data by a key, where the key is a term. Retrieval
  involves finding all data associated with terms that match,
  or unify with, a given term. *)

  module Index(Data : Hashtbl.HashedType) : sig
    type t
      (** A set of term->data bindings, for efficient retrieval by unification *)

    val empty : unit -> t
      (** new, empty index *)

    val copy : t -> t
      (** Full copy of the index *)

    val add : t -> T.t -> Data.t -> t
      (** Add the term->data binding. *)

    val remove : t -> T.t -> Data.t -> t
      (** Remove the term->data binding. *)

    val generalizations : ?oc:bool -> t -> scope -> T.t -> scope ->
                          (Data.t -> Subst.t -> unit) -> unit
      (** Retrieve data associated with terms that are a generalization
          of the given query term *)

    val unify : ?oc:bool -> t -> scope -> T.t -> scope ->
                (Data.t -> Subst.t -> unit) -> unit
      (** Retrieve data associated with terms that unify with the given
          query term *)

    val iter : t -> (T.t -> Data.t -> unit) -> unit
      (** Iterate on bindings *)

    val size : t -> int
      (** Number of bindings *)
  end

  (** {2 Rewriting}
  Rewriting consists in having a set of {b rules}, oriented from left to right,
  that we will write [l -> r] (say "l rewrites to r"). Any term t that l matches
  is {b rewritten} into r by replacing it by sigma(r), where sigma(l) = t.
  *)

  module Rewriting : sig
    type rule = T.t * T.t

    type t
      (** A rewriting system. It is basically a mutable set of rewrite rules. *)

    val create : unit -> t
      (** New rewriting system *)

    val copy : t -> t
      (** Copy the rewriting system *)

    val add : t -> rule -> unit
      (** Add a rule to the system *)

    val add_list : t -> rule list -> unit

    val to_list : t -> rule list
      (** List of rules *)

    val rewrite_root : t -> T.t -> T.t
      (** rewrite the term, but only its root. Subterms are not rewritten
          at all. *)

    val rewrite : t -> T.t -> T.t
      (** Normalize the term recursively. The returned type cannot be rewritten
          any further, assuming the rewriting system is {b terminating} *)
  end
end

(** {2 Implementation} *)

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


module Make(Const : CONST) : S with module Const = Const = struct
  module Const = Const

  type const = Const.t

  let _debug_enabled = ref false
  let _debug format = 
    if !_debug_enabled
      then
        Printf.kfprintf
          (fun oc -> output_char oc '\n')
          stderr format
      else
        Printf.ifprintf stderr format

  let set_debug b = _debug_enabled := b

  module ConstTbl = PersistentHashtbl.Make(Const)
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

    let is_var = function | Var _ -> true | Apply _ -> false
    let is_apply = function | Var _ -> false | Apply _ -> true
    let is_const = function Apply (_, [||]) -> true | _ -> false

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
    | Apply (_, [| |]) -> true
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

    let fmt fmt t = Format.pp_print_string fmt (to_string t)

    let pp_tuple oc l = match l with
      | [] -> output_string oc "()"
      | [t] -> Printf.fprintf oc "(%a)" pp t
      | l ->
        output_string oc "(";
        List.iteri
          (fun i t ->
            if i > 0 then output_string oc ", ";
            pp oc t)
          l;
        output_string oc ")"

    module Tbl = Hashtbl.Make(struct
      type t = term
      let equal = eq
      let hash = hash
    end)
  end

  module Lit = struct
    type aggregate = {
      left : T.t;
      constructor : const;
      var : T.t;
      guard : T.t;
    } (* aggregate: ag_left = ag_constructor set
        where set is the set of bindings to ag_var
        that satisfy ag_guard *)

    type t =
    | LitPos of T.t
    | LitNeg of T.t
    | LitAggr of aggregate

    let mk_pos t = LitPos t
    let mk_neg t = LitNeg t
    let mk sign t =
      if sign then LitPos t else LitNeg t

    let mk_aggr ~left ~constructor ~var ~guard =
      LitAggr {
        left;
        constructor;
        var;
        guard;
      }

    let eq lit1 lit2 = match lit1, lit2 with
    | LitPos t1, LitPos t2
    | LitNeg t1, LitNeg t2 -> T.eq t1 t2
    | LitAggr a1, LitAggr a2 ->
      T.eq a1.left a2.left &&
      T.eq a1.var a2.var &&
      T.eq a1.guard a2.guard &&
      Const.equal a1.constructor a2.constructor
    | _ -> false

    let hash lit = match lit with
    | LitPos t -> T.hash t
    | LitNeg t -> T.hash t + 65599 * 13
    | LitAggr a ->
      combine_hash
        (combine_hash (T.hash a.left) (T.hash a.var))
        (combine_hash (Const.hash a.constructor) (T.hash a.guard))

    let hash_novar lit = match lit with
    | LitPos t -> T.hash_novar t
    | LitNeg t -> T.hash_novar t + 65599 * 13
    | LitAggr a ->
      combine_hash
        (combine_hash (T.hash_novar a.left) (T.hash_novar a.var))
        (combine_hash (Const.hash a.constructor) (T.hash_novar a.guard))

    let to_term = function
    | LitPos t
    | LitNeg t -> t
    | LitAggr a ->
      let head = Const.of_string "aggr" in
      T.mk_apply head [| a.left; T.mk_const a.constructor; a.var; a.guard |]

    let fmap f lit = match lit with
    | LitPos t -> LitPos (f t)
    | LitNeg t -> LitNeg (f t)
    | LitAggr a -> LitAggr {
      a with
      left = f a.left;
      var = f a.var;
      guard = f a.guard;
    }

    let to_string lit = match lit with
    | LitPos t -> T.to_string t
    | LitNeg t -> Printf.sprintf "~%s" (T.to_string t)
    | LitAggr a ->
      Printf.sprintf "%s := %s %s : %s"
        (T.to_string a.left)
        (Const.to_string a.constructor)
        (T.to_string a.var)
        (T.to_string a.guard)

    let pp oc lit = output_string oc (to_string lit)

    let fmt fmt lit = Format.pp_print_string fmt (to_string lit)
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

    let set_head c head = {c with head; }
    let set_body c body = {c with body; }

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

    let fmt fmt c = Format.pp_print_string fmt (to_string c)

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

    (* special renaming *)
    let __dummy_renaming = create_renaming ()

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
      | T.Var _ when renaming == __dummy_renaming -> t (* keep *)
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
      | Lit.LitAggr a ->
        Lit.LitAggr {
          a with
          Lit.left = eval subst ~renaming a.Lit.left scope;
          Lit.var = eval subst ~renaming a.Lit.var scope;
          Lit.guard = eval subst ~renaming a.Lit.guard scope;
        }

    let eval_lits subst ~renaming lits scope =
      List.map
        (fun lit -> match lit with
        | Lit.LitPos t -> Lit.LitPos (eval subst ~renaming t scope)
        | Lit.LitNeg t -> Lit.LitNeg (eval subst ~renaming t scope)
        | Lit.LitAggr _ -> eval_lit subst ~renaming lit scope)
        lits

    let eval_clause subst ~renaming c scope =
      C.fmap (fun t -> eval subst ~renaming t scope) c
  end

  (** {2 Unification, matching...} *)

  type scope = Subst.scope

  module Unif = struct
    exception Fail

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
        Subst.bind subst t2 sc2 t1 sc1
      | T.Apply (c1, [| |]), T.Apply (c2, [| |]) when Const.equal c1 c2 -> subst
      | T.Apply (c1, l1), T.Apply (c2, l2)
        when Const.equal c1 c2 && Array.length l1 = Array.length l2 ->
        _array_fold2
          (fun subst t1' t2' -> unify ~oc ~subst t1' sc1 t2' sc2)
          subst l1 l2
      | _, _ -> raise Fail

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
      | _, _ -> raise Fail

    let rec alpha_equiv ?(subst=Subst.empty) t1 sc1 t2 sc2 =
      let t1, sc1 = Subst.deref subst t1 sc1 in
      let t2, sc2 = Subst.deref subst t2 sc2 in
      match t1, t2 with
      | T.Var i, T.Var j when i = j && sc1 = sc2 -> subst
      | T.Var i, T.Var j when sc1 = sc2 -> raise Fail  (* would be matching *)
      | T.Var i, T.Var j -> Subst.bind subst t1 sc1 t2 sc2  (* can bind *)
      | T.Apply (c1, [| |]), T.Apply (c2, [| |]) when Const.equal c1 c2 -> subst
      | T.Apply (c1, l1), T.Apply (c2, l2)
        when Const.equal c1 c2 && Array.length l1 = Array.length l2 ->
        _array_fold2
          (fun subst t1' t2' -> alpha_equiv ~subst t1' sc1 t2' sc2)
          subst l1 l2
      | _, _ -> raise Fail

    let are_alpha_equiv t1 t2 =
      try
        let _ = alpha_equiv t1 0 t2 1 in
        true
      with Fail ->
        false

    let _lit_alpha_equiv ~subst lit1 sc1 lit2 sc2 = match lit1, lit2 with
    | Lit.LitPos t1, Lit.LitPos t2
    | Lit.LitNeg t1, Lit.LitNeg t2 ->
      alpha_equiv ~subst t1 sc1 t2 sc2
    | _ -> raise Fail

    let clause_are_alpha_equiv c1 c2 =
      List.length c1.C.body = List.length c2.C.body &&
      try
        let subst = alpha_equiv c1.C.head 0 c2.C.head 1 in
        let _ = List.fold_left2
          (fun subst lit1 lit2 -> _lit_alpha_equiv ~subst lit1 0 lit2 1)
          subst c1.C.body c2.C.body
        in
        true
      with Fail ->
        false
  end

  module BuiltinFun = struct
    type t = T.t -> T.t option

    type map = t ConstTbl.t
    
    let create () = ConstTbl.create 17

    let add map c f =
      ConstTbl.replace map c f

    let add_list map l =
      List.fold_left (fun map (c,f) -> add map c f) map l

    let interpreted map c = ConstTbl.mem map c

    let rec eval map t = match t with
      | T.Var _ -> t
      | T.Apply (_, [| |]) -> t   (* non interpreted *)
      | T.Apply (c, _) ->
        let t' =
          try
            let f = ConstTbl.find map c in
            begin match f t with
              | None -> t
              | Some t' -> t'
            end
          with Not_found -> t
        in
        if t == t' then t else eval map t'
  end

  (* hashtable on terms that use alpha-equiv-checking as equality *)
  module TVariantTbl = PersistentHashtbl.Make(struct
    type t = T.t
    let equal = Unif.are_alpha_equiv
    let hash = T.hash_novar
  end)

  module CVariantTbl = PersistentHashtbl.Make(struct
    type t = C.t
    let equal = Unif.clause_are_alpha_equiv
    let hash = C.hash_novar
  end)

  (** {2 Indexing} *)

  (** Functor that allows fast retrieval of sets of values
      for the given {! Data} type, by unification or matching
      with a term.
      This is a kind of fingerprint indexing, but only at the very
      first level of subterms. *)
  module Index(Data : Hashtbl.HashedType) = struct
    (* what is the head of the term we can find at the given position? *)
    type fingerprint =
      | Var
      | Const of Const.t

    (* A hashset of (term * data) *)
    module TermDataTbl = PersistentHashtbl.Make(struct
      type t = T.t * Data.t
      let equal (t1,d1) (t2,d2) =
        Unif.are_alpha_equiv t1 t2 && Data.equal d1 d2
      let hash (t,d) =
        combine_hash (T.hash_novar t) (Data.hash d)
    end)

    type t = {
      sub : t ConstTbl.t;
      var : t option; (* follow var *)
      data : unit TermDataTbl.t option;
    }

    let create size = {
      sub=ConstTbl.create size;
      var=None;
      data=None;
    }

    let empty () = create 23

    (* is the tree empty? *)
    let is_empty tree =
      ConstTbl.length tree.sub = 0 &&
      (match tree.data with | None -> true | Some _ -> false) &&
      (match tree.var with | None -> true | Some _ -> false)

    (* fingerprint of a term *)
    let term_to_fingerprint t =
      match t with
      | T.Var _ -> [| Var |]
      | T.Apply (s, [||]) -> [| Const s |]
      | T.Apply (s, arr) ->
        let n = Array.length arr in
        let a = Array.create (n+1) Var in
        a.(0) <- Const s;
        for i = 0 to n-1 do
          a.(i+1) <- match arr.(i) with
          | T.Var _ -> Var
          | T.Apply (s, _) -> Const s
        done;
        a

    (* recursive copy of an index *)
    let rec copy t =
      let var = match t.var with
        | None -> None
        | Some t' -> Some (copy t')
      in
      let sub = ConstTbl.create 5 in
      let sub = ConstTbl.fold
        (fun sub s t' -> ConstTbl.replace sub s (copy t'))
        sub t.sub
      in
      let data = match t.data with
        | None -> None
        | Some set -> Some (TermDataTbl.copy set)
      in
      { var; sub; data; }

    let add idx t data =
      let arr = term_to_fingerprint t in
      (* traverse the trie *)
      let rec add tree i =
        if i = Array.length arr
        then
          (* add to data *)
          let set = match tree.data with
            | None -> TermDataTbl.create 5
            | Some set -> set
          in
          {tree with data=Some (TermDataTbl.replace set (t,data) ()); }
        else
          match arr.(i) with
          | Var ->
            let tree' = match tree.var with
            | None -> create 5
            | Some tree' -> tree'
            in
            let tree' = add tree' (i+1) in
            { tree with var=Some tree'; }
          | Const s ->
            let tree' =
              try ConstTbl.find tree.sub s
              with Not_found -> create 5
            in
            let tree' = add tree' (i+1) in
            let sub = ConstTbl.replace tree.sub s tree' in
            { tree with sub; }
      in
      add idx 0

    let remove idx t data =
      let arr = term_to_fingerprint t in
      (* traverse the trie *)
      let rec remove tree i =
        if i = Array.length arr
        then match tree.data with
          | None -> tree (* not present *)
          | Some set ->
            let set = TermDataTbl.remove set (t, data) in
            if TermDataTbl.length set = 0
              then {tree with data=None;} (* remove data set *)
              else {tree with data=Some set; }
        else match arr.(i) with
        | Var ->
          begin match tree.var with
          | None -> tree
          | Some tree' ->
            let tree' = remove tree' (i+1) in
            if is_empty tree'
              then {tree with var=None; }  (* remove subtree *)
              else {tree with var=Some tree'; }
          end
        | Const s ->
          begin try
            let tree' = ConstTbl.find tree.sub s in
            let tree' = remove tree' (i+1) in
            if is_empty tree'
              then {tree with sub=ConstTbl.remove tree.sub s;}
              else {tree with sub=ConstTbl.replace tree.sub s tree';}
          with Not_found -> tree
          end
      in
      remove idx 0

    (* unify [t] with terms indexed in [tree]. Successful unifiers
        are passed to [k] along with the data *)
    let unify ?(oc=false) tree s_tree t s_t k =
      let arr = term_to_fingerprint t in
      (* iterate on tree *)
      let rec iter tree i =
        if i = Array.length arr
          then match tree with
            | {data=Some set} ->
              (* unify against the indexed terms now *)
              TermDataTbl.iter set
                (fun (t',data) () ->
                  try
                    let subst = Unif.unify ~oc t' s_tree t s_t in
                    k data subst
                  with Unif.Fail -> ())
            | _ -> ()
          else begin
            (* recurse into subterms which have a variable *)
            begin match tree.var with
            | None -> ()
            | Some tree' -> iter tree' (i+1)
            end;
            match arr.(i) with
            | Var ->
              (* iterate on all subtrees *)
              ConstTbl.iter tree.sub
                (fun _ tree' -> iter tree' (i+1))
            | Const s ->
              (* iterate on the subtree with same symbol, if present *)
              try
                let tree' = ConstTbl.find tree.sub s in
                iter tree' (i+1)
              with Not_found -> ()
            end
      in
      iter tree 0

    (* same as {!unify} but for matching *)
    let generalizations ?(oc=false) tree s_tree t s_t k =
      let arr = term_to_fingerprint t in
      (* iterate on tree *)
      let rec iter tree i =
        if i = Array.length arr
          then match tree with
            | {data=Some set} ->
              (* unify against the indexed terms now *)
              TermDataTbl.iter set
                (fun (t',data) () ->
                  try
                    let subst = Unif.match_ ~oc t' s_tree t s_t in
                    k data subst
                  with Unif.Fail -> ())
            | _ -> ()
          else begin
            (* recurse into subterms which have a variable *)
            begin match tree.var with
            | None -> ()
            | Some tree' -> iter tree' (i+1)
            end;
            match arr.(i) with
            | Var -> ()  (* only variable does it *)
            | Const s ->
              (* iterate on the subtree with same symbol, if present *)
              try
                let tree' = ConstTbl.find tree.sub s in
                iter tree' (i+1)
              with Not_found -> ()
            end
      in
      iter tree 0

    let rec iter t f =
      (match t.var with | None -> () | Some t' -> iter t' f);
      (match t.data with | None -> () | Some set ->
        TermDataTbl.iter set (fun (t,data) () -> f t data));
      ConstTbl.iter t.sub (fun _ t' -> iter t' f) 

    let rec size t =
      let s = match t.var with | None -> 0 | Some t' -> size t' in
      let s = match t.data with
        | None -> s
        | Some set -> TermDataTbl.length set + s
      in
      ConstTbl.fold
        (fun s _ t' -> size t' + s)
        s t.sub
  end

  (** {Rewriting} *)

  module TermIndex = Index(struct
    type t = T.t
    let equal = Unif.are_alpha_equiv
    let hash = T.hash_novar
  end)

  module Rewriting = struct
    type rule = T.t * T.t

    type t = {
      mutable idx : TermIndex.t;
    }

    let create () = {
      idx = TermIndex.empty ();
    }

    let copy trs = { idx = TermIndex.copy trs.idx; }

    let add trs (l,r) =
      trs.idx <- TermIndex.add trs.idx l r

    let rec add_list trs l = match l with
      | [] -> ()
      | ((x,y) as hd)::l' ->
        add trs hd;
        add_list trs l'

    let to_list trs =
      let acc = ref [] in
      TermIndex.iter trs.idx
        (fun l r -> acc := (l,r) :: !acc);
      !acc

    exception RewriteInto of T.t * Subst.t * scope

    let rec rewrite_root trs t =
      match t with
      | T.Var _ -> t
      | T.Apply (s, arr) ->
        try
          TermIndex.generalizations trs.idx 1 t 0
            (fun r subst -> raise (RewriteInto (r, subst, 1)));
          t  (* didn't fire *)
        with RewriteInto (r, subst, scope) ->
          let t' = Subst.eval subst ~renaming:Subst.__dummy_renaming r scope in
          (* rewrite again *)
          rewrite_root trs t'

    (* TODO: more efficient rewriting *)
    let rec rewrite trs t = match t with
      | T.Var _ -> t
      | T.Apply (s, [| |]) -> rewrite_root trs t
      | T.Apply (s, arr) ->
        let arr' = Array.map (rewrite trs) arr in
        rewrite_root trs (T.mk_apply s arr')
  end
end

(** {2 Parsing} *)

module type PARSABLE_CONST = sig
  type t

  val of_string : string -> t
  val of_int : int -> t
end

module type PARSE = sig
  type term
  type lit
  type clause

  type name_ctx = (string, term) Hashtbl.t

  val create_ctx : unit -> name_ctx

  val term_of_ast : ctx:name_ctx -> Ast.term -> term
  val lit_of_ast : ctx:name_ctx -> Ast.literal -> lit
  val clause_of_ast : ?ctx:name_ctx -> Ast.clause -> clause
  val clauses_of_ast : ?ctx:name_ctx -> Ast.clause list -> clause list

  val parse_chan : in_channel -> [`Ok of clause list | `Error of string]
  val parse_file : string -> [`Ok of clause list | `Error of string]
  val parse_string : string -> [`Ok of clause list | `Error of string]

  val clause_of_string : string -> clause
  val term_of_string : string -> term
end

module MakeParse(C : PARSABLE_CONST)(TD : S with type Const.t = C.t) = struct
  type term = TD.T.t
  type lit = TD.Lit.t
  type clause = TD.C.t

  module A = Ast

  type name_ctx = (string, TD.T.t) Hashtbl.t

  let create_ctx () = Hashtbl.create 5

  let _mk_var ~ctx name =
    try
      Hashtbl.find ctx name
    with Not_found ->
      let n = Hashtbl.length ctx in
      let v = TD.T.mk_var n in
      Hashtbl.add ctx name v;
      v

  let rec term_of_ast ~ctx t = match t with
  | A.Apply (s, args) ->
    let args = List.map (term_of_ast ~ctx) args in
    TD.T.mk_apply_l (C.of_string s) args
  | A.Int i ->
    TD.T.mk_const (C.of_int i)
  | A.Var s -> _mk_var ~ctx s
  and lit_of_ast ~ctx lit =
    match lit with
    | A.LitPos t -> TD.Lit.mk_pos (term_of_ast ~ctx t)
    | A.LitNeg t -> TD.Lit.mk_neg (term_of_ast ~ctx t)
    | A.LitAggr a ->
      TD.Lit.mk_aggr
        ~constructor:(C.of_string a.A.ag_constructor)
        ~left:(term_of_ast ~ctx a.A.ag_left)
        ~guard:(term_of_ast ~ctx a.A.ag_guard)
        ~var:(_mk_var ~ctx a.A.ag_var)

  let clause_of_ast ?(ctx=Hashtbl.create 3) c = match c with
    | (head, body) ->
      let head = term_of_ast ~ctx head in
      let body = List.map (lit_of_ast ~ctx) body in
      TD.C.mk_clause head body

  let clauses_of_ast ?ctx l = List.map (clause_of_ast ?ctx) l

  let _parse ~msg lexbuf =
    try
      let decls = Parser.parse_file Lexer.token lexbuf in
      `Ok (clauses_of_ast decls)
    with
    | Parsing.Parse_error ->
      let msg = A.error_to_string msg lexbuf in
      `Error msg
    | Failure msg -> `Error msg

  let parse_chan ic =
    _parse ~msg:"error while parsing <channel>" (Lexing.from_channel ic)

  let parse_file f =
    let ic = open_in f in
    try
      let res = _parse ~msg:("error while parsing " ^ f) (Lexing.from_channel ic) in
      close_in ic;
      res
    with e ->
      close_in ic;
      `Error (Printexc.to_string e)

  let parse_string s = _parse ~msg:"error while parsing string" (Lexing.from_string s)

  let clause_of_string s =
    try
      let lexbuf = Lexing.from_string s in
      let ast = Parser.parse_clause Lexer.token lexbuf in
      clause_of_ast ast
    with Parsing.Parse_error -> failwith "clause_of_string: parse error"

  let term_of_string s =
    try
      let lexbuf = Lexing.from_string s in
      let ast = Parser.parse_term Lexer.token lexbuf in
      let ctx = create_ctx () in
      term_of_ast ~ctx ast
    with Parsing.Parse_error -> failwith "term_of_string: parse error"
end

(** {2 Default Implementation with Strings} *)

type const =
  | Int of int
  | String of string

module Default = struct
  module B = Make(struct
    type t = const
    let equal a b = a = b
    let hash a = Hashtbl.hash a
    let to_string a = match a with
      | String s -> s
      | Int i -> string_of_int i
    let of_string s = String s
    let query = String ""
  end)

  include B
  include MakeParse(struct
    type t = const
    let of_int i = Int i
    let of_string s = String s
  end)(B)
end
