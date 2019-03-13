
(* this file is part of datalog. See README for the license *)

(** {1 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries
    with non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics"
*)

module AST = AST
module Lexer = Lexer
module Parser = Parser

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

    val ground : t -> bool
    val vars : t -> int list
    val max_var : t -> int    (* max var, or 0 if ground *)
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

    val eq : t -> t -> bool
    val hash : t -> int

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

    val fmt : Format.formatter -> t -> unit
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

    val add : map -> Const.t -> t -> unit
      (** Interpret the given constant by the given function. The function
          can assume that any term is it given as a parameter has the
          constant as head. *)

    val add_list : map -> (Const.t * t) list -> unit

    val interpreted : map -> Const.t -> bool
      (** Is the constant interpreted by a built-in function? *)

    val eval : map -> T.t -> T.t
      (** Evaluate the term at root *)
  end

  (** The following hashtables use alpha-equivalence checking instead of
      regular, syntactic equality *)

  module TVariantTbl : Hashtbl.S with type key = T.t
  module CVariantTbl : Hashtbl.S with type key = C.t

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
      (** Recursive copy of the index *)

    val clear : t -> unit

    val add : t -> T.t -> Data.t -> t
      (** Add the term->data binding. This modifies the index! *)

    val remove : t -> T.t -> Data.t -> t
      (** Remove the term->data binding. This modifies the index! *)

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

  (** {2 DB} *)

  (** A DB stores facts and clauses, that constitute a logic program.
      Facts and clauses can only be added.

      Non-stratified programs will be rejected with NonStratifiedProgram.
  *)

  exception NonStratifiedProgram

  module DB : sig
    type t
      (** A database is a repository for Datalog clauses. *)

    type interpreter = T.t -> C.t list
      (** Interpreted predicate. It takes terms which have a given
          symbol as head, and return a list of (safe) clauses that
          have the same symbol as head, and should unify with the
          query term. *)

    val create : ?parent:t -> unit -> t

    val copy : t -> t
    val clear : t -> unit

    val add_fact : t -> T.t -> unit
    val add_facts : t -> T.t list -> unit

    val add_clause : t -> C.t -> unit
    val add_clauses : t -> C.t list -> unit

    val interpret : ?help:string -> t -> const -> interpreter -> unit
      (** Add an interpreter for the given constant. Goals that start with
          this constant will be given to all registered interpreters, all
          of which can add new clauses. The returned clauses must
          have the constant as head symbol. *)

    val interpret_list : t -> (const * string * interpreter) list -> unit
      (** Add several interpreters, with their documentation *)

    val is_interpreted : t -> const -> bool
      (** Is the constant interpreted by some OCaml code? *)

    val add_builtin : t -> Const.t -> BuiltinFun.t -> unit
      (** Add a builtin fun *)

    val builtin_funs : t -> BuiltinFun.map

    val eval : t -> T.t -> T.t
      (** Evaluate the given term at root *)

    val help : t -> string list
      (** Help messages for interpreted predicates *)

    val num_facts : t -> int
    val num_clauses : t -> int
    val size : t -> int

    val find_facts : ?oc:bool -> t -> scope -> T.t -> scope ->
                     (T.t -> Subst.t -> unit) -> unit
      (** find facts unifying with the given term, and give them
          along with the unifier, to the callback *)

    val find_clauses_head : ?oc:bool -> t -> scope -> T.t -> scope ->
                            (C.t -> Subst.t -> unit) -> unit
      (** find clauses whose head unifies with the given term,
          and give them along with the unifier, to the callback *)

    val find_interpretation : ?oc:bool -> t -> scope -> T.t -> scope ->
                              (C.t -> Subst.t -> unit) -> unit
      (** Given an interpreted goal, try all interpreters on it,
          and match the query against their heads. Returns clauses
          whose head unifies with the goal, along with the substitution. *)
  end

  (** {2 Query} *)

  val ask : ?oc:bool -> ?with_rules:C.t list -> ?with_facts:T.t list ->
            DB.t -> T.t -> T.t list
    (** Returns the answers to a query in a given DB. Additional facts and rules can be
        added in a local scope.
        @param oc enable occur-check in unification (default [false]) *)

  val ask_lits : ?oc:bool -> ?with_rules:C.t list -> ?with_facts:T.t list ->
                 DB.t -> T.t list -> Lit.t list -> T.t list
    (** Extension of {! ask}, where the query ranges over the list of
        variables (the term list), all of which must be bound in
        the list of literals that form a constraint.

        [ask_lits db vars lits] queries over variables [vars] with
        the constraints given by [lits].

        Conceptually, the query adds a clause (v1, ..., vn) :- lits, which
        should respect the same safety constraint as other clauses.

        @return a list of answers, each of which is a list of terms that
          map to the given list of variables.
        *)
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
  module Const = Const

  type const = Const.t

  let _debug_enabled = ref false

  let _debug_real k =
    k (fun fmt ->
        Format.fprintf Format.err_formatter "@[";
        Format.kfprintf
          (fun fmt -> Format.fprintf fmt "@]@.")
          Format.err_formatter fmt)

  let _debug k =
    if !_debug_enabled then (
      _debug_real k
    )

  let set_debug b = _debug_enabled := b

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

    let _safe_clause _head _body =
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
      | T.Apply (_c, [| |]) -> t
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

    let fmt out (s:t) =
      let rec aux out = function
        | Nil -> ()
        | Bind (x1,sc1,t2,sc2,tl) ->
          Format.fprintf out "@[X%d[%d] ->@ %a[%d]@],@ %a"
            x1 sc1 T.fmt t2 sc2 aux tl
      in
      Format.fprintf out "{@[<hv>%a@]}" aux s
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
      Subst.bind subst t2 sc2 t1 sc1
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
    | T.Var _, T.Var _ when sc1 = sc2 -> raise UnifFail  (* would be matching *)
    | T.Var _, T.Var _ -> Subst.bind subst t1 sc1 t2 sc2  (* can bind *)
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

  module BuiltinFun = struct
    type t = T.t -> T.t option

    type map = t ConstTbl.t

    let create () = ConstTbl.create 17
    let clear t = ConstTbl.clear t

    let add map c f =
      ConstTbl.replace map c f

    let add_list map l =
      List.iter (fun (c,f) -> add map c f) l

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
    module TermDataTbl = Hashtbl.Make(struct
      type t = T.t * Data.t
      let equal (t1,d1) (t2,d2) =
        are_alpha_equiv t1 t2 && Data.equal d1 d2
      let hash (t,d) =
        combine_hash (T.hash_novar t) (Data.hash d)
    end)

    type t = {
      mutable sub : t ConstTbl.t;
      mutable var : t option; (* follow var *)
      mutable data : unit TermDataTbl.t option;
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
        let a = Array.make (n+1) Var in
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
      ConstTbl.iter (fun s t' -> ConstTbl.add sub s (copy t')) t.sub;
      let data = match t.data with
        | None -> None
        | Some set -> Some (TermDataTbl.copy set)
      in
      { var; sub; data; }

    let clear t =
      ConstTbl.clear t.sub;
      t.var <- None;
      t.data <- None;
      ()

    let add idx t data =
      let arr = term_to_fingerprint t in
      (* traverse the trie *)
      let rec add tree i =
        if i = Array.length arr
        then
          (* add to data *)
          let set = match tree.data with
            | None ->
              let set = TermDataTbl.create 5 in
              tree.data <- Some set;
              set
            | Some set -> set
          in
          TermDataTbl.replace set (t,data) ();
          tree
        else
          match arr.(i) with
          | Var ->
            let tree' = match tree.var with
            | None -> create 5
            | Some tree' -> tree'
            in
            let tree' = add tree' (i+1) in
            tree.var <- Some tree';
            tree
          | Const s ->
            let tree' =
              try ConstTbl.find tree.sub s
              with Not_found -> create 5
            in
            let tree' = add tree' (i+1) in
            ConstTbl.replace tree.sub s tree';
            tree
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
            TermDataTbl.remove set (t, data);
            if TermDataTbl.length set = 0
              then tree.data <- None; (* remove data set *)
            tree
        else match arr.(i) with
        | Var ->
          begin match tree.var with
          | None -> tree
          | Some tree' ->
            let tree' = remove tree' (i+1) in
            (if is_empty tree'
              then tree.var <- None  (* remove subtree *)
              else tree.var <- Some tree');
            tree
          end
        | Const s ->
          begin try
            let tree' = ConstTbl.find tree.sub s in
            let tree' = remove tree' (i+1) in
            (if is_empty tree'
              then ConstTbl.remove tree.sub s
              else ConstTbl.replace tree.sub s tree');
            tree
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
            | {data=Some set;_} ->
              (* unify against the indexed terms now *)
              TermDataTbl.iter
                (fun (t',data) () ->
                  try
                    let subst = unify ~oc t' s_tree t s_t in
                    k data subst
                  with UnifFail -> ())
                set
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
              ConstTbl.iter
                (fun _ tree' -> iter tree' (i+1))
                tree.sub
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
            | {data=Some set;_} ->
              (* unify against the indexed terms now *)
              TermDataTbl.iter
                (fun (t',data) () ->
                  try
                    let subst = match_ ~oc t' s_tree t s_t in
                    k data subst
                  with UnifFail -> ())
                set
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
        TermDataTbl.iter (fun (t,data) () -> f t data) set);
      ConstTbl.iter
        (fun _ t' -> iter t' f)
        t.sub

    let rec size t =
      let s = match t.var with | None -> 0 | Some t' -> size t' in
      let s = match t.data with
        | None -> s
        | Some set -> TermDataTbl.length set + s
      in
      ConstTbl.fold
        (fun _ t' s -> size t' + s)
        t.sub s
  end

  (** {Rewriting} *)

  module TermIndex = Index(struct
    type t = T.t
    let equal = are_alpha_equiv
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
      | hd::l' ->
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
      | T.Apply _ ->
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
      | T.Apply (_, [| |]) -> rewrite_root trs t
      | T.Apply (s, arr) ->
        let arr' = Array.map (rewrite trs) arr in
        rewrite_root trs (T.mk_apply s arr')
  end

  (** {2 DB} *)

  (* TODO: aggregate {b functions} that collapse all their arguments
            into a constant (e.g., sum, average, max, min).
            Plug this into [slg_complete_aggregate]. *)

  (* TODO: dependency graph to check whether program is stratified *)

  (* TODO: reification of DB, with open(db) predicate that evaluates the rest
          of the clause in the scope of the given DB (parent: current context)*)

  (* TODO: on-disk DB, for instance append-only set of Bencode records?
          see the dict format google uses for bigtable *)

  exception NonStratifiedProgram

  module DB = struct
    type interpreter = T.t -> C.t list
      (** Interpreted predicate *)

    module ClauseIndex = Index(struct
      type t = C.t
      let equal = clause_are_alpha_equiv
      let hash = C.hash_novar
    end)

    type t = {
      mutable rules : ClauseIndex.t;  (* clauses with non null body *)
      mutable facts : TermIndex.t;  (* set of facts *)
      interpreters : interpreter list ConstTbl.t;  (* constants -> interpreters *)
      builtin : BuiltinFun.map;
      mutable help : string list;
      parent : t option;  (* for further query *)
    }

    let create ?parent () =
      let db = {
        rules = ClauseIndex.empty ();
        facts = TermIndex.empty ();
        interpreters = ConstTbl.create 7;
        builtin = BuiltinFun.create ();
        help = [];
        parent;
      } in
      db

    let rec copy db =
      let rules = ClauseIndex.copy db.rules in
      let facts = TermIndex.copy db.facts in
      let interpreters = ConstTbl.copy db.interpreters in
      let parent = match db.parent with
        | None -> None
        | Some db' -> Some (copy db')
      in
      { db with rules; facts; parent; interpreters; }

    let clear db =
      ClauseIndex.clear db.rules;
      TermIndex.clear db.facts;
      ConstTbl.clear db.interpreters;
      BuiltinFun.clear db.builtin;
      db.help <- [];
      ()

    let add_fact db t =
      db.facts <- TermIndex.add db.facts t t

    let add_facts db l = List.iter (fun f -> add_fact db f) l

    let add_clause db c =
      match c.C.body with
      | [] -> add_fact db c.C.head
      | _::_ -> db.rules <- ClauseIndex.add db.rules c.C.head c

    let add_clauses db l = List.iter (fun c -> add_clause db c) l

    let builtin_funs db = db.builtin

    let add_builtin db c f = BuiltinFun.add db.builtin c f

    let rec eval db t =
      let t' = BuiltinFun.eval db.builtin t in
      if t == t'
        (* try with parent DB, may have more success *)
        then match db.parent with
          | None -> t'
          | Some db' -> eval db' t'
        else eval db t'

    let interpret ?help db c inter =
      let help = match help with
      | None -> Printf.sprintf "<symbol %s>" (Const.to_string c)
      | Some h -> h
      in
      db.help <- help :: db.help;
      try
        let l = ConstTbl.find db.interpreters c in
        ConstTbl.replace db.interpreters c (inter :: l)
      with Not_found ->
        ConstTbl.add db.interpreters c [inter]

    let interpret_list db l =
      List.iter (fun (c, help, i) -> interpret ~help db c i) l

    let is_interpreted db c =
      ConstTbl.mem db.interpreters c

    let help db =
      let rec help acc db =
        let acc = List.rev_append db.help acc in
        match db.parent with
        | None -> acc
        | Some db' -> help acc db'
      in help [] db

    let num_facts db = TermIndex.size db.facts

    let num_clauses db = ClauseIndex.size db.rules

    let size db = num_facts db + num_clauses db

    let rec find_facts ?(oc=false) db s_db t s_t k =
      TermIndex.unify ~oc db.facts s_db t s_t k;
      match db.parent with
      | None -> ()
      | Some db' -> find_facts ~oc db' s_db t s_t k

    let rec find_clauses_head ?(oc=false) db s_db t s_t k =
      ClauseIndex.unify ~oc db.rules s_db t s_t k;
      match db.parent with
      | None -> ()
      | Some db' -> find_clauses_head ~oc db' s_db t s_t k

    let rec find_interpretation ?(oc=false) db s_db t s_t k =
      assert (not (T.is_var t));
      let c = T.head_symbol t in
      begin try
        let interpreters = ConstTbl.find db.interpreters c in
        List.iter
          (fun inter ->
            (* call interpreter to get clauses of its extension *)
            let clauses = inter t in
            List.iter
              (fun clause ->
                try
                  let subst = unify ~oc t s_t clause.C.head s_db in
                  k clause subst   (* clause unifies! *)
                with UnifFail -> ())
              clauses)
          interpreters
      with Not_found -> ()
      end;
      match db.parent with
      | None -> ()
      | Some db' -> find_interpretation ~oc db' s_db t s_t k
  end

  (** {2 Query} *)

  module Query = struct
    type t = {
      db : DB.t;
      oc : bool;                              (* perform occur-check? *)
      forest : goal_entry TVariantTbl.t;      (* forest of goals *)
      renaming : Subst.renaming;              (* renaming *)
      mutable stack : action;                 (* stack of actions to do *)
    } (** A global state for querying *)

    and action =
      | Done
      | Enter of goal_entry * action
      | NewClause of goal_entry * C.t * action
      | Aggregate of goal_entry * C.t * T.t * action
      | Exit of goal_entry * action

    and goal_entry = {
      goal : T.t;
      mutable answers : unit T.Tbl.t;         (* set of answers *)
      mutable poss : (goal_entry * C.t) list; (* positive waiters *)
      mutable negs : (goal_entry * C.t) list; (* negative waiters *)
      mutable complete : bool;                (* goal evaluation completed? *)
    } (** Root of the proof forest *)

    (** In a goal entry, [poss] and [negs] are other goals that depend
        on this given goal. IT's the reverse dependency graph.
        When an answer is added to this goal, it's also propagated
        to waiters. *)

    let create ~oc ~db =
      let query = {
        db;
        oc;
        forest = TVariantTbl.create 127;
        stack = Done;
        renaming = Subst.create_renaming ();
      } in
      query

    (* reset and return the renaming *)
    let _get_renaming ~query =
      Subst.reset_renaming query.renaming;
      query.renaming

    (* try to resolve fact with clause's first body literal *)
    let resolve ~query fact clause =
      match clause.C.body with
      | (Lit.LitPos lit) :: body' ->
        begin try
          let subst = unify ~oc:query.oc fact 0 lit 1 in
          let renaming = _get_renaming ~query in
          Some {
            C.head=Subst.eval subst ~renaming clause.C.head 1;
            C.body=Subst.eval_lits subst ~renaming body' 1;
          }
        with UnifFail -> None
        end
      | _ -> None

    let _iter_answers k node =
      T.Tbl.iter (fun t () -> k t) node.answers

    let _get_aggr c = match c.C.body with
      | Lit.LitAggr a :: _ -> a
      | _ -> assert false

    (* main loop for the DFS traversal of SLG resolution *)
    let rec slg_main ~query =
      match query.stack with
      | Done -> ()  (* done *)
      | Enter (goal_entry, stack') ->
        query.stack <- stack';
        (* expand goal entry *)
        slg_subgoal ~query goal_entry;
        slg_main ~query
      | Exit (goal_entry, stack') ->
        query.stack <- stack';
        (* close goal entry *)
        if not goal_entry.complete
          then slg_complete ~query goal_entry;
        slg_main ~query
      | NewClause (goal_entry, clause, stack') ->
        query.stack <- stack';
        (* process new clause in the forest of [goal_entry] *)
        slg_newclause ~query goal_entry clause;
        slg_main ~query
      | Aggregate (goal_entry, clause, subgoal, stack') ->
        query.stack <- stack';
        (* compute the answer of the aggregate *)
        let subgoal_entry = TVariantTbl.find query.forest subgoal in
        slg_complete_aggregate ~query goal_entry clause subgoal_entry.answers;
        slg_main ~query

    (* solve the [goal] by all possible means. Returns the goal_entry
       for this goal. *)
    and slg_solve ~query goal =
      _debug (fun k->k "slg_solve with %a" T.fmt goal);
      try
        TVariantTbl.find query.forest goal
      with Not_found ->
        (* new goal! insert it in the forest, and start solving it *)
        let goal_entry = {
          goal;
          answers = T.Tbl.create 7;
          poss = [];
          negs = [];
          complete = false;
        } in
        TVariantTbl.add query.forest goal goal_entry;
        (* push the goal on stack so that it is solved *)
        query.stack <- Enter (goal_entry, Exit (goal_entry, query.stack));
        goal_entry

    (* [goal_entry] is a fresh goal, resolve it with facts and clauses to
       obtain its answers *)
    and slg_subgoal ~query goal_entry =
      _debug (fun k->k "slg_subgoal with %a" T.fmt goal_entry.goal);
      DB.find_facts ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun _fact subst ->
          let renaming = _get_renaming ~query in
          let answer = Subst.eval subst ~renaming goal_entry.goal 0 in
          (* process the new answer to the goal *)
          slg_answer ~query goal_entry answer);
      (* resolve with rules *)
      DB.find_clauses_head ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun clause subst ->
          let renaming = _get_renaming ~query in
          let clause' = Subst.eval_clause subst ~renaming clause 1 in
          (* add a new clause to the forest of [goal] *)
          query.stack <- NewClause (goal_entry, clause', query.stack));
      (* resolve with interpreters *)
      DB.find_interpretation ~oc:query.oc query.db 1 goal_entry.goal 0
        (fun clause subst ->
          let renaming = _get_renaming ~query in
          let clause' = Subst.eval_clause subst ~renaming clause 1 in
          (* add a new clause to the forest of [goal] *)
          query.stack <- NewClause (goal_entry, clause', query.stack));
      ()

    (* called when a new clause appears in the forest of [goal] *)
    and slg_newclause ~query goal_entry clause =
      _debug (fun k->k "slg_newclause with %a and clause %a" T.fmt goal_entry.goal C.fmt clause);
      match clause.C.body with
      | [] ->
        (* new fact (or clause with only delayed lits) *)
        slg_answer ~query goal_entry clause.C.head
      | (Lit.LitPos subgoal)::_ ->
        (* positive subgoal  *)
        slg_positive ~query goal_entry clause subgoal
      | (Lit.LitNeg neg_subgoal)::body' when T.ground neg_subgoal ->
        (* negative subgoal: if neg_subgoal is solved, continue with clause' *)
        let clause' = {clause with C.body=body'; } in
        slg_negative ~query goal_entry clause' neg_subgoal
      | (Lit.LitAggr a)::_ ->
        (* aggregate: subgoal is a.guard *)
        slg_aggregate ~query goal_entry clause a.Lit.guard
      | _ -> failwith "slg_newclause with non-ground negative goal"

    (* add an answer [ans] to the given [goal]. If [ans] is new,
      insert it into the list of answers of [goal], and update
      positive and negative dependencies *)
    and slg_answer ~query goal_entry ans =
      _debug (fun k->k "slg_answer: %a" T.fmt ans);
      assert (T.ground ans);
      if not goal_entry.complete
      && not (T.Tbl.mem goal_entry.answers ans) then begin
        (* new answer! *)
        T.Tbl.add goal_entry.answers ans ();
        (* it's a fact, negative dependencies must fail *)
        goal_entry.negs <- [];
        (* resolve ans.head with positive dependencies *)
        List.iter
          (fun (goal', clause') ->
            match resolve ~query ans clause' with
            | None -> ()
            | Some clause'' ->
              (* resolution succeeded, add clause to the forest of [goal'] *)
              query.stack <- NewClause (goal', clause'', query.stack))
          goal_entry.poss;
      end

    (* positive subgoal. *)
    and slg_positive ~query goal_entry clause subgoal =
      _debug (fun k->k "slg_positive %a with clause %a, subgoal %a"
        T.fmt goal_entry.goal C.fmt clause T.fmt subgoal);
      let subgoal_entry = slg_solve ~query subgoal in
      (* register for future answers *)
      subgoal_entry.poss <- (goal_entry, clause) :: subgoal_entry.poss;
      (* use current answers *)
      T.Tbl.iter
        (fun ans () -> match resolve ~query ans clause with
          | None -> ()
          | Some clause' ->
            query.stack <- NewClause(goal_entry, clause', query.stack))
        subgoal_entry.answers;
      ()

    (* negative subgoal *)
    and slg_negative ~query goal_entry clause neg_subgoal =
      _debug (fun k->k "slg_negative %a with clause %a, neg_subgoal %a"
        T.fmt goal_entry.goal C.fmt clause T.fmt neg_subgoal);
      let subgoal_entry = slg_solve ~query neg_subgoal in
      if T.Tbl.length subgoal_entry.answers = 0
        then if subgoal_entry.complete
          then
            (* success, the negative goal has been solved *)
            slg_newclause ~query goal_entry clause
          else
            (* negative subgoal can still succeed, wait for completion *)
            subgoal_entry.negs <- (goal_entry, clause) :: subgoal_entry.negs
        else
          () (* failure, there are already positive answers; do nothing *)

    (* subgoal is the guard of the agregate in [clause] *)
    and slg_aggregate ~query goal_entry clause subgoal =
      _debug (fun k->k "slg_aggregate %a with clause %a, subgoal %a"
        T.fmt goal_entry.goal C.fmt clause T.fmt subgoal);
      (* before querying subgoal, prepare to gather its results *)
      query.stack <- Aggregate(goal_entry, clause, subgoal, query.stack);
      (* start subquery, and wait for it to complete *)
      let _ = slg_solve ~query subgoal in
      ()

    (* called exactly once, when the subgoal has completed *)
    and slg_complete_aggregate ~query goal_entry clause answers =
      _debug (fun k->k "slg_complete_aggregate %a with %a (%d ans)"
        T.fmt goal_entry.goal C.fmt clause (T.Tbl.length answers));
      let a = _get_aggr clause in
      (* the group by subst on all vars by [a.var], mapping each
         term (ground except for [a.var] to the set of values for [a.var] *)
      let groups: T.t list T.Tbl.t = T.Tbl.create 24 in
      (* gather all answers, grouped by substitution on variables
         excluding [a.var] *)
      let renaming = _get_renaming ~query in
      (* consistent renaming of [a.var] *)
      let new_var =
        let subst = Subst.bind Subst.empty a.Lit.var 0 a.Lit.var 2 in
        Subst.eval subst ~renaming a.Lit.var 0
      in
      T.Tbl.iter
        (fun t () ->
           (* unify a.guard with the answer, and extract the binding of a.var *)
           let subst =
             try unify a.Lit.guard 0 t 1
             with UnifFail -> failwith "could not unify with var?!"
           in
           (* the value to aggregate *)
           let res = Subst.eval subst ~renaming a.Lit.var 0 in
           assert (T.ground res);
           (* remap [a.var] to [new_var] in [t'] *)
           let subst = Subst.bind subst a.Lit.var 0 a.Lit.var 2 in
           let t' = Subst.eval subst ~renaming a.Lit.guard 0 in
           let l = try T.Tbl.find groups t' with Not_found -> [] in
           T.Tbl.replace groups t' (res::l))
        answers;
      (* now build [subst(goal)] where [a.var] maps to [f(res1…resn)] for
         this particular subst *)
      T.Tbl.iter
        (fun t res_l ->
           assert (res_l <> []);
           let aggr_t_raw = T.mk_apply_l a.Lit.constructor res_l in
           (* eval aggregate *)
           let aggr_t = DB.eval query.db aggr_t_raw in
           _debug (fun k->k "@[slg_aggr.group: t: %a,@ aggr_t: %a,@ eval-into: %a@]"
                      T.fmt t T.fmt aggr_t_raw T.fmt aggr_t);
           try
             let subst = unify ~oc:query.oc a.Lit.guard 0 t 1 in
             (* add [a.left = aggr_t] *)
             let subst = unify ~subst ~oc:query.oc a.Lit.left 0 aggr_t 1 in
             let answer = Subst.eval subst ~renaming goal_entry.goal 0 in
             _debug (fun k->k "@[<2>slg_aggr.answer: %a@ subst: %a@]" T.fmt answer Subst.fmt subst);
             (* add answer *)
             _debug (fun k->k "slg_aggr.yield-answer: %a" T.fmt answer);
             slg_answer ~query goal_entry answer
           with UnifFail ->
             (* answer aggregate does not match left *)
             ()
        )
        groups;
      ()

    (* goal is completely evaluated, no more answers will arrive. *)
    and slg_complete ~query goal_entry =
      _debug (fun k->k "slg_complete %a" T.fmt goal_entry.goal);
      assert (not goal_entry.complete);
      goal_entry.complete <- true;
      if T.Tbl.length goal_entry.answers = 0
        then begin
          (* all negative goals succeed *)
          List.iter
            (fun (goal, clause) -> slg_newclause ~query goal clause)
            goal_entry.negs
        end;
      (* reclaim memory *)
      goal_entry.negs <- [];
      goal_entry.poss <- [];
      ()
  end

  let ask ?(oc=false) ?(with_rules=[]) ?(with_facts=[]) db lit =
    (* create a DB on top of the given one? *)
    let db = match with_rules, with_facts with
    | [], [] -> db
    | _ ->
      let db' = DB.create ~parent:db () in
      DB.add_facts db' with_facts;
      DB.add_clauses db' with_rules;
      db'
    in
    let query = Query.create ~oc ~db in
    (* recursive search for answers *)
    let goal_node = Query.slg_solve ~query lit in
    Query.slg_main ~query;
    (* get fact answers *)
    let l = ref [] in
    Query._iter_answers (fun ans -> l := ans :: !l) goal_node;
    !l

  let ask_lits ?(oc=false) ?(with_rules=[]) ?(with_facts=[]) db vars lits =
    (* special clause that defines the query *)
    let head = T.mk_apply Const.query (Array.of_list vars) in
    let clause = C.mk_clause head lits in
    let with_rules = clause :: with_rules in
    let l = ask ~oc ~with_rules ~with_facts db head in
    l
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

  val term_of_ast : ctx:name_ctx -> AST.term -> term
  val lit_of_ast : ctx:name_ctx -> AST.literal -> lit
  val clause_of_ast : ?ctx:name_ctx -> AST.clause -> clause
  val clauses_of_ast : ?ctx:name_ctx -> AST.clause list -> clause list

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

  module A = AST

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

  let parse_chan ic = _parse ~msg:"error while parsing <channel>" (Lexing.from_channel ic)

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
  module TD = Make(struct
    type t = const
    let equal a b = a = b
    let hash a = Hashtbl.hash a
    let to_string a = match a with
      | String s -> s
      | Int i -> string_of_int i
    let of_string s = String s
    let query = String ""
  end)

  include TD

  include MakeParse(struct
    type t = const
    let of_string s = String s
    let of_int i = Int i
  end)(TD)

  let default_interpreters =
    let _less goal =
      _debug (fun k->k "call less with %a" T.fmt goal);
      match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a < b -> [ C.mk_fact goal ]
      | _ -> []
    and _lesseq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a <= b -> [ C.mk_fact goal ]
      | _ -> []
    and _greater goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a > b -> [ C.mk_fact goal ]
      | _ -> []
    and _greatereq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a >= b -> [ C.mk_fact goal ]
      | _ -> []
    and _eq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a = b -> [ C.mk_fact goal ]
      | _ -> []
    and _neq goal = match goal with
      | T.Apply (_, [| T.Apply (a, [||]); T.Apply (b, [||]) |])
        when a <> b -> [ C.mk_fact goal ]
      | _ -> []
    and _print goal =
      begin match goal with
      | T.Apply (_, [| a |]) when T.ground a ->
        Printf.printf "> %a\n" T.pp a;
      | _ -> ()
      end;
      [ C.mk_fact goal ]
    (* given a list of arguments, "replace" the goal by any of its arguments.
       this allow arguments (variables...) to get to the proposition level *)
    and _eval goal = match goal with
      | T.Apply (String "eval", subgoals) ->
        (* for each goal \in subgoals, add a clause  goal :- subgoal *)
        Array.fold_left
          (fun acc sub -> C.mk_clause goal [Lit.mk_pos sub] :: acc)
          [] subgoals
      | _ -> []
    in
    [ String "lt", "lt(a,b): true if a < b", _less
    ; String "<", "a < b", _less
    ; String "le", "leq(a,b): true if a <= b", _lesseq
    ; String "<=", "a <= b", _lesseq
    ; String "gt", "gt(a,b): true if a > b", _greater
    ; String ">", "a > b", _greater
    ; String "ge", "geq(a, b): true if a >= b", _greatereq
    ; String ">=", "a >= b", _greatereq
    ; String "eq", "eq(a,b): true if a = b", _eq
    ; String "=", "=", _eq
    ; String "neq", "neq(a, b): true if a != b", _neq
    ; String "!=", "!=", _neq
    ; String "print", "print(a): print a term on stdout", _print
    ; String "eval", "eval(*goals): add eval(goals) :- g for each g in goals", _eval
    ]

  let _sum t =
    match t with
    | T.Apply (_, arr) ->
      begin try
        let x = Array.fold_left
          (fun x t' -> match t' with
            | T.Apply (Int i, [| |]) -> i+x
            | _ -> raise Exit)
          0 arr
        in
        Some (T.mk_const (Int x))
      with Exit -> None
      end
    | _ -> None

  let builtin =
    [ String "sum", _sum
    ]

  let setup_default db =
    DB.interpret_list db default_interpreters;
    BuiltinFun.add_list (DB.builtin_funs db) builtin;
    ()
end
