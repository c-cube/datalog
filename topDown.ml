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
    type t =
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

    val to_string : t -> string
    val pp : out_channel -> t -> unit

    module Tbl : Hashtbl.S with type key = t
  end

  (** {2 State} *)

  module State : sig
    type t

    val create : unit -> t

    val add_fact : t -> T.t -> unit
    val add_facts : t -> T.t list -> unit

    val add_clause : t -> C.t -> unit
    val add_clauses : t -> C.t list -> unit
  end

  (** {2 Query computation} *)

  val query : State.t -> T.t -> (T.t -> unit) -> unit
    (** Iterate on the answers of the given query *)
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

    let mk_var i = Var i
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
    | Apply (c, args) ->
      let h = ref (Const.hash c) in
      for i = 0 to Array.length args -1 do
        h := combine_hash !h (hash args.(i))
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

    let head_symbol c = T.head_symbol c.head

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

  (** {2 State} *)

  module State = struct
    let __no_term = T.mk_var (~-1);

    type t = {
      mutable bindings : T.t array;   (* stack of bindings *)
      mutable bindings_n : int;       (* its height *)
      forest : goal T.Tbl.t;          (* forest of goals *)
      rules : unit C.Tbl.t ConstTbl.t;  (* maps constants to non-fact clauses *)
      facts : unit T.Tbl.t ConstTbl.t;  (* maps constants to facts *)
    }
    and goal = {
      goal : T.t;
      mutable answers : T.t list;
    }

    let create () =
      let st = {
        bindings = Array.make 4096 __no_term;
        bindings_n = 0;
        forest = T.Tbl.create 127;
        rules = ConstTbl.create 23;
        facts = ConstTbl.create 23;
      } in
      st

    let add_fact st t =
      let sym = T.head_symbol t in
      try
        let set = ConstTbl.find st.facts sym in
        T.Tbl.replace set t ()
      with Not_found ->
        let set = T.Tbl.create 5 in
        T.Tbl.add set t ();
        ConstTbl.add st.facts sym set

    let add_facts st l = List.iter (add_fact st) l

    let add_clause st c =
      match c.C.body with
      | [] -> add_fact st c.C.head
      | _::_ ->
        let sym = C.head_symbol c in
        try
          let set = ConstTbl.find st.rules sym in
          C.Tbl.replace set c ()
        with Not_found ->
          let set= C.Tbl.create 5 in
          C.Tbl.add set c ();
          ConstTbl.add st.rules sym set

    let add_clauses st l = List.iter (add_clause st) l
  end

  let query st lit k =
    failwith "query: not implemented"
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

  let term_of_ast t = assert false
  let lit_of_ast lit = assert false
  let clause_of_ast c = assert false
  let clauses_of_ast l = List.map clause_of_ast l
end
