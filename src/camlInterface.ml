
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

(** {1 Bridge between Datalog.TopDown and OCaml} *)

(** {2 Constants with universal types} *)

module Univ = struct
  type packed_value =
  | Store : 'a key * 'a -> packed_value

  and 'a key = {
    mutable content : 'a option;
    eq : 'a -> 'a -> bool;
    hash : 'a -> int;
    print : 'a -> string;
  }

  type t = {
    get : unit -> unit;
    value : packed_value;
  }

  let _print_default _ = "<opaque>"

  let new_key ?(eq=(=)) ?(hash=Hashtbl.hash) ?(print=_print_default) () =
    { content=None; eq; hash; print; }

  let pack ~key x =
    let get () = key.content <- Some x in
    { get; value=Store (key, x); }

  let unpack ~key u =
    key.content <- None;
    u.get ();
    key.content

  let compatible ~key u =
    match unpack ~key u with | Some _ -> true | None -> false

  let eq u1 u2 =
    match u1.value with
    | Store (key1, x1) ->
      match unpack ~key:key1 u2 with
      | None -> false
      | Some x2 -> key1.eq x1 x2

  let hash u = match u.value with
    | Store (key, x) -> key.hash x

  let print u = match u.value with
    | Store (key, x) -> key.print x

  let _id x = x
  let _const x _ = x

  let string = new_key ~print:_id ()
  let int = new_key ~hash:_id ~print:string_of_int ()
  let bool = new_key ~print:string_of_bool ()
  let float = new_key ~print:string_of_float ()
  let unit = new_key ~print:(_const "()") ()

  let pair a b =
    new_key
      ~eq:(fun (a1,b1)(a2,b2) -> a.eq a1 a2 && b.eq b1 b2)
      ~hash:(fun (a1,b1) -> a.hash a1 * 65993 + b.hash b1)
      ~print:(fun (a1,b1) -> Printf.sprintf "(%s,%s)" (a.print a1) (b.print b1))
      ()

  let list a =
    let _print_list l =
      let buf = Buffer.create 15 in
      Buffer.add_char buf '[';
      List.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (a.print x))
        l;
      Buffer.add_char buf ']';
      Buffer.contents buf
    in
    new_key
      ~eq:(fun l1 l2 -> try List.for_all2 a.eq l1 l2 with Invalid_argument _ -> false)
      ~hash:(fun l -> List.fold_left (fun h x -> 65993 * h + a.hash x) 13 l)
      ~print:_print_list
      ()

  let array k =
    let _print_arr l =
      let buf = Buffer.create 15 in
      Buffer.add_string buf "[|";
      Array.iteri
        (fun i x ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (k.print x))
        l;
      Buffer.add_string buf "|]";
      Buffer.contents buf
    and _eq a1 a2 =
      Array.length a1 = Array.length a2 && try
        for i = 0 to Array.length a1 - 1 do
          if not (k.eq a1.(i) a2.(i)) then raise Exit
        done;
        true
      with Exit -> false
    and _hash a =
      let h = ref 13 in
      for i = 0 to Array.length a - 1 do
        h := 65993 * !h + k.hash a.(i)
      done;
      !h
    in 
    new_key
      ~eq:_eq ~hash:_hash ~print:_print_arr ()
end

(** Datalog constant *)
type const =
  | String of string
  | Int of int
  | Univ of Univ.t

let _equal c1 c2 = match c1, c2 with
  | String s1, String s2 -> s1 = s2
  | Int i1, Int i2 -> i1 = i2
  | Univ u1, Univ u2 -> Univ.eq u1 u2
  | _ -> false

let _hash = function
  | String s -> Hashtbl.hash s
  | Int i -> max_int land i
  | Univ u -> Univ.hash u

let _to_string = function
  | String s -> s
  | Int i -> string_of_int i
  | Univ u -> Univ.print u

let _key_query = Univ.new_key ~print:(fun () -> "query") ()
  (* special query symbol: unit, with a specific, hidden embedding *)

module TopDown = TopDown.Make(struct
  type t = const
  let equal = _equal
  let hash = _hash
  let to_string = _to_string
  let of_string s = String s
  let query = Univ (Univ.pack ~key:_key_query ())
end)

(** {2 Typed relations} *)

module T = TopDown.T

module Rel1 = struct
  type 'a t = string * 'a Univ.key

  let name (n, _) = n

  let create ?(k=Univ.new_key ()) name = (name, k)

  let get (name,k1) t = match t with
    | T.Apply (String name', [| T.Apply(Univ u1, [| |]) |]) when name = name' ->
      Univ.unpack ~key:k1 u1
    | _ -> None

  let make (n,k) x =
    let a1 = T.mk_const (Univ (Univ.pack ~key:k x)) in
    T.mk_apply (String n) [| a1 |]
end

module Rel2 = struct
  type ('a,'b) t = string * 'a Univ.key * 'b Univ.key

  let name (n, _, _) = n

  let create ?(k1=Univ.new_key ()) ?(k2=Univ.new_key ()) name = (name, k1, k2)

  let get (name,k1,k2) t = match t with
    | T.Apply (String name',
      [| T.Apply(Univ u1, [| |])
      ;  T.Apply(Univ u2, [| |])
      |]) when name = name' ->
      begin match Univ.unpack ~key:k1 u1, Univ.unpack ~key:k2 u2 with
      | Some x1, Some x2 -> Some (x1,x2)
      | _ -> None
      end
    | _ -> None

  let make (n,k1,k2) x1 x2 =
    let a1 = T.mk_const (Univ (Univ.pack ~key:k1 x1)) in
    let a2 = T.mk_const (Univ (Univ.pack ~key:k2 x2)) in
    T.mk_apply (String n) [| a1; a2 |]
end

module Rel3 = struct
  type ('a,'b,'c) t = string * 'a Univ.key * 'b Univ.key * 'c Univ.key

  let name (n, _, _, _) = n

  let create ?(k1=Univ.new_key ()) ?(k2=Univ.new_key ())
    ?(k3=Univ.new_key ()) name = (name, k1, k2, k3)

  let get (name,k1,k2,k3) t = match t with
    | T.Apply (String name',
      [| T.Apply(Univ u1, [| |])
      ;  T.Apply(Univ u2, [| |])
      ;  T.Apply(Univ u3, [| |])
      |]) when name = name' ->
      begin match Univ.unpack ~key:k1 u1, Univ.unpack ~key:k2 u2, Univ.unpack ~key:k3 u3 with
      | Some x1, Some x2, Some x3 -> Some (x1,x2,x3)
      | _ -> None
      end
    | _ -> None

  let make (n,k1,k2,k3) x1 x2 x3 =
    let a1 = T.mk_const (Univ (Univ.pack ~key:k1 x1)) in
    let a2 = T.mk_const (Univ (Univ.pack ~key:k2 x2)) in
    let a3 = T.mk_const (Univ (Univ.pack ~key:k3 x3)) in
    T.mk_apply (String n) [| a1; a2; a3 |]
end

module RelList = struct
  type 'a t = (string * 'a Univ.key)

  let name (n, _) = n

  let create ?(k=Univ.new_key ()) name = name, k

  let get (name, k) t = match t with
    | T.Apply (String name', a) when name = name' ->
      begin try
        let l = Array.fold_left
          (fun acc t' -> match t' with
            | T.Apply (Univ u, [| |]) ->
              begin match Univ.unpack ~key:k u with
                | None -> raise Exit
                | Some x -> x :: acc
              end
            | _ -> raise Exit)
          [] a
        in
        Some (List.rev l)
      with Exit -> None
      end
    | _ -> None

  let make (name,k) l =
    let args = List.map (fun x -> T.mk_const (Univ(Univ.pack ~key:k x))) l in
    T.mk_apply_l (String name) args
end

