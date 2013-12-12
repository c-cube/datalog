
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

(** {2 Constants with universal types}
This module is present to allow the user to pack their own types into Datalog
constants. It is {b NOT thread-safe}.
This is largely inspired by {{: https://ocaml.janestreet.com/?q=node/18}this thread}
*)

module Univ : sig
  type t (** The universal type *)

  type 'a key
    (** Conversion between the universal type and 'a *)

  val new_key : ?eq:('a -> 'a -> bool) ->
                ?hash:('a -> int) ->
                ?print:('a -> string) ->
                unit -> 'a key
    (** Create a new key. Values packed with a given key can
        only be unpacked by the same key.

        @param print a printing function to print the content of a
          universal value. Default just returns "opaque".
        @hash hash values (default: Hashtbl.hash)
        @eq equality on values (default: structural equality) *)

  val pack : key:'a key -> 'a -> t
  val unpack : key:'a key -> t -> 'a option
  val compatible : key:'a key -> t -> bool

  val eq : t -> t -> bool
  val hash : t -> int
  val print : t -> string

  val string : string key
  val int : int key
  val float : float key
  val bool : bool key
  val unit : unit key
  val pair : 'a key -> 'b key -> ('a * 'b) key
  val list : 'a key -> 'a list key
  val array : 'a key -> 'a array key
end

(** Datalog constant. Constants for relations are built using
    {!Univ.string}, which is also used by {!of_string}. *)
type const = Univ.t

val of_string : string -> const
val of_int : int -> const

module Logic : TopDown.S with type Const.t = const

(** {2 Typed relations} *)

module Rel1 : sig
  type 'a t

  val name : _ t -> string
    (** Name of the relation *)

  val create : ?k:'a Univ.key -> string -> 'a t
    (** New relation, with given name and argument *)

  val get : 'a t -> Logic.T.t -> 'a option
    (** Check whether this term is a "R(t)" with [t] an object packed
        with the appropriate key, and "R" the name of the given relation *)

  val make : 'a t -> 'a -> Logic.T.t
    (** Create a term from this relation description *)

  val apply : 'a t -> Logic.T.t -> Logic.T.t
    (** apply the relation symbol to some term *)

  val find : Logic.DB.t -> 'a t -> 'a list
    (** Iterate on all instances of the relation present in the DB *)

  val subset : Logic.DB.t -> 'a t -> 'a t -> unit
    (** [subset db r1 r2] adds to [db] the axiom that [r2(X) :- r1(X)];
        in other words, [r1] is a subset of [r2] as a relation *)

  val from_fun : Logic.DB.t -> 'a t -> ('a -> bool) -> unit
    (** The given function decides of the given relation (if it returns true
        for a constant, then the relation holds for this constant) *)

  val add_list : Logic.DB.t -> 'a t -> 'a list -> unit
    (** Add given list of axioms *)
end

module Rel2 : sig
  type ('a, 'b) t

  val name : (_,_) t -> string

  val create : ?k1:'a Univ.key -> ?k2:'b Univ.key ->
               string -> ('a, 'b) t

  val get : ('a, 'b) t -> Logic.T.t -> ('a * 'b) option

  val make : ('a, 'b) t -> 'a -> 'b -> Logic.T.t

  val apply : (_,_) t -> Logic.T.t -> Logic.T.t -> Logic.T.t

  val find : Logic.DB.t -> ('a,'b) t -> ('a *'b) list

  val subset : Logic.DB.t -> ('a,'b) t -> ('a,'b) t -> unit
    (** [subset db r1 r2] adds to [db] the axiom that [r2(X,Y) :- r1(X,Y)];
        in other words, [r1] is a subset of [r2] as a relation *)

  val transitive : Logic.DB.t -> ('a, 'a) t -> unit
    (** Axioms for transitivity are added to the DB *)

  val symmetry : Logic.DB.t -> ('a, 'a) t -> unit
    (** Axiom for symmetry (ie "r(X,Y) <=> r(Y,X)") added to the DB *)

  val from_fun : Logic.DB.t -> ('a, 'b) t -> ('a -> 'b -> bool) -> unit
    (** The given function decides of the given relation (if it
        returns true for a couple of constants, then the relation
        holds for those constants) *)

  val add_list : Logic.DB.t -> ('a,'b) t -> ('a * 'b) list -> unit
    (** Add given list of axioms *)
end

module Rel3 : sig
  type ('a, 'b, 'c) t

  val name : (_,_,_) t -> string

  val create : ?k1:'a Univ.key -> ?k2:'b Univ.key -> ?k3:'c Univ.key ->
                string -> ('a, 'b, 'c) t

  val get : ('a, 'b, 'c) t -> Logic.T.t -> ('a * 'b * 'c) option

  val make : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> Logic.T.t

  val apply : (_,_,_) t -> Logic.T.t -> Logic.T.t -> Logic.T.t -> Logic.T.t

  val find : Logic.DB.t -> ('a,'b,'c) t -> ('a *'b*'c) list

  val subset : Logic.DB.t -> ('a,'b,'c) t -> ('a,'b,'c) t -> unit

  val from_fun : Logic.DB.t -> ('a, 'b, 'c) t -> ('a -> 'b -> 'c -> bool) -> unit

  val add_list : Logic.DB.t -> ('a, 'b, 'c) t -> ('a * 'b * 'c) list -> unit
end

module RelList : sig
  type 'a t

  val name : _ t -> string

  val create : ?k:'a Univ.key -> string -> 'a t

  val get : 'a t -> Logic.T.t -> 'a list option

  val make : 'a t -> 'a list -> Logic.T.t
end

(** {2 IO} *)

module Parse : sig
  include TopDown.PARSE
   with type term = Logic.T.t
    and type lit = Logic.Lit.t
    and type clause = Logic.C.t

  (** Additional functions, to load clauses directly into the DB *)

  val load_chan : Logic.DB.t -> in_channel -> bool
  val load_file : Logic.DB.t -> string -> bool
  val load_string : Logic.DB.t -> string -> bool
end
