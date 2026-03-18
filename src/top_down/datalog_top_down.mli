(* this file is part of datalog. See README for the license *)

(** {1 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries with
    non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics" *)

module AST = AST
module Lexer = Lexer
module Parser = Parser

(** {2 Signature for symbols} *)

module type CONST = Sigs.CONST
module type TERM = Sigs.TERM
module type S = Sigs.S

(** {2 Generic implementation} *)

module Make (Const : CONST) : S with module Const = Const

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
  val parse_chan : in_channel -> [ `Ok of clause list | `Error of string ]
  val parse_file : string -> [ `Ok of clause list | `Error of string ]
  val parse_string : string -> [ `Ok of clause list | `Error of string ]

  val clause_of_string : string -> clause
  (** Parse a clause from a string, or fail. Useful shortcut to define
      properties of relations without building terms by hand.
      @raise Failure if the string is not a valid clause *)

  val term_of_string : string -> term
  (** @raise Failure if the string is not a valid term *)
end

module MakeParse (C : PARSABLE_CONST) (TD : S with type Const.t = C.t) :
  PARSE with type term = TD.T.t and type lit = TD.Lit.t and type clause = TD.C.t

val set_debug : bool -> unit

(** {2 Default Implementation with Strings} *)

type const =
  | Int of int
  | String of string

module Default : sig
  include S with type Const.t = const

  val default_interpreters : (const * string * DB.interpreter) list
  (** List of default interpreters for some symbols, mostly infix predicates *)

  val builtin : (const * BuiltinFun.t) list
  (** Default builtin functions *)

  val setup_default : DB.t -> unit
  (** Load the default interpreters and builtin functions into the DB *)

  include PARSE with type term = T.t and type lit = Lit.t and type clause = C.t
end
