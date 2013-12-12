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

(** {1 Top-Down Computation} *)

(** This module implements top-down computation of Datalog queries
    with non-stratified negation.

    See "efficient top-down computation of queries under the well-founded
    semantics"
*)

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

module Make(Const : CONST) : S with module Const = Const

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

  val term_of_ast : ctx:name_ctx -> TopDownAst.term -> term
  val lit_of_ast : ctx:name_ctx -> TopDownAst.literal -> lit
  val clause_of_ast : ?ctx:name_ctx -> TopDownAst.clause -> clause
  val clauses_of_ast : ?ctx:name_ctx -> TopDownAst.clause list -> clause list

  val parse_chan : in_channel -> [`Ok of clause list | `Error of string]
  val parse_file : string -> [`Ok of clause list | `Error of string]
  val parse_string : string -> [`Ok of clause list | `Error of string]
end

module MakeParse(C : PARSABLE_CONST)(TD : S with type Const.t = C.t) :
  PARSE with type term = TD.T.t and type lit = TD.Lit.t and type clause = TD.C.t

(** {2 Default Implementation with Strings} *)

type const =
  | Int of int
  | String of string

module Default : sig
  include S with type Const.t = const

  val default_interpreters : (const * string * DB.interpreter) list
    (** List of default interpreters for some symbols, mostly
        infix predicates *)

  val builtin : (const * BuiltinFun.t) list
    (** Default builtin functions *)

  val setup_default : DB.t -> unit
    (** Load the default interpreters and builtin functions into the DB *)

  include PARSE with type term = T.t and type lit = Lit.t and type clause = C.t
end
