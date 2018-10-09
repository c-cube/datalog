
(* this file is part of datalog. See README for the license *)

(** {1 Default literal base, where symbols are just strings} *)

(** {2 Default symbols: hashconsed strings} *)

type symbol = private string

module StringSymbol : sig
  include BottomUp.SymbolType with type t = symbol
  val make : string -> t
end

(** {2 Default implementation}
    
    This is a ready-to-use instance of {!BottomUp.Make}, with hashconsed strings
    as symbols. It also features some handy conversion functions from
    {! BottomUpAST}.
*)

include BottomUp.S with type symbol := symbol

type vartbl = {
  mutable vartbl_count : int;
  vartbl_tbl : (string,int) Hashtbl.t;
}

val mk_vartbl : unit -> vartbl

val literal_of_ast : ?tbl:vartbl -> AST.literal -> literal

val clause_of_ast : AST.clause -> clause

val query_of_ast : AST.query -> (int array * literal list * literal list)
