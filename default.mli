
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

(** {1 Default literal base, where symbols are just strings} *)

(** {2 Default symbols: hashconsed strings} *)

module StringSymbol : BottomUp.SymbolType with type t = string

(** {2 Default implementation}
    
    This is a ready-to-use instance of {!BottomUp.Make}, with hashconsed strings
    as symbols. It also features some handy conversion functions from
    {! BottomUpBottomUpAst}.
*)

include BottomUp.S with type symbol = string

type vartbl = {
  mutable vartbl_count : int;
  vartbl_tbl : (string,int) Hashtbl.t;
}

val mk_vartbl : unit -> vartbl

val literal_of_ast : ?tbl:vartbl -> BottomUpAst.literal -> literal

val clause_of_ast : BottomUpAst.clause -> clause

val query_of_ast : BottomUpAst.query -> (int array * literal list * literal list)
