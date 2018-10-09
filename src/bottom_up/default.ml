
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

module A = BottomUpAst

module StringSymbol = BottomUp.Hashcons(struct
  type t = string
  let equal a b = a = b
  let hash x = Hashtbl.hash x
  let to_string s = s
end)

include BottomUp.Make(StringSymbol)

type vartbl = {
  mutable vartbl_count : int;
  vartbl_tbl : (string,int) Hashtbl.t;
}

let mk_vartbl () =
  { vartbl_count = 0;
    vartbl_tbl = Hashtbl.create 5;
  }

let getvar ~tbl name =
  try Hashtbl.find tbl.vartbl_tbl name
  with Not_found ->
    let n = tbl.vartbl_count in
    Hashtbl.add tbl.vartbl_tbl name n;
    tbl.vartbl_count <- n + 1;
    n

let term_of_ast ~tbl ast = match ast with
  | A.Const s
  | A.Quoted s ->
    mk_const (StringSymbol.make s)
  | A.Var x ->
    mk_var (getvar ~tbl x)

let literal_of_ast ?(tbl=mk_vartbl ()) lit = match lit with
  | A.Atom (s, args) ->
    let s = StringSymbol.make s in
    let args = List.map (term_of_ast ~tbl) args in
    mk_literal s args

let clause_of_ast c = match c with
  | A.Clause (a, l) ->
    let tbl = mk_vartbl () in
    let a = literal_of_ast ~tbl a in
    let l = List.map (literal_of_ast ~tbl) l in
    mk_clause a l

let query_of_ast q = match q with
  | A.Query (vars, lits, neg) ->
    let tbl = mk_vartbl () in
    let lits = List.map (literal_of_ast ~tbl) lits in
    let neg = List.map (literal_of_ast ~tbl) neg in
    let vars = Array.of_list vars in
    let vars = Array.map
      (fun t -> match term_of_ast ~tbl t with
      | Var i -> i
      | Const _ -> failwith "query_of_ast: expected variables")
      vars
    in
    vars, lits, neg
