
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

(** {1 AST} for parsing *)

type term =
  | Var of string
  | Apply of string * term list
  | Int of int

type aggregate = {
  ag_left : term;
  ag_constructor : string;
  ag_var : string;
  ag_guard : term;
} (* aggregate: ag_left = ag_constructor set
    where set is the set of bindings to ag_var
    that satisfy ag_guard *)

type literal =
  | LitPos of term
  | LitNeg of term
  | LitAggr of aggregate

type clause = term * literal list

type file = clause list

exception ParseError of string

let loc_to_str pos =
  Printf.sprintf "line %d, column %d"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let print_error ?(out=stderr) msg lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let stop = Lexing.lexeme_end_p lexbuf in
  Printf.fprintf out "parse error between %s and %s: %s\n"
    (loc_to_str start) (loc_to_str stop) msg

let error_to_string msg lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let stop = Lexing.lexeme_end_p lexbuf in
  Printf.sprintf "parse error between %s and %s: %s\n"
    (loc_to_str start) (loc_to_str stop) msg
