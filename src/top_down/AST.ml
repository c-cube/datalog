
(* this file is part of datalog. See README for the license *)

(** {1 AST for TopDown terms} *)

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
