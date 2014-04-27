
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

(** {1 Prolog-like command line tool} *)

open Datalog

module Logic = Base.Default
module DB = TopDown.Default

(** Options *)

let oc = ref false
let builtin = ref false
let unix = ref false
let doc = ref false
let print = ref true

(** Evaluate query *)

let parse_files_into db files =
  List.fold_left
    (fun db file ->
      match Base.DefaultParse.parse_file file with
      | `Error e ->
        print_endline e;
        db
      | `Ok clauses ->
        DB.add_clauses db clauses)
    db files

let eval_query files tuple goals =
  let db = DB.create () in
  let db = if !builtin then DB.setup_default db else db in
  let db =
    if !unix then Datalog_unix.Default.setup_handlers db else db
  in
  (* print doc and exit, if asked *)
  if !doc then begin
    let l = List.sort compare (DB.help db) in
    print_endline "interpreted predicates:";
    List.iter (fun s -> print_endline ("  " ^ s)) l;
    exit 0
  end;
  let db = parse_files_into db files in
  let answers = DB.ask_lits ~oc:!oc db tuple goals in
  if !print
  then List.iter
    (fun ans -> Printf.printf "  %a.\n" Logic.T.pp ans)
    answers

(** Options *)

let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-debug", Arg.Unit (fun () -> Logic.set_debug true), "enable debug"
  ; "-load", Arg.String add_file, "load given file"
  ; "-oc", Arg.Set oc, "enable occur-check in unification"
  ; "-builtin", Arg.Set builtin, "enable some builtin predicates"
  ; "-quiet", Arg.Clear print, "do not print answer tuples"
  ; "-unix", Arg.Unit (fun () -> unix := true; builtin:= true),
      "enable unix predicates (and builtin)"
  ; "-doc", Arg.Set doc, "print interpreted predicates documentation and exit"
  ]

let help = "topDownCli [options] goal: evaluates goal"
let goal = ref ""

let _ =
  Arg.parse options (fun s -> goal := s) help;
  if !goal = ""
    then failwith "require a goal";
  (* parse goal literals *)
  let tuple, goals = Parser.parse_query Lexer.token (Lexing.from_string !goal) in
  let ctx = Base.DefaultParse.create_ctx () in
  let tuple = List.map (Base.DefaultParse.term_of_ast ~ctx) tuple in
  let goals = List.map (Base.DefaultParse.lit_of_ast ~ctx) goals in
  eval_query !files tuple goals 
