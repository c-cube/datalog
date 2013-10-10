
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

module D = Datalog.TopDown.Default

(** Evaluate query *)

let parse_files_into db files =
  List.iter
    (fun file ->
      let ic = open_in file in
      let lexbuf = Lexing.from_channel ic in
      try
        let ast = TopDownParser.parse_file TopDownLexer.token lexbuf in
        close_in ic;
        let clauses = D.clauses_of_ast ast in
        D.DB.add_clauses db clauses
      with Parsing.Parse_error ->
        close_in ic;
        TopDownAst.print_error "parse error" lexbuf;
        ())
    files

let eval_query files goal =
  let db = D.DB.create () in
  parse_files_into db files;
  let query = D.Query.ask db goal in
  List.iter
    (fun ans -> Printf.printf "  %a\n" D.T.pp ans)
    (D.Query.answers query)

(** Options *)

let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-debug", Arg.Unit (fun () -> D.set_debug true), "enable debug"
  ; "-load", Arg.String add_file, "load given file"
  ]

let help = "topDownCli [options] goal: evaluates goal"
let goal = ref ""

let _ =
  Arg.parse options (fun s -> goal := s) help;
  if !goal = ""
    then failwith "require a goal";
  let goal = TopDownParser.parse_term TopDownLexer.token (Lexing.from_string !goal) in
  let goal = D.term_of_ast ~ctx:(D.create_ctx ()) goal in
  eval_query !files goal
