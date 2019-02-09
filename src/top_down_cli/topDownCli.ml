
(* this file is part of datalog. See README for the license *)

(** {1 Prolog-like command line tool} *)

module D = Datalog_top_down.Default
module DParser = Datalog_top_down.Parser
module DLexer = Datalog_top_down.Lexer
module DAst = Datalog_top_down.AST

(** Options *)

let oc = ref false
let builtin = ref false
let unix = ref false
let doc = ref false
let print = ref true

(** Evaluate query *)

let parse_files_into db files =
  List.iter
    (fun file ->
      match D.parse_file file with
      | `Error e ->
        print_endline e;
        ()
      | `Ok clauses ->
        D.DB.add_clauses db clauses)
    files

let eval_query files tuple goals =
  let db = D.DB.create () in
  if !builtin then D.setup_default db;
  if !unix then Datalog_unix.Default.setup_handlers db;
  (* print doc and exit, if asked *)
  if !doc then begin
    let l = List.sort compare (D.DB.help db) in
    print_endline "interpreted predicates:";
    List.iter (fun s -> print_endline ("  " ^ s)) l;
    exit 0
  end;
  parse_files_into db files;
  let answers = D.ask_lits ~oc:!oc db tuple goals in
  if !print
  then List.iter
    (fun ans -> Printf.printf "  %a.\n" D.T.pp ans)
    answers

(** Options *)

let files = ref []
let add_file f = files := f :: !files

let options =
  [ "-debug", Arg.Unit (fun () -> D.set_debug true), " enable debug"
  ; "-load", Arg.String add_file, " load given file"
  ; "-oc", Arg.Set oc, " enable occur-check in unification"
  ; "-builtin", Arg.Set builtin, " enable some builtin predicates"
  ; "-quiet", Arg.Clear print, " do not print answer tuples"
  ; "-unix", Arg.Unit (fun () -> unix := true; builtin:= true),
      " enable unix predicates (and builtin)"
  ; "-doc", Arg.Set doc, " print interpreted predicates documentation and exit"
  ] |> Arg.align

let help =
"topDownCli [options] goal: evaluates goal

Example: topDownCli -load tests/graph10.pl '(X,Y) :- reachable(X,Y)'
"
let goal = ref ""

let () =
  Arg.parse options (fun s -> goal := s) help;
  if !goal = "" then Arg.usage options help;
  (* parse goal literals *)
  let lexbuf = Lexing.from_string !goal in
  try
    let tuple, goals = DParser.parse_query DLexer.token (Lexing.from_string !goal) in
    let ctx = D.create_ctx () in
    let tuple = List.map (D.term_of_ast ~ctx) tuple in
    let goals = List.map (D.lit_of_ast ~ctx) goals in
    eval_query !files tuple goals
  with Parsing.Parse_error ->
    Printf.eprintf "parse error at %s\n" (DLexer.print_location lexbuf);
    exit 1
