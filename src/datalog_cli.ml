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

(** The main datalog file. It provides a CLI tool to parse clause/fact files
    and compute their fixpoint *)

open Datalog

module DLogic = Base.Default
module DB = BottomUp.Default

module T = DLogic.T
module C = DLogic.C
module Cst = Base

let progress = ref false
let print_input = ref false
let print_result = ref false
let print_saturated = ref false
let print_size = ref false
let print_version = ref false
let sums = ref []
let patterns = (ref [] : T.t list ref)
let goals = (ref [] : T.t list ref)
let explains = ref []
let files = ref []
(* TODO: update
let queries = ref []
*)

(** Parse file and returns the clauses *)
let parse_file filename =
  Format.printf "%% parse file %s@." filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let clauses = Parser.parse_file Lexer.token lexbuf in
    close_in ic;
    clauses
  with Parsing.Parse_error ->
    (* error, signal it and return no clause *)
    Format.eprintf "%% error parsing %s (%s)@." filename
      (Lexer.print_location lexbuf);
    []

(** Parse files *)
let parse_files () =
  let clauses = List.fold_left
    (fun clauses file ->
      List.rev_append (parse_file file) clauses)
    [] !files
  in List.rev clauses

let pp_progress i total =
  Format.printf "\r%% clause %-5d / %-5d  " i total;
  Format.print_flush ()

(** Simple goal handler (interprets 'lt') *)
let handle_goal db g =
  (* debug: Format.printf "%% goal %a@." DLogic.pp_literal lit; *)
  let compare a b =
    match a, b with
    | T.Apply (Cst.String a, [| |]), T.Apply (Cst.String b, [| |]) ->
        Some (String.compare a b)
    | T.Apply (Cst.Int a, [| |]), T.Apply (Cst.Int b, [| |]) ->
        Some (a-b)
    | _ -> None
  in
  match g with
  | T.Apply (Cst.String "lt", [| a; b |]) ->
      begin match compare a b with
      | Some n when n < 0 ->
        db := DB.add_fact !db g (* literal is true *)
      | _ -> ()
      end
  | T.Apply (Cst.String "le", [| a; b |]) ->
      begin match compare a b with
      | Some n when n <= 0 ->
        db := DB.add_fact !db g (* literal is true *)
      | _ -> ()
      end
  | T.Apply (Cst.String "equal", [| a; b |])
    when compare a b = Some 0 ->
    db := DB.add_fact !db g  (* literal is true *)
  | _ -> ()

(** Compute fixpoint of clauses *)
let process_clauses clauses =
  Format.printf "%% process %d clauses@." (List.length clauses);
  if !print_input then
    List.iter (fun c -> Printf.printf "  clause %a\n" C.pp c) clauses;
  Format.printf "%% computing fixpoint...@.";
  let db = ref (DB.create ()) in
  (* handlers *)
  List.iter (fun (symbol,handler,_) -> DB.subscribe_fact !db symbol handler) !sums;
  (* goals *)
  DB.subscribe_goal !db (handle_goal db);
  List.iter (fun goal -> db := DB.goal !db goal) !goals;
  (* add clauses one by one *)
  let total = List.length clauses in
  ignore (List.fold_left
    (fun i clause ->
      if !progress then pp_progress i total;
      db := DB.add !db clause;
      i+1)
    1 clauses);
  Format.printf "%% done.@.";
  (* print fixpoint of set after application of clauses *)
  if !print_size then
    Format.printf "%% size of saturated set: %d@." (DB.size !db);
  if !print_saturated then
    DB.fold (fun () clause ->
      Printf.printf "  %a\n" C.pp clause) () !db
  else if !print_result then
    DB.fold (fun () clause ->
      if C.is_fact clause then
        Format.printf "  @[<h>%a@]@." C.fmt clause) () !db;
  (* print aggregates *)
  List.iter (fun (_,_,printer) -> printer ()) !sums;
  (* print patterns *)
  List.iter (fun pattern ->
    Format.printf "%% facts matching pattern %a:@." T.fmt pattern;
    DB.match_ !db pattern
      (fun fact -> Format.printf "  @[<h>%a.@]@." C.fmt fact))
    !patterns;
  (* TODO: update
  (* run queries *)
  List.iter (fun (vars, lits, neg) ->
    let set = DLogic.Query.ask ~neg db vars lits in
    let l = DLogic.Query.to_list set in
    Format.printf "%% query plan: @[<h>%a@]@." DLogic.Query.pp_plan set;
    Format.printf "%% @[<v2>query answer:@ ";
    List.iter
      (fun terms ->
        Array.iteri
          (fun i t ->
             (if i > 0 then Format.printf ", %a" else Format.printf "%a") DLogic.pp_term t)
          terms;
        Format.printf "@;")
      l;
    Format.printf "@]@.")
  !queries;
  *)
  (* print explanations *)
  List.iter (fun pattern ->
    DB.match_ !db pattern
      (fun fact ->
        (* premises *)
        Format.printf "  premises of @[<h>%a@]: @[<h>" C.fmt fact;
        let clause, premises = DB.premises !db fact.C.head in
        List.iter (fun fact' -> Format.printf "%a, " T.fmt fact') premises;
        Format.printf " with @[<h>%a@]" C.fmt clause;
        Format.printf "@]@.";
        (* explanation *)
        let explanation = DB.explain !db fact.C.head in
        Format.printf "  explain @[<h>%a@] by: @[<h>" C.fmt fact;
        List.iter (fun fact' -> Format.printf " %a" T.fmt fact') explanation;
        Format.printf "@]@."))
    !explains;
  (* print memory usage *)
  let stat = Gc.quick_stat () in
  Format.printf "%% max_heap_size: %d; minor_collections: %d; major collections: %d@."
    stat.Gc.top_heap_words stat.Gc.minor_collections stat.Gc.major_collections;
  ()

(** Handler that aggregates the number of facts with this head symbol. It adds the
    handler to the global variable [sums] *)
let add_sum symbol =
  let count = ref 0 in
  (* print result at exit *)
  let printer () = Format.printf "%% number of fact with head %s: %d@." symbol !count in
  let handler _ = incr count in
  sums := (Cst.String symbol, handler, printer) :: !sums

let ctx = Base.DefaultParse.create_ctx ()

(** Handler that prints facts that match the given [pattern] once the
    set is saturated *)
let add_pattern p =
  let lexbuf = Lexing.from_string p in
  let t = Parser.parse_term Lexer.token lexbuf in
  let t = Base.DefaultParse.term_of_ast ~ctx t in
  patterns := t :: !patterns

(** Handler that add a goal *)
let add_goal p =
  let lexbuf = Lexing.from_string p in
  let t = Parser.parse_term Lexer.token lexbuf in
  let t = Base.DefaultParse.term_of_ast ~ctx t in
  goals := t :: !goals

(** Add the pattern to the list of patterns to explain *)
let add_explain p =
  let lexbuf = Lexing.from_string p in
  let t = Parser.parse_term Lexer.token lexbuf in
  let t = Base.DefaultParse.term_of_ast ~ctx t in
  explains := t :: !explains

(** Add a query to the list of queries to run *)
let add_query q_str = failwith "queries not handled"
(* TODO: update?
  try
    let lexbuf = Lexing.from_string q_str in
    let ast = DParser.parse_query DLexer.token lexbuf in
    let q = DLogic.query_of_ast ast in
    queries := q :: !queries
  with Parsing.Parse_error ->
    failwith ("could not parse query string " ^ q_str)
*)

(** parse CLI arguments *)
let parse_args () =
  let options =
    [ ("-progress", Arg.Set progress, "print progress");
      ("-input", Arg.Set print_input, "print input clauses");
      ("-output", Arg.Set print_result, "print facts after fixpoint");
      ("-saturated", Arg.Set print_saturated, "print facts and clauses after fixpoint");
      ("-sum", Arg.String add_sum, "aggregate number of literals for the given symbol");
      ("-pattern", Arg.String add_pattern, "print facts matching this pattern");
      ("-goal", Arg.String add_goal, "add a goal for backward chaining");
      ("-explain", Arg.String add_explain, "explain facts matching this pattern");
      ("-query", Arg.String add_query, "execute the query once fixpoint is reached");
      ("-size", Arg.Set print_size, "print number of clauses after fixpoint");
      ("-version", Arg.Set print_version, "print version");
    ]
  in
  Arg.parse options (fun f -> files := f :: !files) "compute fixpoint of given files"

let () =
  Format.printf "%% start datalog@.";
  parse_args ();
  (if !print_version then Printf.printf "%% version : %s\n" Datalog.Version.version);
  let clauses = parse_files () in
  let clauses = List.map (Base.DefaultParse.clause_of_ast ~ctx) clauses in
  process_clauses clauses
