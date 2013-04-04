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

(** The main datalog file. It provides a CLI tool to parse clause/fact files and compute
    their fixpoint *)

module DLogic = Logic.Default
module DB = DB.Make(DLogic)
module DParser = Parser
module DLexer = Lexer

let progress = ref false
let print_input = ref false
let print_result = ref false
let print_saturated = ref false
let print_size = ref false
let sums = ref []
let patterns = (ref [] : DLogic.literal list ref)
let goals = (ref [] : DLogic.literal list ref)
let explains = ref []
let files = ref []

(** Parse file and returns the clauses *)
let parse_file filename =
  Format.printf "%% parse file %s@." filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let clauses = DParser.parse_file DLexer.token lexbuf in
    close_in ic;
    clauses
  with Parsing.Parse_error ->
    (* error, signal it and return no clause *)
    Format.eprintf "%% error parsing %s (%s)@." filename (Utils.print_location ());
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

(** Basic handler for a few goals (interprets 'lt', 'equal'...) *)
let handler result =
  let open DLogic.T in
  let compare_atoms a b =
    try let a = int_of_string a and b = int_of_string b in
        a - b
    with Invalid_argument _ -> String.compare a b
  in
  match result with
  | DB.NewFact _ -> []
  | DB.NewClause _ -> []
  | DB.NewGoal lit ->
    begin match lit.term with 
    | Apply ({term=Const "lt"}, [|{term=Const a}; {term=Const b}|])
      when compare_atoms a b < 0 ->
      [DB.AddFact (lit, DB.Axiom)]  (* literal is true *)
    | Apply ({term=Const "le"}, [|{term=Const a}; {term=Const b}|])
      when compare_atoms a b <= 0 ->
      [DB.AddFact (lit, DB.Axiom)]  (* literal is true *)
    | Apply ({term=Const "equal"}, [|{term=Const a}; {term=Const b}|])
      when compare_atoms a b = 0 ->
      [DB.AddFact (lit, DB.Axiom)]  (* literal is true *)
    | _ -> []
    end

(** Compute fixpoint of clauses *)
let process_clauses clauses =
  Format.printf "%% process %d clauses@." (List.length clauses);
  (if !print_input then
    List.iter (Format.printf "  clause @[<h>%a@]@." DLogic.pp_clause) clauses);
  Format.printf "%% computing fixpoint...@.";
  let db = DB.empty () in
  (* handlers *)
  List.iter (fun (symbol,handler,_) -> DB.add_handler db handler) !sums;
  (* goals *)
  DB.add_handler db handler;
  List.iter (fun goal -> DB.add_goal db goal; ()) !goals;
  (* add clauses one by one *)
  let total = List.length clauses in
  ignore (List.fold_left (fun i clause -> (if !progress then pp_progress i total);
                          DB.add db clause; i+1)
          1 clauses);
  Format.printf "%% done.@.";
  (* print fixpoint of set after application of clauses *)
  (if !print_size then
    Format.printf "%% size of saturated set: %d@." (DB.size db));
  (if !print_saturated then
    DB.fold (fun () clause ->
      Format.printf "  @[<h>%a@]@." DLogic.pp_clause clause) () db
  else if !print_result then
    DB.fold (fun () clause ->
      if DLogic.is_fact clause then
        Format.printf "  @[<h>%a@]@." DLogic.pp_clause clause) () db);
  (* print aggregates *)
  List.iter (fun (_,_,printer) -> printer ()) !sums;
  (* print patterns *)
  let ctx1 = DLogic.T.mk_context () in
  let ctx2 = DLogic.T.mk_context () in
  List.iter (fun pattern ->
    Format.printf "%% facts matching pattern %a:@." DLogic.T.pp pattern;
    DB.match_with db ctx1 pattern ctx2
      (fun fact -> Format.printf "  @[<h>%a.@]@." DLogic.T.pp fact))
    !patterns;
  (* print explanations *)
  List.iter (fun pattern ->
    DB.match_with db ctx1 pattern ctx2
      (fun fact ->
        (* premises *)
        Format.printf "  premises of @[<h>%a@]: @[<h>" DLogic.T.pp fact;
        let clause, premises = DB.premises db fact in
        List.iter (fun fact' -> Format.printf "%a, " DLogic.T.pp fact') premises;
        Format.printf " with @[<h>%a@]" DLogic.pp_clause clause;
        Format.printf "@]@.";
        (* explanation *)
        let explanation = DB.support db fact in
        Format.printf "  explain @[<h>%a@] by: @[<h>" DLogic.T.pp fact;
        List.iter (fun fact' -> Format.printf " %a" DLogic.T.pp fact') explanation;
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
  let printer () =
    Format.printf "%% number of fact with head %s: %d@." symbol !count in
  let handler result =
    let open DLogic.T in
    match result with
    | DB.NewFact ({term=Apply ({term=Const s}, _)}, _) when s == symbol ->
      incr count;
      []
    | _ -> []
  in
  sums := (symbol, handler, printer) :: !sums

(** Handler that prints facts that match the given [pattern] once the
    set is saturated *)
let add_pattern p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  patterns := literal :: !patterns

(** Handler that add a goal *)
let add_goal p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  goals := literal :: !goals

(** Add the pattern to the list of patterns to explain *)
let add_explain p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  explains := literal :: !explains

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
      ("-size", Arg.Set print_size, "print number of clauses after fixpoint");
    ]
  in
  Arg.parse options (fun f -> files := f :: !files) "compute fixpoint of given files"

let () =
  Format.printf "%% start datalog@.";
  parse_args ();
  let clauses = parse_files () in
  process_clauses clauses
