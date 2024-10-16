
(* this file is part of datalog. See README for the license *)

(** The main datalog file. It provides a CLI tool to parse clause/fact files and compute
    their fixpoint *)

module DLogic = Datalog.Default
module DParser = Datalog.Parser
module DLexer = Datalog.Lexer
module DSym = DLogic.StringSymbol

let quiet = ref false
let progress = ref false
let print_input = ref false
let print_result = ref false
let print_saturated = ref false
let print_size = ref false
let print_version = ref false
let sums = ref []
let patterns = (ref [] : DLogic.literal list ref)
let goals = (ref [] : DLogic.literal list ref)
let explains = ref []
let files = ref []
let queries = ref []

(** Parse file and returns the clauses *)
let parse_file filename =
  if not !quiet then Format.printf "%% parse file %s@." filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let clauses = DParser.parse_file DLexer.token lexbuf in
    close_in ic;
    clauses
  with Parsing.Parse_error ->
    (* error, signal it and return no clause *)
    Format.eprintf "%% error parsing %s (%s)@." filename (DLexer.print_location lexbuf);
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
let handle_goal db lit =
  (* debug: Format.printf "%% goal %a@." DLogic.pp_literal lit; *)
  let compare a b =
    try
      let a = int_of_string (DSym.to_string a)
      and b = int_of_string (DSym.to_string b) in
      Stdlib.compare a b
    with Invalid_argument _ -> compare a b
  in
  match (DLogic.open_literal lit :> string * DLogic.term list) with
  | "lt", [DLogic.Const a; DLogic.Const b] when compare a b < 0 ->
    DLogic.db_add_fact db lit (* literal is true *)
  | "le", [DLogic.Const a; DLogic.Const b] when compare a b <= 0 ->
    DLogic.db_add_fact db lit (* literal is true *)
  | "equal", [DLogic.Const a; DLogic.Const b] when a = b ->
    DLogic.db_add_fact db lit (* literal is true *)
  | _ -> ()

(** Compute fixpoint of clauses *)
let process_clauses clauses =
  if not !quiet then Format.printf "%% process %d clauses@." (List.length clauses);
  if !print_input then (
    List.iter (Format.printf "  clause @[<h>%a@]@." DLogic.pp_clause) clauses
  );
  if not !quiet then Format.printf "%% computing fixpoint...@.";
  let db = DLogic.db_create () in
  (* handlers *)
  List.iter (fun (symbol,handler,_) -> DLogic.db_subscribe_fact db symbol handler) !sums;
  (* goals *)
  DLogic.db_subscribe_goal db (handle_goal db);
  List.iter (fun goal -> DLogic.db_goal db goal) !goals;
  (* add clauses one by one *)
  let total = List.length clauses in
  List.iteri
    (fun i clause ->
       if !progress then pp_progress i total;
       DLogic.db_add db clause)
    clauses;
  if not !quiet then Format.printf "%% done.@.";
  (* print fixpoint of set after application of clauses *)
  if !print_size then (
    Format.printf "%% size of saturated set: %d@." (DLogic.db_size db)
  );
  if !print_saturated then (
    DLogic.db_fold (fun () clause ->
        Format.printf "  @[<h>%a@]@." DLogic.pp_clause clause) () db
  ) else if !print_result then (
    DLogic.db_fold (fun () clause ->
      if DLogic.is_fact clause then
        Format.printf "  @[<h>%a@]@." DLogic.pp_clause clause) () db
  );
  (* print aggregates *)
  List.iter (fun (_,_,printer) -> printer ()) !sums;
  (* print patterns *)
  List.iter (fun pattern ->
    Format.printf "%% facts matching pattern %a:@." DLogic.pp_literal pattern;
    DLogic.db_match db pattern
      (fun fact -> Format.printf "  @[<h>%a.@]@." DLogic.pp_literal fact))
    !patterns;
  (* run queries *)
  List.iter (fun (vars, lits, neg) ->
    let set = DLogic.Query.ask ~neg db vars lits in
    let l = DLogic.Query.to_list set in
    if not !quiet then Format.printf "%% query plan: @[<h>%a@]@." DLogic.Query.pp_plan set;
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
  (* print explanations *)
  List.iter (fun pattern ->
    DLogic.db_match db pattern
      (fun fact ->
        (* premises *)
        Format.printf "  premises of @[<h>%a@]: @[<h>" DLogic.pp_literal fact;
        let clause, premises = DLogic.db_premises db fact in
        List.iter (fun fact' -> Format.printf "%a, " DLogic.pp_literal fact') premises;
        Format.printf " with @[<h>%a@]" DLogic.pp_clause clause;
        Format.printf "@]@.";
        (* explanation *)
        let explanation = DLogic.db_explain db fact in
        Format.printf "  explain @[<h>%a@] by: @[<h>" DLogic.pp_literal fact;
        List.iter (fun fact' -> Format.printf " %a" DLogic.pp_literal fact') explanation;
        Format.printf "@]@."))
    !explains;
  (* print memory usage *)
  let stat = Gc.quick_stat () in
  if not !quiet then (
    Format.printf "%% max_heap_size: %d; minor_collections: %d; major collections: %d@."
      stat.Gc.top_heap_words stat.Gc.minor_collections stat.Gc.major_collections;
  );
  ()

(** Handler that aggregates the number of facts with this head symbol. It adds the
    handler to the global variable [sums] *)
let add_sum symbol =
  let count = ref 0 in
  (* print result at exit *)
  let printer () = Format.printf "%% number of fact with head %s: %d@." symbol !count in
  let handler _ = incr count in
  sums := (DSym.make symbol, handler, printer) :: !sums

(** Handler that prints facts that match the given [pattern] once the
    set is saturated *)
let add_pattern p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  let literal = DLogic.literal_of_ast literal in
  patterns := literal :: !patterns

(** Handler that add a goal *)
let add_goal p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  let literal = DLogic.literal_of_ast literal in
  goals := literal :: !goals

(** Add the pattern to the list of patterns to explain *)
let add_explain p =
  let lexbuf = Lexing.from_string p in
  let literal = DParser.parse_literal DLexer.token lexbuf in
  let literal = DLogic.literal_of_ast literal in
  explains := literal :: !explains

(** Add a query to the list of queries to run *)
let add_query q_str =
  try
    let lexbuf = Lexing.from_string q_str in
    let ast = DParser.parse_query DLexer.token lexbuf in
    let q = DLogic.query_of_ast ast in
    queries := q :: !queries
  with Parsing.Parse_error ->
    failwith ("could not parse query string " ^ q_str)

(** parse CLI arguments *)
let parse_args () =
  let options =
    [ ("--progress", Arg.Set progress, " print progress");
      ("-p", Arg.Set progress, " alias to --progress");
      ("--input", Arg.Set print_input, " print input clauses");
      ("-i", Arg.Set print_input, " alias to --input");
      ("--output", Arg.Set print_result, " print facts after fixpoint");
      ("-o", Arg.Set print_result, " alias to --output");
      ("--saturated", Arg.Set print_saturated, " print facts and clauses after fixpoint");
      ("--sum", Arg.String add_sum, " aggregate number of literals for the given symbol");
      ("--pattern", Arg.String add_pattern, " print facts matching this pattern");
      ("--goal", Arg.String add_goal, " add a goal for backward chaining");
      ("--explain", Arg.String add_explain, " explain facts matching this pattern");
      ("--query", Arg.String add_query, " execute the query once fixpoint is reached");
      ("--size", Arg.Set print_size, " print number of clauses after fixpoint");
      ("--version", Arg.Set print_version, " print version");
      ("--quiet", Arg.Set quiet, " quiet");
      ("-q", Arg.Set quiet, " quiet");
    ] |> Arg.align
  in
  Arg.parse options (fun f -> files := f :: !files) "compute fixpoint of given files"

let () =
  parse_args ();
  if not !quiet then Format.printf "%% start datalog@.";
  if !print_version then (
    Printf.printf "%% version : %s\n" Datalog.Version.version;
  );
  let clauses = parse_files () in
  let clauses = List.map DLogic.clause_of_ast clauses in
  process_clauses clauses
