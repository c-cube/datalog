(** The main datalog file. It provides a CLI tool to parse rule/fact files and compute
    their fixpoint *)

module Logic = Logic

let progress = ref false
let print_input = ref false
let print_result = ref false
let files = ref []

(** Parse file and returns the rules *)
let parse_file filename =
  Format.printf "%% parse file %s@." filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let rules = Parser.parse_file Lexer.token lexbuf in
  close_in ic;
  rules

(** Parse files *)
let parse_files () =
  List.fold_left
    (fun rules file ->
      List.rev_append (parse_file file) rules)
    [] !files

let pp_progress i total =
  Format.printf "\r%% rule %-5d / %-5d  " i total;
  Format.print_flush ()

(** Compute fixpoint of rules *)
let process_rules rules =
  Format.printf "%% process %d rules@." (List.length rules);
  (if !print_input then
    List.iter (Format.printf "  rule @[<h>%a@]@." (Logic.pp_rule ?to_s:None)) rules);
  Format.printf "%% computing fixpoint...@.";
  let db = Logic.db_create () in
  (* add rules one by one *)
  let total = List.length rules in
  ignore (List.fold_left (fun i rule -> (if !progress then pp_progress i total);
                          Logic.db_add db rule; i+1)
          1 rules);
  Format.printf "%% done.@.";
  (* print fixpoint of set after application of rules *)
  if !print_result then 
    Logic.db_fold (fun () rule ->
      if Logic.is_fact rule then
        Format.printf "  @[<h>%a@]@." (Logic.pp_rule ?to_s:None) rule) () db

(** parse CLI arguments *)
let parse_args () =
  let options =
    [ ("-progress", Arg.Set progress, "print progress");
      ("-input", Arg.Set print_input, "print input rules");
      ("-output", Arg.Set print_result, "print rules after fixpoint");
    ]
  in
  Arg.parse options (fun f -> files := f :: !files) "compute fixpoint of given files"

let () =
  Format.printf "%% start datalog@.";
  parse_args ();
  let rules = parse_files () in
  process_rules rules
