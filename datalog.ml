(** The main datalog file. It provides a CLI tool to parse rule/fact files and compute
    their fixpoint *)

module Logic = Logic

(** Parse file and returns the rules *)
let parse_file filename =
  Format.printf "parse file %s@." filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let rules = Parser.parse_file Lexer.token lexbuf in
  close_in ic;
  rules

let pp_progress i total =
  Format.printf "\r%% rule %-5d / %-5d  " i total;
  Format.print_flush ()

(** Compute fixpoint of rules *)
let process_rules rules =
  Format.printf "process the following %d rules:@." (List.length rules);
  List.iter (Format.printf "  rule @[<h>%a@]@." (Logic.pp_rule ?to_s:None)) rules;
  Format.printf "@.computing fixpoint...@.";
  let db = Logic.db_create () in
  (* add rules one by one *)
  let total = List.length rules in
  ignore (List.fold_left (fun i rule -> pp_progress i total; Logic.db_add db rule; i+1) 1 rules);
  Format.printf "done.@.";
  (* print fixpoint of set after application of rules *)
  Logic.db_fold (fun () rule ->
    if Logic.is_fact rule then Format.printf "  @[<h>%a@]@." (Logic.pp_rule ?to_s:None) rule) () db

let () =
  Format.printf "start datalog@.";
  match Sys.argv with
  | [|_; f|] ->
    let rules = parse_file f in
    process_rules rules
  | _ -> ()
