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

(** Compute fixpoint of rules *)
let process_rules rules =
  Format.printf "process %d rules@." (List.length rules);
  List.iter (Format.printf "  rule @[<h>%a@]@." (Logic.pp_rule ?to_s:None)) rules

let () =
  Format.printf "start datalog@.";
  match Sys.argv with
  | [|_; f|] ->
    let rules = parse_file f in
    process_rules rules
  | _ -> ()
