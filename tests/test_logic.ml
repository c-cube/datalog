
(** Test basic logic module *)

open OUnit

module L = Datalog.Logic.Default

(** Parse a literal from a string *)
let parse_lit str =
  Datalog.Parser.parse_literal Datalog.Lexer.token (Lexing.from_string str)

let are_alpha_equiv lit1 lit2 =
  try
    ignore (L.alpha_equiv (lit1,0) (lit2,1));
    true
  with L.UnifFailure ->
    false

let test_parse_lit () =
  let _ = parse_lit "p(a,b,'c',42,X)" in
  let lit = parse_lit "p(f(X,Y),X)" in
  let lit' = parse_lit "p(f(X42,Y539),X42)" in
  OUnit.assert_bool "alpha_renaming" (are_alpha_equiv lit lit');
  ()

let suite =
  "test_logic" >:::
    [ "test_parse_lit" >:: test_parse_lit;
    ]
