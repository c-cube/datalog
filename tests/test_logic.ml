
(** Test basic logic module *)

open OUnit

module L = Datalog.Logic.Default

(** Parse a literal from a string *)
let parse_lit str =
  Datalog.Parser.parse_literal Datalog.Lexer.token (Lexing.from_string str)

let parse_clause str =
  Datalog.Parser.parse_clause Datalog.Lexer.token (Lexing.from_string str)

let are_alpha_equiv lit1 lit2 =
  try
    ignore (L.alpha_equiv (lit1,0) (lit2,1));
    true
  with L.UnifFailure ->
    false

let are_unifiable lit1 lit2 =
  try
    ignore (L.unify (lit1,0) (lit2,1));
    true
  with L.UnifFailure ->
    false

let test_parse_lit () =
  let _ = parse_lit "p(a,b,'c',42,X)" in
  let lit = parse_lit "p(f(X,Y),X)" in
  let lit' = parse_lit "p(f(X42,Y539),X42)" in
  OUnit.assert_bool "alpha_renaming" (are_alpha_equiv lit lit');
  ()

let test_hashconsing () =
  let lit1 = parse_lit "p(X,Y,Z)" in
  let lit2 = parse_lit "p(X,Y,Z)" in
  OUnit.assert_equal ~cmp:(==) lit1 lit2;
  let lit3 = L.mk_apply "foo" [lit1; lit1] in
  let lit4 = L.mk_apply "foo" [lit2; lit2] in
  OUnit.assert_equal ~cmp:(==) lit3 lit4;
  OUnit.assert_bool "not_eq" (lit1 != lit3);
  ()

let test_ground () =
  OUnit.assert_bool "not ground" (not (L.is_ground (parse_lit "p(X,a,b)")));
  OUnit.assert_bool "ground" (L.is_ground (parse_lit "p(a,b,'c',42,d)"));
  OUnit.assert_bool "ground" (L.is_ground (parse_lit "q(r(p(a,b),c),d)"));
  ()

let test_arity () =
  OUnit.assert_equal 3 (L.arity (parse_lit "p(a,b,c)"));
  OUnit.assert_equal 1 (L.arity (parse_lit "p(a)"));
  OUnit.assert_equal 0 (L.arity (parse_lit "X"));
  ()

let test_safe () =
  let c = parse_clause "p(a,b,c)." in
  OUnit.assert_bool "safe" (L.check_safe c);
  OUnit.assert_bool "fact" (L.is_fact c);
  let c' = parse_clause "r(X,Y) :- r(X,Z), rr(Z,Y)." in
  OUnit.assert_bool "safe" (L.check_safe c');
  OUnit.assert_bool "not fact" (not (L.is_fact (parse_clause "p(X,Y).")));
  let c'' = parse_clause "r(X,Y,Z) :- p(X), q(Y), fail(a)." in
  OUnit.assert_bool "not safe" (not (L.check_safe c''));
  ()

let test_unif () =
  let lit1 = parse_lit "p(X,b,c)" in
  let lit2 = parse_lit "p(a,Y,Z)" in
  OUnit.assert_bool "unif" (are_unifiable lit1 lit2);
  let lit1 = parse_lit "p(X,X,b)" in
  let lit2 = parse_lit "p(a,Y,Y)" in
  OUnit.assert_raises L.UnifFailure (fun () -> L.unify (lit1,0) (lit2,0));
  ()

let suite =
  "test_logic" >:::
    [ "test_parse_lit" >:: test_parse_lit;
      "test_hashconsing" >:: test_hashconsing;
      "test_ground" >:: test_ground;
      "test_arity" >:: test_arity;
      "test_safe" >:: test_safe;
      "test_unif" >:: test_unif;
    ]
