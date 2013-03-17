
open OUnit

let suite = OUnit.TestList
  [ Test_logic.suite;
  ]

let _ =
  OUnit.run_test_tt_main suite
