open OUnit2
open Checkers

let board_tests =
  let open Board in
  "test suite for board"
  >::: [ ("board is 8x8" >:: fun _ -> assert_equal 8 size) ]

let _ = run_test_tt_main board_tests
