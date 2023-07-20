open OUnit2
open Checkers

let board_tests =
  let open Board in
  "test suite for board"
  >::: [
         ("board is 8x8" >:: fun _ -> assert_equal 8 size);
         ( "to list length is 64" >:: fun _ ->
           assert_equal 64 (List.length (to_list blank)) );
       ]

let _ = run_test_tt_main board_tests
