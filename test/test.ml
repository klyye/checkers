open OUnit2
open Checkers

(* TODO: test of_2d_list*)
let board_tests =
  let open Board in
  "test suite for board"
  >::: [
         ("board is 8x8" >:: fun _ -> assert_equal 8 size);
         ( "to list length is 64" >:: fun _ ->
           assert_equal 64 (List.length (List.concat (to_2d_list blank))) );
         ( "blank board only contains None" >:: fun _ ->
           assert_bool "blank board contains non-None"
             (List.for_all (fun x -> x = None) (List.concat (to_2d_list blank)))
         );
         ( "set and get basic" >:: fun _ ->
           let p = Normal P1 in
           let b = put blank 4 7 p in
           assert_equal (Some p) (get b 4 7) ~printer:string_of_square );
       ]

let _ = run_test_tt_main board_tests
