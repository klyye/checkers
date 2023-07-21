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
         ( "of and to 2d list" >:: fun _ ->
           let n = None in
           let p = Some (King P1) in
           let q = Some (Normal P2) in
           let lst =
             [
               [ n; p; q; n; p; q; n; p ];
               [ p; q; n; p; q; n; p; q ];
               [ q; n; p; q; n; p; q; n ];
               [ n; n; n; n; n; n; n; n ];
               [ q; q; q; q; q; q; q; q ];
               [ p; p; p; p; p; p; p; p ];
               [ n; n; n; n; p; p; p; q ];
               [ p; p; p; p; q; q; n; n ];
             ]
           in
           assert_equal lst (to_2d_list (of_2d_list lst)) );
       ]

let _ = run_test_tt_main board_tests
