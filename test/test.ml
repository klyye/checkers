open OUnit2
open Checkers.Board

let board_tests =
  let n = None in
  let p = Some (Normal P1) in
  "test suite for board"
  >::: [
         ("board is 8x8" >:: fun _ -> assert_equal 8 size);
         ( "to list length is 64" >:: fun _ ->
           assert_equal 64 (List.length (List.concat (to_2d_list blank))) );
         ( "blank board only contains None" >:: fun _ ->
           assert_bool "blank board contains non-None"
             (List.for_all (fun x -> x = None) (List.concat (to_2d_list blank)))
         );
         ( "put and get basic 1" >:: fun _ ->
           let b = put blank 4 7 p in
           assert_equal p (get b 4 7) ~printer:string_of_square );
         ( "put and get basic 2" >:: fun _ ->
           let b = put blank 7 2 p in
           assert_equal n (get b 4 7) ~printer:string_of_square );
         ( "put corners" >:: fun _ ->
           let b = blank in
           let b1 = put (put (put (put b 0 0 p) 0 7 p) 7 0 p) 7 7 p in
           let lst =
             [
               [ p; n; n; n; n; n; n; p ];
               [ n; n; n; n; n; n; n; n ];
               [ n; n; n; n; n; n; n; n ];
               [ n; n; n; n; n; n; n; n ];
               [ n; n; n; n; n; n; n; n ];
               [ n; n; n; n; n; n; n; n ];
               [ n; n; n; n; n; n; n; n ];
               [ p; n; n; n; n; n; n; p ];
             ]
           in
           assert_equal lst (to_2d_list b1) );
         ( "of and to 2d list" >:: fun _ ->
           let q = Some (King P2) in
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
         ( "immutability basic 1" >:: fun _ ->
           let a, b = (blank, blank) in
           let a1, b1 = (put a 4 1 p, put b 4 1 p) in
           assert_equal a1 b1 );
         ( "immutability basic 2" >:: fun _ ->
           let a = put blank 5 3 p in
           let _ = put a 5 3 n in
           assert_equal p (get a 5 3) );
         ( "immutability basic 3" >:: fun _ ->
           let a = put blank 2 6 p in
           let a1, a2 = (put a 5 3 p, put a 5 3 p) in
           assert_equal a1 a2 ~printer:string_of_board );
       ]

let _ = run_test_tt_main board_tests
