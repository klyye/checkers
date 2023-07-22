open OUnit2
open Checkers.Board
open Checkers.Game_state

let o = None
let h = Some (Normal P1)
let j = Some (Normal P2)

let board_tests =
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
           let b = put blank 4 7 h in
           assert_equal h (get b 4 7) ~printer:string_of_square );
         ( "put and get basic 2" >:: fun _ ->
           let b = put blank 7 2 h in
           assert_equal o (get b 4 7) ~printer:string_of_square );
         ( "put corners" >:: fun _ ->
           let b = blank in
           let b1 = put (put (put (put b 0 0 h) 0 7 h) 7 0 h) 7 7 h in
           let lst =
             [
               [ h; o; o; o; o; o; o; h ];
               [ o; o; o; o; o; o; o; o ];
               [ o; o; o; o; o; o; o; o ];
               [ o; o; o; o; o; o; o; o ];
               [ o; o; o; o; o; o; o; o ];
               [ o; o; o; o; o; o; o; o ];
               [ o; o; o; o; o; o; o; o ];
               [ h; o; o; o; o; o; o; h ];
             ]
           in
           assert_equal lst (to_2d_list b1) );
         ( "of and to 2d list" >:: fun _ ->
           let k = Some (King P2) in
           let lst =
             [
               [ o; h; k; o; h; k; o; h ];
               [ h; k; o; h; k; o; h; k ];
               [ k; o; h; k; o; h; k; o ];
               [ o; o; o; o; o; o; o; o ];
               [ k; k; k; k; k; k; k; k ];
               [ h; h; h; h; h; h; h; h ];
               [ o; o; o; o; h; h; h; k ];
               [ h; h; h; h; k; k; o; o ];
             ]
           in
           assert_equal lst (to_2d_list (of_2d_list lst)) );
         ( "immutability basic 1" >:: fun _ ->
           let a, b = (blank, blank) in
           let a1, b1 = (put a 4 1 h, put b 4 1 h) in
           assert_equal a1 b1 );
         ( "immutability basic 2" >:: fun _ ->
           let a = put blank 5 3 h in
           let _ = put a 5 3 o in
           assert_equal h (get a 5 3) );
         ( "immutability basic 3" >:: fun _ ->
           let a = put blank 2 6 h in
           let a1, a2 = (put a 5 3 h, put a 5 3 h) in
           assert_equal a1 a2 ~printer:string_of_board );
         ( "oob get" >:: fun _ ->
           assert_raises (Invalid_argument "index out of bounds") (fun () ->
               get blank 8 0) );
         ( "oob put" >:: fun _ ->
           assert_raises (Invalid_argument "index out of bounds") (fun () ->
               put blank 5 (-1) h) );
       ]

(*
    Ruleset: https://www.se.rit.edu/~swen-261/projects/WebCheckers/American%20Rules.html

    Illegal Moves:
    1 Tried to move opponent's piece
    2 Tried to move blank space
    3 Tried to move Normal piece backwards
    4 Tried to move onto occupied space
    5 Tried to move orthogonally
    6 Tried to move more than 1 space in simple move
    7 Tried to take simple move when jump was an option
    8 Tried to end jump early when another capture existed
    9 Tried to jump backwards with normal piece
    10 tried to capture piece on edge/corner

    legal edge case moves:
    11 Took a single jump when a multi jump was available
    12 two possible jump options after single jump should both be available
  *)

let setup_no_capture _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; j; o; o; o; o ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; o; h; o; o; o; o ] (* 5 *);
        [ o; o; o; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b

let teardown_noop _ _ = ()

let legal_move_tests =
  "test suite for legal moves"
  >::: [
         ( "legal simple" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "p1 simple move should be legal"
             (is_legal state ((5, 3), (4, 2))) );
         ( "illegal 1" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "p2 piece cannot be moved on p1 turn"
             (not (is_legal state ((2, 3), (3, 2)))) );
         ( "illegal 2" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "cannot move if no piece on square"
             (not (is_legal state ((4, 3), (3, 2)))) );
       ]

let _ = run_test_tt_main board_tests
let _ = run_test_tt_main legal_move_tests
