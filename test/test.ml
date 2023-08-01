open OUnit2
open Checkers.Board
open Checkers.Move
open Checkers.Game_state
open Checkers.Utility

let o = None
let h = Some { player = P1; is_king = false }
let j = Some { player = P2; is_king = false }
let k = Some { player = P1; is_king = true }

let assert_any_exn msg thunk =
  match thunk () with exception _ -> () | _ -> assert_failure msg

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
         ( "piece set p1" >:: fun _ ->
           let lst =
             [
               (*0  1  2  3  4  5  6  7*)
               [ h; o; o; o; o; o; o; k ] (* 0 *);
               [ o; o; o; o; o; o; o; o ] (* 1 *);
               [ o; o; o; o; o; o; o; o ] (* 2 *);
               [ o; o; o; j; j; o; o; o ] (* 3 *);
               [ o; o; o; j; j; o; o; o ] (* 4 *);
               [ o; o; o; o; o; o; o; o ] (* 5 *);
               [ o; o; o; o; o; o; o; o ] (* 6 *);
               [ k; o; o; o; o; o; o; h ] (* 7 *);
             ]
           in
           assert_equal
             (CoordSet.of_list [ (0, 0); (0, 7); (7, 0); (7, 7) ])
             (find_pieces (of_2d_list lst) P1) );
         ( "piece set p2" >:: fun _ ->
           let lst =
             [
               (*0  1  2  3  4  5  6  7*)
               [ h; o; o; o; o; o; o; k ] (* 0 *);
               [ o; o; o; o; o; o; o; o ] (* 1 *);
               [ o; o; o; o; o; o; o; o ] (* 2 *);
               [ o; o; o; j; j; o; o; o ] (* 3 *);
               [ o; o; o; j; j; o; o; o ] (* 4 *);
               [ o; o; o; o; o; o; o; o ] (* 5 *);
               [ o; o; o; o; o; o; o; o ] (* 6 *);
               [ k; o; o; o; o; o; o; h ] (* 7 *);
             ]
           in
           assert_equal
             (CoordSet.of_list [ (3, 3); (3, 4); (4, 3); (4, 4) ])
             (find_pieces (of_2d_list lst) P2) );
         ( "immutability basic 1" >:: fun _ ->
           let a, b = (blank, blank) in
           let a1, b1 = (put a 4 1 h, put b 4 1 h) in
           assert_equal a1 b1 ~printer:string_of_board );
         ( "immutability basic 2" >:: fun _ ->
           let a = put blank 5 3 h in
           let _ = put a 5 3 o in
           assert_equal h (get a 5 3) );
         ( "immutability basic 3" >:: fun _ ->
           let a = put blank 2 6 h in
           let a1, a2 = (put a 5 3 h, put a 5 3 h) in
           assert_equal a1 a2 ~printer:string_of_board );
         ( "oob get" >:: fun _ ->
           assert_any_exn "get did not fail on oob" (fun () -> get blank 8 0) );
         ( "oob put" >:: fun _ ->
           assert_any_exn "put did not fail on oob" (fun () ->
               put blank 5 (-1) h) );
         ( "string of square" >:: fun _ ->
           assert_equal "K2"
             (string_of_square (Some { player = P2; is_king = true })) );
         ( "string of blank square" >:: fun _ ->
           assert_equal "--" (string_of_square None) );
       ]

(*
    Ruleset: https://www.se.rit.edu/~swen-261/projects/WebCheckers/American%20Rules.html

    Illegal Simple Moves:
    1 Tried to move opponent's piece
    2 Tried to move blank space
    3 Tried to move Normal piece backwards
    4 Tried to move onto occupied space

    Illegal Jump Moves:
    7 Tried to take simple move when jump was an option
    8 Tried to end jump early when another capture existed
    9 Tried to jump backwards with normal piece
    10 tried to capture piece on edge/corner
    11 Tried to jump without capture

    legal edge case moves:
    1 Took a single jump when a multi jump was available
    2 two possible jump options after single jump should both be available
    3 "diamond" move where you can land on the same square via jumping (U, R) (U, L) or (U, L) (U, R)
    4 king should be able to jump backwards
    5 king should be able to simple move backwards
    6 king "cycle" case
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
        [ o; o; o; o; h; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_capture _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; o; j; o; j; j ] (* 2 *);
        [ o; o; o; o; o; o; h; o ] (* 3 *);
        [ o; o; o; o; j; o; o; o ] (* 4 *);
        [ o; o; o; h; o; o; o; o ] (* 5 *);
        [ o; o; j; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_single_and_multijump _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; o; o; j; o; o ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; j; o; j; o; o ] (* 4 *);
        [ o; o; o; o; o; o; o; o ] (* 5 *);
        [ o; j; o; j; o; o; o; o ] (* 6 *);
        [ o; o; h; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_diamond _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; o; o; o; o; o ] (* 2 *);
        [ o; o; j; o; j; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; j; o; j; o; o; o ] (* 5 *);
        [ o; o; o; h; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_king_no_capture _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; k; o; o; o; o ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; o; o; j; o; o; o ] (* 5 *);
        [ o; o; h; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_king_capture _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; k; o; o; o; o ] (* 2 *);
        [ o; o; j; o; j; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; o; o; j; o; o; o ] (* 5 *);
        [ o; o; o; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_king_cycle _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; k; o; o; o; o ] (* 2 *);
        [ o; o; j; o; j; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; j; o; j; o; o; o ] (* 5 *);
        [ o; o; o; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_king_edge _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; j; o; j; o; j; o; o ] (* 1 *);
        [ h; o; k; o; o; o; o; o ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; o ] (* 4 *);
        [ o; o; o; o; o; o; o; o ] (* 5 *);
        [ o; o; o; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1

let setup_mid_jump _test_ctxt =
  let b =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; o; o; o; o; o; o; o ] (* 0 *);
        [ o; o; o; o; o; o; o; o ] (* 1 *);
        [ o; o; o; o; o; o; o; o ] (* 2 *);
        [ o; o; o; o; j; j; o; o ] (* 3 *);
        [ o; o; o; h; o; o; h; o ] (* 4 *);
        [ o; o; o; o; o; o; o; o ] (* 5 *);
        [ o; o; o; o; o; o; o; o ] (* 6 *);
        [ o; o; o; o; o; o; o; o ] (* 7 *);
      ]
  in
  init b P1 ~capturing_piece:(Some (4, 3))

let teardown_noop _ _ = ()

let legal_move_tests =
  "test suite for legal moves"
  >::: [
         (* TODO: rewrite tests (yet again) to check for set of legal moves *)
         ( "legal simple" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "p1 simple move should be legal"
             (is_legal state { r = 5; c = 3; dir = (U, L); is_jump = false }) );
         ( "illegal wrong turn" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "p2 piece cannot be moved on p1 turn"
             (not
                (is_legal state { r = 2; c = 3; dir = (D, R); is_jump = false }))
         );
         ( "illegal no piece" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "cannot move if no piece on square"
             (not
                (is_legal state { r = 4; c = 3; dir = (U, R); is_jump = false }))
         );
         ( "illegal backwards" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "cannot move a normal piece backwards"
             (not
                (is_legal state { r = 5; c = 3; dir = (D, L); is_jump = false }))
         );
         ( "illegal occupied" >:: fun tc ->
           let state = bracket setup_no_capture teardown_noop tc in
           assert_bool "cannot move onto occupied space"
             (not
                (is_legal state { r = 6; c = 4; dir = (U, L); is_jump = false }))
         );
         ( "illegal forced jump 1" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "must take forced jump"
             (not
                (is_legal state { r = 5; c = 3; dir = (U, L); is_jump = false }))
         );
         ( "illegal forced jump 2" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "cannot simple move if forced jump"
             (not
                (is_legal state { r = 3; c = 6; dir = (U, L); is_jump = false }))
         );
         ( "legal jump - first in multiple" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "must take continued forced jump"
             (is_legal state { r = 5; c = 3; dir = (U, R); is_jump = true }) );
         ( "illegal backwards jump" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "normal piece cannot jump backwards"
             (not
                (is_legal state { r = 5; c = 3; dir = (D, L); is_jump = true }))
         );
         ( "illegal oob jump" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "cannot capture if jump goes oob"
             (not
                (is_legal state { r = 3; c = 6; dir = (U, R); is_jump = true }))
         );
         ( "legal branching multijump 1" >:: fun tc ->
           let state = bracket setup_single_and_multijump teardown_noop tc in
           assert_bool "legal to choose single jump over multijump"
             (is_legal state { r = 7; c = 2; dir = (U, L); is_jump = true }) );
         ( "illegal no capture" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_bool "cannot jump without capture"
             (not
                (is_legal state { r = 5; c = 3; dir = (U, L); is_jump = true }))
         );
         ( "legal king simple" >:: fun tc ->
           let state = bracket setup_king_no_capture teardown_noop tc in
           assert_bool "king can move all 4 ways"
             (is_legal state { r = 2; c = 3; dir = (D, R); is_jump = false }
             && is_legal state { r = 2; c = 3; dir = (U, R); is_jump = false }
             && is_legal state { r = 2; c = 3; dir = (D, L); is_jump = false }
             && is_legal state { r = 2; c = 3; dir = (U, L); is_jump = false })
         );
         ( "legal king single jump" >:: fun tc ->
           let state = bracket setup_king_capture teardown_noop tc in
           assert_bool "king can single jump backwards"
             (is_legal state { r = 2; c = 3; dir = (D, L); is_jump = true }) );
         ( "mid jump: legal to keep jumping" >:: fun tc ->
           let state = bracket setup_mid_jump teardown_noop tc in
           assert_bool "can keep moving piece mid-jump"
             (is_legal state { r = 4; c = 3; dir = (U, R); is_jump = true }) );
         ( "mid jump: illegal to move another piece" >:: fun tc ->
           let state = bracket setup_mid_jump teardown_noop tc in
           assert_bool "cannot move piece that is not currently jumping"
             (not
                (is_legal state { r = 4; c = 6; dir = (U, R); is_jump = false }))
         );
         ( "mid jump: illegal to jump another piece" >:: fun tc ->
           let state = bracket setup_mid_jump teardown_noop tc in
           assert_bool "cannot jump piece that is not currently jumping"
             (not
                (is_legal state { r = 4; c = 6; dir = (U, L); is_jump = true }))
         );
         ( "mid jump: illegal to take simple move" >:: fun tc ->
           let state = bracket setup_mid_jump teardown_noop tc in
           assert_bool "cannot simple move after jumping"
             (not
                (is_legal state { r = 4; c = 3; dir = (U, L); is_jump = false }))
         );
       ]

let make_moves state moves = List.fold_left make_move state moves

let make_move_tests =
  "test suite for make move"
  >::: [
         ( "illegal simple" >:: fun tc ->
           let state = bracket setup_king_no_capture teardown_noop tc in
           assert_raises IllegalMove (fun () ->
               make_move state { r = 6; c = 2; dir = (D, R); is_jump = false })
         );
         ( "illegal jump" >:: fun tc ->
           let state = bracket setup_king_no_capture teardown_noop tc in
           assert_raises IllegalMove (fun () ->
               make_move state { r = 6; c = 2; dir = (D, L); is_jump = true })
         );
         ( "simple 1" >:: fun tc ->
           let state = bracket setup_king_no_capture teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; o; o; o ] (* 2 *);
                    [ o; o; k; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; h; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; j; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:3)
             (make_moves state
                [
                  { r = 6; c = 2; dir = (U, L); is_jump = false };
                  { r = 5; c = 4; dir = (D, R); is_jump = false };
                  { r = 2; c = 3; dir = (D, L); is_jump = false };
                ]) );
         ( "mid jump 1" >:: fun tc ->
           let state = bracket setup_single_and_multijump teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; j; o; o ] (* 2 *);
                    [ o; o; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; j; o; j; o; o ] (* 4 *);
                    [ o; o; o; o; h; o; o; o ] (* 5 *);
                    [ o; j; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P1 ~capturing_piece:(Some (5, 4)) ~turn_count:1)
             (make_moves state
                [ { r = 7; c = 2; dir = (U, R); is_jump = true } ]) );
         ( "mid jump 2" >:: fun tc ->
           let state = bracket setup_single_and_multijump teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; j; o; o ] (* 2 *);
                    [ o; o; o; o; o; o; h; o ] (* 3 *);
                    [ o; o; o; j; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; j; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P1 ~capturing_piece:(Some (3, 6)) ~turn_count:2)
             (make_moves state
                [
                  { r = 7; c = 2; dir = (U, R); is_jump = true };
                  { r = 5; c = 4; dir = (U, R); is_jump = true };
                ]) );
         ( "multijump 1" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; h; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; o; j; j ] (* 2 *);
                    [ o; o; o; o; o; o; h; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; j; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 5; c = 3; dir = (U, R); is_jump = true };
                  { r = 3; c = 5; dir = (U, L); is_jump = true };
                ]) );
         ( "multijump 2" >:: fun tc ->
           let state = bracket setup_capture teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; h ] (* 1 *);
                    [ o; o; o; o; j; o; o; j ] (* 2 *);
                    [ o; o; o; o; o; o; h; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; j; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 5; c = 3; dir = (U, R); is_jump = true };
                  { r = 3; c = 5; dir = (U, R); is_jump = true };
                ]) );
         ( "branching multijump 2" >:: fun tc ->
           let state = bracket setup_single_and_multijump teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; j; o; o ] (* 2 *);
                    [ o; o; h; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; j; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; j; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 7; c = 2; dir = (U, R); is_jump = true };
                  { r = 5; c = 4; dir = (U, L); is_jump = true };
                ]) );
         ( "branching multijump 2" >:: fun tc ->
           let state = bracket setup_single_and_multijump teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; h; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; o; o; o ] (* 2 *);
                    [ o; o; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; j; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; j; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:3)
             (make_moves state
                [
                  { r = 7; c = 2; dir = (U, R); is_jump = true };
                  { r = 5; c = 4; dir = (U, R); is_jump = true };
                  { r = 3; c = 6; dir = (U, L); is_jump = true };
                ]) );
         ( "diamond left path" >:: fun tc ->
           let state = bracket setup_diamond teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; h; o; o; o; o ] (* 2 *);
                    [ o; o; o; o; j; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; j; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 6; c = 3; dir = (U, L); is_jump = true };
                  { r = 4; c = 1; dir = (U, R); is_jump = true };
                ]) );
         ( "diamond right path" >:: fun tc ->
           let state = bracket setup_diamond teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; h; o; o; o; o ] (* 2 *);
                    [ o; o; j; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; j; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 6; c = 3; dir = (U, R); is_jump = true };
                  { r = 4; c = 5; dir = (U, L); is_jump = true };
                ]) );
         ( "king multi jump" >:: fun tc ->
           let state = bracket setup_king_capture teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; o; o; o; o; o ] (* 2 *);
                    [ o; o; j; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; k; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 2; c = 3; dir = (D, R); is_jump = true };
                  { r = 4; c = 5; dir = (D, L); is_jump = true };
                ]) );
         ( "king cycle illegal" >:: fun tc ->
           let state = bracket setup_king_cycle teardown_noop tc in
           assert_raises IllegalMove (fun () ->
               make_moves state
                 [
                   { r = 2; c = 3; dir = (D, R); is_jump = true };
                   { r = 4; c = 5; dir = (D, L); is_jump = true };
                   { r = 6; c = 3; dir = (U, L); is_jump = true };
                   { r = 4; c = 1; dir = (U, R); is_jump = true };
                   { r = 2; c = 3; dir = (D, R); is_jump = true };
                 ]) );
         ( "king cycle legal" >:: fun tc ->
           let state = bracket setup_king_cycle teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; o; o; o ] (* 1 *);
                    [ o; o; o; k; o; o; o; o ] (* 2 *);
                    [ o; o; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:4)
             (make_moves state
                [
                  { r = 2; c = 3; dir = (D, R); is_jump = true };
                  { r = 4; c = 5; dir = (D, L); is_jump = true };
                  { r = 6; c = 3; dir = (U, L); is_jump = true };
                  { r = 4; c = 1; dir = (U, R); is_jump = true };
                ]) );
         ( "promotion" >:: fun tc ->
           let state = bracket setup_king_edge teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; k; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; j; o; j; o; o ] (* 1 *);
                    [ o; o; k; o; o; o; o; o ] (* 2 *);
                    [ o; o; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:1)
             (make_moves state
                [ { r = 2; c = 0; dir = (U, R); is_jump = true } ]) );
         ( "promotion ends turn 1" >:: fun tc ->
           let state = bracket setup_king_edge teardown_noop tc in
           assert_raises IllegalMove (fun () ->
               make_moves state
                 [
                   { r = 2; c = 0; dir = (U, R); is_jump = true };
                   { r = 0; c = 2; dir = (D, R); is_jump = true };
                 ]) );
         ( "promotion ends turn 2" >:: fun tc ->
           let state = bracket setup_king_edge teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; k; o; o; o; o; o ] (* 0 *);
                    [ o; o; o; o; o; j; o; o ] (* 1 *);
                    [ o; o; o; o; o; o; o; o ] (* 2 *);
                    [ o; j; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P1 ~turn_count:2)
             (make_moves state
                [
                  { r = 2; c = 0; dir = (U, R); is_jump = true };
                  { r = 1; c = 3; dir = (D, L); is_jump = true };
                ]) );
         ( "king does not promote" >:: fun tc ->
           let state = bracket setup_king_edge teardown_noop tc in
           assert_equal ~printer:string_of_state
             (let b =
                of_2d_list
                  [
                    (*0  1  2  3  4  5  6  7*)
                    [ o; o; o; o; o; o; o; o ] (* 0 *);
                    [ o; j; o; o; o; o; o; o ] (* 1 *);
                    [ h; o; o; o; o; o; k; o ] (* 2 *);
                    [ o; o; o; o; o; o; o; o ] (* 3 *);
                    [ o; o; o; o; o; o; o; o ] (* 4 *);
                    [ o; o; o; o; o; o; o; o ] (* 5 *);
                    [ o; o; o; o; o; o; o; o ] (* 6 *);
                    [ o; o; o; o; o; o; o; o ] (* 7 *);
                  ]
              in
              init b P2 ~turn_count:2)
             (make_moves state
                [
                  { r = 2; c = 2; dir = (U, R); is_jump = true };
                  { r = 0; c = 4; dir = (D, R); is_jump = true };
                ]) );
       ]

let _ = run_test_tt_main board_tests
let _ = run_test_tt_main legal_move_tests
let _ = run_test_tt_main make_move_tests
