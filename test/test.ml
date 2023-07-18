open OUnit2
open Checkers

let game_state_tests = 
  let open Game_state in 
  "test suite for game state" >::: [
    ( "board is 8x8" >:: fun _ -> assert_equal 8 size)
  ]

let _  = run_test_tt_main game_state_tests