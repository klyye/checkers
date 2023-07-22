(* open Board *)

(* exception IllegalMove *)

type move = (int * int) * (int * int)

type t = {
  board : Board.t;
      (* curr_player : player; *)
      (* TODO: hash map of board states to counts for three move stalemates clause *)
      (* TODO: might be simpler to generate list of legal moves and then just check if user input is contained in that list *)
}

let init board = { board (* curr_player = P1 *) }
let board state = state.board

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
   *)
let is_legal _state _move = true

(* let make_move state move =
   if is_legal state move then
     let src, dest = move in
     let r0, c0 = src in
     let r1, c1 = dest in
     state
   else raise IllegalMove *)

(*
let legal_moves state = []
let winner state = None
let is_game_over state = true
let current_player state = P1
let to_string state = "" *)
