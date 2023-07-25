open Board
open Move

(* exception IllegalMove of string *)

type t = {
  board : Board.t;
  curr_player : player;
      (* TODO: hash map of board states to counts for three move stalemates clause *)
      (* TODO: might be simpler to generate list of legal moves and then just check if user input is contained in that list *)
}

let init board = { board; curr_player = P1 }
let board state = state.board

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
    3 "diamond" move where you can land on the same square via jumping UR UL or UL UR
    4 king should be able to jump backwards
    5 king should be able to simple move backwards

  *)
let player_dirs = [ (P1, U); (P2, D) ]
let is_oob r c = r < 0 || r >= size || c < 0 || c >= size

let is_movable state r c =
  (not (is_oob r c))
  &&
  let piece = get state.board r c in
  Option.fold ~none:false ~some:(fun p -> p.player = state.curr_player) piece

let (* rec *) is_jump_legal _state _r _c jumps =
  match jumps with [] -> false | _hd :: _rst -> false

let is_legal state move =
  if is_movable state move.r move.c then
    let piece = Option.get (get state.board move.r move.c) in
    match move.kind with
    | Simple ->
        let v, _ = move.dir in
        let adj_r, adj_c = adj move.r move.c move.dir in
        Option.is_none (get state.board adj_r adj_c)
        && (piece.is_king || v = List.assoc piece.player player_dirs)
        (* separate recursive helper function *)
    | Jump lst -> is_jump_legal state move.r move.c (move.dir :: lst)
  else false

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
