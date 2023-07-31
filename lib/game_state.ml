open Board
open Move
open Utility

exception IllegalMove

type t = {
  board : Board.t;
  curr_player : player;
  capturing_piece : (int * int) option;
      (* TODO: hash map of board states to counts for three move stalemates clause *)
      (* TODO: might be simpler to generate list of legal moves and then just check if user input is contained in that list *)
}

let board state = state.board
let player_dirs = [ (P1, U); (P2, D) ]

let piece_dirs piece =
  match piece with
  | { is_king = true; _ } -> [ (U, L); (U, R); (D, L); (D, R) ]
  | { player = p; is_king = false } ->
      let v = List.assoc p player_dirs in
      [ (v, L); (v, R) ]

let is_jump_legal state move piece =
  if move.is_jump then
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    let dest_r, dest_c = step move.r move.c move.dir 2 in
    List.mem move.dir (piece_dirs piece)
    && (not (is_oob adj_r adj_c))
    && is_piece_at state.board adj_r adj_c (opp_player piece.player)
    && (not (is_oob dest_r dest_c))
    && Option.is_none (get state.board dest_r dest_c)
    && (Option.is_none state.capturing_piece
       || Option.get state.capturing_piece = (move.r, move.c))
  else raise (Invalid_argument "is_jump_legal can only take jump moves")

let is_simple_legal state move piece =
  if not move.is_jump then
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    Option.is_none (get state.board adj_r adj_c)
    && Option.is_none state.capturing_piece
    && List.mem move.dir (piece_dirs piece)
  else raise (Invalid_argument "is_simple_legal can only take simple moves")

let movable_piece_coords state =
  let open CoordSet in
  match state.capturing_piece with
  | Some coord -> singleton coord
  | None -> find_pieces state.board state.curr_player

let legal_moves state =
  let legality_check f is_jump =
    CoordSet.fold
      (fun coord acc ->
        let r, c = coord in
        let piece = Option.get (get state.board r c) in
        (* why tf are the param order swapped for set fold and list fold?? *)
        List.fold_left
          (fun acc2 dir ->
            let move = { r; c; dir; is_jump } in
            if f state move piece then MoveSet.add move acc2 else acc2)
          acc (piece_dirs piece))
      (movable_piece_coords state)
      MoveSet.empty
  in
  let jumps = legality_check is_jump_legal true in
  let simples =
    if MoveSet.is_empty jumps then legality_check is_simple_legal false
    else MoveSet.empty
  in
  MoveSet.union jumps simples

let is_legal state move = MoveSet.mem move (legal_moves state)

(* https://stackoverflow.com/questions/1667232/optional-argument-cannot-be-erased *)
let init ?(board = start) ?(curr_player = P1) ?(capturing_piece = None) () =
  { board; curr_player; capturing_piece }

let make_move state move =
  if is_legal state move then state else raise IllegalMove

(*
let winner state = None
let is_game_over state = true
let current_player state = P1
let to_string state = "" *)
