open Board
open Move

(* exception IllegalMove of string *)

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

let is_jump_legal state r c piece dir =
  let adj_r, adj_c = step r c dir 1 in
  let dest_r, dest_c = step r c dir 2 in
  List.mem dir (piece_dirs piece)
  && (not (is_oob adj_r adj_c))
  && is_piece_at state.board adj_r adj_c (opp_player piece.player)
  && (not (is_oob dest_r dest_c))
  && Option.is_none (get state.board dest_r dest_c)
  && (Option.is_none state.capturing_piece
     || Option.get state.capturing_piece = (r, c))

let are_jumps_possible state r c piece =
  List.exists (fun dir -> is_jump_legal state r c piece dir) (piece_dirs piece)

let is_legal state move =
  (* TODO: idea: List.mem move state.legal_moves *)
  if is_piece_at state.board move.r move.c state.curr_player then
    let piece = Option.get (get state.board move.r move.c) in
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    if move.is_jump then is_jump_legal state move.r move.c piece move.dir
    else
      Option.is_none (get state.board adj_r adj_c)
      && Option.is_none state.capturing_piece
      && List.mem move.dir (piece_dirs piece)
      && not (are_jumps_possible state move.r move.c piece)
  else false

let legal_moves _state =
  (* idea: two phases: generate legal jumps, then generate legal simple moves *)
  []

(* https://stackoverflow.com/questions/1667232/optional-argument-cannot-be-erased *)
let init ?(board = start) ?(curr_player = P1) ?(capturing_piece = None) () =
  { board; curr_player; capturing_piece }

(* let make_move state move =
   if is_legal state move then
     let src, dest = move in
     let r0, c0 = src in
     let r1, c1 = dest in
     state
   else raise IllegalMove *)

(*
let winner state = None
let is_game_over state = true
let current_player state = P1
let to_string state = "" *)
