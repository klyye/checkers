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

let init board = { board; curr_player = P1; capturing_piece = None }
let board state = state.board
let is_oob r c = r < 0 || r >= size || c < 0 || c >= size
let player_dirs = [ (P1, U); (P2, D) ]

let piece_dirs piece =
  match piece with
  | { is_king = true; _ } -> [ (U, L); (U, R); (D, L); (D, R) ]
  | { player = p; is_king = false } ->
      let v = List.assoc p player_dirs in
      [ (v, L); (v, R) ]

let is_piece_at state r c player =
  (not (is_oob r c))
  &&
  let piece = get state.board r c in
  Option.fold ~none:false ~some:(fun p -> p.player = player) piece

let is_jump_legal state r c piece dir =
  let adj_r, adj_c = step r c dir 1 in
  let dest_r, dest_c = step r c dir 2 in
  List.mem dir (piece_dirs piece)
  && (not (is_oob adj_r adj_c))
  && is_piece_at state adj_r adj_c (opp_player piece.player)
  && (not (is_oob dest_r dest_c))
  && Option.is_none (get state.board dest_r dest_c)

let are_jumps_possible state r c piece =
  List.exists (fun dir -> is_jump_legal state r c piece dir) (piece_dirs piece)

(* FIXME: needs to account for pieces already captured*)
let rec are_jumps_legal state r c piece jumps =
  match jumps with
  | [] -> not (are_jumps_possible state r c piece)
  | dir :: rst ->
      let dest_r, dest_c = step r c dir 2 in
      is_jump_legal state r c piece dir
      && are_jumps_legal state dest_r dest_c piece rst

(* check if landing space is empty *)

let is_legal state move =
  if is_piece_at state move.r move.c state.curr_player then
    let piece = Option.get (get state.board move.r move.c) in
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    match move.kind with
    | Simple ->
        Option.is_none (get state.board adj_r adj_c)
        && List.mem move.dir (piece_dirs piece)
        && not (are_jumps_possible state move.r move.c piece)
    | Jump lst -> are_jumps_legal state move.r move.c piece (move.dir :: lst)
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
