open Board
open Move
open Utility

exception IllegalMove

type t = {
  board : Board.t;
  curr_player : player;
  turn_count : int;
  capturing_piece : (int * int) option;
      (* TODO: hash map of board states to counts for three move stalemates clause *)
}

let board state = state.board
let player_dirs = [ (P1, U); (P2, D) ]

let piece_dirs = function
  | { is_king = true; _ } -> [ (U, L); (U, R); (D, L); (D, R) ]
  | { player = p; is_king = false } ->
      let v = List.assoc p player_dirs in
      [ (v, L); (v, R) ]

let is_jump_legal state move piece =
  if move.is_jump then
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    let dest_r, dest_c = dest move in
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
    let adj_r, adj_c = dest move in
    (not (is_oob adj_r adj_c))
    && Option.is_none (get state.board adj_r adj_c)
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
let init board ?(capturing_piece = None) ?(turn_count = 0) curr_player =
  { board; curr_player; capturing_piece; turn_count }

let is_promoted piece row =
  (not piece.is_king)
  && ((piece.player = P1 && row = 0) || (piece.player = P2 && row = size - 1))

let make_move state move =
  if is_legal state move then
    let piece = Option.get (get state.board move.r move.c) in
    let adj_r, adj_c = step move.r move.c move.dir 1 in
    let jump_r, jump_c = step move.r move.c move.dir 2 in
    let removed = put state.board move.r move.c None in
    if move.is_jump then
      let promoted_piece =
        { piece with is_king = piece.is_king || is_promoted piece jump_r }
      in
      let captured = put removed adj_r adj_c None in
      let placed = put captured jump_r jump_c (Some promoted_piece) in
      let post_jump_state =
        {
          board = placed;
          curr_player = piece.player;
          capturing_piece = Some (jump_r, jump_c);
          turn_count = state.turn_count + 1;
        }
      in
      let jumps_possible = legal_moves post_jump_state in
      if is_promoted piece jump_r || MoveSet.is_empty jumps_possible then
        {
          board = placed;
          curr_player = opp_player piece.player;
          capturing_piece = None;
          turn_count = state.turn_count + 1;
        }
      else post_jump_state
    else
      let promoted_piece =
        { piece with is_king = piece.is_king || is_promoted piece adj_r }
      in
      let placed = put removed adj_r adj_c (Some promoted_piece) in
      {
        board = placed;
        curr_player = opp_player state.curr_player;
        capturing_piece = None;
        turn_count = state.turn_count + 1;
      }
  else raise IllegalMove

let string_of_state state =
  let board_str = string_of_board state.board in
  let player_str = if state.curr_player = P1 then "P1" else "P2" in
  let capturing_str =
    Option.fold ~none:"None"
      ~some:(fun coord ->
        let r, c = coord in
        "(" ^ string_of_int r ^ ", " ^ string_of_int c ^ ")")
      state.capturing_piece
  in
  Printf.sprintf "%s\nplayer: %s\ncapturing piece: %s\nturn: %d\n" board_str
    player_str capturing_str state.turn_count

let winner state =
  if MoveSet.is_empty (legal_moves state) then
    Some (opp_player state.curr_player)
  else None

let current_player state = state.curr_player
let turn_count state = state.turn_count
