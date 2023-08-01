open Game_state
open Utility

let search_depth = 6

let heuristic state =
  let open Board in
  match winner state with
  | Some P1 -> max_int
  | Some P2 -> min_int
  | None ->
      let piece_list = Board.piece_list (board state) in
      List.fold_left
        (fun acc piece ->
          acc
          + (if piece.is_king then 3 else 1)
            * if piece.player = P1 then 1 else -1)
        0 piece_list

(* https://www.youtube.com/watch?v=l-hh51ncgDI *)
let rec minimax state depth =
  let is_maximizing = current_player state = Board.P1 in
  let extrema = if is_maximizing then max else min in
  let start = if is_maximizing then min_int else max_int in
  if depth = 0 || Option.is_some (winner state) then heuristic state
  else
    MoveSet.fold
      (fun move max_eval ->
        let child = make_move state move in
        let eval = minimax child (depth - 1) in
        extrema eval max_eval)
      (legal_moves state) start

let find_move state =
  let is_maximizing = current_player state = Board.P1 in
  let extrema = if is_maximizing then fun a b -> a - b else fun a b -> b - a in
  let start = if is_maximizing then min_int else max_int in
  let moves = legal_moves state in
  let _, move =
    MoveSet.fold
      (fun move move_and_eval ->
        let max_eval, _ = move_and_eval in
        let child = make_move state move in
        let eval = minimax child search_depth in
        if extrema eval max_eval > 0 then (eval, move) else move_and_eval)
      moves
      (start, MoveSet.choose moves)
  in
  move
