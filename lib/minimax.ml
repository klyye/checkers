open Game_state
open Utility

let search_depth = 25

(* TODO: add turn counter for openings, add "king only" phase *)
let heuristic state =
  let open Board in
  match winner state with
  | Some P1 -> max_int
  | Some P2 -> min_int
  | None ->
      let curr_player = current_player state in
      let board = board state in
      let coordset =
        CoordSet.union
          (Board.find_pieces board curr_player)
          (Board.find_pieces board (opp_player curr_player))
      in
      CoordSet.fold
        (fun coord score ->
          let r, c = coord in
          let piece = Option.get (Board.get board r c) in
          let home_row = if piece.player = P1 then 7 else 0 in
          let polarity = if piece.player = P1 then 1 else -1 in
          (polarity * if piece.is_king then 200 else 15 * abs (r - home_row))
          + score)
        coordset 0

(* https://www.youtube.com/watch?v=l-hh51ncgDI *)
let rec minimax state depth alpha beta =
  let is_maximizing = current_player state = Board.P1 in
  let extrema = if is_maximizing then max else min in
  let start = if is_maximizing then min_int else max_int in
  if depth = 0 || beta <= alpha || Option.is_some (winner state) then
    heuristic state
  else
    MoveSet.fold
      (fun move max_eval ->
        let child = make_move state move in
        let eval =
          minimax child (depth - 1) (extrema max_eval alpha)
            (extrema max_eval beta)
        in
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
        let eval = minimax child search_depth min_int max_int in
        if extrema eval max_eval > 0 then
          let () =
            Printf.printf "\nEval: %d %s" eval (Move.string_of_move move)
          in
          (eval, move)
        else move_and_eval)
      moves
      (start, MoveSet.min_elt moves)
  in
  move
