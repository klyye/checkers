open Game_state
open Utility

let search_depth = 9

let heuristic state =
  let open Board in
  match winner state with
  | Some P1 -> max_int
  | Some P2 -> min_int
  | None ->
      (let piece_list = Board.piece_list (board state) in
       List.fold_left
         (fun acc piece ->
           acc
           + (if piece.is_king then 175 else 75)
             * if piece.player = P1 then 1 else -1)
         0 piece_list)
      +
      let coordset = Board.find_pieces (board state) (current_player state) in
      CoordSet.fold
        (fun coord acc ->
          let r, c = coord in
          let piece = Option.get (Board.get (board state) r c) in
          if piece.is_king then acc else acc - r)
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
          let () = Printf.printf "Eval: %d" eval in
          (eval, move)
        else move_and_eval)
      moves
      (start, MoveSet.choose moves)
  in
  move
