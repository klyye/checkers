open Game_state
open Utility

let search_depth = 10

(* TODO: add "king only" phase *)
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
          let polarity = if piece.player = P1 then 1 else -1 in
          (polarity * if piece.is_king then 200 else 100) + score)
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

let start_minimax state =
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
        let () =
          Printf.printf "\nEval: %d %s" eval (Move.string_of_move move)
        in
        if extrema eval max_eval > 0 then (eval, move) else move_and_eval)
      moves
      (start, MoveSet.min_elt moves)
  in
  move

let match_opening state =
  let open Move in
  let open Board in
  let b = board state in
  let o = None in
  let h = Some { player = P2; is_king = false } in
  let j = Some { player = P1; is_king = false } in
  let dundee =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; j; o; o; o; o; o; o ] (* 4 *);
        [ o; o; j; o; j; o; j; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let bristol =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; j; o; o; o; o; o; o ] (* 4 *);
        [ j; o; o; o; j; o; j; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let kelso =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; j; o; o; o; o ] (* 4 *);
        [ j; o; j; o; o; o; j; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let denny =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; j; o; o ] (* 4 *);
        [ j; o; j; o; o; o; j; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let double_corner =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; j; o; o ] (* 4 *);
        [ j; o; j; o; j; o; o; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let edinburgh =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; o; o; o; o; j ] (* 4 *);
        [ j; o; j; o; j; o; o; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  let old_faithful =
    of_2d_list
      [
        (*0  1  2  3  4  5  6  7*)
        [ o; h; o; h; o; h; o; h ] (* 0 *);
        [ h; o; h; o; h; o; h; o ] (* 1 *);
        [ o; h; o; h; o; h; o; h ] (* 2 *);
        [ o; o; o; o; o; o; o; o ] (* 3 *);
        [ o; o; o; j; o; o; o; o ] (* 4 *);
        [ j; o; o; o; j; o; j; o ] (* 5 *);
        [ o; j; o; j; o; j; o; j ] (* 6 *);
        [ j; o; j; o; j; o; j; o ] (* 7 *);
      ]
  in
  if b = start then { r = 5; c = 2; dir = (U, R); is_jump = false }
  else if b = edinburgh then { r = 2; c = 5; dir = (D, L); is_jump = false }
  else if b = double_corner then { r = 2; c = 5; dir = (D, L); is_jump = false }
  else if b = denny then { r = 2; c = 1; dir = (D, R); is_jump = false }
  else if b = kelso then { r = 2; c = 7; dir = (D, L); is_jump = false }
  else if b = bristol then { r = 2; c = 3; dir = (D, R); is_jump = false }
  else if b = dundee then { r = 2; c = 1; dir = (D, L); is_jump = false }
  else if b = old_faithful then { r = 2; c = 5; dir = (D, R); is_jump = false }
  else start_minimax state

let find_move state =
  let moves = legal_moves state in
  if turn_count state <= 1 then match_opening state
  else if MoveSet.cardinal moves = 1 then MoveSet.choose moves
  else start_minimax state
