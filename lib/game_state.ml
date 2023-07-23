(* open Board *)

(* exception IllegalMove *)

type t = {
  board : Board.t;
      (* curr_player : player; *)
      (* TODO: hash map of board states to counts for three move stalemates clause *)
      (* TODO: might be simpler to generate list of legal moves and then just check if user input is contained in that list *)
}

let init board = { board (* curr_player = P1 *) }
let board state = state.board
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
