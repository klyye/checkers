(* type square = int * int
type move = square * square
type player = P1 | P2
type piece = Normal | King
type board = (square * piece option) list
type t = {
  board: board
  (* TODO: hash map of board states to counts for three move stalemates clause *)
} *)

let size = 8

(* let init = {
board = []
}
let legal_moves state = []
let make_move state move = {board = []}
let winner state = None
let is_game_over state = true
let is_legal state move = false
let current_player state = P1
let to_string state = "" *)