(*
   type move = coord * coord
   

type t = {
  board : Board.t;
      (* TODO: hash map of board states to counts for three move stalemates clause *)
}


let t = { board = [||] }
let legal_moves state = []
let make_move state move = {board = []}
let winner state = None
let is_game_over state = true
let is_legal state move = false
let current_player state = P1
let to_string state = "" *)
