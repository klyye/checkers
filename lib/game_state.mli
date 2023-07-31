open Utility

exception IllegalMove

type t

val init :
  ?board:Board.t ->
  ?curr_player:Board.player ->
  ?capturing_piece:(int * int) option ->
  unit ->
  t

val is_legal : t -> Move.t -> bool
val board : t -> Board.t
val legal_moves : t -> MoveSet.t
val make_move : t -> Move.t -> t
