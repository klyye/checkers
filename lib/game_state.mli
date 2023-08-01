open Utility

exception IllegalMove

type t

val init :
  Board.t ->
  ?capturing_piece:(int * int) option ->
  ?turn_count:int ->
  Board.player ->
  t

val is_legal : t -> Move.t -> bool
val board : t -> Board.t
val legal_moves : t -> MoveSet.t
val make_move : t -> Move.t -> t
val string_of_state : t -> string
val winner : t -> Board.player option
val current_player : t -> Board.player
val turn_count : t -> int
