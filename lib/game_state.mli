open Move

(* exception IllegalMove of string *)

type t

val init :
  ?board:Board.t ->
  ?curr_player:Board.player ->
  ?capturing_piece:(int * int) option ->
  unit ->
  t

val is_legal : t -> move -> bool
val board : t -> Board.t
