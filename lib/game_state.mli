type square
type move
type player
type piece
type board
type t

val size : int
val legal_moves : t -> move list
val make_move : t -> move -> t
val winner : t -> player option
val is_game_over : t -> bool
val is_legal : t -> move -> bool
val current_player : t -> player
val init : t
val to_string : t -> string