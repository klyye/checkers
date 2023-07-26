type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type move = { r : int; c : int; dir : direction; is_jump : bool }

val step : int -> int -> direction -> int -> int * int
