type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type t = { r : int; c : int; dir : direction; is_jump : bool }

val step : int -> int -> direction -> int -> int * int
val compare : t -> t -> int
val dir_of_string : string -> string -> direction
