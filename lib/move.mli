type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type move_kind = Simple | Jump of direction list
type move = { r : int; c : int; dir : direction; kind : move_kind }

val adj : int -> int -> direction -> int * int
