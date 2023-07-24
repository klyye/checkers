type direction = UL | UR | DL | DR

(* list on jump indicates any subsequent jumps *)
type move_kind = Simple | Jump of direction list
type move = { r : int; c : int; dir : direction; kind : move_kind }

val move_dir : int -> int -> direction -> int * int
