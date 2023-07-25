type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type move_kind = Simple | Jump of direction list
type move = { r : int; c : int; dir : direction; kind : move_kind }

(* in retrospect i couldve just made some sort of 2d vector class but probably overkill *)
let step r c dir dist =
  let v, h = dir in
  ( (match v with U -> r - dist | D -> r + dist),
    match h with L -> c - dist | R -> c + dist )
