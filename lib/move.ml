type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type move_kind = Simple | Jump of direction list
type move = { r : int; c : int; dir : direction; kind : move_kind }

let adj r c dir =
  let v, h = dir in
  ( (match v with U -> r - 1 | D -> r + 1),
    match h with L -> c - 1 | R -> c + 1 )
