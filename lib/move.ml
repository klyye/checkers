type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type t = { r : int; c : int; dir : direction; is_jump : bool }

(* in retrospect i couldve just made some sort of 2d vector class but probably overkill *)
let step r c dir dist =
  let v, h = dir in
  ( (match v with U -> r - dist | D -> r + dist),
    match h with L -> c - dist | R -> c + dist )

(* this is so fucking stupid that i have to go through this much effort just to make a set of moves *)
let dir_to_int dir =
  let v, h = dir in
  (match v with U -> 0 | D -> 1) + match h with L -> 0 | R -> 2

let compare m1 m2 =
  if m1.is_jump = m2.is_jump then
    if m1.dir = m2.dir then if m1.r = m2.r then m1.c - m2.c else m1.r - m2.r
    else dir_to_int m1.dir - dir_to_int m2.dir
  else if m1.is_jump then 1
  else -1
