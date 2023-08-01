type vert_dir = U | D
type horiz_dir = L | R
type direction = vert_dir * horiz_dir
type t = { r : int; c : int; dir : direction; is_jump : bool }

(* in retrospect i couldve just made some sort of 2d vector class but probably overkill *)
let step r c dir dist =
  let v, h = dir in
  ( (match v with U -> r - dist | D -> r + dist),
    match h with L -> c - dist | R -> c + dist )

let dest move = step move.r move.c move.dir (if move.is_jump then 2 else 1)

let string_of_move move =
  let v_dir, h_dir = move.dir in
  let v_str = match v_dir with U -> "U" | D -> "D" in
  let h_str = match h_dir with L -> "L" | R -> "R" in
  let jump = if move.is_jump then "jump" else "simple" in
  Printf.sprintf "(%d, %d) (%s, %s) %s" move.r move.c v_str h_str jump

(* this is so fucking stupid that i have to go through this much effort just to make a set of moves

   sort in a way such that moves towards the center are higher *)
let compare m1 m2 =
  if m1.is_jump = m2.is_jump then
    let adj_r1, adj_c1 = dest m1 in
    let adj_r2, adj_c2 = dest m2 in
    let dist1 = abs (adj_r1 - 4) + abs (adj_c1 - 4) in
    let dist2 = abs (adj_r2 - 4) + abs (adj_c2 - 4) in
    dist1 - dist2
  else if m1.is_jump then 1
  else -1

let dir_of_string v_token h_token =
  ( (match v_token with
    | "U" -> U
    | "D" -> D
    | _ -> raise (Invalid_argument "Vertical direction must be U or D")),
    match h_token with
    | "L" -> L
    | "R" -> R
    | _ -> raise (Invalid_argument "Horizontal direction must be L or R") )
