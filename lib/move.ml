type direction = UL | UR | DL | DR
type move_kind = Simple | Jump of direction list
type move = { r : int; c : int; dir : direction; kind : move_kind }

let move_dir r c dir =
  match dir with
  | UL -> (r - 1, c - 1)
  | UR -> (r - 1, c + 1)
  | DL -> (r + 1, c - 1)
  | DR -> (r + 1, c + 1)
