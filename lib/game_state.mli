open Move

(* exception IllegalMove *)
type t

val init : Board.t -> t
val is_legal : t -> move -> bool
val board : t -> Board.t
