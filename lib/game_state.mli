(* exception IllegalMove *)

type move = (int * int) * (int * int)
type t

val init : Board.t -> t
val is_legal : t -> move -> bool
val board : t -> Board.t
