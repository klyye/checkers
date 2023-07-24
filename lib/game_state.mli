open Move

(* exception IllegalMove of string *)

type t

val init : Board.t -> t
val is_legal : t -> move -> bool
val board : t -> Board.t
