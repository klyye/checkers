type player = P1 | P2
type piece = { player : player; is_king : bool }
type t
(* there is no square type; a square is a piece option *)

val size : int
val blank : t
val start : t
val to_2d_list : t -> piece option list list
val of_2d_list : piece option list list -> t
val put : t -> int -> int -> piece option -> t
val get : t -> int -> int -> piece option
val string_of_square : piece option -> string
val string_of_board : t -> string
