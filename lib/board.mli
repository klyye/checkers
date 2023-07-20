type player = P1 | P2
type piece = Normal of player | King of player
type t = piece option array array

val size : int
val blank : t
val to_list : t -> piece option list
(* val put : t -> int -> int -> piece -> t
   val get : t -> int -> int -> piece option *)
