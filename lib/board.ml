(* Gotta make my own immutable board type since OCaml doesn't have immutable arrays by default and I don't want to use Jane Street's library *)
type player = P1 | P2
type piece = Normal of player | King of player
type t = piece option array array

let size = 8
let blank = Array.make_matrix size size None
let to_list board = List.concat (Array.to_list (Array.map Array.to_list board))
(* let put board x y piece = [||]
   let get board x y = None *)
