(* open Board
   open Move *)

(* exception IllegalMove of string *)

type t = {
  board : Board.t;
      (* curr_player : player; *)
      (* TODO: hash map of board states to counts for three move stalemates clause *)
      (* TODO: might be simpler to generate list of legal moves and then just check if user input is contained in that list *)
}

let init board = { board (* curr_player = P1 *) }
let board state = state.board

(*
    Ruleset: https://www.se.rit.edu/~swen-261/projects/WebCheckers/American%20Rules.html

    Illegal Simple Moves:
    1 Tried to move opponent's piece
    2 Tried to move blank space
    3 Tried to move Normal piece backwards
    4 Tried to move onto occupied space

    Illegal Jump Moves:
    7 Tried to take simple move when jump was an option
    8 Tried to end jump early when another capture existed
    9 Tried to jump backwards with normal piece
    10 tried to capture piece on edge/corner
    11 Tried to jump without capture

    legal edge case moves:
    1 Took a single jump when a multi jump was available
    2 two possible jump options after single jump should both be available
    3 "diamond" move where you can land on the same square via jumping UR UL or UL UR
    4 king should be able to jump backwards
    5 king should be able to simple move backwards

  *)
(* let player_dirs = [ (P1, [ UL; UR ]); (P2, [ DL; DR ]) ] *)

let (* rec *) is_legal _state _move =
  (* TODO: bounds checking *)
  (* let b = state.board in
     let match_piece r c ~normal ~king =
       match get b r c with
       | Some (Normal player) -> normal player
       | Some (King player) -> king player
       | None -> false
       | exception Invalid_argument _ -> false
     in
     match move with
     | Simple (r, c, dir) ->
         match_piece r c
           ~normal:(fun player -> List.mem dir (List.assoc player player_dirs))
           ~king:(fun player -> false)
     | Jump (_, _, []) -> raise (Invalid_argument "Jump with no directions")
     | Jump (r, c, [ last ]) -> false
     | Jump (r, c, dir :: rst) -> false *)
  false

(* let make_move state move =
   if is_legal state move then
     let src, dest = move in
     let r0, c0 = src in
     let r1, c1 = dest in
     state
   else raise IllegalMove *)

(*
let legal_moves state = []
let winner state = None
let is_game_over state = true
let current_player state = P1
let to_string state = "" *)
