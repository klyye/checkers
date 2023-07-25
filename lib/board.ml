(* Gotta make my own immutable board type since OCaml doesn't have immutable arrays by default and I don't want to use Jane Street's library *)
type player = P1 | P2
type piece = { player : player; is_king : bool }
type t = piece option array array

let size = 8
let blank = Array.make_matrix size size None

let start =
  let p, q =
    ( Some { player = P1; is_king = false },
      Some { player = P2; is_king = false } )
  in
  let n = None in
  [|
    [| n; q; n; q; n; q; n; q |];
    [| q; n; q; n; q; n; q; n |];
    [| n; q; n; q; n; q; n; q |];
    [| n; n; n; n; n; n; n; n |];
    [| n; n; n; n; n; n; n; n |];
    [| p; n; p; n; p; n; p; n |];
    [| n; p; n; p; n; p; n; p |];
    [| p; n; p; n; p; n; p; n |];
  |]

let to_2d_list board = Array.to_list (Array.map Array.to_list board)

let of_2d_list lst =
  if List.length lst <> size || List.exists (fun r -> List.length r <> size) lst
  then
    raise (Invalid_argument ("Must be " ^ string_of_int size ^ " size square"))
  else Array.of_list (List.map Array.of_list lst)

(* I HATE MUTABILITY!!! I HATE MUTABILITY!! *)
let put board r c piece =
  let copy = Array.(map copy) board in
  let () = copy.(r).(c) <- piece in
  copy

let get board r c = board.(r).(c)

let string_of_square x =
  Option.fold ~none:"--"
    ~some:(fun y ->
      match y with
      | { player = P1; is_king = false } -> "N1"
      | { player = P1; is_king = true } -> "K1"
      | { player = P2; is_king = false } -> "N2"
      | { player = P2; is_king = true } -> "K2")
    x

(* TODO: rewrite this so that it prints row and col numbers
   maybe write a fold_lefti? *)
let string_of_board b =
  Array.fold_left
    (fun acc row ->
      acc
      ^ Array.fold_left (fun acc2 p -> acc2 ^ string_of_square p ^ ", ") "" row
      ^ "\n")
    "\n" b

let opp_player p = if p = P1 then P2 else P1
