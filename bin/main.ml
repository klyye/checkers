open Checkers
open Printf

let print_help () =
  let () = printf "Enter: Row Column U/D L/R J/S\n" in
  let () = printf "Example: 5 4 U L S\n" in
  let () =
    printf "U = Up, D = Down, L = Left, R = Right, J = Jump, S = Simple\n"
  in
  printf "Row and Column should be two integers between 0 and 7\n"

let parse_move = function
  | [ r_token; c_token; v_token; h_token; j_token ] ->
      let open Checkers.Move in
      let r = int_of_string r_token in
      let c = int_of_string c_token in
      let dir = Move.dir_of_string v_token h_token in
      { r; c; dir; is_jump = j_token = "J" }
  | _ -> raise (Invalid_argument "Input must have 5 tokens")

let rec input_loop () =
  let () = printf "> " in
  let line = read_line () in
  let processed_line = line |> String.uppercase_ascii |> String.trim in
  if processed_line = "HELP" then
    let () = print_help () in
    (input_loop [@tailcall]) ()
  else
    let split_line =
      List.filter
        (fun s -> String.length s > 0)
        (String.split_on_char ' ' processed_line)
    in
    try parse_move split_line with
    | Invalid_argument s ->
        let () = printf "%s\n" s in
        (input_loop [@tailcall]) ()
    | Failure s ->
        let () = printf "%s\n" s in
        (input_loop [@tailcall]) ()

let rec game_loop state =
  let open Game_state in
  if Option.is_some (winner state) then
    printf "%s wins!!!\n" (if winner state = Some P1 then "P1" else "P2")
  else
    let () = printf "%s" (string_of_state state) in
    let () = printf "Enter: Row Column U/D L/R J/S\n" in
    let () = printf "Type \"help\" for input help" in
    let move =
      if current_player state = P1 then input_loop ()
      else Minimax.find_move state
    in
    (game_loop [@tailcall])
      (try make_move state move
       with IllegalMove ->
         let () = printf "Illegal move\n" in
         state)
;;

game_loop (Game_state.init Board.start Board.P1)
