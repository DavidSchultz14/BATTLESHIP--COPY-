open Graphics

(**[is_string_int] returns -1 if [input_string] is not an int, and
   returns the int value otherwise**)
let is_string_int input_string =
  try int_of_string input_string with Failure _ -> -1

(**[read_input_list] reads the user's input and checks to make sure it's
   of the format "int,int". If so it returns a list of the input, else
   it prompts the user again**)
let rec read_terminal () =
  match String.split_on_char ',' (read_line ()) with
  | [ a; b ] ->
      if is_string_int a > -1 && is_string_int b > -1 then
        (int_of_string a, int_of_string b)
      else
        print_endline "Bad format, try again" |> fun () ->
        read_terminal ()
  | _ ->
      print_endline "Bad format, try again";
      read_terminal ()

(** [read_and_check_input] reads input from the terminal and returns a
    valid point. If it isn't valid it'll ask again for user input **)
let rec read_and_check_input width height ship_lst =
  let user_input = read_terminal () in
  if TextMenuhf.check_dimensions user_input 20 20 then
    if ship_lst = [] then user_input
    else if TextMenuhf.check_adjacent user_input ship_lst then
      user_input
    else (
      print_endline "Ship placement not adjacent, try again";
      read_and_check_input width height ship_lst)
  else (
    print_endline "Ship placement out of bounds, try again";
    read_and_check_input width height ship_lst)

(** [create_ship_points] prompts the user to enter points for a ship and
    creates a list of points representing a ship**)
let rec create_ship_points (ship_list : (int * int) list) max_size =
  if List.length ship_list < max_size then (
    print_endline "Enter ship point (format: x,y): ";
    let new_list = read_and_check_input 20 20 ship_list :: ship_list in
    create_ship_points new_list max_size)
  else ship_list

let rec create_player_ships (lst : int list) : (int * int) list list =
  match lst with
  | [] -> []
  | h :: tail ->
      print_endline " Ship\n";
      let first = create_ship_points [] h in
      first :: create_player_ships tail

(**[create_both_player_ships] generates a list of ships for each player**)
let rec create_both_player_ships lst :
    (int * int) list list * (int * int) list list =
  let player_1_ships =
    print_endline "player 1 ships" |> fun () -> create_player_ships lst
  in
  let player_2_ships =
    print_endline "player 2 ships" |> fun () -> create_player_ships lst
  in
  (player_1_ships, player_2_ships)

(** [ players_ships] prompts for the game to play, then starts it. *)
let players_ships () =
  let rec list_maker_ships i : int list =
    match is_string_int i with
    | 1 -> [ 5 ]
    | 2 -> [ 5; 4 ]
    | 3 -> [ 5; 4; 4 ]
    | 4 -> [ 5; 4; 4; 3 ]
    | 5 -> [ 5; 4; 4; 3; 2 ]
    | _ -> (
        print_endline "Max Ships is 5, min Ships is 0. Try again.";
        match read_line () with
        | some_input -> list_maker_ships some_input)
  in
  print_endline "\n\nWelcome to Battleship!.\n";
  print_endline
    "Press any input to continue and then Please enter the number of \
     ships you will use.\n\
    \ Then, enter your ship points. \n\
    \     (player two will start out as attacking)\n";
  let (num_ships_list : int list) =
    match read_line () with
    | some_input -> list_maker_ships some_input
  in
  () |> fun () -> create_both_player_ships num_ships_list
