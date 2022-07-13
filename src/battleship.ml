open Graphics

type point = int * int

type ship = {
  position : point list;
  hits : point list;
      (*sunk : bool; bow : point; stern : point; name : string;*)
}

(*type map = { size : int; obstructions : point list; }*)

type player_board = {
  (*owner : string;*)
  (*pl_map : map;*)
  ships : ship list;
  misses_against : point list;
  hits_against : point list;
  has_lost : bool;
}

type game = {
  player_one : player_board;
  player_two : player_board;
  defending_player : player_board;
}

(** [hit_point] returns the point of the ship that is hit or None if the
    ship is not hit **)
let rec hit_point (position : point list) point =
  match position with
  | h :: t -> if h = point then Some point else hit_point t point
  | [] -> None

(** [remove_dup] returns a list with all duplicated elements removed**)
let remove_dup lst =
  List.sort_uniq (fun a b -> if a = b then 0 else -1) lst

(** [update_ship_hit] returns a ship record updated with the hit if
    there is a hit. If there is no hit, it returns the same ship record
    **)
let update_ship_hit ship point =
  match hit_point ship.position point with
  | Some hit_point ->
      { ship with hits = remove_dup (hit_point :: ship.hits) }
  | None -> ship

(** [is_ship_hit] returns true if the given ship is hit with the given
    point and false otherwise ***)
let is_ship_hit ship point =
  match hit_point ship.position point with
  | Some _ -> true
  | None -> false

(** [is_sunk_ship] returns true if the given ship is sunk and false
    otherwise **)
let is_sunk_ship ship =
  List.length ship.position = List.length ship.hits

(** [remove_ships_ship_lisy] removes a ship from the playerboard's ship
    list if it has been sunk**)

let rec check_lost (board : player_board) = board.ships = []

let rec check_board_hits lst position =
  match lst with
  | [] -> false
  | h :: t -> is_ship_hit h position || check_board_hits t position

(** [update_hits] returns a new board with the updated hits list*)
let update_hits board point =
  if check_board_hits board.ships point then
    { board with hits_against = point :: board.hits_against }
  else { board with misses_against = point :: board.misses_against }

let rec update_all_ships shiplst point =
  match shiplst with
  | [] -> []
  | h :: t ->
      if is_ship_hit h point then
        let ship = update_ship_hit h point in
        if is_sunk_ship ship then t else ship :: t
      else h :: update_all_ships t point

let guess current_game pos =
  let defending_board = current_game.defending_player in
  let board_w_hits = update_hits defending_board pos in
  let board_w_ships =
    {
      board_w_hits with
      ships = update_all_ships board_w_hits.ships pos;
    }
  in

  let board_w_loss =
    { board_w_ships with has_lost = check_lost board_w_ships }
  in

  if defending_board = current_game.player_one then
    {
      player_one = board_w_loss;
      player_two = current_game.player_two;
      defending_player = current_game.player_two;
    }
  else
    {
      player_one = current_game.player_one;
      player_two = board_w_loss;
      defending_player = current_game.player_one;
    }

let make_ship point_list = { position = point_list; hits = [] }

let rec make_ships ship_list =
  match ship_list with
  | [] -> []
  | h :: t -> make_ship h :: make_ships t

let make_board ships_list =
  {
    ships = make_ships ships_list;
    misses_against = [];
    hits_against = [];
    has_lost = false;
  }

let make_game input_list =
  {
    player_one = make_board (fst input_list);
    player_two = make_board (snd input_list);
    defending_player = make_board (fst input_list);
  }

(**[is_string_int] returns -1 if [input_string] is not an int, and
   returns the int value otherwise**)
let is_string_int input_string =
  try int_of_string input_string with Failure _ -> -1

(**[read_input_list] reads the user's input and checks to make sure it's
   of the format "int,int". If so it returns a list of the input, else
   it prompts the user again**)
let rec read_input_list () =
  match String.split_on_char ',' (read_line ()) with
  | [ a; b ] ->
      if is_string_int a > -1 && is_string_int b > -1 then [ a; b ]
      else
        print_endline "Bad format, try again" |> fun () ->
        read_input_list ()
  | _ ->
      print_endline "Bad format, try again";
      read_input_list ()

let draw_point x y scale color off_x off_y =
  Graphics.draw_circle
    ((x * scale) + (scale / 2) + off_x)
    ((y * scale) + (scale / 2) + off_y)
    (scale / 4);
  Graphics.set_color color;
  Graphics.fill_circle
    ((x * scale) + (scale / 2) + off_x)
    ((y * scale) + (scale / 2) + off_y)
    (scale / 4)

let rec display_hits_offence board scale off_x off_y =
  Graphics.set_color black;
  match board.hits_against with
  | [] -> ()
  | (x, y) :: tail ->
      draw_point x y scale black off_x off_y;
      display_hits_offence
        { board with hits_against = tail }
        scale off_x off_y

let rec display_misses_offence board scale off_x off_y =
  Graphics.set_color red;
  match board.misses_against with
  | [] -> ()
  | (x, y) :: tail ->
      draw_point x y scale red off_x off_y;
      display_misses_offence
        { board with misses_against = tail }
        scale off_x off_y

(*let display_ships game_states scale off_x off_y = if game_states.*)
let place_ship (ship : ship) scale off_x off_y =
  match ShipImages.sort_ship ship.position with
  | (x, y) :: tail ->
      ShipImages.display_ship_image
        (ShipImages.sort_ship ship.position)
        ((x * scale) + off_x)
        ((y * scale) + off_y)
  | [] -> ()

let rec place_ships board scale off_x off_y =
  match board.ships with
  | [] -> ()
  | h :: tail ->
      place_ship h scale off_x off_y;
      place_ships { board with ships = tail } scale off_x off_y

let display_offence game scale off_x off_y =
  let off_board =
    if game.defending_player = game.player_one then game.player_two
    else game.player_one
  in
  place_ships off_board scale off_x off_y;
  display_misses_offence off_board scale off_x off_y;
  display_hits_offence off_board scale off_x off_y

let rec display_hits game_state scale off_x off_y =
  Graphics.set_color black;
  match game_state.defending_player.hits_against with
  | [] -> ()
  | (x, y) :: tail ->
      draw_point x y scale black off_x off_y;
      display_hits
        {
          game_state with
          defending_player =
            { game_state.defending_player with hits_against = tail };
        }
        scale off_x off_y

let rec display_misses game_state scale off_x off_y =
  Graphics.set_color red;
  match game_state.defending_player.misses_against with
  | [] -> ()
  | (x, y) :: tail ->
      draw_point x y scale red off_x off_y;
      display_misses
        {
          game_state with
          defending_player =
            { game_state.defending_player with misses_against = tail };
        }
        scale off_x off_y

let label_instruct x y scale gs mode =
  Graphics.set_color black;
  Graphics.moveto 40 650;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "BATTLESHIP";
  ShipImages.draw_image "bin/images/battleship-pl.jpg" 320 660;
  moveto x y;

  (*Graphics.set_font()*)
  if gs.defending_player = gs.player_one && mode = 1 then (
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string "Pl2 Guesses: click to guess on Pl1")
  else if gs.defending_player = gs.player_two && mode = 1 then (
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string "Pl1 Guesses: click to guess on Pl2")
  else (
    Graphics.set_font
      "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string "Click to guess against the Computer")

let label_boards x y scale gs mode =
  moveto x y;
  Graphics.set_text_size (scale * 2);
  (*Graphics.set_font()*)
  if gs.defending_player = gs.player_one && mode = 1 then begin
    Graphics.draw_string "Player Two Ships";
    moveto (x + 620) y;
    Graphics.draw_string "Player Two Guess"
  end
  else if gs.defending_player = gs.player_two && mode = 1 then begin
    Graphics.draw_string "Player One Ships";
    moveto (x + 620) y;
    Graphics.draw_string "Player One Guess"
  end
  else begin
    Graphics.draw_string "Your Ships";
    moveto (x + 620) y;
    Graphics.draw_string "Your Guesses"
  end

let rec mouse_guess scale width height off_x off_y game_state =
  print_endline "Please click on the square you want to guess";
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  let y = event.Graphics.mouse_y in
  if x > off_x && x < off_x + width && y > off_y && y < off_y + height
  then guess game_state ((x - off_x) / scale, (y - off_y) / scale)
  else (
    print_endline "Please click on the guess board";
    mouse_guess scale width height off_x off_y game_state)

let rec run_game game_state =
  Graphics.clear_graph ();
  let scale = 54 in
  let width = scale * 10 in
  let height = 540 in
  let off_x = 20 in
  let off_y = 20 in
  BoardMaker.make_grid 20 20;
  BoardMaker.make_grid 640 20;
  (*make_board scale off_x 0; make_board scale off_x off_y;*)
  display_offence game_state scale off_x 20;
  display_hits game_state scale 640 off_y;
  display_misses game_state scale 640 off_y;
  label_instruct 20 605 500 game_state 1;
  label_boards 20 570 scale game_state 1;
  if game_state.player_one.has_lost then (
    print_endline "Player One Loses!";
    Screens.end_screen 2)
  else if game_state.player_two.has_lost then (
    print_endline "Player Two Loses!";
    Screens.end_screen 1)
  else (
    if game_state.defending_player = game_state.player_one then
      print_endline "It is Player Two's turn to guess"
    else print_endline "It is Player One's turn to guess";
    let state = mouse_guess scale width height 640 20 game_state in
    let loss = state.player_two.has_lost || state.player_one.has_lost in
    if game_state.defending_player = game_state.player_one && not loss
    then Screens.transition "Pass to player 1!"
    else if not loss then Screens.transition "Pass to player 2!";
    run_game state)

let hit_list_from_board board = board.hits_against
let miss_list_from_board board = board.misses_against

let rec run_game_singleplayer game_state =
  Graphics.clear_graph ();
  let scale = 54 in
  let width = scale * 10 in
  let height = 540 in
  let off_x = 20 in
  let off_y = 20 in
  BoardMaker.make_grid 20 20;
  BoardMaker.make_grid 640 20;
  (*make_board scale off_x 0; make_board scale off_x off_y;*)
  display_offence game_state scale off_x 20;
  display_hits game_state scale 640 off_y;
  display_misses game_state scale 640 off_y;
  label_instruct 20 605 500 game_state 0;
  label_boards 20 570 scale game_state 0;
  if game_state.player_one.has_lost then (
    print_endline "You won!";
    Screens.end_screen 3)
  else if game_state.player_two.has_lost then (
    print_endline "You lost";
    Screens.end_screen 4)
  else
    let player_guess_board =
      mouse_guess scale width height 640 20 game_state
    in
    let ai_guess_board =
      guess player_guess_board
        (AiSystem.guess_ai 10
           (hit_list_from_board player_guess_board.defending_player)
           (miss_list_from_board player_guess_board.defending_player))
    in
    run_game_singleplayer ai_guess_board

let draw_row_guess hits_list missing_list row =
  for x = 0 to 10 do
    if List.mem (x, row) hits_list then print_string "| X "
    else if List.mem (x, row) missing_list then print_string "| O "
    else print_string "|   "
  done;
  print_endline "|"

let list_of_ships (board : player_board) =
  let rec take_list (ships : ship list) acc =
    match ships with
    | h :: tail -> take_list tail (h.position :: acc)
    | [] -> acc
  in

  take_list board.ships []

let draw_row_ships hits_list missing_list ships_list row =
  for x = 0 to 10 do
    if List.mem (x, row) hits_list then print_string "|█X█"
    else if
      List.fold_right
        (fun y acc -> List.mem (x, row) y && acc)
        ships_list true
    then print_string "|███"
    else if List.mem (x, row) missing_list then print_string "| O "
    else print_string "|   "
  done;
  print_endline "|"

let make_guess_board_text game_state =
  for x = 0 to 10 do
    print_endline "---------------------------------------------";
    draw_row_guess game_state.defending_player.hits_against
      game_state.defending_player.misses_against x
  done;
  print_endline "---------------------------------------------"

let get_offence game_state =
  if game_state.defending_player = game_state.player_two then
    game_state.player_one
  else game_state.player_two

let make_ship_board_text game_state =
  for x = 0 to 10 do
    print_endline "---------------------------------------------";
    draw_row_ships (get_offence game_state).hits_against
      (get_offence game_state).misses_against
      (list_of_ships (get_offence game_state))
      x
  done;
  print_endline "---------------------------------------------"

let print_transition =
  for x = 0 to 50 do
    print_endline "."
  done

let rec run_game_text game_state =
  if game_state.player_one.has_lost then
    print_endline "Player One Loses!"
  else if game_state.player_two.has_lost then
    print_endline "Player Two Loses!"
  else if game_state.defending_player = game_state.player_two then (
    print_endline "It is Player One's turn to guess";
    print_endline "Player One's guess board: ";
    make_guess_board_text game_state;
    print_endline "Player One's ship board: ";
    make_ship_board_text game_state)
  else if game_state.defending_player = game_state.player_one then (
    print_endline "It is Player Two's turn to guess";
    print_endline "Player Two's guess board: ";
    make_guess_board_text game_state;
    print_endline "Player Two's ship board: ";
    make_ship_board_text game_state)
  else failwith "not correct board";
  print_endline "Enter your guess in the format (x,y)";
  match read_input_list () with
  | [ x; y ] ->
      let current_game =
        guess game_state (int_of_string x, int_of_string y)
      in
      print_transition;
      run_game_text current_game
  | _ ->
      print_transition;
      run_game_text game_state
