open Game
(** [main ()] prompts for the game to play, then starts it. *)

let rec read_gamemode () =
  match read_line () with
  | "0" -> true
  | "1" -> false
  | _ ->
      print_endline "Bad format, try again";
      read_gamemode ()

let main () =
  print_endline
    "Enter in a '1' to play in graphical mode and enter in a '0' to \
     play in text-based mode";
  match read_gamemode () with
  | true ->
      Game.TextMenu.players_ships ()
      |> Game.Battleship.make_game |> Game.Battleship.run_game_text
  | false ->
      if Screens.welcome_screen () then
        if Screens.start_menu () then
          Screens.place_instructions ()
          |> Game.GraphicsMenu.players_ships
          |> Game.Battleship.make_game |> Game.Battleship.run_game
        else
          Screens.place_instructions ()
          |> GraphicsMenu.player1_ships |> Game.Battleship.make_game
          |> Game.Battleship.run_game_singleplayer

(* Execute the game engine. *)
let () = main ()