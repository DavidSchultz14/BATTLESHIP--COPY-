(**main game*)

type game
(**[game] the type that is used for the game running. Encapsulated in
   this file*)

type point = int * int
(**[point] self explanitory, touple of ints representing x and y
   coordinates*)

val make_game : point list list * point list list -> game
(**[make_game]takes in a touple of point list lists which represent the
   lists of ships for player1 and player two*)

val run_game : game -> unit
(**[run_game] runs the gui-based battleship game*)

val run_game_singleplayer : game -> unit
(**[run_game_singleplayer] runs the gui-based singleplayer battleship
   game*)

val run_game_text : game -> unit
(**[run_game_text] runs the text-based battleship game*)
