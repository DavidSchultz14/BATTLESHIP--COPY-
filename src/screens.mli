(**All of the transition screens, instruction screens, and start menu
   info. *)

val start_menu : unit -> bool
(**[start_menu] creates the screen that lets the player choose
   two-player or single player mode. Returns true if the two player
   button is clicked and returns false if the single player button is
   clicked*)

val transition : string -> unit
(**[transition] creates a transition screen, so that players can pass
   the game back and forth without revealing the other player's board.*)

val welcome_screen : 'a -> bool
(**[welcome_screen] creates the starting welcome screen. Returns true
   when the player wants to continue*)

val place_instructions : unit -> unit
(**[place_instructions] creates a screen that shows the player
   instructions on how to play the game *)

val ready_screen : unit -> unit
(**[ready_screen] creates a screen between the ship placement and game
   play stages of the game that proceeds when the player is ready*)

val end_screen : int -> unit
(**[end_screen] creates a screen at the end of a game notifying the
   players that the game has ended and displaying the results of the
   game*)
