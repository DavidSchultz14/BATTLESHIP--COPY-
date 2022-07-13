(**text-based menu for the game*)

val players_ships :
  unit -> (int * int) list list * (int * int) list list
(**[players_ships] runs a basic text-based menu and generates a touple
   of ship list list so that a playerboard can be generated*)
