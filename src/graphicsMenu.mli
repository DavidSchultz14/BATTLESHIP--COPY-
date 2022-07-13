(** graphical menu application*)

val players_ships :
  unit -> (int * int) list list * (int * int) list list
(** [players_ships] opens a graphical menu and generates a touple of
    ship list list so that a playerboard can be generated two player*)

val player1_ships :
  unit -> (int * int) list list * (int * int) list list
(** [player1_ships] opens a graphical menu and generates a touple of
    ship list list so that a playerboard can be generated single player*)
