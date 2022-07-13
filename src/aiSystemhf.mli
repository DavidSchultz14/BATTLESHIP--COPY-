(**helper functions for the ai system for the single player game*)

val check_already_placed_or_outside :
  int -> int * int -> (int * int) list -> bool
(** [check_already_placed_or_outside] takes in a grid size and checks if
    a point is already placed in an int*int list**)

val generate_ship_y : int -> int -> int -> (int * int) list
(** [generate_ship_y] takes in a starting point (first two int inputs)
    and a size and gives a ship in y direction*)

val generate_ship_x : int -> int -> int -> (int * int) list
(** [generate_ship_x] takes in a starting point (first two int inputs)
    and a size and gives a ship in x direction*)
