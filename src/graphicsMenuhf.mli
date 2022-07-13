(** helper functions for the graphical menu application*)

val place_ships :
  int -> int -> (int * int) * (int * int) -> (int * int) list
(**[place_ships] takes in two adjacent (int*int) points and generates 5
   more points and adds them all into a list based on width of the grid
   given and the ship size given(the first two int args)*)

val check_overlap : (int * int) list -> (int * int) list list -> bool
(**[check_overlap] takes in a int*int list and checks if it has any
   points in common with a list of int*int list. returns false if there
   is overlap returns true if none*)

val check_adjacency : (int * int) list -> bool
(**[check_adjacency] take in a int*int list and checks if all of the
   points represented are adacent*)
