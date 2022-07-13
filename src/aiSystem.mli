(**ai system for the single player game*)

val generate_ships : int -> int list -> (int * int) list list
(**[generate_ships] takes in a int grid size a list of ships and gives a
   list of ships*)

val guess_ai : int -> (int * int) list -> (int * int) list -> int * int

(**[guess] takes in a grid size, a list of hits and a list of misses and
   gives a point*)
