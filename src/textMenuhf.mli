(**non-io helper functions for textmenu*)

val check_adjacent : int * int -> (int * int) list -> bool
(**[check_adjacent] takes in a a point and checks if it is adjacent to a
   ship a point list*)

val check_dimensions : int * int -> int -> int -> bool
(**[check_dimenstions]* checks if a point is inside certain dimensions*)
