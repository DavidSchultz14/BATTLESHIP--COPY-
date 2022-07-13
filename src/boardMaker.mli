(** creates the board for the main gamplay*)

val make_grid : int -> int -> unit
(**[make_grid] draws the grid at position [x] [y]*)

val draw_square : int -> int -> int -> int -> int -> int -> unit
(** [draw_square] draws a square of size [scale] and color [color] on
    coordinates [x] [y] on the grid board, with [off_x] and [off_y]
    offsets depending on the offset of the grid*)
