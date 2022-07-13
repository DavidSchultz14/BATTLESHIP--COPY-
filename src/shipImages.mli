(**module for processing images and displaying them*)

val display_ships_images :
  (int * int) list list -> int -> int -> int -> unit
(** [display_ships_images] takes [acc], which is a list of ships, as an
    input and draws them on a grid board scaled according to [scale] and
    offset by [off_x] and [off_y] *)

val display_ship_image : ('a * 'b) list -> int -> int -> unit
(** [display_ships_image] takes in a list of points and then prints them
    as a ship offset by [off_x] and [off_y] *)

val sort_ship : ('a * 'b) list -> ('a * 'b) list
(** [sort_ship] sorts a ship's points in acending order*)

val draw_image : string -> int -> int -> unit
(** [draw_image] draws the image from [image_file] at position [x] [y]
    on the screen *)
