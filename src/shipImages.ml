open Jpeg
open Images
open Graphic_image

let draw_image image_file x y =
  let img = Jpeg.load image_file [] in
  let color_matrix = Graphic_image.of_image img in
  Graphics.draw_image color_matrix x y

let ship_orientation ship =
  match ship with
  | (x1, y1) :: (x2, y2) :: t -> if y1 = y2 then "h" else "v"
  | _ -> "n"

let display_ship_image ship x y =
  match List.length ship with
  | 5 ->
      if ship_orientation ship = "v" then
        draw_image "bin/images/carrier-v.jpg" x y
      else draw_image "bin/images/carrier-h.jpg" x y
  | 4 ->
      if ship_orientation ship = "v" then
        draw_image "bin/images/battleship-v.jpg" x y
      else draw_image "bin/images/battleship-h.jpg" x y
  | 3 ->
      if ship_orientation ship = "v" then
        draw_image "bin/images/destroyer-v.jpg" x y
      else draw_image "bin/images/destroyer-h.jpg" x y
  | 2 ->
      if ship_orientation ship = "v" then
        draw_image "bin/images/patrol-v.jpg" x y
      else draw_image "bin/images/patrol-h.jpg" x y
  | _ -> ()

let sort_ship ship =
  List.sort
    (fun (x1, y1) (x2, y2) ->
      if x1 = x2 then compare y1 y2 else compare x1 x2)
    ship

let rec display_ships_images acc scale offset_x offset_y =
  match acc with
  | [] -> ()
  | head :: tail -> (
      let sorted_ship = sort_ship head in
      match sorted_ship with
      | (x, y) :: t ->
          display_ship_image sorted_ship
            ((x * scale) + offset_x)
            ((y * scale) + offset_y);
          display_ships_images tail scale offset_x offset_y
      | _ -> ())
