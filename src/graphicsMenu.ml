open Graphics
open Jpeg
open Images

let check_for_overlap_adjacency
    (ship : (int * int) list)
    (ships : (int * int) list list) =
  GraphicsMenuhf.check_adjacency ship
  && GraphicsMenuhf.check_overlap ship ships

type button = {
  top_right : int * int;
  bottom_left : int * int;
  name : string;
  size : int;
}

let ship_info =
  [
    ("Carrier", 5);
    ("Battleship", 4);
    ("Destroyer", 3);
    ("Cruiser", 3);
    ("Patrol", 2);
  ]

let rec create_button_list button_x button_y ship_info y_offset :
    button list =
  match ship_info with
  | [] -> []
  | (ship_name, ship_size) :: t ->
      {
        top_right = (button_x + (ship_size * 40), button_y + 40);
        bottom_left = (button_x, button_y);
        name = ship_name;
        size = ship_size;
      }
      :: create_button_list button_x
           (button_y + 40 + y_offset)
           t y_offset

(* [create buttons] takes in the format for a button and a button list
   and creates the buttons on the display*)
let rec create_buttons (button_list : button list) =
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  match button_list with
  | [] -> ()
  | {
      top_right = top_right_x, top_right_y;
      bottom_left = bottom_left_x, bottom_left_y;
      name = n;
      size = s;
    }
    :: t ->
      Graphics.set_text_size ((top_right_y - bottom_left_y) / 2);
      if s = 5 then
        ShipImages.draw_image "bin/images/carrier-pl.jpg" bottom_left_x
          bottom_left_y
      else if s = 4 then
        ShipImages.draw_image "bin/images/battleship-pl.jpg"
          bottom_left_x bottom_left_y
      else if s = 3 then
        ShipImages.draw_image "bin/images/destroyer-pl.jpg"
          bottom_left_x bottom_left_y
      else
        ShipImages.draw_image "bin/images/patrol-pl.jpg" bottom_left_x
          bottom_left_y;
      create_buttons t

let rec plot_point_2 button grid_x grid_y grid_size scale point1 =
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  let y = event.Graphics.mouse_y in
  if
    x > grid_x
    && x < grid_x + grid_size
    && y > grid_y
    && y < grid_y + grid_size
  then
    let point2 = ((x - grid_x) / scale, (y - grid_y) / scale) in
    GraphicsMenuhf.place_ships 10 button.size (point1, point2)
  else plot_point_2 button grid_x grid_y grid_size scale point1

let rec ship_placement (button : button) grid_x grid_y grid_size scale =
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  let y = event.Graphics.mouse_y in
  if
    x > grid_x
    && x < grid_x + grid_size
    && y > grid_y
    && y < grid_y + grid_size
  then
    let point1 = ((x - grid_x) / scale, (y - grid_y) / scale) in
    plot_point_2 button grid_x grid_y grid_size scale point1
  else ship_placement button grid_x grid_y grid_size scale

let rec draw_base_ship (ship : (int * int) list) scale off_x off_y =
  match ship with
  | (x, y) :: tail ->
      BoardMaker.draw_square x y scale 0x707070 off_x off_y;
      draw_base_ship tail scale off_x off_y
  | [] -> ()

(**[display_ships] displays the ships as grey cubes. Only used for
   debugging ship placement and adjacency testing. if these squares
   aren't hidden underneath an image something is very wrong*)
let rec draw_base_ships ships scale off_x off_y =
  match ships with
  | [] -> ()
  | h :: tail ->
      draw_base_ship h scale off_x off_y;
      draw_base_ships tail scale off_x off_y

let rec remove_clicked button_list (x, y) acc =
  match button_list with
  | [] -> acc @ button_list
  | { top_right = xr, yr; bottom_left = xl, yl; name = n; size = s }
    :: t ->
      if x < xr && x > xl && y < yr && y > yl then acc @ t
      else
        remove_clicked t (x, y)
          ({
             top_right = (xr, yr);
             bottom_left = (xl, yl);
             name = n;
             size = s;
           }
          :: acc)

(* [do_button_click] creates a ship by taking in click input*)
let rec do_button_click (button_list : button list) (x, y) =
  match button_list with
  | [] -> None
  | { top_right = xr, yr; bottom_left = xl, yl; name = n; size = s }
    :: t ->
      if x < xr && x > xl && y < yr && y > yl then
        Some
          (ship_placement
             {
               top_right = (xr, yr);
               bottom_left = (xl, yl);
               name = n;
               size = s;
             }
             330 0 540 54)
      else do_button_click t (x, y)

(**[button_click] runs the FULL MENU. *)
let rec button_click
    (tag : string)
    (button_list : button list)
    (acc : (int * int) list list) =
  Graphics.clear_graph ();
  BoardMaker.make_grid 330 0;
  create_buttons button_list;
  draw_base_ships acc 54 330 0;
  (*^ hidden behind the ship images, used for debug*)
  ShipImages.display_ships_images acc 54 330 0;
  Graphics.set_color black;
  Graphics.moveto 40 650;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "BATTLESHIP";
  ShipImages.draw_image "bin/images/battleship-pl.jpg" 320 660;
  Graphics.moveto 40 600;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string tag;

  match button_list with
  | [] -> acc
  | lst -> (
      let event = Graphics.wait_next_event [ Graphics.Button_down ] in
      let x = event.Graphics.mouse_x in
      let y = event.Graphics.mouse_y in

      match do_button_click lst (x, y) with
      | None -> button_click tag button_list acc
      | Some s ->
          if check_for_overlap_adjacency s acc then
            button_click tag (remove_clicked lst (x, y) []) (s :: acc)
          else button_click tag button_list acc)

let players_ships () : (int * int) list list * (int * int) list list =
  Graphics.open_graph " 1200x720";
  BoardMaker.make_grid 330 0;
  let buttons =
    (* make buttons that button_click uses*)
    create_button_list 20 20 ship_info 10
  in
  let p1_ships =
    button_click "Player 1, Place your Ships on the Board!" buttons []
  in
  print_int (List.length p1_ships);
  Screens.transition "Pass to player 2!";
  let p2_ships =
    button_click "Player 2, Place your Ships on the Board!" buttons []
  in
  Screens.ready_screen ();
  print_int (List.length p1_ships);
  (p1_ships, p2_ships)

let player1_ships () : (int * int) list list * (int * int) list list =
  Graphics.open_graph " 1200x720";
  BoardMaker.make_grid 330 0;
  let buttons =
    (* make buttons that button_click uses*)
    create_button_list 20 20 ship_info 10
  in
  let p1_ships =
    button_click "Player 1, Place your Ships on the Board!" buttons []
  in
  print_int (List.length p1_ships);
  let rand_ships = AiSystem.generate_ships 10 [ 5; 4; 3; 3; 2 ] in
  Screens.ready_screen ();
  (rand_ships, p1_ships)
