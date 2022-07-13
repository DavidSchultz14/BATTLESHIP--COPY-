let rec check_already_placed_or_outside grid_size (x, y) miss_lst : bool
    =
  match miss_lst with
  | (x1, y1) :: tail ->
      if x = x1 && y = y1 then true
      else if x < 0 || x > grid_size - 1 || y < 0 || y > grid_size - 1
      then true
      else check_already_placed_or_outside grid_size (x, y) tail
  | [] -> false

let rec generate_ship_y x y size =
  match size with
  | 0 -> []
  | size -> (x, y) :: generate_ship_y x (y + 1) (size - 1)

let rec generate_ship_x x y size =
  match size with
  | 0 -> []
  | size -> (x, y) :: generate_ship_x (x + 1) y (size - 1)
