let rec check_adjacent point ship_lst =
  match ship_lst with
  | [] -> false
  | (ship_x, ship_y) :: tail -> (
      match point with
      | pt_x, pt_y ->
          if
            pt_x + 1 = ship_x
            || pt_x - 1 = ship_x
            || pt_y + 1 = ship_y
            || pt_y - 1 = ship_y
          then true
          else check_adjacent point tail)

let check_dimensions point width height =
  match point with
  | x, y ->
      if x < width && x >= 0 && y < height && y >= 0 then true
      else false