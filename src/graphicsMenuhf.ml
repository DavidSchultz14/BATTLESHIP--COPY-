let up_y_left_x_3 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 + 1 = grid_size then [ (x, y - 1); (x, y); (x, y2) ]
    else [ (x, y); (x2, y2); (x, y2 + 1) ]
  else if x2 + 1 = grid_size then [ (x - 1, y); (x, y); (x2, y) ]
  else [ (x, y); (x2, y); (x2 + 1, y) ]

let up_y_left_x_4 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 + 1 = grid_size then
      [ (x, y - 2); (x, y - 1); (x, y); (x, y2) ]
    else if y2 + 2 = grid_size then
      [ (x, y - 1); (x, y); (x2, y2); (x, y2 + 1) ]
    else [ (x, y); (x2, y2); (x, y2 + 1); (x, y2 + 2) ]
  else if x2 + 1 = grid_size then
    [ (x - 2, y); (x - 1, y); (x, y); (x2, y2) ]
  else if x2 + 2 = grid_size then
    [ (x - 1, y); (x, y); (x2, y2); (x2 + 1, y) ]
  else [ (x, y); (x2, y2); (x2 + 1, y); (x2 + 2, y) ]

let up_y_left_x_5 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 + 1 = grid_size then
      [ (x, y - 3); (x, y - 2); (x, y - 1); (x, y); (x, y + 1) ]
    else if y2 + 2 = grid_size then
      [ (x, y - 2); (x, y - 1); (x, y); (x2, y + 1); (x, y + 2) ]
    else if y2 + 3 = grid_size then
      [ (x, y - 1); (x, y); (x2, y2); (x2, y2 + 1); (x2, y2 + 2) ]
    else [ (x, y); (x2, y + 1); (x2, y + 2); (x2, y + 3); (x2, y + 4) ]
  else if x2 + 1 = grid_size then
    [ (x - 3, y); (x - 2, y); (x - 1, y); (x, y); (x + 1, y2) ]
  else if x2 + 2 = grid_size then
    [ (x - 2, y); (x - 1, y); (x, y); (x + 1, y2); (x + 2, y) ]
  else if x2 + 3 = grid_size then
    [ (x - 1, y); (x, y); (x2, y2); (x2 + 1, y); (x2 + 2, y) ]
  else [ (x, y); (x + 1, y2); (x + 2, y); (x + 3, y); (x + 4, y) ]

let down_y_right_x_3 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 - 1 = -1 then [ (x, y2); (x, y); (x, y + 1) ]
    else [ (x, y2 - 1); (x, y2); (x, y) ]
  else if x2 - 1 = -1 then [ (x2, y); (x, y); (x + 1, y) ]
  else [ (x, y); (x2, y); (x2 - 1, y) ]

let down_y_right_x_4 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 - 1 = -1 then [ (x, y); (x, y2); (x, y + 1); (x, y + 2) ]
    else if y2 - 2 = -1 then
      [ (x, y); (x, y2); (x2, y2 - 1); (x, y + 1) ]
    else [ (x, y); (x2, y2); (x, y2 - 1); (x, y2 - 2) ]
  else if x2 - 1 = -1 then [ (x, y); (x2, y); (x + 1, y); (x + 2, y) ]
  else if x2 - 2 = -1 then [ (x2 - 1, y); (x, y); (x2, y2); (x + 1, y) ]
  else [ (x, y); (x2, y2); (x2 - 1, y); (x2 - 2, y) ]

let down_y_right_x_5 grid_size ((x, y), (x2, y2)) =
  if x = x2 then
    if y2 - 1 = -1 then
      [ (x, y + 3); (x, y + 2); (x, y + 1); (x, y); (x, y2) ]
    else if y2 - 2 = -1 then
      [ (x, y + 2); (x, y + 1); (x, y); (x2, y - 1); (x, y - 2) ]
    else if y2 - 3 = -1 then
      [ (x, y - 3); (x, y - 1); (x2, y); (x2, y - 2); (x2, y + 1) ]
    else [ (x, y - 4); (x2, y - 3); (x2, y - 1); (x2, y - 2); (x2, y) ]
  else if x2 - 1 = -1 then
    [ (x, y); (x + 1, y); (x + 2, y); (x - 1, y); (x + 3, y2) ]
  else if x2 - 2 = -1 then
    [ (x - 2, y); (x - 1, y); (x, y); (x + 1, y2); (x + 2, y) ]
  else if x2 - 3 = -1 then
    [ (x - 3, y); (x, y); (x - 2, y2); (x - 1, y); (x + 1, y) ]
  else [ (x, y); (x - 1, y2); (x - 2, y); (x - 3, y); (x - 4, y) ]

let place_ships grid_size ship_size ((x, y), (x2, y2)) =
  if x < x2 || y < y2 then
    match ship_size with
    | 2 -> [ (x, y); (x2, y2) ]
    | 3 -> up_y_left_x_3 grid_size ((x, y), (x2, y2))
    | 4 -> up_y_left_x_4 grid_size ((x, y), (x2, y2))
    | 5 -> up_y_left_x_5 grid_size ((x, y), (x2, y2))
    | _ -> failwith "place_ships given invalid input"
  else
    match ship_size with
    | 2 -> [ (x, y); (x2, y2) ]
    | 3 -> down_y_right_x_3 grid_size ((x, y), (x2, y2))
    | 4 -> down_y_right_x_4 grid_size ((x, y), (x2, y2))
    | 5 -> down_y_right_x_5 grid_size ((x, y), (x2, y2))
    | _ -> failwith "place_ships given invalid input"

let rec check_adjacency (ship : (int * int) list) =
  match
    List.sort
      (fun (k1, v1) (k2, v2) ->
        if k1 = k2 then compare v1 v2 else compare k1 k2)
      ship
  with
  | [ (x, y); (x1, y1); (x2, y2); (x3, y3); (x4, y4) ] ->
      (x + 1 = x1 && x + 2 = x2 && x + 3 = x3 && x + 4 = x4)
      && y = y1 && y = y2 && y = y3 && y = y4
      || (x - 1 = x1 && x - 2 = x2 && x - 3 = x3 && x - 4 = x4)
         && y = y1 && y = y2 && y = y3 && y = y4
      || (y + 1 = y1 && y + 2 = y2 && y + 3 = y3 && y + 4 = y4)
         && x = x1 && x = x2 && x = x3 && x = x4
      || (y - 1 = y1 && y - 2 = y2 && y - 3 = y3 && y - 4 = y4)
  | [ (x, y); (x1, y1); (x2, y2); (x3, y3) ] ->
      (x + 1 = x1 && x + 2 = x2 && x + 3 = x3)
      && y = y1 && y = y2 && y = y3
      || (x - 1 = x1 && x - 2 = x2 && x - 3 = x3)
         && y = y1 && y = y2 && y = y3
      || (y + 1 = y1 && y + 2 = y2 && y + 3 = y3)
         && x = x1 && x = x2 && x = x3
      || (y - 1 = y1 && y - 2 = y2 && y - 3 = y3)
         && x = x1 && x = x2 && x = x3
  | [ (x, y); (x1, y1); (x2, y2) ] ->
      ((x + 1 = x1 && x + 2 = x2) && y = y1 && y = y2)
      || ((x - 1 = x1 && x - 2 = x2) && y = y1 && y = y2)
      || ((y + 1 = y1 && y + 2 = y2) && x = x1 && x = x2)
      || ((y - 1 = y1 && y - 2 = y2) && x = x1 && x = x2)
  | [ (x, y); (x1, y1) ] ->
      (x + 1 = x1 && y = y1)
      || (x - 1 = x1 && y = y1)
      || (y + 1 = y1 && x = x1)
      || (y - 1 = y1 && x = x1)
  | _ -> failwith "invalid ship size"

let rec check_equal_points (point : int * int) (lst : (int * int) list)
    =
  match lst with
  | h :: tail ->
      (if h = point then false else true)
      && check_equal_points point tail
  | [] -> true

let rec check_same points lst =
  match points with
  | h :: tail -> check_equal_points h lst && check_same tail lst
  | [] -> true

let rec check_overlap
    (ship : (int * int) list)
    (ships : (int * int) list list) =
  match ships with
  | h :: tail -> check_same ship h && check_overlap ship tail
  | [] -> true
