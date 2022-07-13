open Graphics

let rec make_row x_offset y_offset rows scale width =
  match rows with
  | 0 ->
      moveto x_offset ((rows * scale) + y_offset);
      lineto (x_offset + width) ((rows * scale) + y_offset)
  | _ ->
      moveto x_offset ((rows * scale) + y_offset);
      lineto (x_offset + width) ((rows * scale) + y_offset);
      make_row x_offset y_offset (rows - 1) scale width

let rec make_column x_offset y_offset columns scale height =
  match columns with
  | 0 ->
      moveto (x_offset + (columns * scale)) y_offset;
      lineto (x_offset + (columns * scale)) (height + y_offset)
  | _ ->
      moveto (x_offset + (columns * scale)) y_offset;
      lineto (x_offset + (columns * scale)) (height + y_offset);
      make_column x_offset y_offset (columns - 1) scale height

let make_grid x y =
  (*Graphics.open_graph " 540x540";*)
  make_row x y 10 54 540;
  make_column x y 10 54 540

let draw_square x y scale color off_x off_y =
  Graphics.draw_rect
    ((x * scale) + off_x)
    ((y * scale) + off_y)
    scale scale;
  Graphics.set_color color;
  Graphics.fill_rect
    ((x * scale) + off_x)
    ((y * scale) + off_y)
    scale scale
