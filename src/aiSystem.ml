open Random

let rec generate_ships grid_size ships =
  match ships with
  | [] -> []
  | h :: t ->
      if h > 5 then failwith "incorrect ship size"
      else
        let orientation = Random.int 1 in
        let x = Random.int grid_size in
        let y = Random.int grid_size in
        if orientation = 0 then
          if x + (h - 1) < grid_size then
            if
              GraphicsMenuhf.check_overlap
                (AiSystemhf.generate_ship_x x y h)
                (generate_ships grid_size t)
            then
              AiSystemhf.generate_ship_x x y h
              :: generate_ships grid_size t
            else generate_ships grid_size ships
          else generate_ships grid_size ships
        else if y + (h - 1) < grid_size then
          if
            GraphicsMenuhf.check_overlap
              (AiSystemhf.generate_ship_y x y h)
              (generate_ships grid_size t)
          then
            AiSystemhf.generate_ship_y x y h
            :: generate_ships grid_size t
          else generate_ships grid_size ships
        else generate_ships grid_size ships

let rec generate_random_point grid_size hit_lst miss_lst : int * int =
  let pnt = (Random.int grid_size, Random.int grid_size) in
  if
    AiSystemhf.check_already_placed_or_outside grid_size pnt hit_lst
    || AiSystemhf.check_already_placed_or_outside grid_size pnt miss_lst
  then generate_random_point grid_size hit_lst miss_lst
  else pnt

let rec hit_from_hit_list grid_size hit_lst miss_lst hits_list2 :
    int * int =
  if Random.int 2 < 1 then
    generate_random_point grid_size hit_lst miss_lst
  else
    match hits_list2 with
    | (x, y) :: tail ->
        let pnt1 = (x + 1, y) in
        if
          AiSystemhf.check_already_placed_or_outside grid_size pnt1
            hit_lst
          || AiSystemhf.check_already_placed_or_outside grid_size pnt1
               miss_lst
        then
          let pnt2 = (x - 1, y) in
          if
            AiSystemhf.check_already_placed_or_outside grid_size pnt2
              hit_lst
            || AiSystemhf.check_already_placed_or_outside grid_size pnt2
                 miss_lst
          then
            let pnt3 = (x, y + 1) in
            if
              AiSystemhf.check_already_placed_or_outside grid_size pnt3
                hit_lst
              || AiSystemhf.check_already_placed_or_outside grid_size
                   pnt3 miss_lst
            then
              let pnt4 = (x, y - 1) in
              if
                AiSystemhf.check_already_placed_or_outside grid_size
                  pnt4 hit_lst
                || AiSystemhf.check_already_placed_or_outside grid_size
                     pnt4 miss_lst
              then hit_from_hit_list grid_size hit_lst miss_lst tail
              else pnt4
            else pnt3
          else pnt2
        else pnt1
    | [] -> generate_random_point grid_size hit_lst miss_lst

let rec guess_ai grid_size hit_lst (miss_lst : (int * int) list) :
    int * int =
  let x, y =
    if List.length hit_lst = 0 then
      generate_random_point grid_size hit_lst miss_lst
    else hit_from_hit_list grid_size hit_lst miss_lst hit_lst
  in
  print_int x;
  print_int y;

  (x, y)
(* let rec guess_ai grid_size hit_lst (miss_lst : (int * int) list) :
   int * int = let x, y = if hit_lst = [] && miss_lst = [] then
   (Random.int grid_size, Random.int grid_size) else if List.length
   hit_lst = 0 && List.length miss_lst > 0 then let pnt = (Random.int
   grid_size, Random.int grid_size) in if
   AiSystemhf.check_already_placed_or_outside grid_size pnt miss_lst
   then guess_ai grid_size hit_lst miss_lst else pnt else
   hit_from_hit_list grid_size hit_lst miss_lst hit_lst in print_int x;
   print_int y; (x, y) *)

(* let rec guess_ai2 grid_size hit_lst miss_lst : int * int = if
   (List.length hit_lst) = 0 && List.length miss_lst = 0 then *)
