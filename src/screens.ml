open Graphics

let rec start_menu _ : bool =
  Graphics.clear_graph ();
  Graphics.moveto 375 400;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Choose your Gamemode!";
  Graphics.draw_rect 250 200 300 60;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto 265 215;
  Graphics.draw_string "Two Player Mode";
  Graphics.draw_rect 650 200 300 60;
  Graphics.moveto 665 215;
  Graphics.draw_string "One Player Mode";
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  x < 600

let wait_for_click () =
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  let y = event.Graphics.mouse_y in
  x + y

let transition input =
  Graphics.clear_graph ();
  Graphics.moveto 465 325;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--35-*-*-*-*-*-iso8859-1";
  Graphics.draw_string input;
  Graphics.draw_rect 450 200 300 60;
  Graphics.moveto 475 215;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Click to Continue!";
  ignore (wait_for_click ())

let welcome_screen _ : bool =
  Graphics.open_graph " 1200x720";
  Graphics.clear_graph ();
  ShipImages.draw_image "bin/images/battleship-h.jpg" 492 500;
  Graphics.moveto 375 400;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Welcome to Battleship";
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_rect 450 200 300 60;
  Graphics.moveto 475 215;
  Graphics.draw_string "Click to Continue!";
  Graphics.moveto 0 0;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  Graphics.draw_string
    "Made in 2022 by; Alexander Mustafich-Nutley, Nico Haam, David \
     Schultz, Ryan Chan";
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  let x = event.Graphics.mouse_x in
  x < 1200

let place_instructions () =
  Graphics.clear_graph ();
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto 20 680;
  Graphics.draw_string "Welcome to the game!";
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  Graphics.moveto 50 640;
  Graphics.draw_string " - First, you need to place your ships.";
  Graphics.moveto 50 600;
  Graphics.draw_string
    " - To do this, click on the button for the ship you want to place.";
  Graphics.moveto 50 560;
  Graphics.draw_string
    " - Then click the square you would like your ship to start at.";
  Graphics.moveto 50 520;
  Graphics.draw_string
    " - Finally, click any of the squares directly next to the start \
     position to point the ship in that direction.";
  Graphics.moveto 50 480;
  Graphics.draw_string
    " - Congrats, you have placed a ship. Repeat for the rest of your \
     ships!";
  Graphics.draw_rect 450 200 300 60;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto 475 215;
  Graphics.draw_string "Click to Continue!";
  ignore (wait_for_click ())

let ready_screen () =
  Graphics.clear_graph ();
  ShipImages.draw_image "bin/images/battleship-h.jpg" 492 500;
  Graphics.moveto 420 400;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Ready to Play?";
  Graphics.moveto 300 350;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Click on the Guess Board to make your Guess!";
  Graphics.draw_rect 450 200 300 60;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto 475 215;
  Graphics.draw_string "Click to Continue!";
  ignore (wait_for_click ())

let end_screen (winner : int) =
  Graphics.clear_graph ();
  ShipImages.draw_image "bin/images/battleship-h.jpg" 492 500;
  Graphics.moveto 420 400;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  if winner = 1 then Graphics.draw_string "Player One Wins!"
  else if winner = 2 then Graphics.draw_string "Player Two Wins!"
  else if winner = 3 then (
    Graphics.moveto 380 400;
    Graphics.draw_string "You Beat the Computer!")
  else (
    Graphics.moveto 380 400;
    Graphics.draw_string "You Lost to Computer");
  Graphics.draw_rect 450 200 300 60;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.moveto 300 350;
  Graphics.set_font
    "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  Graphics.draw_string "Thank you for playing this game of Battleship!";
  Graphics.moveto 475 215;
  Graphics.draw_string "Click to End Game";
  ignore (wait_for_click ())
