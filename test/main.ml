open OUnit2
open Game
(****************************************************)
(**** testing plan

  We manually tested every function that uses Input/Output, but the
  supporting code was important to test too. Our module structure allows
  for non-io specific functions to be put in a seperate module (see
  graphicsmenu and graphicsmenhf for example). This way, not only were
  they abstracted, we also could test them more effectively. We tested
  those non-io functions here, with a combination of black box and clear
  box tests. The modules that we OUnit tested are: AiSystemhf,
  GraphicsMenuhf, and TextMenuhf

  Modules like battleship and other io-based modules were tested
  manually. Additionally we added make bisect to the build targets so we
  could check code coverageto make sure we are properly testing through
  the functions

  We belive that this is an effective testing strategy because for the
  io-based functions we are able to see almost all bugs almost
  immediately by manual testing. However, we know that this would not be
  effective for the the functions that generate important gameplay data
  and check for correctness of gamestate. This is why we use the
  combination of both black box and clear box testing for the vital and
  complex peices of the codebase where edge cases are significant.*)

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_touple (x, y) =
  "\"" ^ string_of_int x ^ ", " ^ string_of_int y ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list]
   to get helpful output from OUnit. *)

(* *******************************************************************)
(* ********************  GraphicsMenuhf tests:  **********************)
(* *******************************************************************)
let place_ships_test_2 name pair1 pair2 size cpair1 cpair2 =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_touple)
    (GraphicsMenuhf.place_ships size 2 (pair1, pair2))
    [ cpair1; cpair2 ]

let place_ships_test_3 name pair1 pair2 size cpair1 cpair2 cpair3 =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_touple)
    (GraphicsMenuhf.place_ships size 3 (pair1, pair2))
    [ cpair1; cpair2; cpair3 ]

let place_ships_test_4 name pair1 pair2 size cpair1 cpair2 cpair3 cpair4
    =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_touple)
    (GraphicsMenuhf.place_ships size 4 (pair1, pair2))
    [ cpair1; cpair2; cpair3; cpair4 ]

let place_ships_test_5
    name
    pair1
    pair2
    size
    cpair1
    cpair2
    cpair3
    cpair4
    cpair5 =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_touple)
    (GraphicsMenuhf.place_ships size 5 (pair1, pair2))
    [ cpair1; cpair2; cpair3; cpair4; cpair5 ]

(*place_ships_tests are glass-box tests intending for full code coverage
  of place_ship in module GraphicsMenuhf. Testing only valid inputs.
  Used bisect for higher code coverage*)
let place_ships_tests =
  [
    place_ships_test_2 "placed vertically on board" (1, 2) (1, 3) 10
      (1, 2) (1, 3);
    place_ships_test_2 "placed vertically on board (opposite direction)"
      (1, 3) (1, 2) 10 (1, 2) (1, 3);
    place_ships_test_3
      "placed vertically on board (no edge cases, downwards)" (1, 3)
      (1, 2) 10 (1, 2) (1, 3) (1, 1);
    place_ships_test_3
      "placed vertically on board (no edge cases, upwards)" (1, 2)
      (1, 3) 10 (1, 2) (1, 3) (1, 4);
    place_ships_test_3
      "placed horizontally on board (no edge cases, rightwards)" (2, 1)
      (3, 1) 10 (2, 1) (3, 1) (4, 1);
    place_ships_test_3
      "placed horizontally on board (no edge cases, leftwards)" (3, 1)
      (2, 1) 10 (2, 1) (3, 1) (1, 1);
    place_ships_test_3
      "placed vertically on board (second point on edge case, \
       downwards)"
      (1, 1) (1, 0) 10 (1, 1) (1, 2) (1, 0);
    place_ships_test_3
      "placed vertically on board (second point on edge, upwards)"
      (1, 8) (1, 9) 10 (1, 7) (1, 8) (1, 9);
    place_ships_test_3
      "placed horizontally on board (second point on edge, rightwards)"
      (8, 2) (9, 2) 10 (8, 2) (9, 2) (7, 2);
    place_ships_test_3
      "placed horizontally on board (second point on edge, leftwards)"
      (1, 1) (0, 1) 10 (0, 1) (1, 1) (2, 1);
    place_ships_test_4
      "placed vertically on board (no edge cases, downwards)" (1, 3)
      (1, 2) 10 (1, 2) (1, 3) (1, 1) (1, 0);
    place_ships_test_4
      "placed vertically on board (no edge cases, upwards)" (1, 2)
      (1, 3) 10 (1, 2) (1, 3) (1, 4) (1, 5);
    place_ships_test_4
      "placed horizontally on board (no edge cases, rightwards)" (2, 1)
      (3, 1) 10 (2, 1) (3, 1) (4, 1) (5, 1);
    place_ships_test_4
      "placed horizontally on board (no edge cases, leftwards)" (3, 1)
      (2, 1) 10 (2, 1) (3, 1) (1, 1) (0, 1);
    place_ships_test_4
      "placed vertically on board (second point on edge, downwards)"
      (3, 1) (3, 0) 10 (3, 1) (3, 0) (3, 2) (3, 3);
    place_ships_test_4
      "placed vertically on board (second point on edge, upwards)"
      (3, 8) (3, 9) 10 (3, 8) (3, 9) (3, 7) (3, 6);
    place_ships_test_4
      "placed horozontally on board (second point on edge, rightwards)"
      (8, 8) (9, 8) 10 (8, 8) (9, 8) (7, 8) (6, 8);
    place_ships_test_4
      "placed horozontally on board (second point on edge, rightwards)"
      (1, 8) (0, 8) 10 (1, 8) (0, 8) (2, 8) (3, 8);
    place_ships_test_4
      "placed vertically on board (second point one away from edge, \
       downwards)"
      (3, 2) (3, 1) 10 (3, 1) (3, 0) (3, 2) (3, 3);
    place_ships_test_4
      "placed vertically on board (second point one away from edge, \
       upwards)"
      (3, 7) (3, 8) 10 (3, 8) (3, 9) (3, 7) (3, 6);
    place_ships_test_4
      "placed horozontally on board (second point one away from edge, \
       rightwards)"
      (7, 8) (8, 8) 10 (8, 8) (9, 8) (7, 8) (6, 8);
    place_ships_test_4
      "placed horozontally on board (second point one away from edge, \
       rightwards)"
      (2, 8) (1, 8) 10 (1, 8) (0, 8) (2, 8) (3, 8);
    (*place ships 5:*)
    place_ships_test_5
      "placed vertically on board (no edge cases, downwards)" (1, 4)
      (1, 3) 10 (1, 2) (1, 3) (1, 1) (1, 0) (1, 4);
    place_ships_test_5
      "placed vertically on board (no edge cases, upwards)" (1, 2)
      (1, 3) 10 (1, 2) (1, 3) (1, 4) (1, 5) (1, 6);
    place_ships_test_5
      "placed horizontally on board (no edge cases, rightwards)" (2, 1)
      (3, 1) 10 (2, 1) (3, 1) (4, 1) (5, 1) (6, 1);
    place_ships_test_5
      "placed horizontally on board (no edge cases, leftwards)" (4, 1)
      (3, 1) 10 (2, 1) (3, 1) (1, 1) (0, 1) (4, 1);
    place_ships_test_5
      "placed vertically on board (third point off edge, downwards)"
      (3, 1) (3, 0) 10 (3, 1) (3, 0) (3, 2) (3, 3) (3, 4);
    place_ships_test_5
      "placed vertically on board (third point off edge, upwards)"
      (3, 8) (3, 9) 10 (3, 8) (3, 9) (3, 7) (3, 6) (3, 5);
    place_ships_test_5
      "placed horozontally on board (third point off edge, rightwards)"
      (8, 8) (9, 8) 10 (8, 8) (9, 8) (7, 8) (6, 8) (5, 8);
    place_ships_test_5
      "placed horozontally on board (third point off edge, rightwards)"
      (1, 8) (0, 8) 10 (1, 8) (0, 8) (2, 8) (3, 8) (4, 8);
    place_ships_test_5
      "placed vertically on board (fourth point off of the edge, \
       downwards)"
      (3, 2) (3, 1) 10 (3, 1) (3, 0) (3, 2) (3, 3) (3, 4);
    place_ships_test_5
      "placed vertically on board (fourth point off of the edge, \
       upwards)"
      (3, 7) (3, 8) 10 (3, 8) (3, 9) (3, 7) (3, 6) (3, 5);
    place_ships_test_5
      "placed horozontally on board (fourth point off of the edge, \
       rightwards)"
      (7, 8) (8, 8) 10 (8, 8) (9, 8) (7, 8) (6, 8) (5, 8);
    place_ships_test_5
      "placed horozontally on board (fourth point off of the edge, \
       leftwards)"
      (2, 8) (1, 8) 10 (1, 8) (0, 8) (2, 8) (3, 8) (4, 8);
    (* *)
    place_ships_test_5
      "placed vertically on board (fifth point off of the edge, \
       downwards)"
      (3, 3) (3, 2) 10 (3, 1) (3, 0) (3, 2) (3, 3) (3, 4);
    place_ships_test_5
      "placed vertically on board (fifth point off of the edge, \
       upwards)"
      (3, 6) (3, 7) 10 (3, 8) (3, 9) (3, 7) (3, 6) (3, 5);
    place_ships_test_5
      "placed horozontally on board (fifth point off of the edge, \
       rightwards)"
      (6, 8) (7, 8) 10 (8, 8) (9, 8) (7, 8) (6, 8) (5, 8);
    place_ships_test_5
      "placed horozontally on board (fifth point off of the edge, \
       leftwards) -BORKED- FIXED"
      (3, 8) (2, 8) 10 (1, 8) (0, 8) (2, 8) (3, 8) (4, 8);
  ]

let check_adjacency_test_2_true name point1 point2 =
  name >:: fun _ ->
  assert_equal (GraphicsMenuhf.check_adjacency [ point1; point2 ]) true

let check_adjacency_test_2_false name point1 point2 =
  name >:: fun _ ->
  assert_equal (GraphicsMenuhf.check_adjacency [ point1; point2 ]) false

let check_adjacency_test_3_true name point1 point2 point3 =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency [ point1; point2; point3 ])
    true

let check_adjacency_test_3_false name point1 point2 point3 =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency [ point1; point2; point3 ])
    false

let check_adjacency_test_4_true name point1 point2 point3 point4 =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency [ point1; point2; point3; point4 ])
    true

let check_adjacency_test_4_false name point1 point2 point3 point4 =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency [ point1; point2; point3; point4 ])
    false

let check_adjacency_test_5_true name point1 point2 point3 point4 point5
    =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency
       [ point1; point2; point3; point4; point5 ])
    true

let check_adjacency_test_5_false name point1 point2 point3 point4 point5
    =
  name >:: fun _ ->
  assert_equal
    (GraphicsMenuhf.check_adjacency
       [ point1; point2; point3; point4; point5 ])
    false

(*[adjacency_tests] are (mostly) black-box tests to make sure the
  function check_adjacency checks adjacency correctness for each boat
  size (each boat size is necessary because of how the program is
  structure. This is the clear-box testing part of this). This function
  was easy to write incorrectly so many tests are created to catch
  errors *)
let adjacency_tests =
  [
    check_adjacency_test_2_false
      "edge case that might not be covered by function, points corner \
       touching BORKED"
      (1, 2) (2, 3);
    check_adjacency_test_2_false
      "edge case that might not be covered by function, points corner \
       touching BORKED"
      (2, 3) (1, 2);
    check_adjacency_test_2_false
      "arbatrary non-touching, but y is adjacent -BORKED-FIXED" (3, 3)
      (8, 2);
    check_adjacency_test_2_false
      "arbatrary non-touching, but x is adjacent -BORKED-FIXED" (1, 9)
      (2, 5);
    check_adjacency_test_2_false "arbatrary non-touching" (3, 4) (8, 2);
    check_adjacency_test_2_false "arbatrary non-touching" (9, 9) (2, 5);
    check_adjacency_test_2_false "inline with a space between vertical"
      (9, 1) (9, 3);
    check_adjacency_test_2_false
      "inline with a space between horizontal" (1, 9) (3, 9);
    check_adjacency_test_2_false "duplicate point" (1, 2) (1, 2);
    check_adjacency_test_2_true "check for adjacency horizontal 2"
      (1, 2) (1, 3);
    check_adjacency_test_2_true "check for adjacency horizontal 2"
      (1, 3) (1, 2);
    check_adjacency_test_2_true "check for adjacency vertical 2" (1, 2)
      (2, 2);
    check_adjacency_test_2_true "check for adjacency vertical 2" (2, 2)
      (1, 2);
    check_adjacency_test_3_true "check for adjacency vertical 3" (2, 1)
      (2, 2) (2, 3);
    check_adjacency_test_3_true "check for adjacency vertical 3" (2, 3)
      (2, 2) (2, 1);
    check_adjacency_test_3_true "check for adjacency horizontal 3"
      (2, 1) (3, 1) (4, 1);
    check_adjacency_test_3_true "check for adjacency horizontal 3"
      (4, 1) (3, 1) (2, 1);
    check_adjacency_test_3_true "random order 3" (4, 1) (2, 1) (3, 1);
    check_adjacency_test_3_false "space inbetween false horizontal 3"
      (4, 1) (2, 1) (1, 1);
    check_adjacency_test_3_false "space inbetween false vertical 3"
      (1, 4) (1, 2) (1, 1);
    check_adjacency_test_3_false
      "one of the points is offset by one -BORKED-FIXED " (1, 2) (2, 2)
      (3, 3);
    check_adjacency_test_3_false
      "one of the points is offset by one vertical -BORKED-FIXED" (2, 1)
      (2, 2) (3, 3);
    check_adjacency_test_3_false "one point is a duplicate 3" (2, 1)
      (2, 1) (2, 2);
    check_adjacency_test_3_false "random ass points 3" (1, 2) (9, 6)
      (2, 1);
    check_adjacency_test_4_true "vertical true 4" (1, 2) (1, 3) (1, 4)
      (1, 5);
    check_adjacency_test_4_true "vertical true 4" (1, 5) (1, 4) (1, 3)
      (1, 2);
    check_adjacency_test_4_true "horizontal true 4" (1, 5) (2, 5) (3, 5)
      (4, 5);
    check_adjacency_test_4_true "horizontal true 4" (4, 5) (3, 5) (2, 5)
      (1, 5);
    check_adjacency_test_4_true "out of order 4" (3, 5) (4, 5) (2, 5)
      (1, 5);
    check_adjacency_test_4_false "one point not inline horizontal 4"
      (2, 5) (3, 5) (4, 6) (5, 5);
    check_adjacency_test_4_false "one point not inline vertical 4"
      (5, 1) (6, 2) (5, 3) (5, 4);
    check_adjacency_test_4_false "there is a gap vertical 4" (3, 4)
      (3, 5) (3, 7) (3, 8);
    check_adjacency_test_4_false "there is a gap horizontal 4" (3, 4)
      (4, 4) (6, 4) (7, 4);
    check_adjacency_test_4_false "random ass points 4" (1, 2) (8, 9)
      (2, 7) (8, 2);
    check_adjacency_test_4_false "duplicate points 4" (1, 2) (1, 2)
      (1, 2) (1, 2);
    check_adjacency_test_5_true "check for adjacency vertical 5" (1, 6)
      (1, 5) (1, 4) (1, 3) (1, 2);
    check_adjacency_test_5_true "check for adjacency vertical 5" (1, 2)
      (1, 3) (1, 4) (1, 5) (1, 6);
    check_adjacency_test_5_true "check for adjacency horizontal 5"
      (1, 1) (2, 1) (3, 1) (4, 1) (5, 1);
    check_adjacency_test_5_true "check for adjacency horizontal 5"
      (6, 1) (5, 1) (4, 1) (3, 1) (2, 1);
    check_adjacency_test_5_false "one point not inline horizontal 5"
      (2, 5) (3, 5) (4, 6) (5, 5) (6, 5);
    check_adjacency_test_5_false "one point not inline vertical 5"
      (5, 1) (6, 2) (5, 3) (5, 4) (5, 5);
    check_adjacency_test_5_false "there is a gap vertical 5" (3, 4)
      (3, 5) (3, 7) (3, 8) (3, 9);
    check_adjacency_test_5_false "there is a gap horizontal 5" (3, 4)
      (4, 4) (6, 4) (7, 4) (8, 4);
    check_adjacency_test_5_false "random ass points 5" (1, 2) (8, 9)
      (2, 7) (8, 2) (2, 3);
    check_adjacency_test_5_false "duplicate points 5" (1, 2) (1, 2)
      (1, 2) (1, 2) (1, 2);
  ]

let check_overlap_true name lst listlst =
  name >:: fun _ ->
  assert_equal (GraphicsMenuhf.check_overlap lst listlst) true

let check_overlap_false name lst listlst =
  name >:: fun _ ->
  assert_equal (GraphicsMenuhf.check_overlap lst listlst) false

(*tests for check_overlap:*)
let overlap_tests =
  [
    (let lst = [ (0, 1); (0, 2); (0, 3) ] in
     let lstlist = [] in
     check_overlap_true "checking for adding to an empty list" lst
       lstlist);
    (let lst = [ (0, 1); (0, 2); (0, 3) ] in
     let lstlist =
       [
         [ (1, 1); (1, 2); (1, 3) ];
         [ (4, 1); (4, 2); (4, 3) ];
         [ (5, 1); (5, 2); (5, 3); (5, 4) ];
       ]
     in
     check_overlap_true "checking for adding to some arbitrary ships"
       lst lstlist);
    (let lst = [ (0, 1); (0, 2); (0, 3) ] in
     let lstlist =
       [
         [ (1, 1); (1, 2); (1, 3) ];
         [ (8, 7); (8, 6); (8, 5) ];
         [ (0, 1); (0, 2); (0, 3) ];
         [ (3, 1); (3, 2) ];
       ]
     in

     check_overlap_false
       "full ship match test (with other ships) horizontal" lst lstlist);
    (let lst = [ (1, 0); (2, 0); (3, 0) ] in
     let lstlist =
       [
         [ (1, 1); (1, 2); (1, 3) ];
         [ (4, 1); (4, 2); (4, 3) ];
         [ (1, 0); (2, 0); (3, 0) ];
         [ (3, 1); (3, 2) ];
       ]
     in
     check_overlap_false
       "full ship match test (with other ships) vertical" lst lstlist);
    (let lst = [ (0, 1); (0, 2); (0, 3) ] in
     let lstlist =
       [
         [ (7, 1); (8, 1); (9, 1) ];
         [ (4, 1); (4, 2); (4, 0) ];
         [ (0, 1); (1, 1); (2, 1) ];
       ]
     in
     check_overlap_false "only one matching point the on board" lst
       lstlist);
    (let lst = [ (0, 1); (0, 2); (0, 3) ] in
     let lstlist =
       [
         [ (7, 1); (8, 1); (9, 1) ];
         [ (5, 1); (5, 2); (5, 0); (5, 3) ];
         [ (0, 2); (0, 3); (0, 4) ];
       ]
     in
     check_overlap_false "two matching points on the board" lst lstlist);
    (let lst = [ (3, 1); (3, 2); (3, 3) ] in
     let lstlist =
       [
         [ (7, 1); (8, 1); (9, 1) ];
         [ (5, 2); (4, 2); (3, 2) ];
         [ (0, 1); (1, 1); (2, 1) ];
       ]
     in
     check_overlap_false
       "only one matching point the on board. checking vertical just \
        to be sure"
       lst lstlist);
    (let lst = [ (3, 1); (3, 2); (3, 3) ] in
     let lstlist =
       [
         [ (7, 1); (8, 1); (9, 1) ];
         [ (3, 5); (3, 4); (3, 3) ];
         [ (0, 1); (1, 1); (2, 1) ];
       ]
     in
     check_overlap_false
       "only one matching point the on board. checking vertical just \
        to be sure (with vertical ship in already placed)"
       lst lstlist);
  ]

(*********************************)
(*********  AiSystemhf  **********)
(*********************************)

let outside_or_already_placed_test_false name grid_size point list =
  name >:: fun _ ->
  assert_equal
    (AiSystemhf.check_already_placed_or_outside grid_size point list)
    false

let outside_or_already_placed_test_true name grid_size point list =
  name >:: fun _ ->
  assert_equal
    (AiSystemhf.check_already_placed_or_outside grid_size point list)
    true

let test_generate_ship_y name x y size cship =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists
    (AiSystemhf.generate_ship_y x y size)
    cship

let test_generate_ship_x name x y size cship =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists
    (AiSystemhf.generate_ship_x x y size)
    cship

let aisystem_tests =
  [
    outside_or_already_placed_test_true
      "overlapping point -BORKED-FIXED" 10 (1, 2)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true
      "point's x coordinate is below 0" 10 (-1, 2)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true
      "point's y coordinate is below 0" 10 (0, -1)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true
      "point's x coordinate is above grid size" 10 (12, 2)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true
      "point's y coordinate is above grid size" 10 (2, 12)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_false "not overlapping" 10 (2, 8)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_false "edge x" 10 (10, 1)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_false "edge y" 10 (3, 10)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true "outside x" 10 (11, 1)
      [ (3, 7); (1, 2); (8, 9) ];
    outside_or_already_placed_test_true "outside y" 10 (7, 11)
      [ (3, 7); (1, 2); (8, 9) ];
    test_generate_ship_y "ship starts at 0,0 size 5 going to the east" 0
      0 5
      [ (0, 0); (0, 1); (0, 2); (0, 3); (0, 4) ];
    test_generate_ship_y "ship starts at 0,0, size 4 going to the east"
      0 0 4
      [ (0, 0); (0, 1); (0, 2); (0, 3) ];
    test_generate_ship_y "ship starts at 0,0, size 3 going to the east"
      0 0 3
      [ (0, 0); (0, 1); (0, 2) ];
    test_generate_ship_y "ship starts at 0,0, size 2 going to the east"
      0 0 2
      [ (0, 0); (0, 1) ];
    test_generate_ship_y "ship starts at 0,0, size 1 going to the east"
      0 0 1
      [ (0, 0) ];
    test_generate_ship_y "ship starts at 0,0, size 0" 0 0 0 [];
    test_generate_ship_x "ship starts at 0,0, size 5 going to the north"
      0 0 5
      [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0) ];
    test_generate_ship_x "ship starts at 0,0, size 4 going to the north"
      0 0 4
      [ (0, 0); (1, 0); (2, 0); (3, 0) ];
    test_generate_ship_x "ship starts at 0,0, size 3 going to the north"
      0 0 3
      [ (0, 0); (1, 0); (2, 0) ];
    test_generate_ship_x "ship starts at 0,0, size 2 going to the north"
      0 0 2
      [ (0, 0); (1, 0) ];
    test_generate_ship_x "ship starts at 0,0, size 1 going to the north"
      0 0 1
      [ (0, 0) ];
    test_generate_ship_x "ship starts at 0,0, size 0" 0 0 0 [];
  ]

let check_adjacent_test_true name x y list =
  name >:: fun _ ->
  assert_equal (TextMenuhf.check_adjacent (x, y) list) true

let check_adjacent_test_false name x y list =
  name >:: fun _ ->
  assert_equal (TextMenuhf.check_adjacent (x, y) list) false

let check_dimensions_true name x y width height =
  name >:: fun _ ->
  assert_equal (TextMenuhf.check_dimensions (x, y) width height) true

let check_dimensions_false name x y width height =
  name >:: fun _ ->
  assert_equal (TextMenuhf.check_dimensions (x, y) width height) false

let text_menu_tests =
  [
    check_adjacent_test_false "empty check" 1 2 [];
    check_adjacent_test_true "horizontal check" 1 2
      [ (1, 3); (1, 4); (1, 5) ];
    check_adjacent_test_true "horizontal check" 1 6
      [ (1, 3); (1, 4); (1, 5) ];
    check_adjacent_test_true "vertical check" 2 1
      [ (3, 1); (4, 1); (5, 1) ];
    check_adjacent_test_true "vertical check" 6 1
      [ (3, 1); (4, 1); (5, 1) ];
    check_adjacent_test_false "matching y" 1 2 [ (4, 2); (3, 2) ];
    check_adjacent_test_false "matching x" 3 9 [ (3, 1); (3, 2) ];
    check_adjacent_test_false "random point" 1 7 [ (3, 1); (3, 2) ];
    check_dimensions_true "inside" 1 8 10 10;
    check_dimensions_true "inside" 5 3 10 10;
    check_dimensions_false "outside x" (-1) 8 10 10;
    check_dimensions_false "outside x" 11 8 10 10;
    check_dimensions_false "border x" 10 8 10 10;
    check_dimensions_false "outside y" 8 (-1) 10 10;
    check_dimensions_false "outside y" 8 11 10 10;
    check_dimensions_false "border y" 8 10 10 10;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [] @ place_ships_tests @ adjacency_tests
       @ aisystem_tests @ overlap_tests @ text_menu_tests

let _ = run_test_tt_main suite
