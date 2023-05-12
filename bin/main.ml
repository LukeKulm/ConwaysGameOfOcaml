open Graphics
open Life.Display
open Life.World

let win_width = 1400
let win_height = 700
let square_size = 20
let num_squares_x = win_width / square_size
let num_squares_y = win_height / square_size
let grid = Array.make_matrix num_squares_y num_squares_x white
let frame_rate = 0.03
let color_red () = rgb 255 0 0

let draw_square x y color =
  set_color color;
  fill_rect (x + 1) (y + 1) (square_size - 2) (square_size - 2)

let draw_grid () =
  for i = 0 to num_squares_y - 1 do
    for j = 0 to num_squares_x - 1 do
      draw_square (j * square_size) (i * square_size) grid.(i).(j)
    done
  done;
  set_color (rgb 200 200 200);
  for i = 1 to num_squares_x - 1 do
    let x = i * square_size in
    moveto x 0;
    lineto x win_height
  done;
  for i = 1 to num_squares_y - 1 do
    let y = i * square_size in
    moveto 0 y;
    lineto win_width y
  done

let caml_list =
  [
    (8, 30);
    (8, 31);
    (8, 32);
    (8, 33);
    (8, 34);
    (8, 35);
    (8, 36);
    (8, 37);
    (8, 38);
    (8, 39);
    (8, 40);
    (8, 41);
    (8, 42);
    (8, 43);
    (8, 44);
    (8, 45);
    (8, 46);
    (8, 47);
    (9, 30);
    (9, 31);
    (9, 32);
    (9, 33);
    (9, 34);
    (9, 35);
    (9, 36);
    (9, 37);
    (9, 38);
    (9, 39);
    (9, 40);
    (9, 41);
    (9, 42);
    (9, 43);
    (9, 44);
    (9, 45);
    (9, 46);
    (9, 47);
    (10, 30);
    (11, 30);
    (12, 30);
    (13, 30);
    (14, 30);
    (15, 30);
    (16, 30);
    (17, 30);
    (18, 30);
    (19, 30);
    (20, 30);
    (21, 30);
    (22, 30);
    (23, 30);
    (23, 31);
    (23, 32);
    (23, 33);
    (23, 34);
    (23, 35);
    (23, 36);
    (23, 37);
    (23, 38);
    (23, 39);
    (23, 40);
    (23, 41);
    (23, 42);
    (23, 43);
    (23, 44);
    (23, 45);
    (23, 46);
    (23, 47);
    (22, 47);
    (21, 47);
    (20, 47);
    (19, 47);
    (18, 47);
    (17, 47);
    (16, 47);
    (15, 47);
    (14, 47);
    (13, 47);
    (12, 47);
    (11, 47);
    (10, 47);
    (15, 32);
    (16, 32);
    (17, 32);
    (18, 32);
    (19, 32);
    (16, 33);
    (17, 33);
    (18, 33);
    (19, 33);
    (20, 33);
    (17, 34);
    (18, 34);
    (19, 34);
    (20, 34);
    (21, 34);
    (17, 35);
    (18, 35);
    (19, 35);
    (20, 35);
    (21, 35);
    (17, 36);
    (18, 36);
    (19, 36);
    (20, 36);
    (17, 37);
    (18, 37);
    (19, 37);
    (20, 37);
    (21, 37);
    (17, 38);
    (18, 38);
    (19, 38);
    (20, 38);
    (21, 38);
    (14, 39);
    (15, 39);
    (16, 39);
    (17, 39);
    (18, 39);
    (19, 39);
    (11, 38);
    (12, 38);
    (13, 38);
    (14, 38);
    (11, 39);
    (16, 40);
    (17, 40);
    (18, 40);
    (14, 41);
    (15, 41);
    (16, 41);
    (17, 41);
    (18, 41);
    (11, 42);
    (12, 42);
    (13, 42);
    (14, 42);
    (17, 42);
    (18, 42);
    (19, 42);
    (20, 42);
    (11, 43);
    (17, 43);
    (18, 43);
    (19, 43);
    (20, 43);
    (18, 44);
    (19, 44);
    (18, 45);
    (19, 45);
  ]

let love_list =
  [
    (15, 8);
    (16, 8);
    (17, 8);
    (18, 8);
    (19, 8);
    (20, 8);
    (21, 8);
    (22, 8);
    (23, 8);
    (24, 8);
    (25, 8);
    (15, 19);
    (16, 18);
    (17, 17);
    (18, 16);
    (19, 15);
    (20, 14);
    (21, 13);
    (22, 13);
    (23, 13);
    (24, 14);
    (25, 15);
    (25, 16);
    (24, 17);
    (23, 18);
    (22, 19);
    (23, 20);
    (24, 21);
    (25, 22);
    (25, 23);
    (24, 24);
    (23, 25);
    (22, 25);
    (21, 25);
    (20, 24);
    (19, 23);
    (18, 22);
    (17, 21);
    (16, 20);
    (15, 31);
    (15, 32);
    (15, 33);
    (15, 34);
    (15, 35);
    (16, 30);
    (17, 30);
    (18, 30);
    (19, 30);
    (20, 30);
    (21, 30);
    (22, 30);
    (23, 30);
    (24, 30);
    (25, 31);
    (25, 32);
    (25, 33);
    (25, 34);
    (25, 35);
    (24, 36);
    (23, 36);
    (22, 36);
    (21, 36);
    (20, 36);
    (19, 36);
    (18, 36);
    (17, 36);
    (16, 36);
    (15, 39);
    (15, 40);
    (15, 41);
    (16, 42);
    (17, 42);
    (16, 38);
    (17, 38);
    (18, 38);
    (19, 38);
    (20, 38);
    (21, 38);
    (22, 38);
    (23, 38);
    (24, 38);
    (25, 39);
    (25, 40);
    (25, 41);
    (24, 42);
    (23, 42);
    (16, 44);
    (17, 44);
    (18, 44);
    (19, 44);
    (20, 45);
    (20, 46);
    (19, 47);
    (18, 48);
    (17, 48);
    (16, 48);
    (15, 49);
    (16, 49);
    (17, 49);
    (18, 49);
    (19, 49);
    (20, 49);
    (15, 45);
    (15, 46);
    (15, 47);
    (15, 51);
    (15, 51);
    (16, 51);
    (17, 51);
    (18, 51);
    (19, 51);
    (20, 51);
    (19, 52);
    (18, 53);
    (19, 54);
    (18, 55);
    (17, 55);
    (16, 55);
    (15, 55);
    (15, 57);
    (16, 57);
    (17, 57);
    (18, 57);
    (19, 57);
    (20, 57);
    (21, 57);
    (22, 57);
    (23, 57);
    (24, 57);
  ]

let handle_click x y =
  let square_x = x / square_size in
  let square_y = y / square_size in
  let new_color = if grid.(square_y).(square_x) = white then black else white in
  grid.(square_y).(square_x) <- new_color;
  draw_grid ();
  draw_square (square_x * square_size) (square_y * square_size) new_color

let handle_drag x y =
  let curr_state = grid.(y).(x) in
  let new_state = if curr_state = black then white else black in
  grid.(y).(x) <- new_state;
  set_color new_state;
  fill_rect (x * square_size) (y * square_size) square_size square_size

let get_alive_cells grid =
  let alive_cells = ref [] in
  for i = 0 to num_squares_y - 1 do
    for j = 0 to num_squares_x - 1 do
      if grid.(i).(j) = black then alive_cells := (j, i) :: !alive_cells
    done
  done;
  !alive_cells

let to_world grid n =
  init_world_with_alive (win_width / square_size) (win_height / square_size)
    (get_alive_cells grid) n

let rec animate state color_var n =
  if key_pressed () && read_key () = 'x' then main ()
  else
    (clear_graph ();
     set_color color_var;
     draw_frame state;
     draw_frame_dead_alt state n;
     draw_string "PRESS";
     Unix.sleepf frame_rate;
     animate (update_world state) color_var)
      n

and main () =
  open_graph "";
  resize_window win_width win_height;
  draw_grid ();

  let rec handle_events () =
    let event = wait_next_event [ Button_down; Key_pressed ] in
    if event.button then (
      handle_click event.mouse_x event.mouse_y;
      (*while button_down () do let new_event = wait_next_event [ Button_up ] in
        let new_x = new_event.mouse_x / square_size in let new_y =
        new_event.mouse_y / square_size in if new_x >= 0 && new_x <
        num_squares_x && new_y >= 0 && new_y < num_squares_y then handle_drag
        new_x new_y done; *)
      handle_events ())
    else if event.keypressed && event.key = 'r' then
      animate (to_world grid 0) (rgb 255 0 0) 0
    else if event.keypressed && event.key = 'g' then
      animate (to_world grid 1) (rgb 0 255 0) 1
    else if event.keypressed && event.key = 'b' then
      animate (to_world grid 0) (rgb 0 0 255) 0
    else if event.keypressed && event.key = 'c' then draw_love caml_list
    else if event.keypressed && event.key = 'l' then draw_love love_list
    else handle_events ()
  and draw_love lst =
    match lst with
    | h :: t ->
        grid.(fst h).(snd h) <- black;
        draw_grid ();
        draw_love t
    | _ -> handle_events ()
  in
  handle_events ()

let rec start () =
  (* print_string "\n\nWelcome to the Game of Life!.\n"; print_endline "What
     color do you want your cells to start off as?.\n"; print_endline "Please
     enter: blue, red, or green.\n"; *)

  (*match read_line () with | exception End_of_file -> () | file_name ->
    play_game (data_dir_prefix ^ file_name ^ ".json")*)
  open_graph (" " ^ string_of_int win_width ^ "X" ^ string_of_int win_height);
  set_window_title "The Game of Life";
  set_font "-*-fixed-medium-r-semicondensed--70-*-*-*-*-*-iso8859-1";
  moveto 350 600;
  draw_string "Conway's Game of Life!!";
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  moveto 315 480;
  draw_string "Press ANY key to start the grid!";
  moveto 370 380;
  draw_string "Click boxes to select them!";
  moveto 220 340;
  set_color (rgb 225 0 127);
  set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  draw_string
    "Hint: While drawing boxes, press 'c' or 'l' for a functional surprise!";
  moveto 30 280;
  set_color black;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string "THEN, press one of the following to start the animation:";
  moveto 160 180;
  set_color (rgb 255 0 0);
  draw_string "'R' for red";
  moveto 540 180;
  set_color (rgb 0 255 0);
  draw_string "'G' for green";
  moveto 940 180;
  set_color (rgb 0 0 255);
  draw_string "'B' for blue";
  set_color (rgb 0 0 0);
  moveto 215 80;
  draw_string "While running, Press 'X' to exit reset!";

  if (wait_next_event [ Key_pressed ]).keypressed then main () else start ()
(* Wait for a short time before updating the cells again *)

let _ = start ()
