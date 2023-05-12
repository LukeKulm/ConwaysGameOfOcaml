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

let to_world grid =
  init_world_with_alive (win_width / square_size) (win_height / square_size)
    (get_alive_cells grid)

let rec animate state color_var =
  if key_pressed () && read_key () = 'x' then main ()
  else (
    clear_graph ();
    set_color color_var;
    draw_frame state;
    draw_frame_dead_alt state;
    draw_string "PRESS";
    Unix.sleepf frame_rate;
    animate (update_world state) color_var)

and main () =
  open_graph "";
  resize_window win_width win_height;
  draw_grid ();

  let rec handle_events () =
    let event = wait_next_event [ Button_down; Key_pressed ] in
    if event.button then (
      handle_click event.mouse_x event.mouse_y;
      (* while button_down () do let new_event = wait_next_event [ Button_up ]
         in let new_x = new_event.mouse_x / square_size in let new_y =
         new_event.mouse_y / square_size in if new_x >= 0 && new_x <
         num_squares_x && new_y >= 0 && new_y < num_squares_y then handle_drag
         new_x new_y done; *)
      handle_events ())
    else if event.keypressed && event.key = 'r' then
      animate (to_world grid) (rgb 255 0 0)
    else if event.keypressed && event.key = 'g' then
      animate (to_world grid) (rgb 0 255 0)
    else if event.keypressed && event.key = 'b' then
      animate (to_world grid) (rgb 0 0 255)
    else handle_events ()
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
  moveto 30 280;
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
