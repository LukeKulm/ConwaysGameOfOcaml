open Graphics
open World

let cell_size = 20

let initial =
  init_world_with_alive 30 30 [ (0, 22); (1, 21); (2, 21); (2, 22); (2, 23) ]

let initial2 =
  init_world_with_alive 50 50
    [
      (1, 25);
      (1, 26);
      (2, 25);
      (2, 26);
      (4, 24);
      (4, 25);
      (4, 26);
      (4, 27);
      (5, 27);
      (5, 28);
      (5, 24);
      (5, 23);
      (6, 22);
      (6, 29);
      (8, 22);
      (8, 29);
      (9, 22);
      (9, 29);
      (9, 24);
      (9, 27);
      (10, 25);
      (10, 26);
      (11, 25);
      (11, 26);
      (12, 23);
      (12, 24);
      (12, 27);
      (12, 28);
    ]

(* This one is very cool. *)
let initial3 =
  init_world_with_alive 50 50
    [
      (0, 48);
      (1, 47);
      (2, 47);
      (2, 48);
      (2, 49);
      (1, 25);
      (1, 26);
      (2, 25);
      (2, 26);
      (4, 24);
      (4, 25);
      (4, 26);
      (4, 27);
      (5, 27);
      (5, 28);
      (5, 24);
      (5, 23);
      (6, 22);
      (6, 29);
      (8, 22);
      (8, 29);
      (9, 22);
      (9, 29);
      (9, 24);
      (9, 27);
      (10, 25);
      (10, 26);
      (11, 25);
      (11, 26);
      (12, 23);
      (12, 24);
      (12, 27);
      (12, 28);
    ]

let window_size state =
  (cell_size * fst (get_dims state), cell_size * snd (get_dims state))

let rows state = fst (window_size state) / cell_size
let columns state = snd (window_size state) / cell_size

let draw_cell num1 num2 =
  fill_rect (num1 * cell_size) (num2 * cell_size) cell_size cell_size

let rec draw_frame_helper g =
  match g with
  | [] -> ()
  | h :: t ->
      draw_cell (fst h) (snd h);
      draw_frame_helper t

(* let draw_grid state = set_color (rgb 200 200 200); let num_squares_x =
   cell_size * fst (get_dims state) in for i = 1 to num_squares_x - 1 do let x =
   i * cell_size in moveto x 0; lineto x (snd (get_dims state)) done; let
   num_squares_y = cell_size * snd (get_dims state) in for i = 1 to
   num_squares_y - 1 do let y = i * cell_size in moveto 0 y; lineto y (fst
   (get_dims state)) done *)

let draw_frame state =
  match get_alive state with
  | [] -> ()
  | h :: t ->
      draw_cell (fst h) (snd h);
      draw_frame_helper t
(* draw_grid state *)

(*code to turn HSL values to RGB because Graphics takes RGB but HSL is what we
  want to animate the dead cells nicely, probably will store a lightness value
  in with a cell to determine how long it has been dead*)
let howlongdead_to_hsl hld = (0.0, 0.0, 0.70 +. (float_of_int hld /. 100.0))

let hsl_to_RGB_helper x =
  match x with
  | a, b, c, m ->
      ( int_of_float ((a +. m) *. 255.0),
        int_of_float ((b +. m) *. 255.0),
        int_of_float ((c +. m) *. 255.0) )

let hsl_to_RGB h s l =
  let c = (1.0 -. abs_float ((2.0 *. l) -. 1.0)) *. s in
  let x = 1.0 -. abs_float (mod_float (h /. 60.0) 2.0 -. 1.0) in
  let m = l -. (c /. 2.0) in
  if h < 60.0 then (int_of_float c, int_of_float x, 0)
  else if h < 120.0 then hsl_to_RGB_helper (x, c, 0.0, m)
  else if h < 180.0 then hsl_to_RGB_helper (0.0, c, x, m)
  else if h < 240.0 then hsl_to_RGB_helper (0.0, x, c, m)
  else if h < 300.0 then hsl_to_RGB_helper (x, 0.0, c, m)
  else hsl_to_RGB_helper (c, 0.0, x, m)

let get_first a =
  match a with
  | x, y, z -> x

let get_sec a =
  match a with
  | x, y, z -> y

let get_third a =
  match a with
  | x, y, z -> z

(*this will draw the fading cells that have become dead after being alive*)
let rec draw_frame_dead_helper a =
  match a with
  | [] -> ()
  | (x, y, z) :: t ->
      set_color
        (rgb
           (get_first
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z))))
           (get_sec
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z))))
           (get_third
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z)))));
      draw_cell x y;
      draw_frame_dead_helper t

let draw_frame_dead state =
  match get_dead state with
  | [] -> ()
  | (x, y, z) :: t ->
      set_color
        (rgb
           (get_first
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z))))
           (get_sec
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z))))
           (get_third
              (hsl_to_RGB
                 (get_first (howlongdead_to_hsl z))
                 (get_sec (howlongdead_to_hsl z))
                 (get_third (howlongdead_to_hsl z)))));
      draw_cell x y;
      draw_frame_dead_helper t
