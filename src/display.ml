open Graphics
open World

let cell_size = 10

let initial2 =
  init_world_with_alive 50 50 [ (0, 42); (1, 41); (2, 41); (2, 42); (2, 43) ]

let initial3 =
  init_world_with_alive 100 50
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
let initial =
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

let draw_frame state =
  match get_alive state with
  | [] -> ()
  | h :: t ->
      draw_cell (fst h) (snd h);
      draw_frame_helper t
