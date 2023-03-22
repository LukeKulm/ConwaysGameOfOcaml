open Graphics
open World

let cell_size = 10

let initial =
  init_world_with_alive 50 50 [ (0, 42); (1, 41); (2, 41); (2, 42); (2, 43) ]

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
