open Graphics
open World

let cell_size = 10
let initial = init_world 5 5 [ (0, 1); (1, 0); (2, 0) ]

let window_size state =
  (cell_size * fst (get_dims state), cell_size * snd (get_dims state))

let rows state = fst (window_size state) / cell_size
let collums state = snd (window_size state) / cell_size

let draw_cell num1 num2 =
  fill_rect (num1 * cell_size) (num2 * cell_size) cell_size cell_size

let draw_frame state =
  match get_which_alive state with
  | [] -> ()
  | h :: t -> draw_cell (fst h) (snd h)
