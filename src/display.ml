open Graphics
open World

let window_size = (500, 500)
let cell_size = 10
let rows = fst window_size / cell_size
let collums = snd window_size / cell_size

let draw_cell num1 num2 =
  fill_rect (num1 * cell_size) (num2 * cell_size) cell_size cell_size
