open Graphics

let window_size = (500, 500)
let cell_size = 10
let rows = fst window_size / cell_size
let collums = snd window_size / cell_size
let draw_cell cord1 cord2 = fill_rect cord1 cord2 cell_size cell_size
