(*size of the window*)
val window_size : int * int

(*size of each cell*)
val cell_size : int

(*gives the number of cells in the window up and down*)
val rows : int

(*the number of cells in the window left to right*)
val collums : int

(*draw_cell [a][b] draws a cell at the coordinate ([a][b]) where the coordinate
  is the cell number, not the pixel number*)
val draw_cell : int -> int -> unit
