val window_size : int * int

(*size of the window*)
val cell_size : int

(*size of each cell*)
val rows : int

(*gives the number of cells in the window up and down*)
val collums : int

(*the number of cells in the window left to right*)
val draw_cell : int -> int -> unit
(*draw_cell [a][b] draws a cell at the coordinate ([a][b])*)
