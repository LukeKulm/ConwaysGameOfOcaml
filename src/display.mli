open World

(*size of each cell*)
val cell_size : int
val initial : t

(*size of the window*)
val window_size : t -> int * int

(*gives the number of cells in the window up and down*)
val rows : t -> int

(*the number of cells in the window left to right*)
val collums : t -> int

(*draw_frame [state] draws cells using ([state]) in the graph*)
val draw_frame : t -> unit
