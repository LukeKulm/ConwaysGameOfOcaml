open World

val cell_size : int
(* [cell_size] is the size of each cell *)

val initial : World.t
(* [initial] is a starting state for the world. It currently contains a single
   glider. *)

val window_size : World.t -> int * int
(* [window_size state] returns the size of window in [state] *)

val rows : World.t -> int
(* [rows state] gives the number of rows in the window [state]. *)

val columns : World.t -> int
(* [columns state] gives the number of columns in window [state]. *)

val draw_frame : World.t -> unit
(* [draw_frame state] draws cells using the current [state] of the world. *)

val draw_frame_dead : World.t -> unit
