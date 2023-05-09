(** Display current game state. *)

open World

val cell_size : int
(** [cell_size] is the size of each cell *)

val initial : World.t
(** [initial] is a starting state for the world. It currently contains a single
    glider. *)

val window_size : World.t -> int * int
(** [window_size state] returns the size of window in [state] *)

val rows : World.t -> int
(** [rows state] gives the number of rows in the window [state]. *)

val columns : World.t -> int
(** [columns state] gives the number of columns in window [state]. *)

val draw_frame : World.t -> unit
(** [draw_frame state] draws cells using the current [state] of the world. *)

val draw_frame_dead : World.t -> unit
(** [draw_frame_dead state] draws dead cells to be the correct shaded color
    corresponding to how long each cell has been dead. *)

(* [draw_frame_dead state] draws the dead cells using the current [state] of the
   world.*)
val draw_frame_dead_alt : World.t -> unit
(** [draw_frame_dead_alt state] draws dead cells to be the correct shaded color
    corresponding to how long each cell has been dead. It stops drawing cells
    that have been dead longer than a certain threshold, so they are drawn as
    normal dead cells. *)
