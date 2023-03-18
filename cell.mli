type t
(** The abstract type of one cell in the grid of the game of life*)

val init_cell : bool -> t
(** Initializes a new cell of type t, alive if bool = true, dead if bool = false*)

val get_alive : t -> bool
(** xxxx *)

val cell_update : t -> bool -> t
(** xxxx do we need this if we are not adding position locations to the cell? we
    would just re initialize. so should we add x and y points to the cell?
    design question! *)

exception NotImplemented
