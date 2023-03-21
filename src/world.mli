type t
(*abstract type . . . *)

val init_world : int -> int -> (int * int) list -> t
(* Initializes a world that is of x by y dimensions, where the int by int list
   are the coordinates of the originally alive cells *)

val get_dims : t -> int * int
(* returns # of rows by # of cols*)

val get_which_alive : t -> (int * int) list
(*returns the coordinates of which points are alive within the grid, with (0,0)
  being the top left corner *)

val world_update : t -> t
(* Update the world by calling upon each cell to update itself: important to
   note that the order in whcih we do so matters*)

exception NotImplemented
