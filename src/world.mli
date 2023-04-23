type t
(* [t] is the abstract type for the world. *)

val init_world : int -> int -> t
(* [init_world width height] Initializes a world with all dead cells that is
   [width] wide and [height] tall. *)

val init_world_with_alive : int -> int -> (int * int) list -> t
(* [init_world_with_cells width height living] Initializes a world that is of
   [width] wide and [height] tall and sets all cells at coordinates specified by
   [living] to be alive. All other cells are dead. *)

val get_dims : t -> int * int
(* [get_dims world] returns the dimensions of [world]. *)

val get_alive : t -> (int * int) list
(* [get_alive world] returns the list of coordinates of which points are alive
   within [world], with (0,0) being the top left corner. *)

val get_dead : t -> (int * int * int) list
(* [get_dead world] returns the list of coordinates of which points are dead
   after having been alive beforeand and the amount of frames for which the cell
   has been dead within [world], with (0,0) being the top left corner. *)

val update_world : t -> t
(* [update_world world] updates [world] according to its current state. *)

exception NotImplemented
