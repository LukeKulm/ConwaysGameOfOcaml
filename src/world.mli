type t
(* [t] is the abstract type for the world. *)

exception InvalidDims of (int * int)
(** Raised when the dimensions of a world are invalid, so the world cannot be
    initialized. It carries the attempted dimensions. *)

exception InvalidPts
(** Raised when a point trying to be marked as alive is not within the grid
    specified. *)

val init_world : int -> int -> t
(* [init_world width height] Initializes a world with all dead cells that is
   [width] wide and [height] tall when both width and height are >0. Otherwise,
   raises InvalidDims (width,height). *)

val init_world_with_alive : int -> int -> (int * int) list -> t
(* [init_world_with_cells width height living] Initializes a world that is of
   [width] wide and [height] tall and sets all cells at coordinates specified by
   [living] to be alive. All other cells are dead. All points within living must
   be within the range of the width and height. If not, raises InvalidDims(x,y)
   with the coordinates ofs the point in violation. REQUIRES no repeats in
   living *)

val get_dims : t -> int * int
(* [get_dims world] returns the dimensions of [world]. *)

val get_alive : t -> (int * int) list
(* [get_alive world] returns the list of coordinates of which points are alive
   within [world], with (0,0) being the top left corner. *)

val update_world : t -> t
(* [update_world world] updates [world] according to its current state. *)

exception NotImplemented
