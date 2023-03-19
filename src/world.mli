type t
(*abstract type . . . *)

val init_world : int -> int -> (int * int) list -> t
(* Initializes a world that is of x by y dimensions, where the int by int list
   are the coordinates of the originally alive cells *)

val world_update : t -> t
(* Update the world by calling upon each cell to update itself: important to
   note that the order in whcih we do so matters*)

exception NotImplemented
