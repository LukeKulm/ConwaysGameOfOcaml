(* to create the list of neighbors for cell at pos (x,y) in array a, here are
   the indices of who we want to check: a[x][y+1] a[x][y-1] a[x+1][y+1]
   a[x+1][y-1] a[x-1][y] a[x+1][y] a[x-1][y-1] a[x-1][y+1]*)

exception NotImplemented

type t = {
  cols : int;
  rows : int;
  cells : Cell.t list list;
}

let rec in_selected lst pt =
  List.filter (fun s -> if s = pt then true else false) lst

let init_row = raise NotImplemented
let init_world = raise NotImplemented

(* option: make a list list of the right dimensions with all ones, and then use
   map to actually do the right thing?*)
(* for all BUT the points in the list , we want to initialize as FALSE, so: if
   in_selected . . . true, else . . . false*)
let world_update = raise NotImplemented
