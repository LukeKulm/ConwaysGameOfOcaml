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

let make_neighbors pt = raise NotImplemented
let init_row = raise NotImplemented

let init_cells col row =
  match row with
  | h :: t -> []
  | [] -> []
(* use map!!!!! *)
(*col and row are each lists up to that int*)
(* essentially want a for loop: step 1: make a list of cell objects with empty
   neighbor lists that is col long step 2: call map on this list with function
   make_neighbors*)

let rec list_upto_helper i n =
  if i = n then [] else i :: list_upto_helper (i + 1) n

let list_upto n = list_upto_helper 0 n

let init_world c r =
  { cols = c; rows = r; cells = init_cells (list_upto c) (list_upto r) }

(* option: make a list list of the right dimensions with all ones, and then use
   map to actually do the right thing?*)
(* for all BUT the points in the list , we want to initialize as FALSE, so: if
   in_selected . . . true, else . . . false*)
let world_update = raise NotImplemented
