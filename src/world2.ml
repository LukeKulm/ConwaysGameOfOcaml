(* to create the list of neighbors for cell at pos (x,y) in array a, here are
   the indices of who we want to check: a[x][y+1] a[x][y-1] a[x+1][y+1]
   a[x+1][y-1] a[x-1][y] a[x+1][y] a[x-1][y-1] a[x-1][y+1]*)

exception NotImplemented

type t = {
  cols : int;
  rows : int;
  cells : Cell.t list list;
}

let get_dims grid = (grid.cols, grid.rows)

let rec in_selected lst pt =
  List.filter (fun s -> if s = pt then true else false) lst

let make_neighbors pt = raise NotImplemented
let init_row = raise NotImplemented

let rec init_matrix (col : int list) (row : int list) (acc : int list list) =
  match row with
  | h :: t -> init_matrix col t (col :: acc)
  | [] -> acc

let rec list_upto_helper i n =
  if i = n then [] else i :: list_upto_helper (i + 1) n

let list_upto n = list_upto_helper 0 n

let rec init_cell_helper matrix row_counter acc =
  match matrix with
  | h :: t -> (
      match h with
      | h :: t -> Cell.init_cell false (h, row_counter) [] :: acc
      | [] -> init_cell_helper t (row_counter + 1) [])
  | [] -> acc

(* essentially just want to call Cell.init_cell, passing the element's row and
   column as the indices*)
let init_cells col row = init_cell_helper (init_matrix col row []) 0 []

let init_world c r =
  { cols = c; rows = r; cells = [ init_cells (list_upto c) (list_upto r) ] }

let world_update = raise NotImplemented
