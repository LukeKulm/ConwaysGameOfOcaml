exception NotImplemented

type t = {
  alive : bool;
  position : int * int;
  neighbors : t list;
}

let init_cell status pos nlist =
  { alive = status; position = pos; neighbors = nlist }

let get_alive cell = cell.alive
let check_neighbor cell = if cell.alive = true then 1 else 0

let check_neighbors cell =
  List.fold_left ( + ) 0 (List.map check_neighbor cell.neighbors)

let cell_alive_update cell =
  if check_neighbors cell = 2 || check_neighbors cell = 3 then
    init_cell true cell.position cell.neighbors
  else init_cell false cell.position cell.neighbors

let cell_dead_update cell =
  if check_neighbors cell = 3 then init_cell true cell.position cell.neighbors
  else init_cell false cell.position cell.neighbors

let cell_update cell =
  if cell.alive = true then cell_alive_update else cell_dead_update
