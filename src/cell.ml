module Cell = struct
  exception NotImplemented

  type t = {
    alive : bool;
    position : int * int;
    neighbors : t list;
  }

  let init_cell status pos nlist =
    { alive = status; position = pos; neighbors = nlist }

  let get_alive cell = cell.alive
  let check_neighbors cell = 1
  let cell_update cell = raise NotImplemented
end
