exception NotImplemented

type t = { alive : bool }

let init_cell status = { alive = status }
let get_alive cell = cell.alive
let cell_update cell status = raise NotImplemented
