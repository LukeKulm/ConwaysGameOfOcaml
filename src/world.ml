exception NotImplemented
exception InvalidDims of (int * int)
exception InvalidPts

type cell =
  | Dead
  | Alive

type t = cell array array

let init_world width height =
  if width > 0 && height > 0 then Array.make_matrix height width Dead
  else raise (InvalidDims (width, height))

let get_dims world = (Array.length world.(0), Array.length world)

let set_cell world x y cell =
  if y > 0 && x > 0 then
    match get_dims world with
    | a, b -> if x < a && y < b then world.(y).(x) <- cell else raise InvalidPts

let get_cell world x y = world.(y).(x)

let init_world_with_alive width height alive =
  let world = init_world width height in
  List.iter (fun (x, y) -> set_cell world x y Alive) alive;
  world

let get_alive world =
  let alive = ref [] in
  for y = 0 to Array.length world - 1 do
    for x = 0 to Array.length world.(0) - 1 do
      if get_cell world x y = Alive then alive := (x, y) :: !alive
    done
  done;
  !alive

let number_living_neighbors world x y =
  let count = ref 0 in
  let add_alive x y =
    let x' = (x + Array.length world.(0)) mod Array.length world.(0) in
    let y' = (y + Array.length world) mod Array.length world in
    if get_cell world x' y' = Alive then count := !count + 1
  in

  add_alive (x - 1) (y - 1);
  add_alive x (y - 1);
  add_alive (x + 1) (y - 1);
  add_alive (x - 1) y;
  add_alive (x + 1) y;
  add_alive (x - 1) (y + 1);
  add_alive x (y + 1);
  add_alive (x + 1) (y + 1);
  !count

let update_world world =
  let new_world = init_world (Array.length world.(0)) (Array.length world) in
  for y = 0 to Array.length world - 1 do
    for x = 0 to Array.length world.(0) - 1 do
      let alive_neighbors = number_living_neighbors world x y in
      match get_cell world x y with
      | Dead -> if alive_neighbors = 3 then set_cell new_world x y Alive
      | Alive ->
          if alive_neighbors < 2 || alive_neighbors > 3 then
            set_cell new_world x y Dead
          else set_cell new_world x y Alive
    done
  done;
  new_world

let print_world world =
  Array.iter
    (fun row ->
      Array.iter
        (fun cell ->
          match cell with
          | Dead -> print_string "."
          | Alive -> print_string "H")
        row;
      print_newline ())
    world

let rec run world n =
  if n = 0 then ()
  else begin
    print_world world;
    print_newline ();
    run (update_world world) (n - 1)
  end

(* let w = init_world 10 10;;

   set_cell w 1 2 Alive; set_cell w 2 3 Alive; set_cell w 3 1 Alive; set_cell w
   3 2 Alive; set_cell w 3 3 Alive; run w 10 *)
