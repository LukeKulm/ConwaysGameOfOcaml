open Graphics
open Life.Display
open Life.World

let rec animate state =
  clear_graph ();
  draw_frame state;
  Unix.sleepf 0.5;
  animate (update_world state)

let start () =
  open_graph
    (" "
    ^ string_of_int (fst (window_size initial))
    ^ "X"
    ^ string_of_int (snd (window_size initial)));
  set_window_title "The Game of Life";
  animate initial
(* Wait for a short time before updating the cells again *)

let _ = start ()
