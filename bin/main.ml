open Graphics
open Life.Display
open Life.World

let frame_rate = 0.01

let rec animate state =
  clear_graph ();
  draw_frame state;
  Unix.sleepf frame_rate;
  animate (update_world state)

let rec start () =
  open_graph
    (" "
    ^ string_of_int (fst (window_size initial))
    ^ "X"
    ^ string_of_int (snd (window_size initial)));
  set_window_title "The Game of Life";
  draw_string "Press a Key When You're Happy";
  if (wait_next_event [ Key_pressed ]).keypressed then animate initial
  else start ()
(* Wait for a short time before updating the cells again *)

let _ = start ()
