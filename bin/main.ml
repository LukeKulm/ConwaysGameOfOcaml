open Graphics
open Life.Display

let rec animate () =
  open_graph
    (" "
    ^ string_of_int (fst window_size)
    ^ "X"
    ^ string_of_int (snd window_size));
  set_window_title "The Game of Life";
  draw_string "Press a key when your happy with the start (;";
  Unix.sleepf 0.25;
  animate ()
(* Wait for a short time before updating the cells again *)

let _ = animate ()
