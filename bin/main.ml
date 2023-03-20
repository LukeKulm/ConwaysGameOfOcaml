open Graphics

let rec animate () =
  open_graph " 500X500";
  set_window_title "The Game of Life";
  Unix.sleepf 0.05;
  (* Wait for a short time before updating the cells again *)
  animate ()

let _ = animate ()
