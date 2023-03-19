open Graphics

let main () =
  open_graph " 600X600";
  moveto 0 0;
  draw_string "Resize window, then press a key to start...";
  ignore (read_key () : char);
  clear_graph ()

let _ = main ()
