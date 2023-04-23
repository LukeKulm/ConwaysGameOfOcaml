open OUnit2
open Life
open Cell
open World
open Util

let get_alive_test (name : string) (input : Cell.t) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Cell.get_alive input) ~printer:string_of_bool

let update_cell_test_bool (name : string) (input : Cell.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (input |> Cell.cell_update |> Cell.get_alive)
    ~printer:string_of_bool

let printer_help_tuple (a, b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"

let print_to_string d = Util.string_of_list printer_help_tuple d

let cell_tests =
  [
    get_alive_test
      "Cell initalized with true boolean should return true (meaning alive)"
      (Cell.init_cell true (0, 0) [])
      true;
    get_alive_test
      "Cell initalized with false boolean should return false (meaning dead)"
      (Cell.init_cell false (0, 0) [])
      false;
  ]

let get_dims_test (name : string) (input : World.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (World.get_dims input)
    ~printer:printer_help_tuple

let dims_invalid_test (name : string) (input_1 : int) (input_2 : int) : test =
  name >:: fun _ ->
  assert_raises
    (World.InvalidDims (input_1, input_2))
    (fun () -> World.init_world input_1 input_2)

let world_get_alive_test (name : string) (input : World.t)
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (World.get_alive input) ~printer:print_to_string

let world_tests =
  [
    get_dims_test
      "get_dims called on world initialized with 30 width and 30 height \
       returns (30,30)"
      (World.init_world 30 30) (30, 30);
    get_dims_test
      "get_dims called on world initialized with 40 width and 30 height \
       returns (40,30)"
      (World.init_world 40 30) (40, 30);
    dims_invalid_test
      "trying to initialize a world with width 0 and height 100 raises \
       InvalidDims error"
      0 100;
    dims_invalid_test
      "trying to initialize a world with width 100 and height 0 raises \
       InvalidDims error"
      100 0;
    dims_invalid_test
      "trying to initialize a world with width 0 and height 0 raises \
       InvalidDims error"
      0 0;
  ]

let suite =
  "test suite for Final Project" >::: List.flatten [ cell_tests; world_tests ]

let _ = run_test_tt_main suite
