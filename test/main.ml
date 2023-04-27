open OUnit2
open Life
open Cell
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

let print_to_string (d : (int * int) list) =
  Util.string_of_list printer_help_tuple d

let updated_get_alive_test (name : string) (input : World.t)
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (World.get_alive input) ~printer:print_to_string

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

let suite = "test suite for Final Project" >::: List.flatten [ cell_tests ]
let _ = run_test_tt_main suite
