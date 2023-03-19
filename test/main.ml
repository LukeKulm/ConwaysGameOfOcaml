open OUnit2
open Life
open Cell

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

(* should we also test that the neighbors and position didn't change? if so: let
   update_cell_test_neighbors = . . . . . *)
let cell_tests =
  [
    get_alive_test
      "Cell initalized with true boolean should return true (meaning alive)"
      (Cell.init_cell true (0, 0) [])
      true;
    get_alive_test
      "Cell initalized with false boolean should return false (meaning dead"
      (Cell.init_cell false (0, 0) [])
      false;
  ]

let suite = "test suite for Final Project" >::: List.flatten [ cell_tests ]
let _ = run_test_tt_main suite
