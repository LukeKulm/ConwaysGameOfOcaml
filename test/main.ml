open OUnit2

let get_alive_test (name : string) (input : Cell.t) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Cell.get_alive input) ~printer:string_of_bool

let update_cell_test_bool (name : string) (input : Cell.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (input |> Cell.update_cell input |> Cell.get_alive)
    ~printer:string_of_bool

(* should we also test that the neighbors and position didn't change? if so: let
   update_cell_test_neighbors = . . . . . *)
let cell_tests = [ get_alive_test "" ]
let suite = "test suite for Final Project" >::: List.flatten [ cell_tests ]
let _ = run_test_tt_main suite
