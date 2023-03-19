open OUnit2

let get_alive_test (name : string) (input : Cell.t) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Cell.get_alive input) ~printer:String.escaped

let cell_tests = [ get_alive_test ]
let suite = "test suite for A2" >::: List.flatten [ cell_tests ]
let _ = run_test_tt_main suite
