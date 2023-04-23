open OUnit2
open Life
open Cell
open World
open Util

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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
  assert_equal ~cmp:cmp_set_like_lists expected_output (World.get_alive input)
    ~printer:print_to_string

let init_alive_invalid_test (name : string) (width : int) (height : int)
    (input_3 : (int * int) list) : test =
  name >:: fun _ ->
  assert_raises World.InvalidPts (fun () ->
      World.init_world_with_alive width height input_3)

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
    world_get_alive_test
      "get_alive returns an empty list for a world just initialized using \
       init_world 30 30"
      (World.init_world 30 30) [];
    world_get_alive_test
      "get_alive returns the same list entered when calling init_world alive. \
       Here, shown with one point in middle"
      (World.init_world_with_alive 30 30 [ (14, 14) ])
      [ (14, 14) ];
    world_get_alive_test
      "get_alive returns the same list entered when calling init_world alive. \
       Here, shown with two points: both in column 14, but different rows. "
      (World.init_world_with_alive 30 30 [ (14, 18); (14, 20) ])
      [ (14, 18); (14, 20) ];
    world_get_alive_test
      "get_alive returns the same list entered when calling init_world alive. \
       Here, shown with two points: both in column 14, but different rows. "
      (World.init_world_with_alive 30 30 [ (14, 18); (14, 20) ])
      [ (14, 18); (14, 20) ];
    world_get_alive_test
      "get_alive returns the same list entered when calling init_world alive. \
       Here, shown with two points: both in row 14, but different columns. "
      (World.init_world_with_alive 30 30 [ (10, 14); (2, 14) ])
      [ (10, 14); (2, 14) ];
    world_get_alive_test
      "get_alive returns the empty list  when calling init_world alive with \
       living as empty list"
      (World.init_world_with_alive 30 30 [])
      [];
    init_alive_invalid_test
      "one point in living, out of bounds both height and width. " 4 4
      [ (5, 5) ];
    init_alive_invalid_test "one point in living, out of bounds only in width. "
      4 4
      [ (5, 3) ];
    init_alive_invalid_test
      "one point in living, out of bounds only in height (also testing that \
       the height value itself is out of bounds, should stop at n-1). "
      4 4
      [ (3, 4) ];
    init_alive_invalid_test "two points in living, both are out of bounds. " 4 4
      [ (5, 5); (4, 4) ];
    init_alive_invalid_test
      "one point in living, first is okay, second is out of bounds. " 4 4
      [ (1, 2); (5, 5) ];
    init_alive_invalid_test
      "one point in living, second is okay, first is out of bounds. " 4 4
      [ (10, 20); (3, 1) ];
  ]

let suite =
  "test suite for Final Project" >::: List.flatten [ cell_tests; world_tests ]

let _ = run_test_tt_main suite
