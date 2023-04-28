open OUnit2
open Life
open World
open Util
open Display

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

let printer_help_tuple (a, b) =
  "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"

let print_to_string d = Util.string_of_list printer_help_tuple d

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

let window_size_test (name : string) (input : World.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Display.window_size input)
    ~printer:printer_help_tuple

let rows_test (name : string) (input : World.t) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (Display.rows input) ~printer:string_of_int

let columns_test (name : string) (input : World.t) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Display.columns input) ~printer:string_of_int

let display_tests =
  [
    rows_test "rows called on a world with dimensions 4*8 should return 4"
      (World.init_world 4 8) 4;
    columns_test "columns called on a world with dimensions 4*8 should return 8"
      (World.init_world 4 8) 8;
  ]

let grid_stable_square =
  World.init_world_with_alive 4 4 [ (1, 1); (1, 2); (2, 1); (2, 2) ]

let grid_pre_stable_square =
  World.init_world_with_alive 4 4 [ (1, 1); (1, 2); (2, 2) ]

let grid_three_tall = World.init_world_with_alive 5 5 [ (2, 1); (2, 2); (2, 3) ]
let grid_three_wide = World.init_world_with_alive 5 5 [ (1, 2); (2, 2); (3, 2) ]
let grid_diag = World.init_world_with_alive 5 5 [ (1, 1); (2, 2); (3, 3) ]
let grid_triomino = World.init_world_with_alive 5 5 [ (1, 1); (1, 2); (2, 3) ]

let update_test (name : string) (input : World.t)
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (World.update_world input |> World.get_alive)
    ~printer:print_to_string

let update_tests =
  [
    update_test "update called on stable 2*2 square returns the same config"
      grid_stable_square
      (World.get_alive grid_stable_square);
    update_test
      "update called on 3/4 full square returns stable square after one time"
      grid_pre_stable_square
      (World.get_alive grid_stable_square);
    update_test "update called on three vertical returns three horizontal"
      grid_three_tall
      (World.get_alive grid_three_wide);
    update_test "update called on three horizontal returns three vertical"
      grid_three_wide
      (World.get_alive grid_three_tall);
    update_test "update called on diag returns just middle point" grid_diag
      [ (2, 2) ];
    update_test
      "update called on final triomino pattern returns two alive points in \
       horizontal pattern along middle axis"
      grid_triomino
      [ (1, 2); (2, 2) ];
  ]

let suite =
  "test suite for Final Project"
  >::: List.flatten [ world_tests; display_tests; update_tests ]

let _ = run_test_tt_main suite
