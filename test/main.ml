open OUnit2
open Life
open World
open Util
open Display

(** Test Plan and Approach: Our project is largely centered around a GUI, and as
    a result, a large portion of our testing was done via manual observation of
    the behavior of the GUI. We automated testing with OUnit for the game logic,
    contained in the World module. A large portion of this testing was black box
    and semi randomized, and the semi randomized fuzz style testing should help
    tease out potential edge cases that we had not thought of. We also made use
    of some glass box testing by targeting particular branches of the game logic
    in our implementation.

    As previously mentioned, most of our testing for the display module was
    manual black box testing, where we used and interacted with our GUI and
    checked that it behaved as we wanted. Testing for certain visual
    characteristics, or for I/O behaviors like clicking and keyboard input, was
    much easier for us to test by actually using the GUI than it would be by
    devoloping complex OUnit tests. We were also able to incorporate some white
    box manual testing into our experimentation by performing certain actions
    that targeted specific code branches in display.ml.

    We have used our GUI a lot in our testing, and as we have found bugs we have
    fixed them. We have also extensively tested the game logic, and by checking
    every permutation a single cell configuration, we can be certain that our
    game logic is correct and will not fail us. From our extensive manual
    testing of the GUI, we have demonstrated the correct mapping of our correct
    logic to the display, and so we can be confident that the display works
    properly. There are few inputs and behaviors that we have not tried
    ourselves, and as such we can be confident that the game will work properly
    for any input that the user throws at it. *)

let id_string s = s

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

(***************************************************************************
  Begin Terminal Tests ***************************************************
  ************************************************************************ *)
let glider_world =
  init_world_with_alive 8 8 [ (0, 4); (1, 4); (2, 4); (2, 5); (1, 6) ]

let glider_top =
  init_world_with_alive 8 8 [ (0, 0); (1, 0); (2, 0); (2, 1); (1, 2) ]

let glider_corner =
  init_world_with_alive 8 8 [ (5, 7); (6, 7); (7, 7); (7, 6); (6, 5) ]

let glider_corner_5 =
  World.(
    update_world
      (update_world (update_world (update_world (update_world glider_corner)))))

let glider_corner_10 =
  World.(
    update_world
      (update_world
         (update_world (update_world (update_world glider_corner_5)))))

(* This small pattern "blows up" to create a large area of active cells with
   complicated interactions. We are computing our expected values with a
   different implementation of the game of life from the internet. We can then
   compare our output to the output of the other implemntation and compare*)
let bomb =
  init_world_with_alive 60 40
    [
      (29, 20);
      (30, 20);
      (31, 20);
      (29, 21);
      (31, 21);
      (29, 22);
      (31, 22);
      (30, 21);
    ]

let rec update world n =
  match n with
  | 0 -> world
  | _ -> update (World.update_world world) (n - 1)

let bomb_10 = update bomb 10
let bomb_50 = update bomb 50
let bomb_100 = update bomb 100
let bomb_150 = update bomb 150
let bomb_200 = update bomb 200

let logic_test (name : string) (input : World.t) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (World.world_to_string input) ~printer:id_string

(* The tests below may seem verbose, but they serve as documentation of the
   extensive testing that has been done by observing our automata in action. *)
let logic_tests =
  [
    logic_test "glider test: step 0" glider_world
      "\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       HHH.....\n\
       ..H.....\n\
       .H......\n\
       ........\n";
    logic_test "glider test: step 1"
      (World.update_world glider_world)
      "\n\
       ........\n\
       ........\n\
       ........\n\
       .H......\n\
       .HH.....\n\
       H.H.....\n\
       ........\n\
       ........\n";
    logic_test "glider test: step 2"
      (World.update_world glider_world |> World.update_world)
      "\n\
       ........\n\
       ........\n\
       ........\n\
       .HH.....\n\
       H.H.....\n\
       ..H.....\n\
       ........\n\
       ........\n";
    logic_test "glider test: step 3"
      (World.update_world glider_world
      |> World.update_world |> World.update_world)
      "\n\
       ........\n\
       ........\n\
       ........\n\
       .HH.....\n\
       ..HH....\n\
       .H......\n\
       ........\n\
       ........\n";
    logic_test "glider test: step 4"
      (World.update_world glider_world
      |> World.update_world |> World.update_world |> World.update_world)
      "\n\
       ........\n\
       ........\n\
       ........\n\
       .HHH....\n\
       ...H....\n\
       ..H.....\n\
       ........\n\
       ........\n";
    logic_test "test wraparound edges top: step 0" glider_top
      "\n\
       HHH.....\n\
       ..H.....\n\
       .H......\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n";
    logic_test "test wraparound edges top: step 1"
      (World.update_world glider_top)
      "\n\
       .HH.....\n\
       H.H.....\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .H......\n";
    logic_test "test wraparound edges top: step 2"
      (World.update_world glider_top |> World.update_world)
      "\n\
       H.H.....\n\
       ..H.....\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .HH.....\n";
    logic_test "test wraparound edges top: step 3"
      (World.update_world glider_top |> World.update_world |> World.update_world)
      "\n\
       ..HH....\n\
       .H......\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .HH.....\n";
    logic_test "test wraparound edges top: step 4"
      (World.update_world glider_top
      |> World.update_world |> World.update_world |> World.update_world)
      "\n\
       ...H....\n\
       ..H.....\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .HHH....\n";
    logic_test "test wraparound edges corner: step 0" glider_corner
      "\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ......H.\n\
       .......H\n\
       .....HHH\n";
    logic_test "test wraparound edges corner: step 1"
      World.(update_world glider_corner)
      "\n\
       ......H.\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .....H.H\n\
       ......HH\n";
    logic_test "test wraparound edges corner: step 2"
      World.(update_world (update_world glider_corner))
      "\n\
       ......HH\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .......H\n\
       .....H.H\n";
    logic_test "test wraparound edges corner: step 3"
      World.(update_world (update_world (update_world glider_corner)))
      "\n\
       ......HH\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ......H.\n\
       H......H\n";
    logic_test "test wraparound edges corner: step 4"
      World.(
        update_world (update_world (update_world (update_world glider_corner))))
      "\n\
       H.....HH\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .......H\n\
       H.......\n";
    logic_test "test wraparound edges corner: step 5" glider_corner_5
      "\n\
       H......H\n\
       .......H\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       H.....H.\n";
    logic_test "test wraparound edges corner: step 6"
      (World.update_world glider_corner_5)
      "\n\
       H.....H.\n\
       H......H\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       H.......\n";
    logic_test "test wraparound edges corner: step 7"
      World.(update_world (update_world glider_corner_5))
      "\n\
       HH......\n\
       H......H\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       .......H\n";
    logic_test "test wraparound edges corner: step 8"
      World.(update_world (update_world (update_world glider_corner_5)))
      "\n\
       .H......\n\
       HH.....H\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       H.......\n";
    logic_test "test wraparound edges corner: step 9"
      World.(
        update_world
          (update_world (update_world (update_world glider_corner_5))))
      "\n\
       .H.....H\n\
       HH......\n\
       H.......\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n";
    logic_test "test wraparound edges corner: step 10" glider_corner_10
      "\n\
       .H......\n\
       .H.....H\n\
       HH......\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n";
    logic_test "test wraparound edges corner: step 11"
      (World.update_world glider_corner_10)
      "\n\
       H.......\n\
       .HH.....\n\
       HH......\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n";
    logic_test "test wraparound edges corner: step 12"
      World.(update_world (update_world glider_corner_10))
      "\n\
       .H......\n\
       ..H.....\n\
       HHH.....\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n\
       ........\n";
    logic_test "semi random 'bomb' test: step 0" bomb
      "\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .............................HHH............................\n\
       .............................HHH............................\n\
       .............................H.H............................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n";
    logic_test "semi random 'bomb' test: step 10" bomb_10
      "\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .............................HHH............................\n\
       ............................H...H...........................\n\
       ...........................H.....H..........................\n\
       ..........................H...H...H.........................\n\
       ..........................H.......H.........................\n\
       ..........................H.H...H.H.........................\n\
       ...........................HH...HH..........................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n";
    logic_test "semi random 'bomb' test: step 50" bomb_50
      "\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .............................HHH............................\n\
       ..............................H.............................\n\
       ............................HH.HH...........................\n\
       ............................HH.HH...........................\n\
       ........................HH..H...H..HH.......................\n\
       .......................H.HH.H...H.HH.H......................\n\
       .......................H.............H......................\n\
       ......................H..HH.......HH..H.....................\n\
       ............................................................\n\
       .......................HH.....H.....HH......................\n\
       ..............................H.............................\n\
       ...................HHH........H........HHH..................\n\
       ...................H.HH...............HH.H..................\n\
       .....................HH...............HH....................\n\
       ......................HH..HH.....HH..HH.....................\n\
       ................HH.....H.............H.....HH...............\n\
       ...............H..H....HH..H.....H..HH....H..H..............\n\
       ...............HH.H.....HHH.......HHH.....H.HH..............\n\
       ...............HH.HH.....................HH.HH..............\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n";
    logic_test "semi-random 'bomb' test: step 100" bomb_100
      "\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .......................HH....HHH....HH......................\n\
       .......................HH...........HH......................\n\
       ............................................................\n\
       ................H...........................H...............\n\
       ...............HH...........................HH..............\n\
       ..............H..H.........................H..H.............\n\
       .............HH.H...........................H.HH............\n\
       .............H.HH...........................HH.H............\n\
       .............HHH.........H.........H.........HHH............\n\
       ........................H...........H.......................\n\
       .........................H.HH...HH.H........................\n\
       ....................H....H.........H....H...................\n\
       .................HH.H....H...H.H...H....H.HH................\n\
       ...............H..H.......HH.H.H.HH.......H..H..............\n\
       ...............HH...........H...H...........HH..............\n\
       ............................................................\n\
       ............................................................\n\
       ......................H.H...........H.H.....................\n\
       ......................H..H..HH.HH..H..H.....................\n\
       ......................H..H.........H..H.....................\n\
       .......................HH...HH.HH...HH......................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n";
    logic_test "semi-random 'bomb' test: step 150" bomb_150
      "\n\
       ....................HH.................HH...................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .............................HHH............................\n\
       ............................................................\n\
       ..............HH.............................HH.............\n\
       ............HH..H...........................H..HH...........\n\
       .................H.........................H................\n\
       .................H.........................H................\n\
       .................H.........................H................\n\
       ..........H............H.............H............H.........\n\
       ................H........H.........H........H...............\n\
       ...........H..H.....H....H.........H....H.....H..H..........\n\
       .............HH.....H....H.........H....H.....HH............\n\
       ..........HH........H..HH...........HH..H........HH.........\n\
       .....................H.................H....................\n\
       ........H.....H...............................H.....H.......\n\
       ........H.....H...............................H.....H.......\n\
       ........H.....H...............................H.....H.......\n\
       ............................................................\n\
       ..........HHH...................................HHH.........\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ................H.H.......................H.H...............\n\
       ................H.H.......................H.H...............\n\
       ................H..H.....................H..H...............\n\
       .................HH...H...............H...HH................\n\
       ................H..H...H.............H...H..H...............\n\
       ................HHH.....H...........H.....HHH...............\n\
       .................H.....H.............H.....H................\n\
       ..................HHHHH...............HHHHH.................\n";
    logic_test "semi-random 'bomb' test: step 200" bomb_200
      "\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       .............................HHH............................\n\
       ............................................................\n\
       .............HH...............................HH............\n\
       .............HH...............................HH............\n\
       .................HH.......................HH................\n\
       .................HH.......................HH................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ........H...........................................H.......\n\
       ........H...........................................H.......\n\
       ........H...........................................H.......\n\
       ............................................................\n\
       ..........HHH...................................HHH.........\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ............................................................\n\
       ..............HH.............................HH.............\n\
       .............H..H........HH.......HH........H..H............\n\
       .............H..H........HH.......HH........H..H............\n\
       ..............HH.............................HH.............\n";
  ]

(*****************************************************************************
  End Terminal Tests ***************************************************
  ************************************************************************ *)

let suite =
  "test suite for Final Project"
  (* >::: List.flatten [ world_tests; display_tests; update_tests ] *)
  >::: List.flatten [ logic_tests ]

let _ = run_test_tt_main suite
