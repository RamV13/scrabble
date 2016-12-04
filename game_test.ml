open OUnit2
open Game
open Grid

let s = create_game "user" "game"
let move = {tiles_placed = []; player = "user"; swap = []}
let tp1 = [((7,7),'a');((7,8),'b');((8,7),'c')]
let tp2 = [((6,7),'a');((6,8),'b');((6,9),'c')]
let tp3 = [((7,7),'a');((7,8),'b');((7,9),'c');((7,11),'d')]
let tp4 = [((7,7),'a');((7,8),'b');((7,9),'c');((7,10),'d')]

let init_p1 () = {
  player_name = "alan";
  tiles = ['a';'p';'e';'r';'l';'o';'s'];
  score = 0;
  order = 0;
  ai = false
}

let init_p2 () = {
  player_name = "bob";
  tiles = ['b';'q';'f';'s';'e';'o';'s'];
  score = 0;
  order = 1;
  ai = false
}

let init_p3 () = {
  player_name = "chris";
  tiles = ['e';'h';'v';'w';'j';'n';'?'];
  score = 0;
  order = 2;
  ai = true
}

let init_p4 () = {
  player_name = "don";
  tiles = [];
  score = 0;
  order = 3;
  ai = true
}

let init_st () = {
  name = "game";
  grid = Grid.empty;
  players = [init_p1 ();init_p2 ();init_p3 ();init_p4 ()];
  remaining_tiles = ['a';'b';'c';'d';'e';'f';'g';'h';'i'];
  turn = 0;
  score_history = [0;0;0;0;0;0]
}

let validate_cg s : bool =
  let players = s.players in
  let tiles_full = List.fold_left (fun acc p -> acc && (List.length p.tiles = 7)) true players in
  let ais = List.fold_left (fun acc p -> acc && p.ai) true (List.tl players) in
  s.grid = Grid.empty && tiles_full && (not (List.hd players).ai) && ais && s.turn = 0 && s.score_history = [0;0;0;0;0;0]

let create_game_tests = [
  "standard test1" >::
    (fun _ -> assert_equal
      true
      (create_game "user" "game" |> validate_cg));
  "standard test1" >::
    (fun _ -> assert_equal
      true
      (create_game "user" "game" |> validate_cg));
]

let st1 = init_st ()
let add_result = add_player st1 "joe"
let add_player_tests = [
  "replaced first ai" >:: (fun _ -> assert_equal
    2
    add_result);
  "no longer ai" >:: (fun _ -> assert_equal
    false
    ((List.nth (st1.players) 2).ai));
  "same order" >:: (fun _ -> assert_equal
    (init_p3 ()).order
    ((List.nth (st1.players) 2).order));
  "same tiles" >:: (fun _ -> assert_equal
    (init_p3 ()).tiles
    ((List.nth (st1.players) 2).tiles));
  "same score" >:: (fun _ -> assert_equal
    (init_p3 ()).score
    ((List.nth (st1.players) 2).score));
  "add duplicate" >:: (fun _ -> assert_raises
    PlayerExists
    (fun () -> add_player st1 "joe"));
  "game full" >:: (fun _ -> assert_raises
    Full
    (fun () -> add_player {st1 with players = [init_p1 ();init_p2 ()]} "jake"));
]

let st2 = init_st ()
let (removed, removed_turn) = remove_player st2 "alan"
let remove_player_tests = [
  "removed alan" >:: (fun _ -> assert_equal
    ("(AI)",0)
    (String.sub removed ((String.length removed) - 4) 4, removed_turn));
  "now ai" >:: (fun _ -> assert_equal
    true
    ((List.nth (st2.players) removed_turn).ai));
  "same order" >:: (fun _ -> assert_equal
    (init_p1 ()).order
    ((List.nth (st2.players) removed_turn).order));
  "same tiles" >:: (fun _ -> assert_equal
    (init_p1 ()).tiles
    ((List.nth (st2.players) removed_turn).tiles));
  "same score" >:: (fun _ -> assert_equal
    (init_p1 ()).score
    ((List.nth (st2.players) removed_turn).score));
]

let error_tests = [
  "(1) not horiz or vert" >::(fun _ -> assert_raises
    (FailedMove "tiles must be placed horizontally or vertically")
    (fun () -> execute s {move with tiles_placed = tp1}));
  "(2) not on star" >:: (fun _ -> assert_raises
    (FailedMove "first move must have one tile on star")
    (fun () -> execute s {move with tiles_placed = tp2}));
  "(3) gap in tiles" >:: (fun _ -> assert_raises
    (FailedMove "gap in tiles placed")
    (fun () -> execute s {move with tiles_placed = tp3}));
  "(4) not a word" >:: (fun _ -> assert_raises
    (FailedMove "illegimate word(s) formed: abcd")
    (fun () -> execute s {move with tiles_placed = tp4}));
]

let st3 = init_st ()
let is_over_tests = [
  "6 zero score turns" >:: (fun _ -> assert_equal true (is_over st3));
  "empty tile rack" >:: (fun _ -> assert_equal
    true
    (is_over {st3 with remaining_tiles = []; score_history = [1;1;1;1;1;1]}));
  "not over 1" (* still have tiles remaining*) >:: (fun _ -> assert_equal
    false
    (is_over {st3 with score_history = [1;1;1;1;1;1]}));
  "not over 2" (* everyone still has tiles*) >:: (fun _ -> assert_equal
    false
    (is_over {st3 with remaining_tiles = []; players = [init_p1 ();init_p2 ();init_p3 ()]; score_history = [1;1;1;1;1;1]}));
]

let tests =
  "test suite for game"  >::: error_tests @ is_over_tests @ create_game_tests @
  add_player_tests @ remove_player_tests

let _ = run_test_tt_main tests
