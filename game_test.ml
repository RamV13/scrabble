open OUnit2
open Game
open Grid

let init_s () = create_game "user" "game"
let move = {tiles_placed = []; player = "user"; swap = []}
let tp1 = [((7,7),'A');((7,8),'B');((8,7),'C')]
let tp2 = [((6,7),'A');((6,8),'B');((6,9),'C')]
let tp3 = [((7,7),'A');((7,8),'B');((7,9),'C');((7,11),'D')]
let tp4 = [((7,7),'A');((7,8),'B');((7,9),'C');((7,10),'D')]
let tp5 = [((7,5),'R');((7,6),'O');((7,7),'P');((7,8),'E')]
let tp6 = [((5,5),'S');((6,5),'T');((8,5),'I');((9,5),'K');((10,5),'E')]
let tp7 = [((6,8),'W');((8,8),'N')]
let tp8 =
  [((7,9),'W');((7,10),'A');((7,11),'L');((7,12),'K');((7,13),'E');((7,14),'R')]

let init_p1 () = {
  player_name = "alan";
  tiles = ['A';'P';'E';'R';'L';'O';'S'];
  score = 0;
  order = 0;
  ai = false
}

let init_p2 () = {
  player_name = "bob";
  tiles = ['T';'I';'K';'S';'E';'O';'S'];
  score = 0;
  order = 1;
  ai = false
}

let init_p3 () = {
  player_name = "chris";
  tiles = ['E';'H';'V';'W';'J';'N';'?'];
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

let init_p5 () = {
  player_name = "don";
  tiles = ['W';'A';'L';'K';'E';'R';'S'];
  score = 0;
  order = 3;
  ai = true
}

let init_st () = {
  name = "game";
  grid = Grid.empty;
  players = [init_p1 ();init_p2 ();init_p3 ();init_p4 ()];
  remaining_tiles = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'A';'B';'C';'D';'E';
    'F';'G';'H';'I';'A';'B';'C';'D';'E';'F';'G';'H';'I';];
  turn = 0;
  score_history = [0;0;0;0;0;0]
}

let validate_cg s =
  let players = s.players in
  let tiles_full = List.fold_left
    (fun acc p -> acc && (List.length p.tiles = 7))
    true players
  in
  let ais = List.fold_left (fun acc p -> acc && p.ai) true (List.tl players) in
  s.grid = Grid.empty && tiles_full && (not (List.hd players).ai) && ais &&
  s.turn = 0 && s.score_history = [-1;-1;-1;-1;-1;-1]

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

let st5 = init_s ()
let st6 = init_st ()
let _ = execute st6 {move with tiles_placed = tp5; player = "alan"}
let error_tests = [
  "(1) not horiz or vert" >::(fun _ -> assert_raises
    (FailedMove "tiles must be placed horizontally or vertically")
    (fun () -> execute st5 {move with tiles_placed = tp1}));
  "(2) not on star" >:: (fun _ -> assert_raises
    (FailedMove "first move must have one tile on star")
    (fun () -> execute st5 {move with tiles_placed = tp2}));
  "(3) gap in tiles" >:: (fun _ -> assert_raises
    (FailedMove "gap in tiles placed")
    (fun () -> execute st5 {move with tiles_placed = tp3}));
  "(4) not a word" >:: (fun _ -> assert_raises
    (FailedMove "illegimate word(s) formed: abcd")
    (fun () -> execute st5 {move with tiles_placed = tp4}));
  "(5) word away from others" >:: (fun _ -> assert_raises
    (FailedMove "cannot place tiles apart from existing ones")
    (fun () -> execute st6 {move with tiles_placed = [((0,0),'a');((0,1),'b')];
    player = "bob"}));
  "(6) char away from others" >:: (fun _ -> assert_raises
    (FailedMove "cannot place single tile by itself")
    (fun () -> execute st6 {move with tiles_placed = [((0,0),'a')];
    player = "bob"}));
  "(7) failed swap" >:: (fun _ -> assert_raises
    (FailedMove "less than 7 tiles remain in the bag")
    (fun () -> execute {st6 with remaining_tiles = ['A']} {tiles_placed = [];
      player = "bob"; swap = ['T';'I']}));
  "(8) not your turn" >:: (fun _ -> assert_raises
    (FailedMove "it's not your turn")
    (fun () -> execute st6 {move with tiles_placed = [((0,0),'a')];
    player = "alan"}));
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
    (is_over {st3 with remaining_tiles = []; players = [init_p1 ();init_p2 ();
      init_p3 ()]; score_history = [1;1;1;1;1;1]}));
]

let st4 = {(init_st ()) with players = [init_p1 (); init_p2 (); init_p3 ();
  init_p5 ()]}
let do_stuff () =
  (* diff [d], tiles placed [tp], move turn was [turn], [exp_score] is expected
   * score
   * Also checks json functions for diffs *)
  let check_move tp turn exp_score player =
    let d = execute st4 {move with tiles_placed = tp; player = player} in
    let d' = d |> diff_to_json |> Yojson.Basic.from_string |> diff_from_json in
    assert (d = d');
    let p = List.hd d.players_diff in
    d.board_diff = tp && d.new_turn_val = (turn+1) mod 4 && p.score = exp_score
  in
  let first_move = check_move tp5 st4.turn 12 "alan" in
  let triple_letter = check_move tp6 st4.turn 22 "bob" in
  let double_letter = check_move tp7 st4.turn 11 "chris" in
  let triple_word = check_move tp8 st4.turn 60 "don" in
  first_move && triple_letter && double_letter && triple_word

let execute_tests = [
  "sequence of moves" >::(fun _ -> assert_equal
    true
    (do_stuff ()));
]

let tests =
  "test suite for game"  >::: error_tests @ is_over_tests @ create_game_tests @
  add_player_tests @ remove_player_tests @ execute_tests

let _ = run_test_tt_main tests
