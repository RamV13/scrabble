open OUnit2
open Game

let s = create_game "user" "game"
let move = {tiles_placed = []; player = "user"; swap = []}
let tp1 = [((7,7),'a');((7,8),'b');((8,7),'c')]
let tp2 = [((6,7),'a');((6,8),'b');((6,9),'c')]


let error_tests = [
  "(1) not horiz or vert" >:: 
    (fun _ -> assert_raises 
      (FailedMove "tiles must be placed horizontally or vertically") 
      (fun () -> execute s {move with tiles_placed = tp1}));
  "(2) not on star" >:: (fun _ -> assert_raises 
    (FailedMove "first move must have one tile on star") 
    (fun () -> execute s {move with tiles_placed = tp2}));
]

let tests = 
  "test suite for game"  >::: error_tests
     
let _ = run_test_tt_main tests