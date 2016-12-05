open OUnit2
open Ai

(* Functions that need to be thoroughly tested:
 * -------------------------------------------
 * find_slots (DONE)
 * get_surroundings (DONE)
 * valid_chars (DONE)
 * makes_move (DONE)
 * makes_prefix (DONE)
 * out_of_bounds (DONE)
 * reverse_str (DONE)
 * find_anchors (DONE)
 * invalid_pos (DONE)
 * get_next (DONE)
 * search_next (DONE)
 * rem (DONE)
 * no_dups_append (DONE)
 * other_dirs_move (DONE)
 * valid_move (DONE)
 * valid_prefix (DONE)
 * lowercasing functions (DONE)
 * build
 * best_move
 *)

let empty_board = Grid.empty

let apple_board = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;Some 'a';Some 'p'; Some 'p'; Some 'l'; Some 'e';None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let nest_board = [[Some 'n';None;Some 't';None;None;None;None;None;None;None;None;None;None;None;None];
                   [Some 'i';None;Some 'e';None;None;None;None;None;None;None;None;None;None;None;None];
                   [Some 'c';None;Some 'a';None;None;None;None;None;None;None;None;None;None;None;None];
                   [Some 'e';Some 'a';Some 't';None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let anti_board = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;Some 'a'; Some 'n'; Some 't'; Some 'i';None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let app_board = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;Some 'a';Some 'p'; Some 'p';None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                   [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let upper_app_board = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;Some 'A';Some 'P'; Some 'P';None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let po_board = [[Some 'p';Some 'o';None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

let all_board = [[None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;Some 'd';None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;Some 'e';None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;Some 'f';None;None;None;None;None;None;None];
                 [None;None;None;None;Some 'g';Some 'e';Some 'a';Some 'p'; Some 'p'; Some 'd';Some 'a';None;None;None;None];
                 [None;None;None;None;None;None;None;Some 'a';None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;Some 's';None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;Some 'e';None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;Some 'b';Some 'a';None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;Some 'c'];
                 [None;Some 'a';Some 'p';None;None;None;None;None;None;None;None;None;None;None;Some 'd'];
                 [Some 'a';None;None;None;None;None;None;None;None;None;None;None;None;None;None]]


let full_board = [[Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'p';Some 'o';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a'];
                  [Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a';Some 'a']]

let sort_slots a b =
  let (r, c) = a in
  let (r', c') = b in
  if (r - r') = 0 then c - c' else r - r'


let app_slots = [(7,5); (7,9); (8,6); (8,7); (8,8); (6,6); (6,7); (6,8)]

let find_slots_test = [
  "No slots" >:: (fun _ -> assert_equal [] (find_slots empty_board));
  "Basic slots length" >:: (fun _ -> assert_equal (List.length app_slots)
                               (find_slots app_board |> List.length));
  "Basic slots direct" >:: (fun _ -> assert_equal
                               (List.sort sort_slots app_slots)
                               (find_slots app_board |> List.sort sort_slots));
]

let blank_surr = {
  left = "";
  right = "";
  above = "";
  below = "";
}

let apple_surr = {blank_surr with left = "pa"; right = "le"}

let string_of_surr surr =
  let li =
    [
      "\nLEFT: " ^ surr.left;
      "RIGHT: " ^ surr.right;
      "ABOVE: " ^ surr.above;
      "BELOW: " ^ surr.below ^ "\n"
    ]
  in
  String.concat "\n" li

let all_surr = {
  left = "aeg";
  right = "pda";
  above = "fed";
  below = "ase"
}

let empty_surr = {
  left = "";
  right = "";
  above = "";
  below = ""
}

let partial_empty_surr = {
  left = "ab";
  below = "cd";
  right = "";
  above = "";
}

let one_surr = {empty_surr with Ai.above = "esapfed"}

let diag_surr = {empty_surr with Ai.right = "d"}

let get_surroundings_test = [
  "No surroundings" >:: (fun _ -> assert_equal blank_surr
                            (get_surroundings empty_board (7,7)));
  "Left right" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr apple_surr
                       (get_surroundings apple_board (7,8)));
  "All sides" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr all_surr
                      (get_surroundings all_board (7,7)));
  "Empty surroundings" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr
                    empty_surr (get_surroundings all_board (0, 0)));
  "Partial empty surr" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr
                               partial_empty_surr
                               (get_surroundings all_board (11, 14)));
  "One surr" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr
                    one_surr (get_surroundings all_board (11, 7)));
  "Diag surr" >:: (fun _ -> assert_equal ~printer:Ai.string_of_surr
                      diag_surr (get_surroundings all_board (4, 6)));
]

let a_surr = get_surroundings apple_board (7, 11)
let a_tiles = ['s'; 'g'; 'f']
let b_tiles = ['s'; 't'; 'a'; 'g']

let c_tiles = ['a';'b';'c';'i';'g']
let c_surr = get_surroundings anti_board (7, 11)

let d_tiles = Ai.alphabet
let d_surr = get_surroundings all_board (7, 11)

let string_of_charlist ch = List.map (to_str) ch |> String.concat ""
let sort_chars a b = Char.code a - Char.code b

let valid_chars_test = [
  "apple + s" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                      (['s']) (valid_chars a_surr a_tiles));
  "apple + s/t" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                        (List.sort sort_chars ['t';'s'])
                        (valid_chars a_surr b_tiles |> List.sort sort_chars));
  "anti-everything" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                        (List.sort sort_chars ['a';'b';'c';'g'])
                        (valid_chars c_surr c_tiles |> List.sort sort_chars));
  "no valid chars" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                           []
                           (valid_chars d_surr d_tiles |> List.sort sort_chars));
  "Empty board" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                        Ai.alphabet (valid_chars empty_surr Ai.alphabet));
  "Edge of board 1" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                            [] (valid_chars partial_empty_surr Ai.alphabet));
  "Edge of board 2" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                            ['z'; 'a'; 'e'] (valid_chars
                                     (get_surroundings all_board (13, 0))
                                     ['z'; 'a'; 'e']));
]

let z_surr = get_surroundings all_board (13, 0)

let makes_move_test = [
  "apples" >:: (fun _ -> assert_equal true (makes_move Right a_surr 's'));
  "applet" >:: (fun _ -> assert_equal true (makes_move Right a_surr 't'));
  "applea" >:: (fun _ -> assert_equal false (makes_move Right a_surr 'a'));
  "Edge case 1" >:: (fun _ -> assert_equal true (makes_move Left z_surr 'z'));
  "Edge case 2" >:: (fun _ -> assert_equal true (makes_move Up z_surr 'z'));
  "Edge case 3" >:: (fun _ -> assert_equal true (makes_move Right z_surr 'z'));
]


let makes_prefix_test = [
  "Edge z 1" >:: (fun _ -> assert_equal false (makes_prefix Left z_surr 'z'));
  "Edge z 2" >:: (fun _ -> assert_equal true (makes_prefix Up z_surr 'z'));
  "Main 1 apple+s" >:: (fun _ -> assert_equal true
                           (makes_prefix Right
                              (get_surroundings apple_board (7, 11)) 's'));
  "Main 2 e+apple" >:: (fun _ -> assert_equal
                           true
                           (makes_prefix Left
                              (get_surroundings apple_board (7, 5)) 'e'));
  "No surrs" >:: (fun _ -> assert_equal true (makes_prefix Up empty_surr 'a'));
]


let reverse_str_test = [
  "Empty string" >:: (fun _ -> assert_equal "" (Ai.reverse_str ""));
  "One char string" >:: (fun _ -> assert_equal "a" (Ai.reverse_str "a"));
  "Multi-char 1" >:: (fun _ -> assert_equal "apples" (Ai.reverse_str "selppa"));
  "Multi-char 2" >:: (fun _ -> assert_equal "happy" (Ai.reverse_str "yppah"));
  "Multi-char 3" >:: (fun _ -> assert_equal "ha ha" (Ai.reverse_str "ah ah"));
]

let a_state =
  {
    Game.name = "asdf";
    grid = empty_board;
    players = [];
    remaining_tiles = [];
    turn = 1;
    score_history = [];
  }

let out_of_bounds_test = [
  "inside" >:: (fun _ -> assert_equal false
                   (out_of_bounds a_state (1,1)));
  "outside 1" >:: (fun _ -> assert_equal true
                      (out_of_bounds a_state (-1,10)));
  "outside 2" >:: (fun _ -> assert_equal true
                      (out_of_bounds a_state (100,1)));
  "both outside 1" >:: (fun _ -> assert_equal true
                           (out_of_bounds a_state (10000, 10000)));
  "both outside 2" >:: (fun _ -> assert_equal true
                           (out_of_bounds a_state (-1, -1)));
]

let sort_anchors = (fun a b -> (fst (fst a)) - (fst (fst b)))
let po_board_anchors = List.sort sort_anchors
    [((0, 2),['d']); ((1,0), []); ((1, 1), ['d'])]
let string_of_anchor a =
  let ((r, c), al) = a in
  string_of_pair (r,c) ^ string_of_charlist al

let string_of_anchor_list al =
  List.fold_left
    (fun acc s -> (string_of_anchor s) ^ "\n" ^ acc)
    ""
    al

let find_anchors_tests = [
  "Empty board" >:: (fun _ -> assert_equal []
                        (find_anchors Grid.empty Ai.alphabet
                           (find_slots Grid.empty)));
  "Full board" >:: (fun _ -> assert_equal []
                       (find_anchors full_board Ai.alphabet
                          (find_slots full_board)));
  "Normal board" >:: (fun _ -> assert_equal ~printer:string_of_anchor_list
                         po_board_anchors
                         (List.sort sort_anchors
                            (find_anchors po_board ['d']
                               (find_slots po_board))));
]

let invalid_pos_tests = [
  "Has char 1" >:: (fun _ -> assert_equal true
                       (invalid_pos {a_state with Game.grid = po_board}
                          (0, 0)));
  "Has char 2" >:: (fun _ -> assert_equal true
                       (invalid_pos {a_state with Game.grid = full_board}
                          (10, 10)));
  "Out of bounds 1" >:: (fun _ -> assert_equal true
                            (invalid_pos a_state (100, 100)));
  "Out of bounds 2" >:: (fun _ -> assert_equal true
                            (invalid_pos a_state (-1, 5)));
  "Good one" >:: (fun _ -> assert_equal
                     false
                     (invalid_pos
                        {a_state with Game.grid = Grid.empty} (3, 3)))
]

let get_next_tests = [
  "Up" >:: (fun _ -> assert_equal ~printer:string_of_pair
               (2,6) (get_next Up (3,6)));
  "Down" >:: (fun _ -> assert_equal ~printer:string_of_pair
                 (12,101) (get_next Down (11, 101)));
  "Left" >:: (fun _ -> assert_equal ~printer:string_of_pair
                 (10, 12) (get_next Left (10, 13)));
  "Right" >:: (fun _ -> assert_equal ~printer:string_of_pair
                  (15, 100) (get_next Right (15, 99)));
]

let next_state = {a_state with Game.grid = all_board}

let search_next_tests = [
  "No new pos 1" >:: (fun _ -> assert_equal None
                         (search_next next_state Down (13, 0)));
  "No new pos 2" >:: (fun _ -> assert_equal None
                         (search_next next_state Right (0, 14)));
  "No new pos 3" >:: (fun _ -> assert_equal None
                         (search_next
                            {a_state with Game.grid = po_board} Left (0, 2)));
  "Some pos 1" >:: (fun _ -> assert_equal (Some (13,3))
                       (search_next next_state Right (13, 0)));
  "Some pos 2" >:: (fun _ -> assert_equal (Some (7,11))
                       (search_next next_state Right (7,3)));
  "Full board" >:: (fun _ -> assert_equal None
                       (search_next
                          {a_state with Game.grid = full_board} Left (7, 7)));
]

let three = ['a'; 'b'; 'c']
let rando = ['f'; 'g'; 'h'; 'i'; 'q']
let mult = ['a'; 'a'; 'a'; 'b']

let rem_tests = [
  "Empty list" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                       [] (rem [] 'a'));
  "One element, remove" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                                [] (rem ['b'] 'b'));
  "One element, stay" >:: (fun _ -> assert_equal  ~printer:string_of_charlist
                              ['c'] (rem ['c'] 'a'));
  "Not in list" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                        three (rem three 'z'));
  "Regular, in list" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                             ['f'; 'g'; 'h'; 'i'] (rem rando 'q'));
  "Mulitple occurrences" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                                 ['a'; 'a'; 'b'] (rem mult 'a'));
]

let sort_chars a b = Char.code a - Char.code b
let cmp_chars a b = (List.sort sort_chars a) = (List.sort sort_chars b)

let no_dups_append_tests = [
  "Both empty" >:: (fun _ -> assert_equal
                       ~printer:string_of_charlist [] (no_dups_append [] []));
  "First empty only" >:: (fun _ -> assert_equal ~printer:string_of_charlist
                             ['b'; 'a']
                             (no_dups_append [] ['a'; 'b']));
  "Second empty only" >:: (fun _ -> assert_equal
                              ~printer:string_of_charlist
                              ['a'] (no_dups_append ['a'] []));
  "First empty, second dups" >:: (fun _ -> assert_equal
                                     ~printer:string_of_charlist
                                     (List.sort sort_chars ['a'; 'b'; 'c'])
                                     (List.sort sort_chars
                                        (no_dups_append
                                           []
                                           ['a'; 'b'; 'a'; 'c'])));
  "Regular dups 1" >:: (fun _ -> assert_equal
                           ~printer:string_of_charlist
                           ~cmp:cmp_chars
                           ['a'; 'x'; 'g'; 'm'; 'n']
                           (no_dups_append
                              ['a'; 'm'; 'n']
                              ['g';'g';'x';'g';'x']));
  "Regular dups 2" >:: (fun _ -> assert_equal
                           ~printer:string_of_charlist
                           ~cmp:cmp_chars
                           ['a'; 'b'; 'c'; 'd']
                           (no_dups_append ['a'; 'b'] ['c'; 'd'; 'c']));
  "Normal append" >:: (fun _ -> assert_equal
                          ~printer:string_of_charlist
                          ~cmp:cmp_chars
                          ['a'; 'b'; 'c'; 'd']
                          (no_dups_append ['a'; 'd'] ['c'; 'b']));
]

let po_surr = get_surroundings po_board (0, 2)

let other_dirs_move_tests = [
  "Empty board" >:: (fun _ -> assert_equal ~printer:string_of_bool
                        true
                        (other_dirs_move Left
                           (get_surroundings Grid.empty (7, 7)) 'a'));
  "Full board" >:: (fun _ -> assert_equal
                       false
                       (other_dirs_move Right
                          (get_surroundings full_board (5, 7)) 'a'));
  "Corner 1" >:: (fun _ -> assert_equal
                     true
                     (other_dirs_move Up z_surr 'z'));
  "Corner 1b" >:: (fun _ -> assert_equal
                      true
                      (other_dirs_move Left z_surr 'z'));
  "Corner 2" >:: (fun _ -> assert_equal
                     true
                     (other_dirs_move Right po_surr 'd'));
  "Corner 2b" >:: (fun _ -> assert_equal
                      false
                      (other_dirs_move Right
                         (get_surroundings all_board (8,6)) 'k'));
  "Middle 1" >:: (fun _ -> assert_equal
                     true
                     (other_dirs_move Right
                        (get_surroundings anti_board (7, 11)) 'f'));
  "Middle 2" >:: (fun _ -> assert_equal false
                     (other_dirs_move Right
                        (get_surroundings anti_board (6, 7)) 'q'));
]


let valid_move_tests = [
  "Empty board" >:: (fun _ -> assert_equal false
                        (valid_move [] Left empty_surr (7, 7) 'a'));
  "Wrong word Left" >:: (fun _ -> assert_equal
                            false
                            (valid_move [] Left
                               (get_surroundings app_board (7,5)) (7,5) 't'));
  "Wrong word Right" >:: (fun _ -> assert_equal
                             false
                             (valid_move [] Right a_surr (7,11) 'g'));
  "Correct word corner" >:: (fun _ -> assert_equal
                                true
                                (valid_move [] Left
                                   (get_surroundings all_board (13, 0))
                                   (13, 0) 'z'));
  "Correct word top" >:: (fun _ -> assert_equal
                             true
                             (valid_move [] Right po_surr (0, 2) 'd'));
  "Middle correct word" >:: (fun _ -> assert_equal
                                true
                                (valid_move [] Right a_surr (7, 11) 't'));
  "Correct word, wrong dir" >:: (fun _ -> assert_equal
                                false
                                (valid_move [] Down a_surr (7, 11) 't'));
  "Nested word" >:: (fun _ -> assert_equal true
                        (valid_move
                           (find_slots nest_board
                            |> find_anchors nest_board alphabet)
                           Left
                           (get_surroundings nest_board (0,1))
                           (0,1) 'o'));
]

let valid_prefix_tests = [
  "Empty board" >:: (fun _ -> assert_equal true
                        (valid_prefix Down empty_surr 'a'));
  "Valid forwards" >:: (fun _ -> assert_equal
                           true
                           (valid_prefix Down
                              (get_surroundings po_board (1,1)) 'f'));
  "Valid backwards" >:: (fun _ -> assert_equal true
                            (valid_prefix Left
                               (get_surroundings apple_board (7,5)) 'e'));
  "Invalid forwards" >:: (fun _ -> assert_equal false
                             (valid_prefix Right
                                (get_surroundings apple_board (7,11)) 'g'));
  "Invalid backwards" >:: (fun _ -> assert_equal false
                              (valid_prefix Left
                                 (get_surroundings apple_board (7,5)) 'z'));
]

let lowercase_tests = [
  "Tile lowercase" >:: (fun _ -> assert_equal ['a'; 'b'; 'c'; 'd'; '?']
                           (lowercase_tiles ['A'; 'B'; 'C'; 'D'; '?']));
  "Empty tiles" >:: (fun _ -> assert_equal [] (lowercase_tiles []));
  "Empty grid lowercase" >:: (fun _ -> assert_equal empty_board
                                 (lowercase_grid empty_board));
  "Regular grid lowercase" >:: (fun _ -> assert_equal
                                   app_board (lowercase_grid upper_app_board));
  "Widlcard tiles" >:: (fun _ -> assert_equal ~cmp:cmp_chars
                           ['a'; 'b'; '?']
                           (lowercase_tiles (['A'; 'B'; '?'])))
]


let apple_state = {
  Game.name = "";
  grid = apple_board;
  players = [];
  remaining_tiles = [];
  turn = 0;
  score_history = [];
}

let apple_player1 = {
  Game.player_name = "a";
  tiles = ['b'; 'a'; 'c'; 'k'];
  score = 0;
  order = 0;
  ai = true;
}

let apple_player2 = {apple_player1 with Game.tiles = ['b'; 'a'; 'k'; 'e']}
let apple_player3 = {apple_player1 with Game.tiles = ['j'; 'a'; 'c'; 'k'; 's']}
let apple_player4 = {apple_player1 with
                     Game.tiles = ['j'; 'a'; 'c'; 'k'; 's'; '?'; '?']}


let str_of_charlist c =
  List.fold_left (fun acc b -> (to_str (snd b)) ^ acc) "" c

let str_of_charlist_list cl =
  List.fold_left (fun acc b -> (str_of_charlist b) ^ "\n" ^ acc) "" cl

let str_of_build l =
  List.fold_left
    (fun acc b ->
       (str_of_charlist b) ^ acc)
    ""
    l

let build_tests = [
  "No moves" >:: (fun _ -> assert_equal ~printer:str_of_build
                       []
                       (build apple_state apple_player1
                            (find_slots
                               apple_state.Game.grid |>
                             find_anchors
                               apple_state.Game.grid
                               apple_player1.Game.tiles)
                            (7,5) Left));
  "No moves" >:: (fun _ -> assert_equal ~printer:str_of_build
                       [[((7,2), 'b');
                         ((7,3), 'a');
                         ((7,4), 'k');
                         ((7,5), 'e');]]
                       (build apple_state apple_player2
                            (find_slots
                               apple_state.Game.grid |>
                             find_anchors
                               apple_state.Game.grid
                               apple_player2.Game.tiles)
                            (7,5) Left));
  "Forward build" >:: (fun _ -> assert_equal ~printer:str_of_charlist_list
                          [
                           [
                             ((7,14), 'k');
                             ((7,13), 'c');
                             ((7,12), 'a');
                             ((7,11), 'j');
                           ];
                           [((7,11), 's')];
                          ]
                          ((build apple_state apple_player3
                             (find_slots
                                apple_state.Game.grid |>
                              find_anchors
                                apple_state.Game.grid
                                apple_player3.Game.tiles)
                             (7,11) Right)));
  "Ignore wildcard build" >:: (fun _ -> assert_equal
                                  ~printer:str_of_charlist_list
                                  ((build apple_state apple_player4
                                      (find_slots
                                         apple_state.Game.grid |>
                                       find_anchors
                                         apple_state.Game.grid
                                         apple_player4.Game.tiles)
                                      (7,11) Right))
                                  ((build apple_state apple_player3
                                      (find_slots
                                         apple_state.Game.grid |>
                                       find_anchors
                                         apple_state.Game.grid
                                         apple_player3.Game.tiles)
                                      (7,11) Right))
                              )
]

let f_state = {apple_state with Game.grid = full_board}
let f_player = {apple_player1 with Game.tiles = alphabet}
let empty_move = {
  Game.tiles_placed = [];
  player = f_player.Game.player_name;
  swap = [];
}

let single_move = {
  Game.tiles_placed = (List.rev [((7,2), 'b');
                       ((7,3), 'a');
                       ((7,4), 'k');
                                 ((7,5), 'e');]);
  player = apple_player2.Game.player_name;
  swap = [];
}

let best_move_tests = [
  "No moves" >:: (fun _ -> assert_raises GameOver
                     (fun () -> (best_move f_state f_player)));
  "With wildcard" >:: (fun _ -> assert_equal
                          (best_move apple_state apple_player3)
                          (best_move apple_state apple_player4));
]


let suite = "A.I. test suite"
            >:::
            find_slots_test @ get_surroundings_test @ valid_chars_test
            @ makes_move_test @ makes_prefix_test @ out_of_bounds_test
            @ reverse_str_test @ find_anchors_tests @ invalid_pos_tests
            @ get_next_tests @ search_next_tests @ rem_tests
            @ no_dups_append_tests @ other_dirs_move_tests @ valid_move_tests
            @ valid_prefix_tests @ lowercase_tests @ build_tests
            @ best_move_tests

let _ = run_test_tt_main suite
