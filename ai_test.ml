open OUnit2
open Ai

(* Functions that need to be thoroughly tested:
 * -------------------------------------------
 * find_slots (DONE)
 * get_surroundings (DONE)
 * valid_chars
 * get_anchors *)

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
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None];
                 [None;None;None;None;None;None;None;None;None;None;None;None;None;None;None]]

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

let get_surroundings_test = [
  "No surroundings" >:: (fun _ -> assert_equal blank_surr
                            (get_surroundings empty_board (7,7)));
  "Left right" >:: (fun _ -> assert_equal ~printer:string_of_surr apple_surr
                       (get_surroundings apple_board (7,8)));
  "All sides" >:: (fun _ -> assert_equal ~printer:string_of_surr all_surr
                      (get_surroundings all_board (7,7)))
]

let suite = "A.I. test suite"
            >:::
            find_slots_test @ get_surroundings_test

let _ = run_test_tt_main suite
