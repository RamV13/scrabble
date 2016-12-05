open OUnit2
open Grid

let tests = "test suite" >::: [

(*is_empty testing*)

"is_empty1" >::
    (fun _ -> assert_equal
      (true)
      (is_empty empty 0 0));

"is_empty2" >::
    (fun _ -> assert_equal
      (true)
      (is_empty (place empty 0 1 'a') 0 0));

"is_empty3" >::
    (fun _ -> assert_equal
      (false)
      (is_empty (place empty 0 1 'a') 0 1));

(*get_tile_testing*)

"get_tile_at1" >::
    (fun _ -> assert_equal
      (None)
      (get_tile (place empty 0 1 'a') 14 14));

"get_tile_at2" >::
    (fun _ -> assert_equal
      (Some 'a')
      (get_tile (place empty 0 1 'a') 0 1));

"get_tile_at3" >::
    (fun _ -> assert_equal
      (Some 'b')
      (get_tile (place (place empty 1 2 'b') 0 1 'a') 1 2));

(*get diff testing*)

"get_diff" >::
    (fun _ -> assert_equal
      ([])
      (get_diff empty empty));

"get_diff2" >::
    (fun _ -> assert_equal
      (['a'])
      (get_diff empty (place empty 0 1 'a')));

"get_diff3" >::
    (fun _ -> assert_equal
      (['a';'b'])
      (get_diff empty (place (place empty 0 1 'a') 1 1 'b')));

"get_diff4" >::
    (fun _ -> assert_equal
      (['b'])
      (get_diff (place empty 0 1 'a') (place (place empty 0 1 'a') 1 1 'b')));


    (*get neighbors testing*)

  "get_neighbors" >::
    (fun _ -> assert_equal
      ({top=None;left=None;right=None;bottom=None})
      (get_neighbors empty 7 7));

  "get_neighbors2" >::
    (fun _ -> assert_equal
      ({top=None;left=None;right=None;bottom=None})
      (get_neighbors (place empty 7 7 'a') 7 7));

    "get_neighbors3" >::
    (fun _ -> assert_equal
      ({top=None;left=Some 'a';right=None;bottom=None})
      (get_neighbors (place empty 7 6 'a') 7 7));

    "get_neighbors4" >::
    (fun _ -> assert_equal
      ({top=None;left=None;right=Some 'a';bottom=None})
      (get_neighbors (place empty 7 8 'a') 7 7));

    "get_neighbors5" >::
    (fun _ -> assert_equal
      ({top=Some 'a';left=None;right=None;bottom=None})
      (get_neighbors (place empty 6 7 'a') 7 7));

    "get_neighbors6" >::
    (fun _ -> assert_equal
      ({top=None;left=None;right=None;bottom=Some 'a'})
      (get_neighbors (place empty 8 7 'a') 7 7));

    (*bonus word and letter testing*)

    "get_bonus1" >::
    (fun _ -> assert_equal
      (2)
      (bonus_word_at (7,7)));

    "get_bonus2" >::
    (fun _ -> assert_equal
      (1)
      (bonus_word_at (7,6)));

    "get_bonus3" >::
    (fun _ -> assert_equal
      (3)
      (bonus_word_at (14,14)));

    "get_bonus4" >::
    (fun _ -> assert_equal
      (2)
      (bonus_letter_at (2,8)));

    "get_bonus5" >::
    (fun _ -> assert_equal
      (1)
      (bonus_letter_at (0,0)));




    ]



let _ = run_test_tt_main tests
