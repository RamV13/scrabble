open OUnit2
open Dictionary

let tests = "test suite" >::: [

  (*CORE TRIE FUNCTIONALITY TESTING*)
  "empty_trie"  >::
    (fun _ -> assert_equal
      (true)
      (is_empty empty));

    (*mem tests*)

  "simple_add"  >::
    (fun _ -> assert_equal
      (true)
      (mem "hello" (add "hello" empty)));

  "add_two_words_with_prefix" >::
    (fun _ -> assert_equal
      (true)
      (let dict = (add "help" (add "hello" empty)) in
        (mem "help" dict&& mem "hello" dict)));

  "add_two_words_different" >::
    (fun _ -> assert_equal
      (true)
      (let dict = (add "first" (add "second" empty)) in
        (mem "first" dict&& mem "second" dict)));

  "simple_not_mem" >::
    (fun _ -> assert_equal
      (false)
      (mem "word" empty));

  "complex_not_mem" >::
    (fun _ -> assert_equal
      (false)
      (let dict = (add "help" (add "hello" empty)) in
        (mem "helpo" dict)));



    (*validity tests*)

  "middle_valid_word" >::
    (fun _ -> assert_equal
      (true)
      (let dict = (add "help" (add "hello" empty)) in
        (is_valid_word "help" dict)));


  "two_different_valid_words" >::
    (fun _ -> assert_equal
      (true)
      (let dict = (add "first" (add "second" empty)) in
        (is_valid_word "first" dict&& is_valid_word "second" dict)));

  "not_valid" >::
    (fun _ -> assert_equal
      (false)
      (is_valid_word "fron" (add "front" empty)));

  "not_valid_long" >::
    (fun _ -> assert_equal
      (false)
      (is_valid_word "sudowoodorino" (add "sudowoodo" empty)));

    (*leaf testing*)

    "is_leaf" >::
    (fun _ -> assert_equal
      (true)
      (is_leaf "front" (add "front" empty)));

    "not_leaf" >::
    (fun _ -> assert_equal
      (false)
      (is_leaf "cat" (add "catacomb" empty)));





  (*SIMPLE TESTS OF FULL DICTIONARY*)
  "simple_lookup"  >::
    (fun _ -> assert_equal
      (true)
      (in_dict "hello"));

    "complex_lookup" >::
    (fun _ -> assert_equal
      (true)
      (in_dict "catastrophe"));

    "made_up_word">::
    (fun _ -> assert_equal
      (false)
      (in_dict "flarbl"));

    "empty_string_not_word">::
    (fun _ -> assert_equal
      (false)
      (in_dict ""));

    "simple_backwards_lookup">::
    (fun _ -> assert_equal
      (true)
      (in_back_dict "hctac"));

    "simple_not_backwards">::
    (fun _ -> assert_equal
      (false)
      (in_back_dict "hello"));


    "simple_extensions" >::
    (fun _ -> assert_equal
      (true)
      (has_extensions "ca"));

    "no_extensions" >::
    (fun _ -> assert_equal
      (false)
      (has_extensions "zt"));

    "back_extensions" >::
    (fun _ -> assert_equal
      (true)
      (has_back_extensions "stone"));

    "no_back_extensions" >::
    (fun _ -> assert_equal
      (true)
      (has_extensions "bobcat"));

    (*Too long to include in this file but the contents of both
      the forward an back dictionary were tested 1 to 1 with the contents
      of the file using a modified dictionary.ml file*)


    ]



let _ = run_test_tt_main tests
