open OUnit2

let find_slots_test = [
  "dummy" >:: (fun _ -> assert_equal 1 1)
]

let suite = "A.I. test suite"
            >:::
            find_slots_test

let _ = run_test_tt_main suite
