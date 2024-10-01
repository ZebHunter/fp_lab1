open OUnit2

(* Тесты для задачи 14 *)
let test1_probl_14 _ = assert_equal 837799 (Lab1.Collatz.tail_max_chain_val 1_000_000 0 1)
let test2_probl_14 _ = assert_equal 837799 (Lab1.Collatz.get_min_val @@ Lab1.Collatz.generate_list_collatz 1_000_000)
let test3_probl_14 _ = assert_equal 837799 (Lab1.Collatz.seq_solve @@ Lab1.Collatz.collatz)
let test4_probl_14 _ = assert_equal 837799 (Lab1.Collatz.imp_solve 1_000_000)

(* Тест для задачи 17 *)
let test1_probl_17 _ = assert_equal 21124 Lab1.Letters.letters_count

(* Группа тестов *)
let tests =
  "test suit for lab1"
  >::: [ "test1_probl_14" >:: test1_probl_14
       ; "test2_probl_14" >:: test2_probl_14
       ; "test3_probl_14" >:: test3_probl_14
       ; "test4_probl_14" >:: test4_probl_14
       ; "test1_probl_17" >:: test1_probl_17
       ]
;;

(* Запуск тестов *)
let () = run_test_tt_main tests
