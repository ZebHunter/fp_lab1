let () =  
  Printf.printf ("Problem 14: %s\nProblem 17: %s\n")
   (string_of_int @@ Lab1.Collatz.tail_max_chain_val 1_000_000 0 1)
   (string_of_int Lab1.Letters.letters_count)
