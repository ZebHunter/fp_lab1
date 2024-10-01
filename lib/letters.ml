let one =
  [| "one"
   ; "two"
   ; "three"
   ; "four"
   ; "five"
   ; "six"
   ; "seven"
   ; "eight"
   ; "nine"
   ; "ten"
   ; "eleven"
   ; "twelve"
   ; "thirteen"
   ; "fourteen"
   ; "fifteen"
   ; "sixteen"
   ; "seventeen"
   ; "eighteen"
   ; "nineteen"
  |]
;;

let ty = [| "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]

let rec decompose = function
  | 0 -> ""
  | n when n < 20 -> one.(n - 1)
  | n when n >= 20 && n < 100 ->
    let first_digit = (n / 10) - 2 in
    ty.(first_digit) ^ decompose (n mod 10)
  | n when n < 1000 && n mod 100 = 0 -> one.((n / 100) - 1) ^ "hundred"
  | n when n >= 100 && n <= 999 -> String.concat "" [ one.((n / 100) - 1); "hundredand"; decompose (n mod 100) ]
  | 1000 -> "onethousand"
  | _ -> ""
;;

let letters_count = List.init 1000 (fun x -> x + 1) |> List.map decompose |> String.concat "" |> String.length
