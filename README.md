# Лабораторная работа 1. OCaml

- Вариант: 14, 17

- Автор: Рогачев Михаил Сергеевич P34082

- Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

## Условие задания

### Задача 14
The following iterative sequence is defined for the set of positive integers:

<math xmlns="http://www.w3.org/1998/Math/MathML">
  <mi>n</mi>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mi>n</mi>
  <mrow data-mjx-texclass="ORD">
    <mo>/</mo>
  </mrow>
  <mn>2</mn>
</math> (
n is even)

 <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mi>n</mi>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>3</mn>
  <mi>n</mi>
  <mo>+</mo>
  <mn>1</mn>
</math> ( n is odd)

Using the rule above and starting with 13, we generate the following sequence:

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mn>13</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>40</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>20</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>10</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>5</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>16</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>8</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>4</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>2</mn>
  <mo accent="false" stretchy="false">&#x2192;</mo>
  <mn>1.</mn>
</math>

It can be seen that this sequence (starting at 13
 and finishing at 1
) contains 10
 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?
### Задача 17

If the numbers 
1 to 5 
 are written out in words: one, two, three, four, five, then there are 
 <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mn>3</mn>
  <mo>+</mo>
  <mn>3</mn>
  <mo>+</mo>
  <mn>5</mn>
  <mo>+</mo>
  <mn>4</mn>
  <mo>+</mo>
  <mn>4</mn>
  <mo>=</mo>
  <mn>19</mn>
</math>
 letters used in total.

If all the numbers from 
1 to 1000
 (one thousand) inclusive were written out in words, how many letters would be used?

## Реализация

### Реализации 14 задачи

- Простая рекурсия для подсчета длины цепи Коллаца
``` OCaml
let rec collatz n =
  match n with
  | 1 -> 1
  | _ -> 1 + (collatz @@ operation n)
```

- Хвостовая рекурсия для нахождения минимального числа с максимальной длинной цепи Коллаца
``` OCaml
let rec tail_collatz n acc =
  match n with
  | 1 -> acc
  | _ -> tail_collatz (operation n) (acc + 1)
;;

let rec tail_max_chain_val n len acc =
  match n with
  | 1 -> acc
  | _ ->
    let col = tail_collatz n 1 in
    tail_max_chain_val (n - 1) (max col len) (if col > len then n else acc)
;;
```

- Модульная реализация через списки
``` OCaml
let generate_list_collatz n = List.init n (fun x -> x + 1, tail_collatz (x + 1) 1)

let get_min_val l =
  let max_chain = List.fold_left (fun acc (_, y) -> max acc y) 0 l in
  List.filter (fun (_, v) -> v = max_chain) l |> get_first_el |> fst
;;
```

- Решение с помощью бесконечной последовательности
``` OCaml
let seq_solve f =
  Seq.ints 1
  |> Seq.take 1_000_000
  |> Seq.map (fun x -> f x)
  |> Seq.zip (Seq.ints 1 |> Seq.take 1_000_000)
  |> Seq.fold_left (fun (acc, acc1) (x, y) -> if acc1 > y then acc, acc1 else x, y) (0, 0)
  |> fst
;;
```

- Решение с помощью циклов
``` OCaml
let imp_solve n =
  let ci = ref 0 in
  let maxim = ref 0 in
  for i = 1 to n do
    let len = tail_collatz i 1 in
    if !maxim < len
    then (
      maxim := len;
      ci := i)
  done;
  !ci
;;
```

- Решение на языке C
``` C
#define MAX_NUMBER 1000000

int main() {
    uint64_t n;
    uint64_t lens_mass[MAX_NUMBER] = {0};
    int32_t count;
    int32_t max = 0;
    uint64_t ans = 0;
    
    for (int i = 1; i < MAX_NUMBER; i++) {
        n = i;
        count = 0;

        while (n > 1) {
            if (n < MAX_NUMBER && lens_mass[n]) {
                count += lens_mass[n];
                break;
            }
            n = (n % 2 == 0) ? n / 2 : 3 * n + 1;
            count++;
        }

        if (i < MAX_NUMBER) lens_mass[i] = count;

        if (max < count) {
            max = count;
            ans = i;
        }
    }

    printf("Problem 14: %lu\n", ans);
    return 0;
}
```

### Реализации 17 задачи

- Реализация с помощью map
``` OCaml
let teen =
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
  | n when n < 20 -> teen.(n - 1)
  | n when n >= 20 && n < 100 ->
    let first_digit = (n / 10) - 2 in
    ty.(first_digit) ^ decompose (n mod 10)
  | n when n < 1000 && n mod 100 = 0 -> teen.((n / 100) - 1) ^ "hundred"
  | n when n >= 100 && n <= 999 -> String.concat "" [ teen.((n / 100) - 1); "hundredand"; decompose (n mod 100) ]
  | 1000 -> "onethousand"
  | _ -> ""
;;

let letters_count = List.init 1000 (fun x -> x + 1) |> List.map decompose |> String.concat "" |> String.length

```

- Реализация на языке С
``` C
int32_t count_let (int32_t n){
    int32_t mas_to_twenty[] = {0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8};
    int32_t mas_to_handred[] = {0, 0, 6, 6, 5, 5, 5, 7, 6, 6};
    int32_t hundred = 7;
    int32_t thousand = 8;
    int32_t land = 3;
    int32_t count = 0;

    count += (n / 1000) ? mas_to_twenty[n / 1000] + thousand : 0;
    n %= 1000;
    count += (n / 100) ? mas_to_twenty[n / 100] + hundred : 0;
    n %= 100;

    if (n && count) count += land;

    if (n < 20) count += mas_to_twenty[n];
    else {
        count += mas_to_handred[n / 10];
        n %= 10;
        count += mas_to_twenty[n];
    }
    
    return count;
}

int main(){
    int32_t count = 0;
    for (int i = 1; i < 1001; i++){
        count += count_let(i);
    }

    printf("Problem 17: %d\n", count);
}
```

## Вывод

В ходе работы я познакомился с базовым синтаксисом OCaml и реализовал задачи разными способами. Говоря про лабораторную работу, наиболее простым способом реализации оказался способ с хвостовой рекурсией, поскольку все остальные реализации были немного "вымученные" использования написанной функции. Но несмотря на это, данная лабораторная работа дала повод для использования различных подходов для решения задачи, используя выбранный язык