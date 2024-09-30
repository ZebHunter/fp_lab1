let operation n = if n mod 2 = 0 then n / 2 else (3 * n) + 1

(*
   Simple recursion length collatz chain
*)
let rec collatz n =
  match n with
  | 1 -> 1
  | _ -> 1 + (collatz @@ operation n)
;;

(*
   Tail recursion collatz
*)

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

(*
   Collatz problem on module function solution
*)

let generate_list_collatz n = List.init n (fun x -> x + 1, tail_collatz (x + 1) 1)

let get_first_el list =
  match list with
  | [] -> failwith "List is empty"
  | h :: _ -> h
;;

let get_min_val l =
  let max_chain = List.fold_left (fun acc (_, y) -> max acc y) 0 l in
  List.filter (fun (_, v) -> v = max_chain) l |> get_first_el |> fst
;;

(*
   Sequance
*)

let seq_solve f =
  Seq.ints 1
  |> Seq.take 1_000_000
  |> Seq.map (fun x -> f x)
  |> Seq.zip (Seq.ints 1 |> Seq.take 1_000_000)
  |> Seq.fold_left (fun (acc, acc1) (x, y) -> if acc1 > y then acc, acc1 else x, y) (0, 0)
  |> fst
;;


(*
  Simple cicles sollution
*)

let imp_solve n =
  let ci = ref 0 in
  let maxim = ref 0 in
  for i = 1 to n do
    let len = tail_collatz i 1 in
    if !maxim < len then begin
      maxim := len;
      ci := i;
    end;
  done;
  !ci