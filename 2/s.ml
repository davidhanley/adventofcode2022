
(* solving: https://adventofcode.com/2022/day/2 *)

type shapes = Rock | Paper | Scissors

let mts m = match m with
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z'-> Scissors


let rock_val = 1
let paper_val = 2
let scissors_val = 3

let my_value shape = match shape with
   | Rock -> 1
   | Paper -> 2
   | Scissors -> 3

let win = 6
let tie = 3
let loss = 0

let match_result (his,mine) = my_value mine + (match (his,mine) with
  | (Rock,Paper) -> win
  | (Rock,Scissors) -> loss
  | (Paper,Scissors) -> win
  | (Paper,Rock) -> loss
  | (Scissors,Rock) -> win
  | (Scissors,Paper) -> loss
  | _ -> tie)

let line_to_round line =
  let g = String.get line in
  (mts (g 0), mts(g 2))

let string_to_score line =
  match_result (line_to_round line)

let fold_file fn foldf acc_base =
  let inf = open_in fn in
  let rec reader acc =
    try
      let line = input_line inf in
      let a2 = foldf acc line in
      reader a2
    with _ ->
      close_in_noerr inf;
      acc
  in
    reader acc_base

let summer sts tally line =
  Printf.printf "%s " line;
  let s = (sts line) in
  Printf.printf "%d\n" s;
  s + tally

let round_1 =
  assert ((string_to_score "A Y") = 8);
  assert ((string_to_score "B X") = 1);
  assert ((string_to_score "C Z") = 6);
  Printf.printf "%d" (fold_file "elves.dat" (summer string_to_score) 0)








