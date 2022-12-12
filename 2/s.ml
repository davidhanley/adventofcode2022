
(* solving: https://adventofcode.com/2022/day/2 *)

type shapes = Rock | Paper | Scissors

let char_to_shape m = match m with
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

let first_match_result (his,mine) = my_value mine + (match (his,mine) with
  | (Rock,Paper) -> win
  | (Rock,Scissors) -> loss
  | (Paper,Scissors) -> win
  | (Paper,Rock) -> loss
  | (Scissors,Rock) -> win
  | (Scissors,Paper) -> loss
  | _ -> tie)

let line_to_round_1 (h,m) =
  (char_to_shape h, char_to_shape m)

let file_to_strings fn =
  let inf = open_in fn in
   let rec reader acc =
      try
        let line = input_line inf in
        reader (line::acc)
      with _ ->
        close_in_noerr inf;
        acc
    in
      reader []

let sum_list = List.fold_left (fun a b -> a + b) 0

let elflines = file_to_strings "elves.dat"

let line_to_pair line =
  let g = String.get line in
  (g 0, g 2)

let pairs = List.map line_to_pair elflines

let round_1 () =
  let shape_shape_pairs = List.map line_to_round_1 pairs in
  Printf.printf "Round 1: %d\n" (sum_list (List.map first_match_result shape_shape_pairs))



let () =
  round_1 ()




