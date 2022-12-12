
(* solving: https://adventofcode.com/2022/day/2 *)

type shapes = Rock | Paper | Scissors

let shape_to_string = function
  | Rock -> "rock"
  | Paper -> "paper"
  | Scissors -> "scissors"

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

type results = Win | Loss | Draw

let result_value = function
 | Win -> 6
 | Draw -> 3
 | Loss -> 0

let first_match_result (his,mine) = my_value mine + (result_value (match (his,mine) with
  | (Rock,Paper) -> Win
  | (Rock,Scissors) -> Loss
  | (Paper,Scissors) -> Win
  | (Paper,Rock) -> Loss
  | (Scissors,Rock) -> Win
  | (Scissors,Paper) -> Loss
  | _ -> Draw))

let line_to_round_1 (h, m) =
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

let char_to_result c = match c with
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win

let pair_to_shape_result (h,m) =
  (char_to_shape h, char_to_result m)

let pairs_shapes = List.map pair_to_shape_result pairs

let score_round_2 (his_shape, result) =
  let my_shape = match (his_shape, result) with
   | (Rock, Loss) -> Scissors
   | (Rock, Win)  -> Paper
   | (Paper, Win) -> Scissors
   | (Paper, Loss) -> Rock
   | (Scissors, Win) -> Rock
   | (Scissors, Loss) -> Paper
   | _ -> his_shape in
  let shapeScore =  (my_value my_shape) in
   (result_value result) + shapeScore

let round_2 () =
  assert ((score_round_2 (Rock, Draw)) = 4);
      Printf.printf "Round 1: %d\n" (sum_list (List.map score_round_2 pairs_shapes))

let () =
  round_1 ();
  round_2 ()




