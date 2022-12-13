
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


let string_to_pair str =
  let [p1;p2] = String.split_on_char '-' str in
  (int_of_string p1),(int_of_string p2)

let parse_assignment a =
   let [p1;p2] = String.split_on_char ',' a in
   ((string_to_pair p1),(string_to_pair p2))

let contained ((l1, r1), (l2, r2)) = (l1 >= l2) && (r1 <= r2)

let contain_score (p1, p2) = if (contained (p1, p2)) || (contained (p2, p1)) then 1 else 0

let lines = file_to_strings "elves.dat"

let assignments = List.map parse_assignment lines

let part_1 () =
  let res = sum_list (List.map contain_score assignments) in
  Printf.printf "contained %d\n" res

let overlaps ((l1,r1), (l2,r2)) =  if (r1<l2) || (l1 > r2)then 0 else 1

let part_2 () =
  let res = sum_list (List.map overlaps assignments) in
  Printf.printf "overlaps %d\n" res

let () =
  part_1();
  part_2()

