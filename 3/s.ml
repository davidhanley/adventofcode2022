open Set

module CharSet = Set.Make(Char)

let add s e = CharSet.add s e

let sum_list = List.fold_left (fun a b -> a + b) 0

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

let char_scores ="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let char_score c =
  (String.rindex char_scores c) + 1


let string_split str =
  let l = String.length str in
  let h = l / 2 in
  [String.sub str 0 h; String.sub str h (l - h)]

let charsetset_of_string str =
  String.fold_left (fun set elem -> CharSet.add elem set) CharSet.empty str

let score_sacks sacks =
  let sets = List.map charsetset_of_string sacks in
  let shared = List.fold_left CharSet.inter (List.hd sets) sets in
  let c = (List.hd (CharSet.elements shared)) in
  let s = char_score c in
  s

let sacks = file_to_strings "elves.dat"

let partition list size =
  let rec p l acc res = match l with
    | [] -> res
    | e::rest ->
      let ne = e::acc in
        if (List.length ne) = size then
          p rest [] (ne::res)
        else
          p rest ne res
  in
    p list [] []

let first_part () =
  let boxes = List.map string_split sacks in
  let v = sum_list (List.map score_sacks boxes ) in
  Printf.printf "first: %d\n" v

let second_part () =
  let boxes = partition sacks 3 in
  let v = sum_list (List.map score_sacks boxes ) in
    Printf.printf "first: %d\n" v

let () =
  first_part ();
  second_part() 