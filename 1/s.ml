

(* a general function to fold a function over a file the way you fold it over a list
   string -> ('a -> string -> 'a) -> 'a -> 'a
   *)
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

let elf_lines (e1::elves) line =
  try
    let calories = int_of_string line in
    e1+calories::elves
  with _ ->
    0::e1::elves

let () =
  let cmp i1 i2 = i2 - i1  in
  let elves = List.sort cmp (fold_file "elves.dat" elf_lines [0] ) in
  let n = List.nth elves in
  Printf.printf "top three calories: %d" ((n 0) + (n 1) + (n 2))


