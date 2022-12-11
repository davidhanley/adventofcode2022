

let fold_file fn foldf acc_base =
  let inf = open_in fn in
  let rec reader acc =
    try
      let line = input_line inf in
      let a2 = foldf acc line in
      reader a2
    with e ->
      close_in_noerr inf;
      acc
  in
    reader acc_base



let () = 
  let i = open_in "elves.dat"


