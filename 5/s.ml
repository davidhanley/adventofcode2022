open Stack

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

let python_split x =
  String.split_on_char ' ' x |> List.filter (fun x -> x <> "")

let split_on_empty l =
  let rec soe (f::r) acc =
   if f = "" then (r,acc) else (soe r (f::acc)) in
  soe l []

let char_onto_stacks ch stk =
  match ch with
   | 'A' .. 'Z' -> ch::stk
   | _ -> stk

let map_to_stacks stacks strmap =
  let strlen = String.length strmap in
  let rec looper idx stks =
    if idx > strlen then [] else
      (char_onto_stacks (String.get strmap idx) (List.hd stks))::looper (idx + 4) (List.tl stks) in
  looper 1 stacks

let ed = file_to_strings "elves.dat"

let get_stacks h =
  let col_count = ((String.length (List.hd h))-2) / 3 in
  let empty_stacks = List.init col_count (fun _-> [])  in
    List.fold_left map_to_stacks empty_stacks h

let get_command str =
  let ss = python_split str in
  let get x = (int_of_string (List.nth ss ((x*2)+1)))-1 in
  ((get 0)+1),(get 1),(get 2)

let ff (f:int) (t:int) from y l  = if (y=f) then (List.tl l) else if (y=t) then from::l else l

let rec execute_command stacks (c, f, t) =
  if c <= 0 then stacks else
    let from = List.hd (List.nth stacks f) in
    let res = List.mapi (ff f t from) stacks in
    execute_command res ((c-1), f, t)

let rec head_cnt l cnt =
  if cnt = 0 then [] else (List.hd l)::(head_cnt (List.tl l) (cnt-1))

let rec tail_cnt l cnt =
  if cnt = 0 then l else tail_cnt (List.tl l) (cnt-1)

(* let ht lst so = (head_cnt lst so),(tail_cnt lst so) *)

let ff2 count (f:int) (t:int) head index list = if index=f then (tail_cnt list count) else if index=t then (head @ list) else list

let execute_command2 stacks (c, f, t) =
  Printf.printf "%d %d %d\n" c f t;
  let head = head_cnt (List.nth stacks f) c in
  List.mapi (ff2 c f t head) stacks

let (h, t) = split_on_empty ed

let stacks = get_stacks h

let commands = (List.map get_command t)

let first () =
  List.fold_left execute_command stacks commands

let second () =
  List.map List.hd (List.fold_left execute_command2 stacks commands)


