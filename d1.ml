let readin fname = 
  let ic = open_in fname in
  let try_read () = 
    try Some (input_line ic)  with e-> None in
  let rec loop acc = match try_read () with
      | Some s -> loop (int_of_string s :: acc)
      | None -> close_in ic; List.rev acc in
  loop []

(* first part calculate the sum *)
let l = readin "d1input" in List.fold_left (fun h t->h+t) 0 l;;
