let file = "./data/day_one.txt"

let rec print_intlist l =
  match l with
  | [] -> ()
  | h :: t ->
      Printf.printf "%s" (string_of_int h);
      print_intlist t

let rec sum_elements l =
  match l with
  | [] -> 0
  | [ x ] -> if List.hd l = x then x else 0
  | hd :: tl ->
      if hd = List.hd tl then hd + sum_elements tl else sum_elements tl

let part_one =
  match Util.read_intlist file with Some l -> sum_elements l | None -> 0

let sum_matching_elements l =
  let len = List.length l in
  let max = len - 1 in
  let half = len / 2 in

  let rec process lst i acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        let idx = i + half in
        let idx = if idx > max then idx - len else idx in
        let cmp = List.nth l idx in
        if hd = cmp then process tl (i + 1) (acc + hd)
        else process tl (i + 1) acc
  in

  process l 0 0

let part_two =
  match Util.read_intlist file with
  | Some l -> sum_matching_elements l
  | None -> 0

let solve =
  (* Printf.printf "Part one: %d\n" part_one; *)
  Printf.printf "Part two: %d\n" part_two
