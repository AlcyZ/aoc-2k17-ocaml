open Printf

let file = "./data/day_five.txt"

(* utility functions *)

let rec to_int_list = function
  | [] -> []
  | h :: t -> int_of_string h :: to_int_list t

let to_int_arr l = to_int_list l |> Array.of_list

(* solving part one *)

let rec process_one arr pos steps =
  try
    let current_offset = arr.(pos) in
    arr.(pos) <- current_offset + 1;
    process_one arr (pos + current_offset) (steps + 1)
  with Invalid_argument _ -> steps

(* solving part two *)

let rec process_two arr pos steps =
  try
    let current_offset = arr.(pos) in
    let new_offset =
      if current_offset < 3 then current_offset + 1 else current_offset - 1
    in
    arr.(pos) <- new_offset;
    process_two arr (pos + current_offset) (steps + 1)
  with Invalid_argument _ -> steps

(* Part one and Part two *)

let lines = Util.read_lines file

let part_one =
  match lines with
  | Some l ->
      let arr = to_int_arr l in
      let steps = process_one arr 0 0 in
      printf "Part two steps: %d\n" steps
  | None -> printf "Part one not solved yet!\n"

let part_two =
  match lines with
  | Some l ->
      let arr = to_int_arr l in
      let steps = process_two arr 0 0 in
      printf "Part two steps: %d\n" steps
  | None -> printf "Part two not solved yet!\n"

let solve =
  part_one;
  part_two
