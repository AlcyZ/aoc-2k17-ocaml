open Printf

let file = "./data/day_six.txt"
let explode_tab s = String.split_on_char '\t' s

let rec to_int_list = function
  | [] -> []
  | h :: t -> int_of_string h :: to_int_list t

exception InvalidList of string

let rec find_max_helper l max_value max_index current_index =
  match l with
  | [] -> (max_value, max_index)
  | h :: t ->
      if h > max_value then find_max_helper t h current_index (current_index + 1)
      else find_max_helper t max_value max_index (current_index + 1)

let find_max l =
  match l with
  | [] -> raise (InvalidList "empty list not allowed")
  | h :: t -> find_max_helper t h 0 1

let found_list1 l lists = List.exists (fun sub_list -> l = sub_list) lists

let rec found_list2 l = function
  | [] -> false
  | h :: t -> if l = h then true else found_list2 l t

let sample = [ 0; 2; 7; 0 ]

let redistribute l =
  let max_value, max_index = find_max l in
  let arr = Array.of_list l in
  arr.(max_index) <- 0;

  for i = 1 to max_value do
    let unsafe_index = i + max_index in
    let length = Array.length arr in
    let index =
      if unsafe_index < length then unsafe_index else unsafe_index mod length
    in
    arr.(index) <- arr.(index) + 1
  done;
  Array.to_list arr

let process l =
  let rec helper l seen steps =
    let redistributed = redistribute l in
    if found_list1 redistributed seen then steps + 1
    else helper redistributed (redistributed :: seen) (steps + 1)
  in
  helper l [] 0

let process_b l =
  let rec helper l seen =
    let redistributed = redistribute l in
    if found_list1 redistributed seen then (redistributed, redistributed :: seen)
    else helper redistributed (redistributed :: seen)
  in
  helper l []

let rec find_second_occurrence lst target step =
  match lst with
  | [] -> None
  | h :: t ->
      if h = target then
        if step = 0 then find_second_occurrence t target (step + 1)
        else Some step
      else find_second_occurrence t target (step + 1)

let data = Util.read_firstline file

let part_one =
  match data with
  | Some l ->
      let res = l |> explode_tab |> to_int_list |> process in
      printf "Part one: %d\n" res
  | None -> ()

let find_loop_size =
  match data with
  | Some l ->
      let lst, seen = l |> explode_tab |> to_int_list |> process_b in
      find_second_occurrence seen lst 0
  | None -> None

let part_two =
  match find_loop_size with Some r -> printf "Part two: %d\n" r | None -> ()

let solve =
  part_one;
  part_two
