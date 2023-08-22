open Printf

let file = "./data/day_two.txt"

(* functions solving part one *)

let diff l =
  match l with
  | [] -> 0
  | h :: t ->
      let rec find l g = function
        | [] -> g - l
        | x :: y ->
            let new_min = min x l in
            let new_max = max x g in
            find new_min new_max y
      in
      find h h t

let diffs m =
  let rec proc ma = match ma with [] -> [] | h :: t -> diff h :: proc t in
  proc m

(* functions solving part two *)

let divisables l =
  let rec find_divisables acc = function
    | [] -> acc
    | h :: t ->
        let filtered = List.filter (fun x -> h mod x = 0) l in
        let filtered = List.filter (fun x -> h <> x) filtered in
        let pairs = List.map (fun x -> (h, x)) filtered in
        find_divisables (acc @ pairs) t
  in
  find_divisables [] l

let filter_empty l = List.nth_opt l 0

let filter_none l =
  List.filter_map (function Some x -> Some x | None -> None) l

let find_divisable_pairs m =
  let rec find_pairs l =
    match l with
    | [] -> []
    | h :: t ->
        let pairs = divisables h |> filter_empty in
        pairs :: find_pairs t
  in
  find_pairs m |> filter_none

let rec calc_quotients = function
  | [] -> []
  | (x, y) :: t ->
      let res = x / y in
      res :: calc_quotients t

(* functions part_one, part_two and sum, used by both part functions *)

let rec sum = function [] -> 0 | h :: t -> h + sum t

let part_one =
  match Util.read_matrix file with
  | Some m ->
      let diff_list = diffs m in
      sum diff_list
  | None -> 0

let part_two =
  match Util.read_matrix file with
  | Some m -> find_divisable_pairs m |> calc_quotients |> sum
  | None -> 0

let solve =
  (*printf "Part one: %d\n" part_one;*)
  printf "Part two: %d\n" part_two
