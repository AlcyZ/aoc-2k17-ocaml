open Printf

let file = "./data/day_four.txt"
let _test1 = [ "aa"; "bb"; "cc"; "dd"; "ee" ]
let _test2 = [ "aa"; "bb"; "cc"; "dd"; "aa" ]
let _test3 = [ "aa"; "bb"; "cc"; "dd"; "aaa" ]
let _test4 = [ "abcde"; "fghij" ]
let _test5 = [ "abcde"; "xyz"; "ecdab" ]
let _test6 = [ "a"; "ab"; "abc"; "abd"; "abf"; "abj" ]
let _test7 = [ "iiii"; "oiii"; "ooii"; "oooi"; "oooo" ]
let _test8 = [ "oiii"; "ioii"; "iioi"; "iiio" ]

(* functions solving part_one *)

let has_duplicates l =
  let rec check seen = function
    | [] -> false
    | h :: t -> if List.mem h seen then true else check (h :: seen) t
  in
  check [] l

let rec duplicates_bool_list l =
  match l with [] -> [] | h :: t -> has_duplicates h :: duplicates_bool_list t

(* functions solving part_two *)

let reverse_str s =
  let char_list = List.of_seq (String.to_seq s) in
  let reversed_list = List.rev char_list in
  String.of_seq (List.to_seq reversed_list)

let are_anagrams a b =
  let sort_string s =
    String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq
    |> String.of_seq
  in
  sort_string a = sort_string b

let has_anagram l =
  let rec check seen = function
    | [] -> false
    | h :: t ->
        if List.exists (fun e -> are_anagrams h e) seen then true
        else check (h :: seen) t
  in
  check [] l

let rec anagram_bool_list l =
  match l with [] -> [] | h :: t -> has_anagram h :: anagram_bool_list t

(* used for both solutions *)

let rec explode l =
  match l with
  | [] -> []
  | h :: t ->
      let a = String.split_on_char ' ' h in
      a :: explode t

let filter_truthy l = List.filter (fun e -> e <> true) l

(* functions part_one and part_two *)

let part_one =
  match Util.read_lines file with
  | Some l -> List.length (explode l |> duplicates_bool_list |> filter_truthy)
  | None -> 0

let part_two =
  match Util.read_lines file with
  | Some l -> List.length (explode l |> anagram_bool_list |> filter_truthy)
  | None -> 0

let solve =
  printf "Part one: %d\n" part_one;
  printf "Part two: %d\n" part_two
