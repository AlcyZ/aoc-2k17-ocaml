open Printf

let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      printf "%d\n" h;
      print_int_list t

let rec print_string_list = function
  | [] -> ()
  | h :: t ->
      printf "%s\n" h;
      print_string_list t

let dbg_int_list l =
  let rec help = function
    | [] -> ()
    | [ x ] -> printf " %d " x
    | h :: t ->
        printf " %d;" h;
        help t
  in
  printf "[";
  help l;
  printf "]\n"
