let read_firstline filename =
  try
    let ic = open_in filename in
    try
      let result = Some (input_line ic) in
      close_in ic;
      result
    with e ->
      close_in ic;
      let ex = Printexc.to_string e in
      Printf.printf "\t --- Process file error: %s ---" ex;
      None
  with Sys_error msg ->
    Printf.printf "\t --- File error: %s ---\n" msg;
    None

let read_lines filename =
  let lines = ref [] in
  try
    let ic = open_in filename in
    try
      while true do
        lines := input_line ic :: !lines
      done;
      Some !lines
    with End_of_file ->
      close_in ic;
      Some (List.rev !lines)
  with Sys_error _ -> None

let to_int_list_by_tab input =
  input |> String.split_on_char '\t'
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s -> int_of_string s)

let read_matrix filename =
  match read_lines filename with
  | Some l -> Some (List.map to_int_list_by_tab l)
  | None -> None

let read_charlist filename =
  match read_firstline filename with
  | Some c -> Some (List.init (String.length c) (String.get c))
  | None -> None

let read_intlist filename =
  match read_charlist filename with
  | Some l -> Some (List.map (fun c -> int_of_char c - int_of_char '0') l)
  | None -> None
