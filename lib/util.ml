(* Shared module for AoC 2025 *)
(* 8-direction movement: N, NE, E, SE, S, SW, W, NW *)
let directions8 : (int * int) array =
  [|
    (-1,  0);  (* N  *)
    (-1,  1);  (* NE *)
    ( 0,  1);  (* E  *)
    ( 1,  1);  (* SE *)
    ( 1,  0);  (* S  *)
    ( 1, -1);  (* SW *)
    ( 0, -1);  (* W  *)
    (-1, -1);  (* NW *)
  |]

let rec process_lines ic process_line =
  try
    let line = input_line ic in
    process_line line;
    process_lines ic process_line  
  with
  | End_of_file -> () 

(* Read a file and apply process_line to each line *)
let read_file filename process_line =
  let ic = open_in filename in
  try
    process_lines ic process_line;
    close_in ic
  with e ->
    close_in_noerr ic; 
    raise e

let read_file_matrix filename split_chars_of_string =
  let matrix = ref [] in
  read_file filename (fun line ->
    matrix := (split_chars_of_string line) :: !matrix
  );
  List.rev !matrix




(* max of a list *)
let rec max_list list = 
  match list with  
  | [] -> failwith "max_list called on empty list"
  | [x] -> x 
  | h :: t -> 
      let max_tail = max_list t in
      if h > max_tail then h else max_tail 

  (* min of a list *)
let rec min_list list = 
  match list with  
  | [] -> failwith "min_list called on empty list"
  | [x] -> x 
  | h :: t -> 
      let min_tail = min_list t in
      if h < min_tail then h else min_tail

  (* find the max value index in a list *)
let index_of_max arr =
  let max_i = ref 0 in
  for i = 1 to Array.length arr - 1 do
    if arr.(i) > arr.(!max_i) then max_i := i
  done;
  !max_i 
(* only works on digits strings like '1234' *)
let digits_of_string s =
  s
  |> String.to_seq
  |> Seq.filter (fun c -> c >= '0' && c <= '9')
  |> Seq.map (fun c -> Char.code c - 48)
  |> Array.of_seq
let split_line_to_strings line =
  line
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")


let split_line_to_strings_with_padding line =
  let parts = line
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  let max_len = List.fold_left (fun acc s -> max acc (String.length s)) 0 parts in
  List.map (fun s -> 
    let padding = String.make (max_len - String.length s) ' ' in
    padding ^ s  
  ) parts
(* char arry from string  *)

let char_array_of_string s = 
  let arr = Array.make (String.length s) ' ' in
  for i = 0 to String.length s - 1 do
    arr.(i) <- s.[i]
  done;
  arr
