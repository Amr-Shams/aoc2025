(* Shared module for AoC 2025 *)

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



