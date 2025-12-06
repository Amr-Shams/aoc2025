
(* Day 06 template *)

let part1 file =
  
  let matrix_list =  Util.read_file_matrix file Util.split_line_to_strings in  
  let matrix =
  matrix_list
  |> List.map Array.of_list   
  |> Array.of_list
  in

  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  (* loop over the matrix vertically till the last line  *)
  let total = ref 0 in 
  for y = 0 to cols - 1 do
    (* take the last line ops  *)
    let op = matrix.(rows - 1).(y).[0] in
    let curent = match op with 
    | '*' -> 1 
    | '+' -> 0 
    | _ -> failwith "not implemented"
    in
    let current_row = ref curent in 
    for x = rows - 2 downto 0 do
      let cell = matrix.(x).(y) in  
      let num = int_of_string  cell in 
      current_row := match op with
      |'*' -> !current_row * num
      | '+' -> !current_row + num
      |_ -> failwith "not implemented"
    done;
    total := !total + !current_row
  done; 
  string_of_int !total


let part2 file =
  "TODO: implement part 2"

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in

  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

