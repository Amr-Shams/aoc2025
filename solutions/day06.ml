
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
  let allLines = ref [] in
  Util.read_file file (fun line ->
    allLines := line :: !allLines
  );
  let allLines = List.rev !allLines in
  
  if List.length allLines = 0 then
    "0"
  else begin
    let num_rows = List.length allLines in
    let ops_line = List.nth allLines (num_rows - 1) in  (* Last line has operators *)
    let data_lines = List.filteri (fun i _ -> i < num_rows - 1) allLines in
    
    let cols = String.length ops_line in
    
    let grandTotal = ref 0 in
    let current = ref [] in
    
    for i = cols - 1 downto 0 do
      let operation = ops_line.[i] in
      
      let str = ref "" in
      List.iter (fun line ->
        if i < String.length line then
          str := !str ^ String.make 1 line.[i]
      ) data_lines;
      
      let trimmed = String.trim !str in
      
      if trimmed <> "" then begin
        let digit = int_of_string trimmed in
        current := !current @ [digit]
      end;
      
      match operation with
      | '+' ->
          let sum = List.fold_left (+) 0 !current in
          grandTotal := !grandTotal + sum;
          current := []
      | '*' ->
          let prod = List.fold_left ( * ) 1 !current in
          grandTotal := !grandTotal + prod;
          current := []
      | ' ' -> ()
      | _ -> ()
    done;
    
    string_of_int !grandTotal
  end

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in

  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

