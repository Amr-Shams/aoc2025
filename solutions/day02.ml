
(* Day 02 template *)
let process_range lower_bound upper_bound =
  let sum = ref 0 in
  let max_digits = String.length (string_of_int upper_bound) in
  let max_pattern_length = max_digits / 2 in
  
  for n = 1 to max_pattern_length do
    let multiplier = int_of_float (10.0 ** float_of_int n) + 1 in
    let x_min_pattern = if n = 1 then 1 else int_of_float (10.0 ** float_of_int (n - 1)) in
    let x_max_pattern = int_of_float (10.0 ** float_of_int n) - 1 in
    let x_min_range = (lower_bound + multiplier - 1) / multiplier in 
    let x_max_range = upper_bound / multiplier in
    let x_min = max x_min_pattern x_min_range in
    let x_max = min x_max_pattern x_max_range in
    
    for x = x_min to x_max do
      let number = x * multiplier in
      if number >= lower_bound && number <= upper_bound then
        sum := !sum + number
    done
  done;
  !sum

let part1 input =
  let total = ref 0 in
  let all_lines = ref [] in
  
  Util.read_file input (fun line ->
    all_lines := line :: !all_lines
  );
  
  let combined = String.concat "" (List.rev !all_lines) in
  let ranges = String.split_on_char ',' combined in
  
  List.iter (fun range_str ->
    let range_str = String.trim range_str in
    match String.split_on_char '-' range_str with
    | [lower; upper] ->
        let lower_bound = int_of_string lower in
        let upper_bound = int_of_string upper in
        total := !total + process_range lower_bound upper_bound
    | _ -> ()
  ) ranges;
  
  string_of_int !total
let is_repeated_pattern number =
  try
    let s = string_of_int number in
    let len = String.length s in
    
    for pattern_len = 1 to len / 2 do
      if len mod pattern_len = 0 then begin
        let repetitions = len / pattern_len in
        let pattern = String.sub s 0 pattern_len in
        let valid = ref true in
        
        for i = 1 to repetitions - 1 do
          let segment = String.sub s (i * pattern_len) pattern_len in
          if segment <> pattern then
            valid := false
        done;
        
        if !valid then raise Exit
      end
    done;
    false
  with Exit -> true

let part2 input =
  let total = ref 0 in
  let all_lines = ref [] in
  Util.read_file input (fun line ->
    all_lines := line :: !all_lines
  );
  let combines = String.concat "" (List.rev !all_lines) in
  let ranges = String.split_on_char ',' combines in
  List.iter (fun range_str ->
    let range_str = String.trim range_str in
    match String.split_on_char '-' range_str with
    | [lower; upper] ->
        let lower_bound = int_of_string lower in
        let upper_bound = int_of_string upper in
        for num = lower_bound to upper_bound do
          if is_repeated_pattern num then
            total := !total +num 
          done
    | _ -> ()
  ) ranges;
  string_of_int !total

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

