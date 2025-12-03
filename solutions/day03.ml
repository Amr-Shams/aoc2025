
(* Day 03 template *)


let part1 file =
  let total = ref 0 in
  Util.read_file file (fun line ->
    let digits = Util.digits_of_string line in
    let n = Array.length digits in
    
    let left = Array.sub digits 0 (n - 1) in
    let left_index = Util.index_of_max left in
    
    let right_len = n - (left_index + 1) in
    let right = Array.sub digits (left_index + 1) right_len in
    let right_index = Util.index_of_max right in
    
    let first_digit = digits.(left_index) in
    let second_digit = right.(right_index) in
    let num = first_digit * 10 + second_digit in
    
    total := !total + num
  );
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

