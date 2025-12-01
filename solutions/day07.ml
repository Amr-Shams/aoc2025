
(* Day 07 template *)

let part1 input =
  "TODO: implement part 1"

let part2 input =
  "TODO: implement part 2"

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  let ic = open_in file in
  let len = in_channel_length ic in
  let input = really_input_string ic len in
  close_in ic;

  match part with
  | "1" -> print_endline (part1 input)
  | "2" -> print_endline (part2 input)
  | _ -> failwith "Part must be 1 or 2"

