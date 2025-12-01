
(* Day $day template *)

let part1 input =
  \"TODO: implement part 1\"

let part2 input =
  \"TODO: implement part 2\"

let () =
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  let input = In_channel.read_all file in
  match part with
  | \"1\" -> print_endline (part1 input)
  | \"2\" -> print_endline (part2 input)
  | _ -> failwith \"Part must be 1 or 2\"

