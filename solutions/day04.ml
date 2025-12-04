
(* Day 04 template *)
let cell_is_valid matrix x y =
  let counter = ref 0 in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.iter (fun (dx, dy) ->
    let nx = x + dx in
    let ny = y + dy in
    if nx >= 0 && nx < rows && ny >= 0 && ny < cols then
      if matrix.(nx).(ny) = '@' then incr counter
  ) Util.directions8;
  !counter

let part1 file =
  let matrix_list = Util.read_file_matrix file Util.char_array_of_string in
  let matrix = Array.of_list matrix_list in
  let count_valid matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    let count = ref 0 in
    for x = 0 to rows - 1 do
      for y = 0 to cols - 1 do
        if matrix.(x).(y) = '@' && (cell_is_valid matrix x y < 4)then begin 
          incr count
        end
      done
    done;
    !count
  in
  string_of_int (count_valid matrix)

let part2 file =
  let matrix_list = Util.read_file_matrix file Util.char_array_of_string in 
  let matrix = Array.of_list matrix_list in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  
  (* count neighbors for each cell *)
  let neighbor_count = Array.make_matrix rows cols 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if matrix.(x).(y) = '@' then begin
        neighbor_count.(x).(y) <- cell_is_valid matrix x y
      end
    done 
  done;
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

