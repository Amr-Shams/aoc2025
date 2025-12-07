
(* Day 07 template *)

let part1 file =
  (* Track the S column *)
  let start_col = ref 0 in

  let char_array_of_string s =
    let arr = Array.make (String.length s) ' ' in
    for j = 0 to String.length s - 1 do
      if s.[j] = 'S' then start_col := j;
      arr.(j) <- s.[j]
    done;
    arr
  in

  let matrix_list = Util.read_file_matrix file char_array_of_string in
  let matrix = Array.of_list matrix_list in
  let rows  = Array.length matrix in
  let cols  = Array.length matrix.(0) in

  (* visited state: (row, col) *)
  let visited = Hashtbl.create (rows * cols) in
  let rec beam x y = 
    if x<0 || x>=rows || y<0 || y>=cols then 0 
    else if Hashtbl.mem visited (x,y) then 0
    else(
      Hashtbl.add visited (x,y) ();
      match matrix.(x).(y) with
      |'.' | 'S' -> beam (x+1) y
      |'^' -> 
          let left = beam (x+1) (y-1) in
          let right= beam (x+1) (y+1) in
          1 + left + right
      | _ -> 0
    ) 
  in 
  string_of_int (beam 0 !start_col)

let part2 file =
  let start_col = ref 0 in
  let char_array_of_string s =
    let arr = Array.make (String.length s) ' ' in
    for j = 0 to String.length s - 1 do
      if s.[j] = 'S' then start_col := j;
      arr.(j) <- s.[j]
    done;
    arr
  in
  let matrix_list = Util.read_file_matrix file char_array_of_string in
  let matrix = Array.of_list matrix_list in
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  
  (*  column -> number of timelines *)
  let timelines = Hashtbl.create cols in
  Hashtbl.add timelines !start_col 1;
  
  for y = 0 to rows - 1 do
    let next_timelines = Hashtbl.create cols in
    
    let add_ways x ways =
      let current = try Hashtbl.find next_timelines x with Not_found -> 0 in
      Hashtbl.replace next_timelines x (current + ways)
    in
    
    Hashtbl.iter (fun x ways ->
      match matrix.(y).(x) with
      | '^' ->
          if x > 0 then add_ways (x-1) ways;
          if x < cols - 1 then add_ways (x+1) ways
      | '.' | 'S' ->
          add_ways x ways
      | _ -> ()
    ) timelines;
    
    Hashtbl.clear timelines;
    Hashtbl.iter (fun x ways -> Hashtbl.add timelines x ways) next_timelines
  done;
  
  let total = Hashtbl.fold (fun _ ways acc -> acc + ways) timelines 0 in
  string_of_int total

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in

  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

