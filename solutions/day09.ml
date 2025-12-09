
(* Day 09 template *)
type point = {
  x : int;
  y : int;
}

let area p1 p2 = 
  let ar = (p1.x - p2.x) * (p1.y - p2.y) in
  if ar < 0 then -ar else ar 
let all_pairs points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let rec build i j acc =
    if i >= n then acc
    else if j >= n then
      build (i + 1) (i + 2) acc
    else
      let p1 = arr.(i) in 
      let p2 = arr.(j) in 
      let dist = distance p1 p2 in
      build i (j + 1) ({i = i; j = j; dist=dist} :: acc)
  in
  build 0 1 []


let part1 file =
  let points = ref [] in 
  Util.read_file file (fun line -> 
    let nums = List.map int_of_string ( String.split_on_char ',' line) in
    points := {x = List.nth nums 0; y = List.nth nums 1; z = List.nth nums 2} :: !points
  );
  (* convert the list of points to array  *)
  let array = Array.of_list !points in

  "TODO: implement part 1"

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

