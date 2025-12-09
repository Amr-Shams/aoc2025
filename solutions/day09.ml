
(* Day 09 template *)
type point = {
  x : int;
  y : int;
}
type pair_with_area = {
  i: int; 
  j: int; 
  area : int;
}

let area p1 p2 =
  let dx = abs (p1.x - p2.x) in
  let dy = abs (p1.y - p2.y) in
  dx * dy

let all_pairs points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let rec build i j acc =
    if i >= n then acc
    else if j >= n then
      build (i + 1) (i + 1 + 1) acc
    else
      let p1 = arr.(i)
      and p2 = arr.(j) in
      build i (j + 1) ({ i; j; area = area p1 p2 } :: acc)
  in
  build 0 1 []

let sort_pairs pairs =
  let arr = Array.of_list pairs in
  Array.sort (fun a b -> compare a.area b.area) arr;
  arr

let part1 file =
  let points = ref [] in
  Util.read_file file (fun line ->
    match List.map int_of_string (String.split_on_char ',' line) with
    | [x; y] -> points := { x; y } :: !points
    | _ -> failwith "invalid line"
  );
  let point_list = List.rev !points in
  let pairs = all_pairs point_list in
  let sorted = sort_pairs pairs in
  string_of_int sorted.(0).area

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

