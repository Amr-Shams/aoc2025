(* Day 09 template *)
type point = {
  x : int;
  y : int;
}

type edge = {
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
}

let area p1 p2 =
  let dx = abs (p1.x - p2.x) + 1 in
  let dy = abs (p1.y - p2.y) + 1 in
  dx * dy

let manhattan_distance p1 p2 =
  abs (p1.x - p2.x) + abs (p1.y - p2.y)

let bboxes_intersect minX minY maxX maxY edge =
  let eMinX = min edge.x1 edge.x2 in
  let eMaxX = max edge.x1 edge.x2 in
  let eMinY = min edge.y1 edge.y2 in
  let eMaxY = max edge.y1 edge.y2 in
  minX < eMaxX && maxX > eMinX && minY < eMaxY && maxY > eMinY

let rect_intersects_edges minX minY maxX maxY edges =
  let intersects = ref false in
  List.iter (fun edge ->
    if bboxes_intersect minX minY maxX maxY edge then
      intersects := true
  ) edges;
  !intersects

let all_pairs points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  let rec loop_i i acc =
    if i >= n then acc
    else
      let rec loop_j j acc =
        if j >= n then acc
        else
          let p1 = arr.(i) and p2 = arr.(j) in
          if p1.x <> p2.x && p1.y <> p2.y then
            loop_j (j+1) (area p1 p2 :: acc)
          else
            loop_j (j+1) acc
      in
      loop_i (i+1) (loop_j (i+1) acc)
  in
  loop_i 0 []

let all_pairs_bounded points =
  let arr = Array.of_list points in
  let n = Array.length arr in
  
  let edges = ref [] in
  for i = 0 to n - 1 do
    let p1 = arr.(i) in
    let p2 = arr.((i + 1) mod n) in
    edges := {x1 = p1.x; y1 = p1.y; x2 = p2.x; y2 = p2.y} :: !edges
  done;
  
  let max_area = ref 0 in
  
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let p1 = arr.(i) and p2 = arr.(j) in
      if p1.x <> p2.x && p1.y <> p2.y then begin
        let minX = min p1.x p2.x in
        let maxX = max p1.x p2.x in
        let minY = min p1.y p2.y in
        let maxY = max p1.y p2.y in
        
        let manhattan = manhattan_distance p1 p2 in
        if manhattan * manhattan > !max_area then begin
          if not (rect_intersects_edges minX minY maxX maxY !edges) then begin
            let a = area p1 p2 in
            if a > !max_area then
              max_area := a
          end
        end
      end
    done
  done;
  
  [!max_area]

let part1 file =
  let points = ref [] in
  Util.read_file file (fun line ->
    match List.map int_of_string (String.split_on_char ',' line) with
    | [x; y] -> points := { x; y } :: !points
    | _ -> failwith "invalid line"
  );
  let point_list = List.rev !points in
  let areas = all_pairs point_list in
  if areas = [] then
    "0"
  else
    let max_area = List.fold_left max 0 areas in
    string_of_int max_area

let part2 file =
  let points = ref [] in
  Util.read_file file (fun line ->
    match List.map int_of_string (String.split_on_char ',' line) with
    | [x; y] -> points := { x; y } :: !points
    | _ -> failwith "invalid line"
  );
  let point_list = List.rev !points in
  let areas = all_pairs_bounded point_list in
  if areas = [] then
    "0"
  else
    let max_area = List.fold_left max 0 areas in
    string_of_int max_area

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"
