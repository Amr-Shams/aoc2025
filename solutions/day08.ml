(* Day 08 template *)
type point = {
  x : int;
  y : int;
  z : int;
}

type pair_with_distance = {
  i: int; 
  j: int; (* index of the second point  *)
  dist : float;
}

let distance p1 p2 =
  let dx = p1.x - p2.x in
  let dy = p1.y - p2.y in
  let dz = p1.z - p2.z in
  sqrt (float_of_int (dx * dx + dy * dy + dz * dz))

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

let sort_pairs pairs =
  let arr = Array.of_list pairs in
  Array.sort (fun a b -> compare a.dist b.dist) arr;
  arr

let part1 file =
  let points = ref [] in
  Util.read_file file (fun line ->
      let nums = List.map int_of_string (String.split_on_char ',' line) in
      match nums with
      | [x; y; z] -> points := { x; y; z } :: !points
      | _ -> failwith "invalid line"
    );
  
  let point_list = List.rev !points in
  let n = List.length point_list in

  let pairs = all_pairs point_list in
  let sorted_edges = sort_pairs pairs in
  
  let uf = Util.make_uf n in
  
  let edges_to_process = 1000 in 

  for i = 0 to edges_to_process - 1 do
    if i < Array.length sorted_edges then
      let edge = sorted_edges.(i) in
      let _ = Util.union uf edge.i edge.j in
      ()
  done;

  let sizes = Hashtbl.create n in
  for i = 0 to n - 1 do
    let root = Util.find uf i in
    let current_size = Hashtbl.find_opt sizes root |> Option.value ~default:0 in
    Hashtbl.replace sizes root (current_size + 1)
  done;

  let all_sizes = Hashtbl.fold (fun _ size acc -> size :: acc) sizes []
                  |> List.sort (fun a b -> compare b a) in
  

  let ans = match all_sizes with
    | a :: b :: c :: _ -> a * b * c
    | _ -> failwith "Failed to find at least 3 components."
  in
  string_of_int ans

let part2 file =
  let points = ref [] in
  Util.read_file file (fun line ->
      let nums = List.map int_of_string (String.split_on_char ',' line) in
      match nums with
      | [x; y; z] -> points := { x; y; z } :: !points
      | _ -> failwith "invalid line"
    );
  
  let point_list = List.rev !points in
  let n = List.length point_list in

  let point_array = Array.of_list point_list in

  let pairs = all_pairs point_list in
  let sorted_edges = sort_pairs pairs in
  
  let uf = Util.make_uf n in
  
  let target_merges = n - 1 in 
  let successful_merges = ref 0 in
  let last_connecting_edge = ref None in

  for i = 0 to Array.length sorted_edges - 1 do
    if !successful_merges < target_merges then
      let edge = sorted_edges.(i) in
      if Util.union uf edge.i edge.j then
          incr successful_merges;
          if !successful_merges = target_merges then
            last_connecting_edge := Some edge
  done;
  
  let ans = match !last_connecting_edge with
    | None -> failwith "Could not connect all components."
    | Some final_edge ->
        let p1 = point_array.(final_edge.i) in
        let p2 = point_array.(final_edge.j) in
        p1.x * p2.x
  in
  
  string_of_int ans

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in

  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

