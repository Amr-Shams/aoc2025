
(* Day 11 template *)
module StrMap = Map.Make(String)

let parse_edges lines =
  let add_edge (graph, indeg) src dst =
    let children =
      match StrMap.find_opt src graph with
      | None -> [dst]
      | Some lst -> dst :: lst
    in
    let graph = StrMap.add src children graph in
    let indeg =
      let old = match StrMap.find_opt dst indeg with None -> 0 | Some x -> x in
      StrMap.add dst (old + 1) indeg
    in
    let indeg =
      if StrMap.mem src indeg then indeg else StrMap.add src 0 indeg
    in
    (graph, indeg)
  in

  List.fold_left
    (fun acc line ->
       match String.split_on_char ':' line with
       | [src; rest] ->
           let src = String.trim src in
           let dsts =
             rest |> String.trim |> String.split_on_char ' ' |> List.filter ((<>) "")
           in
           List.fold_left (fun acc dst -> add_edge acc src dst) acc dsts
       | _ -> acc)
    (StrMap.empty, StrMap.empty)
    lines

let topo_sort graph indeg =
  let q = Queue.create () in
  StrMap.iter (fun node d -> if d = 0 then Queue.add node q) indeg;

  let order = ref [] in
  let indeg = ref indeg in

  while not (Queue.is_empty q) do
    let v = Queue.take q in
    order := v :: !order;

    match StrMap.find_opt v graph with
    | None -> ()
    | Some children ->
        List.iter (fun c ->
          let d = StrMap.find c !indeg - 1 in
          indeg := StrMap.add c d !indeg;
          if d = 0 then Queue.add c q
        ) children
  done;
  List.rev !order

let count_paths lines source dest =
  let graph, indeg = parse_edges lines in
  let order = topo_sort graph indeg in

  let ways =
    List.fold_left (fun m k -> StrMap.add k 0 m) StrMap.empty order
  in
  let ways = ref (StrMap.add source 1 ways) in

  List.iter (fun v ->
    let wv = StrMap.find v !ways in
    match StrMap.find_opt v graph with
    | None -> ()
    | Some children ->
        List.iter (fun c ->
          let old = StrMap.find c !ways in
          ways := StrMap.add c (old + wv) !ways
        ) children
  ) order;

  StrMap.find dest !ways
let part1 file = 
  let lines = ref [] in 
  Util.read_file file (fun line ->
  lines := line :: !lines
  );
 string_of_int (count_paths !lines "you" "out")

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

