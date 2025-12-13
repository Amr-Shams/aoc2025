
(* Day 11 template *)
module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

let parse_graph lines =
  List.fold_left (fun g line ->
    match String.split_on_char ':' line with
    | [src; rest] ->
        let src = String.trim src in
        let dsts =
          rest |> String.trim |> String.split_on_char ' '
               |> List.filter ((<>) "")
               |> List.map String.trim
        in
        StrMap.add src dsts g
    | _ -> g
  ) StrMap.empty lines

let count_paths graph source dest =
  let rec dfs visited node =
    if node = dest then 1
    else if StrSet.mem node visited then 0
    else
      let visited = StrSet.add node visited in
      match StrMap.find_opt node graph with
      | None -> 0
      | Some children ->
          List.fold_left (fun acc child ->
            acc + dfs visited child
          ) 0 children
  in
  dfs StrSet.empty source

let count_paths_with_required graph source dest required_nodes =
  let required_set = 
    List.fold_left (fun s x -> StrSet.add x s) StrSet.empty required_nodes
  in
  
  let rec dfs visited visited_req node =
    if node = dest then
      if StrSet.equal visited_req required_set then 1 else 0
    else if StrSet.mem node visited then 0
    else
      let visited = StrSet.add node visited in
      let visited_req = 
        if StrSet.mem node required_set 
        then StrSet.add node visited_req 
        else visited_req
      in
      match StrMap.find_opt node graph with
      | None -> 0
      | Some children ->
          List.fold_left (fun acc child ->
            acc + dfs visited visited_req child
          ) 0 children
  in
  dfs StrSet.empty StrSet.empty source

let part1 file = 
  let lines = ref [] in 
  Util.read_file file (fun line ->
    lines := line :: !lines
  );
  let lines = List.rev !lines in 
  let graph = parse_graph lines in
  string_of_int (count_paths graph "you" "out")

let part2 file =
  let lines = ref [] in 
  Util.read_file file (fun line ->
    lines := line :: !lines
  ); 
  let lines = List.rev !lines in 
  let graph = parse_graph lines in
  string_of_int (count_paths_with_required graph "svr" "out" ["dac"; "fft"])

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"
