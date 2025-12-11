
(* Day 11 template *)
module StrMap = Map.Make(String)

let parse_graph lines =
  List.fold_left (fun g line ->
    match String.split_on_char ':' line with
    | [src; rest] ->
        let src = String.trim src in
        let dsts =
          rest |> String.trim |> String.split_on_char ' '
               |> List.filter ((<>) "")
        in
        StrMap.add src dsts g
    | _ -> g
  ) StrMap.empty lines

let rec dfs_count graph node dest =
  if node = dest then 1
  else
    match StrMap.find_opt node graph with
    | None -> 0
    | Some children ->
        List.fold_left
          (fun acc c -> acc + dfs_count graph c dest)
          0
          children

let count_paths lines source dest =
  let graph = parse_graph lines in
  dfs_count graph source dest

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

