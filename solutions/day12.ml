
(* Day 12 template *)


open Str

type phase = Reading_shapes | Reading_regions

let is_region_line line =
  Str.string_match (Str.regexp "^[0-9]+x[0-9]+:") line 0

let count_hashes_in_row s =
  let cnt = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '#' then incr cnt
  done;
  !cnt

let sum_ints lst = List.fold_left (+) 0 lst

let part1 file =
  let phase = ref Reading_shapes in
  let raw_shapes : string list ref = ref [] in
  let current_shape_lines : string list ref = ref [] in
  let region_lines : string list ref = ref [] in

  Util.read_file file (fun line ->
      let line = String.trim line in
      if line = "" then
        (match !phase with
         | Reading_shapes ->
             if !current_shape_lines <> [] then begin
               raw_shapes := (String.concat "\n" (List.rev !current_shape_lines)) :: !raw_shapes;
               current_shape_lines := []
             end
         | Reading_regions -> ()
        )
      else if is_region_line line then begin
        if !current_shape_lines <> [] then begin
          raw_shapes := (String.concat "\n" (List.rev !current_shape_lines)) :: !raw_shapes;
          current_shape_lines := []
        end;
        phase := Reading_regions;
        region_lines := line :: !region_lines
      end else
        match !phase with
        | Reading_shapes ->
            (* accumulate shape lines *)
            current_shape_lines := line :: !current_shape_lines
        | Reading_regions ->
            region_lines := line :: !region_lines
    );

  if !current_shape_lines <> [] then
    raw_shapes := (String.concat "\n" (List.rev !current_shape_lines)) :: !raw_shapes;

  let raw_shapes = List.rev !raw_shapes in
  let region_lines = List.rev !region_lines in

  let shape_num_tiles =
    List.map
      (fun raw_shape ->
         let lines = Str.split (Str.regexp "\n") raw_shape in
         match lines with
         | _index :: grid_lines ->
             if List.length grid_lines <> 3 then
               failwith "expected 3 rows per shape";
             List.iter (fun row ->
                 if String.length row <> 3 then
                   failwith "expected rows of length 3 in shape"
               ) grid_lines;
             sum_ints (List.map count_hashes_in_row grid_lines)
         | _ -> failwith "malformed shape block"
      )
      raw_shapes
  in

  let total = ref 0 in
  let shape_count = List.length shape_num_tiles in

  List.iter
    (fun raw_region ->
       let line = String.trim raw_region in
       let toks = Str.split (Str.regexp "[^0-9]+") line |> List.filter (fun s -> s <> "") in
       match toks with
       | w :: h :: rest ->
           let width = int_of_string w in
           let height = int_of_string h in
           let quantities =
             try List.map int_of_string rest
             with _ -> failwith "could not parse quantities"
           in
           if List.length quantities <> shape_count then
             failwith "mismatch between number of shape definitions and quantities in a region line";

           let max_presents_lower_bound = (width / 3) * (height / 3) in
           let num_presents = sum_ints quantities in
           if num_presents <= max_presents_lower_bound then
             total := !total + 1
           else
             let num_tiles_lower_bound =
               List.fold_left2 (fun acc tiles qty -> acc + (tiles * qty)) 0 shape_num_tiles quantities
             in
             let region_num_tiles = width * height in
             if num_tiles_lower_bound > region_num_tiles then
               ()  
             else
               failwith "shape packing is complicated"
       | _ -> failwith ("could not parse region line: " ^ line)
    )
    region_lines;

  string_of_int !total

let part2 _file = "TODO: implement part 2"

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"
