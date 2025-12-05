
(* Day 05 template *)
(** 
 * Range Validator using OCaml's functional Map module.
 * 
 * We store ranges as key-value pairs in a Map:
 * Key: The starting integer of the range.
 * Value: The ending integer of the range.
 * 
 * Example Data Structure:
 * { 100 -> 200; 500 -> 600; 1000 -> 1010 }
 *)
(* tuple for start and end  *)
type interval = int * int

let compare_interval ((s1,e1):interval) ((s2,e2):interval) : int =
  let start_compare = compare s1 s2 in
  if start_compare <> 0 then start_compare
  else compare e1 e2

module IntervalSet = Set.Make(struct
  type t = interval
  let compare = compare_interval  
end)

type normalized_range_set = IntervalSet.t

let insert_normalized ((s_new, e_new) : interval) (ranges : normalized_range_set) : normalized_range_set =
  let (before, _present, overlap_and_after) = IntervalSet.split (s_new, max_int) ranges in 
  let overlaps = 
    IntervalSet.filter (fun (s_curr, e_curr) -> 
      s_new <= e_curr && e_new >= s_curr
    ) overlap_and_after
  in
  let (merged_start, merged_end) = 
    IntervalSet.fold (fun (s_curr, e_curr) (acc_s, acc_e) ->
      (min acc_s s_curr), (max acc_e e_curr)
    ) overlaps (s_new, e_new) 
  in
  let ranges_without_overlaps = 
    IntervalSet.fold IntervalSet.remove overlaps overlap_and_after 
  in
  let final_set = IntervalSet.union before ranges_without_overlaps in
  IntervalSet.add (merged_start, merged_end) final_set

let is_id_valid (id : int) (ranges : normalized_range_set) : bool =
  IntervalSet.exists (fun (start_id, end_id) ->
    id >= start_id && id <= end_id
  ) ranges

let part1 file =
  let space = ref 0 in 
  let valid_ids = ref IntervalSet.empty in  
  let fresh_count = ref 0 in 
  
  Util.read_file file (fun line ->
    if line = "" then 
      space := !space + 1
    else if !space = 0 then begin 
      (* we are collecting the ranges *)
      match String.split_on_char '-' line with  
      | [num1_str; num2_str] ->
          let num1 = int_of_string num1_str in
          let num2 = int_of_string num2_str in 
          valid_ids := insert_normalized (num1, num2) !valid_ids  
      | _ -> ()  
    end else begin
      let id = int_of_string line in
      if is_id_valid id !valid_ids then  
        fresh_count := !fresh_count + 1
    end
  );
  string_of_int !fresh_count

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
