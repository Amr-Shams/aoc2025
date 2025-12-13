
(* Day 10 template *)
(* Day 10 template *)
open Str
open Queue
open Hashtbl

let mask_of_brackets s =
  let len = String.length s in
  let result = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] = '#' then
      result := !result lor (1 lsl i)
  done;
  !result

let mask_of_point_string s =
  s 
  |> String.split_on_char ','
  |> List.map int_of_string
  |> List.fold_left (fun acc x -> acc lor (1 lsl x)) 0

let parse_line line =
  let re = Str.regexp "\\[\\([^]]*\\)\\] " in
  try
    let _ = Str.search_forward re line 0 in
    let brackets_content = Str.matched_group 1 line in
    let target_mask = mask_of_brackets brackets_content in
    
    (* Extract buttons using parentheses *)
    let paren_re = Str.regexp "(\\([^)]*\\))" in
    let rec extract_buttons acc pos =
      try
        let _ = Str.search_forward paren_re line pos in
        let content = Str.matched_group 1 line in
        let mask = mask_of_point_string content in
        extract_buttons (mask :: acc) (Str.match_end ())
      with Not_found -> List.rev acc
    in
    let buttons = extract_buttons [] (Str.match_end ()) in
    
    (* Extract joltages from curly braces *)
    let jolts_re = Str.regexp "{\\([^}]*\\)}" in
    let joltages = 
      try
        let _ = Str.search_forward jolts_re line 0 in
        let content = Str.matched_group 1 line in
        content
        |> String.split_on_char ','
        |> List.map int_of_string
      with Not_found -> []
    in
    
    Some (target_mask, buttons, joltages)
  with Not_found -> None

(* BFS to find minimal presses *)
let min_presses target buttons =
  let visited = Hashtbl.create 100 in
  let q = Queue.create () in
  Queue.add (0, 0) q;
  Hashtbl.add visited 0 true;
  let rec loop () =
    if Queue.is_empty q then failwith "No solution"
    else
      let (state, presses) = Queue.take q in
      if state = target then presses
      else begin
        List.iter (fun btn ->
          let next_state = state lxor btn in
          if not (Hashtbl.mem visited next_state) then begin
            Queue.add (next_state, presses + 1) q;
            Hashtbl.add visited next_state true
          end
        ) buttons;
        loop ()
      end
  in
  loop ()

let find_patterns target_parity buttons num_jolts =
  let n = List.length buttons in
  let results = ref [] in
  for mask = 0 to (1 lsl n) - 1 do
    let state = List.fold_left (fun (acc, i) btn ->
      if (mask land (1 lsl i)) <> 0 then
        (acc lxor btn, i + 1) 
      else
        (acc, i + 1)
    ) (0, 0) buttons |> fst in
    
    let matches = ref true in
    for j = 0 to num_jolts - 1 do
      let target_bit = (target_parity land (1 lsl j)) <> 0 in
      let state_bit = (state land (1 lsl j)) <> 0 in
      if target_bit <> state_bit then matches := false
    done;
    
    if !matches then begin
      let pressed = ref [] in
      for i = 0 to n - 1 do
        if (mask land (1 lsl i)) <> 0 then
          pressed := i :: !pressed
      done;
      results := (List.rev !pressed, List.length !pressed) :: !results
    end
  done;
  !results

let solve_joltages buttons joltages =
  let memo = Hashtbl.create 1000 in
  let num_jolts = List.length joltages in
  
  let rec solve jolts =
    (* Base case: all zeros *)
    if List.for_all (fun x -> x = 0) jolts then 0
    else if List.exists (fun x -> x < 0) jolts then 1000000
    else begin
      let key = String.concat "," (List.map string_of_int jolts) in
      match Hashtbl.find_opt memo key with
      | Some v -> v
      | None ->
          let parity = List.fold_left (fun (acc, i) x ->
            if x mod 2 = 1 then (acc lor (1 lsl i), i + 1)
            else (acc, i + 1)
          ) (0, 0) jolts |> fst in
          
          let patterns = find_patterns parity buttons num_jolts in
          
          if patterns = [] then begin
            Hashtbl.add memo key 1000000;
            1000000
          end else begin
            let min_cost = List.fold_left (fun best (pattern, num_presses) ->
              let remaining = Array.of_list jolts in
              List.iter (fun btn_idx ->
                let btn = List.nth buttons btn_idx in
                for j = 0 to num_jolts - 1 do
                  if (btn land (1 lsl j)) <> 0 then
                    remaining.(j) <- remaining.(j) - 1
                done
              ) pattern;
              
              let all_even = ref true in
              for i = 0 to num_jolts - 1 do
                if remaining.(i) mod 2 <> 0 then all_even := false
              done;
              
              if !all_even then begin
                let halved = Array.to_list (Array.map (fun x -> x / 2) remaining) in
                let cost = num_presses + 2 * solve halved in
                min best cost
              end else best
            ) 1000000 patterns in
            
            Hashtbl.add memo key min_cost;
            min_cost
          end
    end
  in
  solve joltages

let part1 file =
  let total = ref 0 in
  let line_num = ref 0 in
  Util.read_file file (fun line ->
    incr line_num;
    match parse_line line with
    | Some (target, buttons, _) ->
        Printf.eprintf "Line %d: target=%d, buttons=[%s]\n" 
          !line_num target 
          (String.concat ";" (List.map string_of_int buttons));
        let presses = min_presses target buttons in
        Printf.eprintf "  -> presses=%d\n" presses;
        total := !total + presses
    | None -> 
        Printf.eprintf "Line %d: FAILED TO PARSE: %s\n" !line_num line
  );
  string_of_int !total

let part2 file =
  let total = ref 0 in
  let line_num = ref 0 in
  Util.read_file file (fun line ->
    incr line_num;
    match parse_line line with
    | Some (_, buttons, joltages) when joltages <> [] ->
        Printf.eprintf "Line %d: joltages=[%s], buttons=%d\n" 
          !line_num
          (String.concat ";" (List.map string_of_int joltages))
          (List.length buttons);
        let presses = solve_joltages buttons joltages in
        Printf.eprintf "  -> presses=%d\n" presses;
        total := !total + presses
    | _ -> 
        Printf.eprintf "Line %d: skipped or no joltages\n" !line_num
  );
  string_of_int !total

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"
