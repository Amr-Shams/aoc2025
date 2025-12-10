
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
  let re = Str.regexp "\\[\\([^]]*\\)\\]" in
  try
    let _ = Str.search_forward re line 0 in
    let brackets_content = Str.matched_group 1 line in
    let target_mask = mask_of_brackets  brackets_content in
    
    let paren_re = Str.regexp "(\\([^)]*\\))" in
    let rec extract acc pos =
      try
        let _ = Str.search_forward paren_re line pos in
        let content = Str.matched_group 1 line in
        let mask = mask_of_point_string  content  in
        extract (mask :: acc) (Str.match_end ())
      with Not_found -> List.rev acc
    in
    let buttons = extract [] (Str.match_end ()) in
    Some (target_mask, buttons)
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

let part1 file =
  let total = ref 0 in
  let line_num = ref 0 in
  Util.read_file file (fun line ->
    incr line_num;
    match parse_line line with
    | Some (target, buttons) ->
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

let part2 _file =
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
