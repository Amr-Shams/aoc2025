(* Day 01 template *)

let range = 100 
let start = ref 50  

let part1 file =
  let count = ref 0 in
  Util.read_file file (fun line ->
    (* extract direction and number *)
    let dir = String.get line 0 in
    let num = int_of_string (String.sub line 1 (String.length line - 1)) in

    (* update start based on direction *)
    let delta = match dir with
      | 'L' -> -num
      | 'R' -> num
      | _ -> failwith ("Unknown direction: " ^ String.make 1 dir)
    in

    (* apply modulo wrapping *)
    let res = (!start + delta) mod range in
    let res = if res < 0 then res + range else res in

    (* count zeros *)
    if res = 0 then count := !count + 1;

    (* update start for next line *)
    start := res
  );
  string_of_int !count

let part2 file =
  let count = ref 0 in

  let rec simulate_clicks dir rem= 
    if rem = 0 then () 
    else begin 
      start :=(match dir with 
      | 'L' -> !start - 1
      | 'R' -> !start + 1
      | _ -> failwith "Unknown direction");
      if !start mod range = 0 then 
        count := !count + 1; 
        simulate_clicks dir (rem - 1);
      end 
  in 
  Util.read_file file (fun line ->
    let dir = String.get line 0 in
    let num = int_of_string (String.sub line 1 (String.length line - 1)) in
    simulate_clicks dir num
  );
  string_of_int !count

  let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 file)
  | "2" -> print_endline (part2 file)
  | _ -> failwith "Part must be 1 or 2"

