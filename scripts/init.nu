let file_dirs = [inputs solutions build scripts bin]

let max_days = 25

let year = $env.AOC_YEAR 

let session = $env.AOC_SESSION

# define a func that takes the file_dirs and creates them if not exist
def create_dirs [dirs: list<string>] {
  for dir in $dirs {
    if not ($dir | path exists) {
      mkdir $dir
    }
  }
}

def fmt-day [day:int] {
    if $day < 10 { $"0($day)" } else { $"($day)" }
}

let template = '
(* Day DAY template *)

let part1 input =
  "TODO: implement part 1"

let part2 input =
  "TODO: implement part 2"

let () =
  if Array.length Sys.argv < 3 then
    failwith "usage: program <part> <file>";

  let part = Sys.argv.(1) in
  let file = Sys.argv.(2) in
  let ic = open_in file in
  let len = in_channel_length ic in
  let input = really_input_string ic len in
  close_in ic;

  match part with
  | "1" -> print_endline (part1 input)
  | "2" -> print_endline (part2 input)
  | _ -> failwith "Part must be 1 or 2"

'



def init_day [day: int] {
    let d = (fmt-day $day)

    let sol = $"solutions/day($d).ml"
    let inp = $"inputs/day($d).txt"
    let build_placeholder = $"build/day($d)"
    let init_content = ($template | str replace "DAY" $d)
    if not ($sol | path exists) {
        $init_content | save $sol
    }
    if not ($inp | path exists) {
        if $session != "" {
            curl $"https://adventofcode.com/$year/day/$day/input" -H $"Cookie: session=$session" | save $inp
            print $"Fetched input for day $day"
        }
    }

    if not ($build_placeholder | path exists) {
        "" | save $build_placeholder
    }
}


export def main [] {
  let root = $env.PWD

  print $"Initializing Advent of Code Project…"
  print $"Project root: ($root)"
  print "Creating directories…"

  create_dirs $file_dirs

  # Only move init.nu if it exists in root AND not already in scripts/
  if ($root | path exists) and ($"($root)/init.nu" | path exists) {
      mv $"($root)/init.nu" $"($root)/scripts"
  }

  print "Creating files for days…"
  for day in (seq 1 $max_days) {
      init_day $day
  }

  print $"Done! Project fully initialized in: ($root)"
}

