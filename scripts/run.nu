# Usage: ./run_day.nu 1
# Run day 1
def format-day [day:int] {
  if $day < 10 { $"0($day)" } else { $"($day)" }
}
let year = $env.AOC_YEAR 
let root = $env.PWD
def get_mtime [file:string] {
    if ($file | path exists) {
            ( ls $file | get modified)
    } else {
      date now
    }
}

def run_day [day:int, part:int] {

    let day_str = format-day $day
    let src = $"solutions/day($day_str).ml"
    let exe = $"build/day($day_str)"
    let input_file = $"inputs/day($day_str).txt"

    if not ("build" | path exists) {
        mkdir build
    }

    if not ($input_file | path exists) {
        if $env.session != "" {
            curl $"https://adventofcode.com/($year)/day/($day)/input" -H $"Cookie: session=($env.session)" | save $input_file
            print $"Fetched input for day ($day)"
        } else {
            print $"Warning: input file ($input_file) missing and no session cookie set"
        }
    }

    let util_src = "lib/util.ml"
    let util_cmo = "lib/util.cmo"

    # Compile Util module if necessary
    if (not ( ($util_cmo) | path exists) or ((get_mtime $util_src) > (get_mtime $util_cmo))) {
        print $"Compiling dependency: ($util_src)..."
        do {
            cd lib
            ocamlc -c util.ml
        }
    }

    let src_mtime = (get_mtime $src)
    let exe_mtime = (get_mtime $exe)

    # Compile day's solution if necessary
    if (not ($exe | path exists) or ($src_mtime > $exe_mtime)) {
      print $"Compiling ($src)..."
      do {
        cd build
        ocamlc -I ../lib -o $"day($day_str)" ../lib/util.cmo $"../($src)"
      }
    }
    print $"Running day ($day)..."
    ./($exe) $part $input_file
}

def main [day:int, part:int] {
    run_day $day $part
}
