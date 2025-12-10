#!/usr/bin/env nu

# ============================================================================
# CONFIGURATION
# ============================================================================

def get_config [] {
  {
    year: ($env.AOC_YEAR? | default "2024")
    session: ($env.AOC_SESSION? | default "")
    max_days: 25
    directories: [inputs solutions build lib ]
    languages: {
      ocaml: {
        ext: ".ml"
        needs_dir: false
        compile: true
        template: '(* AOC {{year}} Day {{day}} *)
let part1 filename = "Not implemented"
let part2 filename = "Not implemented"
let () =
  if Array.length Sys.argv < 3 then failwith "Usage: program <part> <file>";
  let part = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  match part with
  | "1" -> print_endline (part1 filename)
  | "2" -> print_endline (part2 filename)
  | _ -> failwith "Part must be 1 or 2"'
      }
      go: {
        ext: ".go"
        needs_dir: true
        compile: true
        template: '// AOC {{year}} Day {{day}}
package main
import ("fmt"; "os")
func part1(fn string) string { return "Not implemented" }
func part2(fn string) string { return "Not implemented" }
func main() {
  if len(os.Args) < 3 { fmt.Println("Usage: program <part> <file>"); os.Exit(1) }
  part, fn := os.Args[1], os.Args[2]
  switch part {
  case "1": fmt.Println(part1(fn))
  case "2": fmt.Println(part2(fn))
  default: fmt.Println("Part must be 1 or 2"); os.Exit(1)
  }
}'
      }
      python: {
        ext: ".py"
        needs_dir: false
        compile: false
        template: '# AOC {{year}} Day {{day}}
import sys
def part1(fn): return "Not implemented"
def part2(fn): return "Not implemented"
if __name__ == "__main__":
  if len(sys.argv) < 3: print("Usage: program <part> <file>"); sys.exit(1)
  part, fn = sys.argv[1], sys.argv[2]
  print(part1(fn) if part == "1" else part2(fn) if part == "2" else "Part must be 1 or 2")'
      }
      rust: {
        ext: ".rs"
        needs_dir: true
        compile: true
        template: '// AOC {{year}} Day {{day}}
use std::{env, process};
fn part1(_fn: &str) -> String { "Not implemented".to_string() }
fn part2(_fn: &str) -> String { "Not implemented".to_string() }
fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 3 { eprintln!("Usage: program <part> <file>"); process::exit(1); }
  match args[1].as_str() {
    "1" => println!("{}", part1(&args[2])),
    "2" => println!("{}", part2(&args[2])),
    _ => { eprintln!("Part must be 1 or 2"); process::exit(1); }
  }
}'
      }
      javascript: {
        ext: ".js"
        needs_dir: false
        compile: false
        template: '// AOC {{year}} Day {{day}}
const fs = require("fs");
const part1 = fn => "Not implemented";
const part2 = fn => "Not implemented";
if (require.main === module) {
  if (process.argv.length < 4) { console.log("Usage: node program.js <part> <file>"); process.exit(1); }
  const [part, fn] = [process.argv[2], process.argv[3]];
  console.log(part === "1" ? part1(fn) : part === "2" ? part2(fn) : "Part must be 1 or 2");
}'
      }
    }
  }
}

# ============================================================================
# UTILITIES
# ============================================================================

def fmt-day [day: int] {
  if $day < 10 { $"0($day)" } else { $"($day)" }
}

def substitute [content: string, vars: record] {
  mut result = $content
  for key in ($vars | columns) {
    $result = ($result | str replace --all $"{{($key)}}" ($vars | get $key))
  }
  $result
}

def get_mtime [file: string] {
  if ($file | path exists) { (ls $file | get modified | first) } else { date now }
}

# ============================================================================
# INIT COMMANDS
# ============================================================================

def init_day [day: int, lang: string, config: record] {
  let d = (fmt-day $day)
  let lang_cfg = ($config.languages | get $lang)
  
  let sol = if $lang_cfg.needs_dir {
    let dir = $"solutions/day($d)"
    if not ($dir | path exists) { mkdir $dir }
    if $lang == "go" { $"($dir)/main($lang_cfg.ext)" } else { $"($dir)/day($d)($lang_cfg.ext)" }
  } else {
    $"solutions/day($d)($lang_cfg.ext)"
  }
  
  if not ($sol | path exists) {
    (substitute $lang_cfg.template {year: $config.year, day: $d}) | save $sol
  }
  
  let inp = $"inputs/day($d).txt"
  if not ($inp | path exists) {
    if $config.session != "" {
      try {
        http get --headers [Cookie $"session=($config.session)"] $"https://adventofcode.com/($config.year)/day/($day)/input" | save $inp
      } catch {
        "" | save $inp
      }
    } else {
      "" | save $inp
    }
  }
}

export def "main init" [lang: string, --days: int = 0] {
  let config = (get_config)
  if not ($lang in ($config.languages | columns)) {
    error make {msg: $"Unknown language: ($lang). Available: (($config.languages | columns | str join ', '))"}
  }
  
  print $"AOC ($config.year) - ($lang)"
  for dir in $config.directories { if not ($dir | path exists) { mkdir $dir } }
  
  let max = if $days > 0 { $days } else { $config.max_days }
  for day in 1..$max { init_day $day $lang $config }
  print $"Initialized ($max) days"
}

export def "main add" [day: int, lang: string] {
  let config = (get_config)
  if not ($lang in ($config.languages | columns)) {
    error make {msg: $"Unknown language: ($lang)"}
  }
  init_day $day $lang $config
  print $"Added day ($day)"
}

export def "main fetch" [day: int] {
  let config = (get_config)
  if $config.session == "" { error make {msg: "AOC_SESSION not set"} }
  let d = (fmt-day $day)
  try {
    http get --headers [Cookie $"session=($config.session)"] $"https://adventofcode.com/($config.year)/day/($day)/input" | save --force $"inputs/($d).txt"
    print $"✓ Fetched day ($day)"
  } catch {
    error make {msg: $"Failed to fetch day ($day)"}
  }
}

# ============================================================================
# RUN COMMANDS
# ============================================================================

def run_ocaml [day: int, part: int, config: record] {
  let d = (fmt-day $day)
  let src = $"solutions/day($d).ml"
  let exe = $"build/day($d)"
  let inp = $"inputs/day($d).txt"
  let util = "lib/util.ml"
  let package = "str"

  # We recompile if the executable doesn't exist, or if either of the
  # source files (.ml) are newer than the executable.
  let exe_mtime = (get_mtime $exe)
  if (not ($exe | path exists)) or ((get_mtime $src) > $exe_mtime) or ((get_mtime $util) > $exe_mtime) {
    if ( ($util | path exists)) {
      ocamlfind ocamlc -I lib -package $package -linkpkg -o $exe $util $src
    } else {
      ocamlc -o $exe $src
    }
  }

  ^$"./($exe)" $part $inp
}

def run_go [day: int, part: int, config: record] {
  let d = (fmt-day $day)
  let src_dir = $"solutions/day($d)"
  let exe = $"build/day($d)"
  let inp = $"inputs/day($d).txt"
  
  if (not ($exe | path exists)) or ((get_mtime $"($src_dir)/main.go") > (get_mtime $exe)) {
    cd $src_dir; go build -o $"../../($exe)" main.go; cd ../..
  }
  
  ^$"./($exe)" $part $inp
}

def run_python [day: int, part: int, config: record] {
  let d = (fmt-day $day)
  python $"solutions/day($d).py" $part $"inputs/day($d).txt"
}

def run_rust [day: int, part: int, config: record] {
  let d = (fmt-day $day)
  let src_dir = $"solutions/day($d)"
  let exe = $"build/day($d)"
  let inp = $"inputs/day($d).txt"
  
  if (not ($exe | path exists)) or ((get_mtime $"($src_dir)/day($d).rs") > (get_mtime $exe)) {
    rustc $"($src_dir)/day($d).rs" -o $exe
  }
  
  ^$"./($exe)" $part $inp
}

def run_javascript [day: int, part: int, config: record] {
  let d = (fmt-day $day)
  node $"solutions/day($d).js" $part $"inputs/day($d).txt"
}

export def "main run" [day: int, part: int, --lang: string = ""] {
  let config = (get_config)
  let d = (fmt-day $day)
  
  if not ("build" | path exists) { mkdir build }
  
  let detected = if $lang == "" {
    let found = ($config.languages | columns | where {|l|
      let cfg = ($config.languages | get $l)
      if $cfg.needs_dir {
        ($"solutions/day($d)" | path exists)
      } else {
        ($"solutions/day($d)($cfg.ext)" | path exists)
      }
    })
    if ($found | length) == 0 { error make {msg: "No solution found"} }
    if ($found | length) > 1 { error make {msg: "Multiple solutions found, use --lang"} }
    $found | first
  } else { $lang }
  
  print $" Day ($day) Part ($part) [($detected)]"
  
  match $detected {
    "ocaml" => { run_ocaml $day $part $config }
    "go" => { run_go $day $part $config }
    "python" => { run_python $day $part $config }
    "rust" => { run_rust $day $part $config }
    "javascript" => { run_javascript $day $part $config }
  }
}

export def "main all" [day: int, --lang: string = ""] {
  print "=== Part 1 ==="; main run $day 1 --lang $lang
  print ""; print "=== Part 2 ==="; main run $day 2 --lang $lang
}

export def "main time" [day: int, part: int, --lang: string = ""] {
  let start = (date now)
  main run $day $part --lang $lang
  print $"⏱️  ((date now) - $start)"
}

# ============================================================================
# INFO COMMANDS
# ============================================================================

export def "main langs" [] {
  let config = (get_config)
  print "Available languages:"
  for lang in ($config.languages | columns) {
    print $"  • ($lang)"
  }
}

export def "main config" [] {
  let config = (get_config)
  print $"Year: ($config.year)"
  print $"Session: (if $config.session != '' { 'Set' } else { 'Not set' })"
  print $"Languages: (($config.languages | columns | length))"
}

# ============================================================================
# HELP
# ============================================================================

export def main [] {
  print "Advent of Code Manager

USAGE:
  aoc init <lang> [--days N]    Initialize project
  aoc add <day> <lang>          Add a new day
  aoc fetch <day>               Fetch input for day
  aoc run <day> <part> [--lang] Run solution
  aoc all <day> [--lang]        Run both parts
  aoc time <day> <part> [--lang] Run with timing
  aoc langs                     List languages
  aoc config                    Show configuration

EXAMPLES:
  aoc init python --days 5      Init Python with 5 days
  aoc run 1 1                   Run day 1 part 1 (auto-detect)
  aoc all 3 --lang go           Run both parts of day 3 in Go
  aoc fetch 10                  Fetch input for day 10

SETUP:
  export AOC_YEAR=2024
  export AOC_SESSION=your_cookie
"
}
