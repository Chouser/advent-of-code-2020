#!/usr/bin/env -S jq --rawfile input ../resources/day08-input.txt -nMf

def parse_prog:
  [scan("(\\w+) ([+-]\\d+)") | {op:.[0], arg:.[1] | tonumber}] ;

def prog_step($prog):
  .seen[.i | tostring] |= true
  | if   $prog[.i].op == "nop" then .i += 1
    elif $prog[.i].op == "acc" then .a += $prog[.i].arg | .i += 1
    elif $prog[.i].op == "jmp" then .i += $prog[.i].arg
    else .done |= true
    end ;

def run($prog):
  {i: 0, a: 0, seen: {}} | until( .seen[.i | tostring] ; prog_step($prog) ) ;

def fixed($prog):
  range($prog | length) as $i
  | select($prog[$i].op != "acc")
  | $prog | .[$i].op |= {nop: "jmp", jmp: "nop", acc: "acc"}[.]
  | run(.) | select(.done) ;

{
  "part 1": run($input | parse_prog).a,
  "part 2": fixed($input | parse_prog).a
}
