#!/usr/bin/env -S jq --rawfile input ../resources/day05-input.txt -nMf

def seat_id:
  reduce scan(".") as $c ( 0 ; . * 2 + (({"F": 0, "L": 0})[$c] // 1) ) ;

{
  "seat id 567": "BFFFBBFRRR" | seat_id,
  "seat id 820": "BBFFBBFRLL" | seat_id,
  "part 1 answer": [$input | scan(".*") | seat_id] | max
}
