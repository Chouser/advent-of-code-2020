#!/usr/bin/env -S jq --rawfile input ../resources/day05-input.txt -nMf

def seat_ids:
  scan("\\S+") | reduce scan(".") as $c ( 0 ; . * 2 + ({F: 0, L: 0}[$c] // 1) ) ;

def missing_seat:
  [ sort | [ . , del(.[0]) ] | transpose[] | select(.[0] + 1 != .[1]) ]
  | .[0][0] + 1;

{
  "seat id 567": "BFFFBBFRRR" | seat_ids,
  "seat id 820": "BBFFBBFRLL" | seat_ids,
  "part 1 answer": [$input | seat_ids] | max,
  "part 2 answer": [$input | seat_ids] | missing_seat
}
