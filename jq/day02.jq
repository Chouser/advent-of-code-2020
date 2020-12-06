#!/usr/bin/env -S jq --rawfile input ../resources/day02-input.txt -nMf

def count_matches:
  [. | scan("([0-9]+)-([0-9]+) (.): (.*)") as [$min, $max, $chr, $pw]
     | $pw | [scan($chr)] | length
     | select( ($min | tonumber) <= . and . <= ($max | tonumber) )
  ] | length ;

def count_matches2:
  [. | scan("([0-9]+)-([0-9]+) (.): (.*)") as [$a, $b, $chr, $pw]
     | select( ($pw[($a | tonumber - 1) : ($a | tonumber)] == $chr)
               != ($pw[($b | tonumber - 1) : ($b | tonumber)] == $chr) )
  ] | length ;

$input | count_matches | {"part 1 answer": .} | debug | empty ,
$input | count_matches2 | {"part 2 answer": .} | debug | empty