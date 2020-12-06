#!/usr/bin/env -S jq --rawfile input ../resources/day03-input.txt -nMf

def count_on_slope($d):
  [
   [. | scan(".+")]                           # get input lines as array
   | [., [range(. | length)]] | transpose     # array of [line, row-number] pairs
   | .[] as [$line, $y]                       # for each line, bind a pair of vars
   | $d as [$dx,$dy]                          # destructure the argument pair
   | fmod( $dx * $y; $line | length ) as $x   # calculate x for this line
   | $line[$x:$x + 1]                         # lookup x in this line
   | select( .=="#" and 0==fmod($y; $dy) )    # keep if tree and y-skip is correct
  ] | length ;

# Why don't I have to divide $dx by $dy before multiplying by $y, like I did in
# my Clojure solution??

$input | count_on_slope([3,1]) | {"part 1 answer": .} | debug | empty,

reduce ([[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]] as $d
        | $input | count_on_slope($d[]))
 as $hits (1; . * $hits)
 | {"part 2 answer": .} | debug | empty

