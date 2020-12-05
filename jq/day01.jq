#!/usr/bin/env -S jq --slurpfile input ../resources/day01-input.edn -nMf

# Find the two numbers in the input that sum to 2020 and return their product
def m2:
  [ (.[] | {a:.}) + (.[] | {b:.})
    | select(.a + .b == 2020)
    | .a * .b
  ][0];

# Now with a binary search for the partner, so should be O(n * log(n)), but is
# actually slower because some kind of implicit extra looping:
#def m2:
#  . | sort as $orig | [2020 - .[]] | .[0:(. | length / 2 )] as $inv | empty,
#  [$orig | bsearch($inv[]) | select (. >= 0)][0] | ($orig[.] * (2020 - $orig[.]));

# Now with an object acting as the set to lookup the partner in:
def m2:
  . as $orig | [.[] | {(. | tostring): 1}] | add as $set | empty ,
  [$orig[] as $x | select($set | .[2020 - $x | tostring] ) | ($x * (2020 - $x))][0];


# Find the three numbers in the input that sum to 2020 and return their product
def m3:
  . as $orig | [.[] | {(. | tostring): 1}] | add as $set | empty ,
  [ ($orig[] | {a:.}) + ($orig[] | {b:.})
    | . as $x
    | select($set | .[2020 - $x.a - $x.b | tostring] )
    | ($x.a * $x.b * (2020 - $x.a - $x.b))
  ][0];

[1721, 979, 366, 299, 675, 1456] | m2 | {"should be 514579": .} | debug | empty ,
$input | m2 | {"part 1 answer": .} | debug | empty ,
$input | m3 | {"part 2 answer": .} | debug | empty

