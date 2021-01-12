#!/usr/bin/env -S jq -L . --rawfile input ../resources/day06-input.txt -nMf

def intersection:
  reduce .[] as $nextset ( .[0] ; 
    . as $iset
    | $nextset | reduce to_entries[] as $entry ( $nextset ;
      if $iset | has($entry.key) then . else del(.[$entry.key]) end
    )
  ) ;

def shared_letters:
  [scan(".+") | [{key: scan(".")}] | from_entries] | intersection ;

# Could use splits() instead of split()[] here, but this is much faster!
{
  "part 1": [$input | split("\n\n")[] | [scan(".")] | unique | length] | add,
  "part 2": [$input | split("\n\n")[] | shared_letters | length] | add
}
