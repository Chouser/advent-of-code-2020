#!/usr/bin/env -S jq --rawfile input ../resources/day04-input.txt -nMf

def partition_after(pred):
  . as $array
  | "done" as $done
  | $array + [$done] | foreach .[] as $item (
    {extract: "empty", state: []} ;
    if $item == $done
      then {extract: .state}
      else .state += [$item]
        | if ($item | pred)
          then {extract: .state, state: []}
          else .extract |= "empty"
        end
    end ;
    if .extract == "empty" then empty else .extract end ) ;

def parse_maps:
  [scan("\\b(\\w\\w\\w):(\\S+)\\b|\n\n")]
  | partition_after( .[0] == null )
  | [ .[]
      | select( .[0] != null ) as [$k, $v]
      | {key: $k, value: $v} ]
  | from_entries ;

# Hm, not as clean a piece of code as I'd hoped. Trying again...

def parse_maps:
  [scan("\\b(\\w\\w\\w):(\\S+)\\b|\n\n")]
  | reduce .[] as [$k, $v]
    ( [{}];
      if $k == null
      then . + [{}]
      else .[. | length - 1][$k] |= $v
      end )
  | .[] ;

def has_required_fields:
  .byr and .iyr and .eyr and .hgt and .hcl and .ecl and .pid ;

def is_valid:
  try ((.byr | tonumber as $n | (1920 <= $n and $n <= 2002)) and
       (.iyr | tonumber as $n | (2010 <= $n and $n <= 2020)) and
       (.eyr | tonumber as $n | (2020 <= $n and $n <= 2030)) and
       (.hgt | scan("(\\d+)(..)") as [$n, $u]
         | $n | tonumber as $n
         | if $u == "cm"
           then 150 <= $n and $n <= 193
           else 59 <= $n and $n <= 76
           end) and
       (.hcl | test("^#[0-9a-f]{6}$")) and
       (.pid | test("^[0-9]{9}$")) and
       (.ecl | test("^(amb|blu|brn|gry|grn|hzl|oth)$")))
   catch false ;

{
  "part 1 answer": [$input | parse_maps | select(has_required_fields)] | length,
  "part 2 answer": [$input | parse_maps | select(is_valid)] | length
}

