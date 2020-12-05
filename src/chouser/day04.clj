(ns chouser.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Count the maps without missing fields

(def example
  (-> "
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
      (str/split #"\n")
      rest))

(defn parse-maps [lines]
  (reduce
   (fn [maps line]
     (if (empty? line)
       (conj maps {})
       (->> (re-seq #"\b(\w\w\w):(\S+)\b" line)
            (map (comp vec rest))
            (into (peek maps))
            (conj (pop maps)))))
   [{}]
   lines))

(def required-fields
  (set (map str '[byr iyr eyr hgt hcl ecl pid])))

(defn count-valid [lines]
  (->> (parse-maps lines)
       (filter (fn [m] (every? m required-fields)))
       count))

(assert (= 2 (count-valid example)))

(count-valid (line-coll "resources/day04-input.txt"))
;;=> 242


;; Count the maps with all valid field values

(defn valid? [m]
  (try
    (and (<= 1920 (Long/parseLong (m "byr")) 2002)
         (<= 2010 (Long/parseLong (m "iyr")) 2020)
         (<= 2020 (Long/parseLong (m "eyr")) 2030)
         (let-re [#"(?<n>\d+)(?<u>..)" (m "hgt")
                  n (Long/parseLong n)]
           (case u
             "cm" (<= 150 n 193)
             "in" (<= 59 n 76)))
         (re-matches #"#[0-9a-f]{6}" (m "hcl"))
         ('#{amb blu brn gry grn hzl oth} (symbol (m "ecl")))
         (re-matches #"[0-9]{9}" (m "pid")))
    (catch Exception e false)))

(defn count-valid2 [lines]
  (->> (parse-maps lines) (filter valid?) count))

(def invalids
  (-> "
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"
      (str/split #"\n")
      rest))

(assert (zero? (count-valid2 invalids)))

(count-valid2 (line-coll "resources/day04-input.txt"))
;;=> 186
