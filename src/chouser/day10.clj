(ns chouser.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [chouser.day02 :refer [line-coll let-re]]))

(def example1
  [16 10 15 5 1 11 7 19 6 12 4])

(def example2
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11
   1 32 25 35 8 17 7 9 4 2 34 10 3])

(def input (into []
                 (map #(Long/parseLong %))
                 (line-coll "resources/day10-input.txt")))

(defn count-gaps [coll]
  (let [{big 3, small 1}
        (->> (concat [0] coll [(+ 3 (apply max coll))])
             sort
             (partition 2 1)
             (map (fn [[L H]] (- H L)))
             frequencies)]
    (* big small)))

(assert (= 220 (count-gaps example2)))

(count-gaps input)
;;=> 1998


(defn count-combos [pairs]
  (let [too-many (apply concat pairs)
        a (first too-many)
        z (last too-many)
        optionals (map first (next pairs))]
    (->> (combo/subsets optionals)
         (remove (fn [nums]
                   (->> (concat [a] nums [z])
                        (partition 2 1)
                        (some (fn [[x y]] (< 3 (- y x)))))))
         count)))

(defn count-all-combos [coll]
  (->> (concat [0] coll [(+ 3 (apply max coll))])
       sort
       (partition 2 1)
       (partition-by (fn [[a b]] (= 3 (- b a))))
       (filter (fn [[[a b]]] (< (- b a) 3)))
       (map count-combos)
       (reduce *)))

(assert (= 8 (count-all-combos example1)))

(assert (= 19208 (count-all-combos example2)))

(count-all-combos input)
;;=> 347250213298688

