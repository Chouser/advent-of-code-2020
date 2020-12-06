(ns chouser.day04
  (:require [clojure.string :as str]
            [chouser.day02 :refer [line-coll]]))

;; Count the unique letters in each group

(def example (-> "
abc

a
b
c

ab
ac

a
a
a
a

b" (str/split #"\n") rest))

(defn sum-unique-count [lines]
  (transduce
   (comp
    (partition-by (partial = ""))
    (map #(count (into #{} (apply concat %)))))
   +
   lines))

(assert (= 11 (sum-unique-count example)))

(sum-unique-count (line-coll "resources/day06-input.txt"))


;; Count letters included in every line of each group

(defn sum-shared-count [lines]
  (transduce
   (comp
    (partition-by (partial = ""))
    (map (fn [lines]
           (->> (frequencies (apply concat lines))
                vals
                (filter #(= % (count lines)))
                count))))
   +
   lines))

(assert (= 6 (sum-shared-count example)))

(sum-shared-count (line-coll "resources/day06-input.txt"))
