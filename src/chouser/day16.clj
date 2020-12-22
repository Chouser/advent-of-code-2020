(ns chouser.day16
  (:require [chouser.day14 :refer [let-re line-coll]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example
  (-> "
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12" (str/split #"\n") rest))

(defn rule-match [rules num]
  (some (fn [[rulename ranges]]
          (when (some (fn [[lo hi]] (<= lo num hi))
                      (partition 2 ranges))
            rulename))
        rules))

(defn count-invalid-tickets [lines]
  (reduce
   (fn [state line]
     (let-re [#"((?<rule>.+): (?<ranges>.*))|(?<tik>(\d+,?)+)" line]
       (cond
         rule (assoc-in state [:rules rule]
                        (map #(Long/parseLong %) (re-seq #"\d+" ranges)))
         tik (update state :invalid-sum +
                     (transduce
                      (comp
                       (map #(Long/parseLong %))
                       (map #(if (rule-match (:rules state) %)
                               0
                               %)))
                      +
                      (str/split tik #",")))
         :else state)))
   {:invalid-sum 0}
   lines))

(assert (= 71 (:invalid-sum (count-invalid-tickets example))))

(:invalid-sum (count-invalid-tickets (line-coll "resources/day16-input.txt")))
;;=> 25984


(defn parse-lines [lines]
  (reduce
   (fn [state line]
     (let-re [#"((?<rule>.+): (?<ranges>.*))|(?<tik>(\d+,?)+)" line]
       (cond
         rule (assoc-in state [:rules rule]
                        (map #(Long/parseLong %) (re-seq #"\d+" ranges)))
         tik (update state :tiks conj
                     (mapv #(Long/parseLong %) (str/split tik #",")))
         :else state)))
   {:tiks []}
   lines))

(defn rule-matches [rules num]
  (keep (fn [[rulename ranges]]
          (when (some (fn [[lo hi]] (<= lo num hi))
                      (partition 2 ranges))
            rulename))
        rules))

(defn rule-idxs [{:keys [rules tiks]}]
  (->> tiks
       (keep (fn [tik]
               (let [rule-sets (map #(set (rule-matches rules %)) tik)]
                 (when (every? seq rule-sets)
                   rule-sets))))
       (apply map set/intersection)
       (map-indexed vector)
       (sort-by #(count (second %)))
       (reduce
        (fn [rule-idx [idx rule-set]]
          (assoc rule-idx (first (remove rule-idx rule-set)) idx))
        {})))

(defn multiply-destinations [lines]
  (let [{:keys [rules tiks] :as parsed} (parse-lines lines)]
    (apply *
           (map (first tiks)
                (keep (fn [[rule idx]]
                        (when (re-find #"^departure" rule)
                          idx))
                      (rule-idxs parsed))))))

(def example2
  (-> "
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9" (str/split #"\n") rest))

(multiply-destinations (line-coll "resources/day16-input.txt"))
;;=> 1265347500049

