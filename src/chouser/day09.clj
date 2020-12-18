(ns chouser.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Sliding window

(def example
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn find-non-sum [preamble-length coll]
  (reduce (fn [{:keys [s q]} n]
            (if (not (some #(s (Math/abs (- % n))) q))
              (reduced n)
              {:s (-> s (disj (peek q)) (conj n))
               :q (-> q pop (conj n))}))
          {:s (set (take preamble-length coll))
           :q (into (clojure.lang.PersistentQueue/EMPTY)
                    (take preamble-length coll))}
          (drop preamble-length coll)))

(assert (= 127 (find-non-sum 5 example)))

(def input (into []
                 (map #(Long/parseLong %))
                 (line-coll "resources/day09-input.txt")))

(find-non-sum 25 input)
;;=> 15353384


;; Find contiguous sum

(defn contiguous-sum [target coll]
  (loop [i 0, j 1, s (+ (first coll) (second coll))]
    (cond
      (= s target) (let [r (subvec coll i (inc j))]
                     (+ (apply min r) (apply max r)))
      (< s target) (recur i (inc j) (+ s (coll (inc j))))
      (< target s) (recur (inc i) j (- s (coll i))))))

(assert (= 62 (contiguous-sum 127 example)))

(contiguous-sum (find-non-sum 25 input) input)
;;=> 2466556
