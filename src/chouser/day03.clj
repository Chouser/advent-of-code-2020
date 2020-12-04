(ns chouser.day03
  (:require [clojure.java.io :as io]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll]]))

;; Count trees on a slope down the mountain

(def example
  (-> "
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"
      (str/split #"\n")
      rest))

(defn count-hits-3x1 [lines]
  (->>
   lines
   (map-indexed (fn [line-num line]
                  (let [x (rem (* 3 line-num) (count line))]
                    (nth line x))))
   (filter #(= % \#))
   count))

(assert (= 7 (count-hits-3x1 example)))

(count-hits-3x1 (into [] (line-coll "resources/day03-input.txt")))
;;=> 294


;; Count trees on multiple slopes down the mountain

(defn count-hits [lines [dx dy]]
  (->>
   lines
   (map-indexed (fn [line-num line]
                  (when (zero? (rem line-num dy))
                    (let [x (rem (* (/ dx dy) line-num) (count line))]
                      (nth line x)))))
   (filter #(= % \#))
   count))

(assert (= 7 (count-hits example [3 1])))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn multiply-slopes [lines slopes]
  (reduce * (map (partial count-hits lines) slopes)))

(assert (= 336 (multiply-slopes example slopes)))

(multiply-slopes (into [] (line-coll "resources/day03-input.txt")) slopes)
;;=> 5774564250


;; Wait, that stinks -- all that work to get the lines progressively through a
;; CollReduce, and I poured them all into a vector before processing! I will try
;; again...

(defn count-coll
  "Like (count coll), but supports CollReduce collections instead of sequences."
  [coll]
  (transduce (map (constantly 1)) + 0 coll))

(defn count-hits [lines [dx dy]]
  (->>
   lines
   (eduction
    (comp
     (map-indexed (fn [line-num line]
                    (when (zero? (rem line-num dy))
                      (let [x (rem (* (/ dx dy) line-num) (count line))]
                        (nth line x)))))
     (filter #(= % \#))))
   count-coll))

(multiply-slopes (line-coll "resources/day03-input.txt") slopes)
;;=> 5774564250

;; ahh, much better.
