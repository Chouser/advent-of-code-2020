(ns chouser.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [chouser.day02 :refer [line-coll let-re]]))

(def example
  (-> "
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
      (str/split #"\n")
      rest))

(def input (line-coll "resources/day11-input.txt"))

(defn load-grid [lines]
  (->>
   (for [[y line] (map-indexed vector lines)
         [x char] (map-indexed vector line)
         :when (= \L char)]
     [x y])
   (reduce (fn [grid coord] (assoc grid coord {:o? false :n 0})) {})))

(def neighbor-deltas
  (->> (combo/selections [-1 0 1] 2)
       (remove #(apply = 0 %))))

(defn set-seat [grid coord o?]
  (let [vald (if o? 1 -1)]
    (-> (reduce
         (fn [grid delta]
           (let [ncoord (mapv + coord delta)]
             (if-not (grid ncoord)
               grid
               (update-in grid [ncoord :n] + vald))))
         grid
         neighbor-deltas)
        (assoc-in [coord :o?] o?))))

(defn step [grid]
  (reduce (fn [new-grid [coord {:keys [o? n]}]]
            (cond
              (and (not o?) (zero? n)) (set-seat new-grid coord true)
              (and o? (<= 4 n)) (set-seat new-grid coord false)
              :else new-grid))
          grid
          grid))

(defn print-board [grid]
  (let [maxes (apply map max (keys grid))
        dots (reduce (fn [v x] (vec (repeat (inc x) v))) '. maxes)]
    (->>
     (reduce (fn [dots [coord {:keys [o?]}]]
               (assoc-in dots (reverse coord) (if o? (symbol "#") 'L)))
             dots grid)
     (map println)
     dorun)))

(defn count-final-occupieds [lines]
  (->> (load-grid (into [] lines))
       (iterate step)
       (partition 2 1)
       (drop-while (fn [[x y]] (not= x y)))
       ffirst
       (filter #(-> % val :o?))
       count))

(assert (= 37 (count-final-occupieds example)))

(count-final-occupieds input)
;;=> 2438


