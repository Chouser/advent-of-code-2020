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
               (assoc-in dots (reverse coord) (if o? "#" "L")))
             dots grid)
     (map #(println (apply str %)))
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

#_
(count-final-occupieds input)
;;=> 2438


;; Follow sightlines

(defn first-seat [grid coord delta]
  (let [maxes (apply map max (keys grid))]
    (loop [coord coord]
      (let [coord (mapv + coord delta)]
        (cond
          (grid coord) coord
          (some #(< % 0) coord) nil
          (some true? (map < maxes coord)) nil
          :else (recur coord))))))

(defn add-grid-neighbors [grid]
  (reduce (fn [grid coord]
            (assoc-in grid [coord :neighbors]
                      (keep #(first-seat grid coord %) neighbor-deltas)))
          grid
          (keys grid)))

(defn set-seat2 [grid coord o?]
  (let [vald (if o? 1 -1)]
    (-> (reduce
         (fn [grid ncoord]
           (update-in grid [ncoord :n] + vald))
         grid
         (:neighbors (grid coord)))
        (assoc-in [coord :o?] o?))))

(defn step2 [grid]
  (reduce (fn [new-grid [coord {:keys [o? n]}]]
            (cond
              (and (not o?) (zero? n)) (set-seat2 new-grid coord true)
              (and o? (<= 5 n)) (set-seat2 new-grid coord false)
              :else new-grid))
          grid
          grid))

;; step2 above is *very slow*. Nearly a minute for the first step, then up to
;; 100 msecs for each of the next few. Is it because filling each seat requires
;; updating so many neighbors in an immutable map?


(defn set-seat2 [grid! coord o?]
  (let [vald (if o? 1 -1)]
    (-> (reduce
         (fn [grid! ncoord]
           (assoc! grid! ncoord (update (grid! ncoord) :n + vald)))
         grid!
         (:neighbors (grid! coord)))
        (assoc! coord (assoc (grid! coord) :o? o?)))))

(defn step2 [grid]
  (persistent!
   (reduce (fn [new-grid! [coord {:keys [o? n]}]]
             (cond
               (and (not o?) (zero? n)) (set-seat2 new-grid! coord true)
               (and o? (<= 5 n)) (set-seat2 new-grid! coord false)
               :else new-grid!))
           (transient grid)
           grid)))

;; First step isn't improved at all, but subsequent steps are perhaps 40%
;; faster. What's going on during that first step?

(defn count-final-occupieds2 [lines]
  (->> (load-grid (into [] lines))
       (add-grid-neighbors)
       (iterate step2)
       (partition 2 1)
       (drop-while (fn [[x y]] (not= x y)))
       ffirst
       (filter #(-> % val :o?))
       count))

(assert (= 26 (count-final-occupieds2 example)))

(defn first-step [lines]
  (->> (load-grid (into [] lines))
       (add-grid-neighbors)
       step2))

(defonce input-grid-s1 (time (first-step input)))

(defn cfo2-input []
  (->> input-grid-s1
       (iterate (fn [grid]
                  (prn :count (count (filter #(-> % val :o?) grid)) :hash (hash grid))
                  (time (step2 grid))))

       (partition 2 1)
       (drop-while (fn [[x y]] (not= x y)))
       ffirst
       (filter #(-> % val :o?))
       count

       ))

(cfo2-input)
;;=> 2174
