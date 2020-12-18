(ns chouser.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Conway's game of life in 3d

(def example
  (-> "
.#.
..#
###"
      (str/split #"\n")
      rest))

(def neighbor-deltas
  (let [deltas [-1 0 1]]
    (vec
     (for [x deltas, y deltas, z deltas
           :when (not= 0 x y z)]
       [x y z]))))

(defn set-bit [grid coord active?]
  (let [vald (if active? +1 -1)]
    (-> (reduce
         (fn [grid delta]
           (update-in grid [(mapv + coord delta) :ncount] (fnil + 0) vald))
         grid
         neighbor-deltas)
        (assoc-in [coord :active?] active?))))

(defn load-grid [lines]
  (->>
   (for [[y line] (map-indexed vector lines)
         [x char] (map-indexed vector line)
         :when (= \# char)]
     [x y 0])
   (reduce (fn [grid coord] (set-bit grid coord true)) {})))

(defn life-step [grid]
  (reduce (fn [new-grid [coord {:keys [ncount active?]}]]
            (cond
              (and active? (not (<= 2 ncount 3))) (set-bit new-grid coord false)
              (and (not active?) (= ncount 3)) (set-bit new-grid coord true)
              :else new-grid))
          grid
          grid))

(defn min-max [& args]
  [(apply min args) (apply max args)])

(defn print-board [grid]
  (let [[xr yr zr] (apply map min-max (keys grid))]
    (doseq [z (apply range zr)]
      (println "z=" z)
      (doseq [y (apply range yr)]
        (->> (apply range xr)
             (map #(if (:active? (grid [% y z])) \# \.))
             (apply str)
             println)))))

(defn count-active [grid]
  (count (filter #(-> % val :active?) grid)))

(assert (= 112 (count-active (nth (iterate life-step (load-grid example)) 6))))

(count-active (nth (iterate life-step
                            (load-grid
                             (into [] (line-coll "resources/day17-input.txt")))) 6))
;;=> 336

;; Conway's game of life in 4d

(def neighbors-3d
  (let [deltas [-1 0 1]]
    (vec
     (for [x deltas, y deltas, z deltas
           :when (not= 0 x y z)]
       [x y z 0]))))

(def neighbors-4d
  (let [deltas [-1 0 1]]
    (vec
     (for [x deltas, y deltas, z deltas, w deltas
           :when (not= 0 x y z w)]
       [x y z w]))))

(defn set-bit [grid coord active?]
  (let [vald (if active? +1 -1)]
    (-> (reduce
         (fn [grid delta]
           (update-in grid [(mapv + coord delta) :ncount] (fnil + 0) vald))
         grid
         (-> grid meta :neighbors))
        (assoc-in [coord :active?] active?))))

(defn load-grid [neighbors lines]
  (->>
   (for [[y line] (map-indexed vector lines)
         [x char] (map-indexed vector line)
         :when (= \# char)]
     [x y 0 0])
   (reduce (fn [grid coord] (set-bit grid coord true))
           (with-meta {} {:neighbors neighbors}))))

(defn print-board [grid]
  (let [[xr yr zr wr] (apply map min-max (keys grid))]
    (doseq [w (or (next (apply range wr)) [0])
            z (next (apply range zr))]
      (println "z=" z ", w=" w)
      (doseq [y (next (apply range yr))]
        (->> (next (apply range xr))
             (map #(if (:active? (grid [% y z w])) \# \.))
             (apply str)
             println)))))

#_(print-board (life-step (load-grid neighbors-3d example)))
#_(print-board (load-grid neighbors-4d example))

(assert (= 112 (count-active (nth (iterate life-step (load-grid neighbors-3d example)) 6))))


(assert (= 848 (count-active (nth (iterate life-step
                                           (load-grid neighbors-4d example))
                                  6))))

(count-active (nth (iterate life-step
                            (load-grid
                             neighbors-4d
                             (into [] (line-coll "resources/day17-input.txt")))) 6))
;;=> 2620
