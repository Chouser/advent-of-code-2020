(ns chouser.day12
  (:require [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Turtle

(def example (-> "F10 N3 F7 R90 F11" (str/split #" ")))

(defn parse-line [line]
  (let-re [#"(?<op>[NSEWLRF])(?<arg>\d+)" line]
    [(symbol op) (Long/parseLong arg)]))

(def loc-delta '{N [0 1] S [0 -1] E [1 0] W [-1 0]})

(def clockwise-seq '[N E S W])
(def turn-maps  ;; HAS A BUG
  {'L (into {} (map vector clockwise-seq (next (cycle (reverse clockwise-seq)))))
   'R (into {} (map vector clockwise-seq (next (cycle clockwise-seq))))})

(def init-state {:heading 'E, :loc [0 0]})

(defn add-delta-factor [coord factor delta]
  (mapv (fn [n delta] (+ n (* delta factor)))
        coord delta))

(defn step [state [op arg]]
  (case op
    (N S E W) (update state :loc add-delta-factor arg (loc-delta op))
    F         (update state :loc add-delta-factor arg (loc-delta (:heading state)))
    (L R)     (update state :heading
                      #(nth (iterate (turn-maps op) %) (quot arg 90)))))

(defn mn-dist-of-travel [lines]
  (let [final (transduce (map parse-line) (completing step) init-state lines)]
    (transduce (map #(Math/abs %)) + (:loc final))))

(assert (= 25 (mn-dist-of-travel example)))

(mn-dist-of-travel (line-coll "resources/day12-input.txt"))
;;=> 696 is incorrect

(def turn-maps
  {'L (into {} (map vector (reverse clockwise-seq)
                    (next (cycle (reverse clockwise-seq)))))
   'R (into {} (map vector clockwise-seq (next (cycle clockwise-seq))))})

(mn-dist-of-travel (line-coll "resources/day12-input.txt"))
;;=> 1010


;; Relative waypoint navigation

(def init-state2 {:waypoint [10 1], :loc [0 0]})

;; huh. I bet I could use matrix multiplication for at least rotation, and maybe
;; more. Oh well.
(def rotate
  {'L (fn [[x y]] [(- y) x])
   'R (fn [[x y]] [y (- x)])})

(defn step2 [state [op arg]]
  (case op
    (N S E W) (update state :waypoint add-delta-factor arg (loc-delta op))
    F         (update state :loc add-delta-factor arg (:waypoint state))
    (L R)     (update state :waypoint
                      #(nth (iterate (rotate op) %) (quot arg 90)))))

(defn mn-dist-of-travel2 [lines]
  (let [final (transduce (map parse-line) (completing step2) init-state2 lines)]
    (transduce (map #(Math/abs %)) + (:loc final))))

(assert (= 286 (mn-dist-of-travel2 example)))

(mn-dist-of-travel2 (line-coll "resources/day12-input.txt"))
;;=> 52742

