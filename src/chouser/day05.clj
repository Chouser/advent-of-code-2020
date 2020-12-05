(ns chouser.day05
  (:require [clojure.java.io :as io]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll]]))

;; parse binary space partition code

(defn seat-id [bsp]
  (-> bsp
      (str/replace #"." #(if (#{"F" "L"} %) "0" "1"))
      (Long/parseLong 2)))

(defn seat-id [bsp]
  (-> (apply str (map #(if (#{\F \L} %) \0 \1) bsp))
      (Long/parseLong 2)))

(defn seat-id [bsp]
  (-> (apply str (map #({\F 0 \L 0} % 1) bsp))
      (Long/parseLong 2)))

(defn seat-id [bsp]
  (->> bsp
       (map #({\F 0 \L 0} % 1))
       (reduce (fn [n d] (+ (* n 2) d)))))

(defn seat-id [bsp]
  (reduce (fn [n d] (+ (* n 2) ({\F 0 \L 0} d 1))) 0 bsp))

(assert (= 567 (seat-id "BFFFBBFRRR"))) ;; row 70, column 7
(assert (= 119 (seat-id "FFFBBBFRRR"))) ;; row 14, column 7
(assert (= 820 (seat-id "BBFFBBFRLL"))) ;; row 102, column 4

(transduce (map seat-id) max 0 (line-coll "resources/day05-input.txt"))
;;=> 980


;; find missing seat

(->> (line-coll "resources/day05-input.txt")
     (into (sorted-set) (map seat-id))
     (partition 2 1)
     (some (fn [[a b]]
             (when (< (inc a) b)
               (inc a)))))
;;=> 607

