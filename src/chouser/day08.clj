(ns chouser.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Find the infinite loop

(def example
  (-> "
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
      (str/split #"\n")
      rest))

(defn parse-prog [lines]
  (mapv #(let-re [#"(?<op>nop|acc|jmp) (?<arg>[+-]\d+)" %]
           [(keyword op) (Long/parseLong arg)])
        lines))

(defn find-loop [prog]
  (loop [i 0, a 0, seen #{}]
    (let [[op arg] (prog i)]
      (if (seen i)
        a
        (let [seen (conj seen i)]
          (case op
            :nop (recur (inc i) a seen)
            :acc (recur (inc i) (+ a arg) seen)
            :jmp (recur (+ i arg) a seen)))))))

(assert (= 5 (find-loop (parse-prog example))))

(find-loop (parse-prog (line-coll "resources/day08-input.txt")))
;;=> 1563


;; Eliminate the infinite loop

(defn final-a [prog]
  (loop [i 0, a 0, seen #{}]
    (let [[op arg] (get prog i)]
      (cond
        (nil? op) a
        (seen i) nil
        :else (let [seen (conj seen i)]
                (case op
                  :nop (recur (inc i) a seen)
                  :acc (recur (inc i) (+ a arg) seen)
                  :jmp (recur (+ i arg) a seen)))))))

(defn fixed-a [prog]
  (some #(final-a (update-in prog [% 0] {:nop :jmp, :jmp :nop, :acc :acc}))
        (range (count prog))))

(assert (= 8 (fixed-a (parse-prog example))))

(fixed-a (parse-prog (line-coll "resources/day08-input.txt")))
;;=> 767
