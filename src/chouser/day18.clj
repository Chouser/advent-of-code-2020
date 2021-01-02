(ns chouser.day18
  (:require [chouser.day14 :refer [line-coll]]))

(defn weval [expr]
  (if-not (list? expr)
    expr
    (reduce
     (fn [acc [op val]]
       ((if (= op '+) + *) acc (weval val)))
     (weval (first expr))
     (partition 2 (rest expr)))))

#_
(do
  (assert (= (weval '(2 * 3 + (4 * 5))) 26))
  (assert (= (weval '(5 + (8 * 3 + 9 + 3 * 4 * 3))) 437))
  (assert (= (weval '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))) 12240))
  (assert (= (weval '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)) 13632)))

#_
(transduce (map (comp weval read-string))
           +
           (line-coll "resources/day18-input.txt"))
;;=> 27724131 ;; too low!?

;; Ah, was only processing the first "expression" of each line

(transduce (comp
            (map #(str "(" % ")"))
            (map read-string)
            (map weval))
           +
           (line-coll "resources/day18-input.txt"))
;;=> 13976444272545

;; evaluate + before *

(defn werval [expr]
  (if-not (sequential? expr)
    expr
    (let [[a op b & more] expr]
      (cond
        (nil? op) (werval a)
        (= op '+) (werval (list* (+ (werval a) (werval b)) more))
        (= op '*) (* (werval a) (werval (list* b more)))))))

#_
(do
  (assert (= (werval '(1 + (2 * 3) + (4 * (5 + 6)))) 51))
  (assert (= (werval '(2 * 3 + (4 * 5))) 46))
  (assert (= (werval '(5 + (8 * 3 + 9 + 3 * 4 * 3))) 1445))
  (assert (= (werval '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))) 669060))
  (assert (= (werval '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)) 23340)))

#_
(transduce (comp
            (map #(str "(" % ")"))
            (map read-string)
            (map werval))
           +
           (line-coll "resources/day18-input.txt"))
;;=> 88500956630893
