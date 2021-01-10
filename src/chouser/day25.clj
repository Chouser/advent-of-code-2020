(ns chouser.day24)

;; multiplication loops

(def example [5764801 17807724])
(def input [11404017 13768789])

(defn find-loop-size [^long subject, ^long public-key]
  (loop [value 1, size 0]
    (if (= value public-key)
      size
      (recur (rem (* value subject) 20201227) (inc size)))))

(defn apply-loop [^long subject, ^long loop-size]
  (loop [value 1, size 0]
    (if (= size loop-size)
      value
      (recur (rem (* value subject) 20201227) (inc size)))))

(defn find-encryption-key [[a b]]
  (apply-loop b (find-loop-size 7 a)))

(assert (= 14897079 (find-encryption-key example)))

#_(time (find-encryption-key input))
;; "Elapsed time: 187.553452 msecs"
;;=> 18862163
