(ns chouser.day13
  (:require [clojure.string :as str]))

(def example "939
7,13,x,x,59,x,31,19")

(defn parse-str [s]
  (let [[start bus-ids] (str/split s #"\n")]
    {:start (Long/parseLong start)
     :bus-ids (read-string (str "[" bus-ids "]"))}))

(defn soonest-bus [{:keys [start bus-ids]}]
  (->> bus-ids
       (keep #(when (number? %)
                {:bus-id %
                 :depart-delay (- % (rem start %))}))
       (apply max-key #(- (:depart-delay %)))))

(assert (= 295 (apply * (vals (soonest-bus (parse-str example))))))

(apply * (vals (soonest-bus (parse-str (slurp "resources/day13-input.txt")))))
;;=> 2545


(defn prime-factors [^long n]
  (loop [primes {}, p 2, n n]
    (cond
      (= n 1) primes
      (zero? (rem n p)) (recur (update primes p (fnil inc 0)) p (quot n p))
      :else (recur primes (inc p) n))))

;; ..turns out all the bus ids are primes :rolling_eyes_face:

#_
(defn earliest-cascade [id-seq]
  (let [start (System/currentTimeMillis)
        ids (->> id-seq
                 (map-indexed (fn [i id] (when (number? id) [id i])))
                 (into {}))
        product (apply * (keys ids))
        max-id (long (apply max (keys ids)))]
    (loop [i (long (- max-id (ids max-id))), last-time (long start)]
      (if (every? (fn [[id j]]
                    (zero? (rem (+ i (long j)) (long id))))
                  ids)
        i
        (recur (+ i max-id)
               (let [now (System/currentTimeMillis)]
                 (if (< (- now last-time) 30000)
                   last-time
                   (do (prn :sofar (quot (- now start) 1000)
                            :i i
                            :percent (* 100 (/ i product)))
                       now))))))))

#_
(prn
 [(earliest-cascade '[17 x 13 19]) ;;= 3417
  (earliest-cascade '[67,7,59,61]) ;;=> 754018
  (earliest-cascade '[67,x,7,59,61]) ;;=> 779210
  (earliest-cascade '[67,7,x,59,61]) ;;=> 1261476
  (earliest-cascade '[1789,37,47,1889])]) ;;=> 1202161486

;;[17 1 13 19]

;; That's too slow, of course, for the real input
;; After much struggle and discussion with my son, we came up with this:
(defn earliest-cascade [id-seq]
  (let [max (apply * (filter number? id-seq))
        ids (map-indexed (fn [i id] [i (if (number? id) id 1)])
                         id-seq)]
    (loop [pos 0, step (second (first ids)), [[idx id] :as ids] (rest ids)]
      #_(prn :pos pos :step step :idx idx :id id)
      (cond
        (nil? idx) pos

        (zero? (rem (+ pos idx) id))
        (recur pos
               (* step id)
               (rest ids))

        (< max pos) :oops

        :else
        (recur (+ pos step)
               step
               ids)))))

;; golf it a bit:
(defn earliest-cascade [id-seq]
  (loop [pos 0, step 1, idx 0, [id :as ids] id-seq]
    (cond
      (nil? id) pos
      (= 'x id) (recur pos step (+ 1 idx) (rest ids))
      (zero? (rem (+ pos idx) id))
      (recur pos (* step id) (+ 1 idx) (rest ids))
      :else (recur (+ pos step) step idx ids))))

(time (earliest-cascade '[19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,643,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,23,x,x,x,x,x,x,x,509,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29]))

;;"Elapsed time: 0.650425 msecs"
;;=> 266204454441577

#_
(earliest-cascade '[12,11,10,9,8,7,6,5,4,3,2,1])

