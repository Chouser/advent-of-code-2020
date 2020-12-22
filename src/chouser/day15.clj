(ns chouser.day15)

(def input [2,20,0,4,1,17])

(defn step [{:keys [turn heard last-heard] :as state}]
  {:turn (inc turn)
   :heard (assoc heard last-heard turn)
   :last-heard (if-let [old-turn (heard last-heard)]
                 (- turn old-turn)
                 0)})

(defn heard-2020 [nums]
  (->> {:turn (count nums)
        :heard (zipmap (drop-last nums) (range 1 Long/MAX_VALUE))
        :last-heard (last nums)}
       (iterate step)
       (some #(when (= 2020 (:turn %)) (:last-heard %)))))

(assert (= 436 (heard-2020 [0 3 6])))

(assert (= 1    (heard-2020 [1,3,2])))
(assert (= 10   (heard-2020 [2,1,3])))
(assert (= 27   (heard-2020 [1,2,3])))
(assert (= 78   (heard-2020 [2,3,1])))
(assert (= 438  (heard-2020 [3,2,1])))
(assert (= 1836 (heard-2020 [3,1,2])))

(heard-2020 [2,20,0,4,1,17])
;;=> 758


;; part 2, the 30 millionth

#_
(defn heard-at [n nums]
  (->> {:turn (count nums)
        :heard (zipmap (drop-last nums) (range 1 Long/MAX_VALUE))
        :last-heard (last nums)}
       (iterate step)
       (some #(when (= n (:turn %)) (:last-heard %)))))

;; 20 seconds -- can we faster?

#_
(defn heard-at [n nums]
  (loop [turn (count nums)
         heard (zipmap (drop-last nums) (range 1 Long/MAX_VALUE))
         last-heard (long (last nums))]
    (if (= turn n)
      last-heard
      (recur
       (inc turn)
       (assoc heard last-heard turn)
       (if-let [old-turn (heard last-heard)]
         (- turn old-turn)
         0)))))

;; 15 seconds.  Maybe if our cache were faster?

(defn heard-at [n nums]
  (loop [last-heard (long (last nums))
         turn (count nums)
         heard (transient (zipmap (drop-last nums) (range 1 Long/MAX_VALUE)))]
    (if (= turn n)
      last-heard
      (recur
       (if-let [old-turn (heard last-heard)]
         (- turn old-turn)
         0)
       (inc turn)
       (assoc! heard last-heard turn)))))

;; 12 seconds. Getting better...

(defn heard-at [n nums]
   (loop [last-heard (long (last nums))
          turn (count nums)
          heard (java.util.HashMap.
                 (zipmap (drop-last nums) (range 1 Long/MAX_VALUE)))]
     (if (= turn n)
       last-heard
       (recur
        (if-let [old-turn (.get heard last-heard)]
          (- turn old-turn)
          0)
        (inc turn)
        (doto heard (.put last-heard turn))))))

;; 6 seconds. Good enough!

#_
(do
  (assert (= 175594  (time (heard-at 30000000 [0 3 6]))))
  (assert (= 2578    (time (heard-at 30000000 [1,3,2]))))
  (assert (= 3544142 (time (heard-at 30000000 [2,1,3]))))
  (assert (= 261214  (time (heard-at 30000000 [1,2,3]))))
  (assert (= 6895259 (time (heard-at 30000000 [2,3,1]))))
  (assert (= 18      (time (heard-at 30000000 [3,2,1]))))
  (assert (= 362     (time (heard-at 30000000 [3,1,2])))))

(time (heard-at 30000000 [2,20,0,4,1,17]))
;; "Elapsed time: 6055.094143 msecs"
;;=> 814

