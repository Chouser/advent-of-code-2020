(ns chouser.day23
  (:require [clojure.string :as str]))

(def example "389125467")
(def input "523764819")

(defn parse-ring [s] (map #(Long/parseLong (str %)) s))

(defprotocol CupRing
  (cups-view [_ idx len] "Return seq of cup ids starting at idx")
  (cups-remove [_ idx len] "Return a cupring with cups starting at idx removed")
  (cups-find [_ id] "Return idx of cup with the given id, or nil if not found")
  (cups-insert [_ idx ids] "Insert seq of cup ids immediately to the right of idx")
  (cups-current-id [_] "Return the id of the current cup")
  (cups-advance-current [_] "Make the next cup 'current'"))

;; Initial, naive implementation, to get to part 2 of the problem. We can
;; optimize later if needed.
(defrecord PLCupRing [pl]
  CupRing
  (cups-view [_ idx len] (->> (cycle pl) (drop idx) (take len)))
  (cups-remove [_ idx len] (let [cpl (cycle pl)]
                             (->> (concat (take idx cpl) (drop (+ idx len) cpl))
                                  (take (- (count pl) len))
                                  PLCupRing.)))
  (cups-find [_ id] (first (keep-indexed (fn [idx pid]
                                           (when (= pid id) idx))
                                         pl)))
  (cups-insert [_ idx ids] (->> (concat (take idx pl) ids (drop idx pl))
                                PLCupRing.))
  (cups-current-id [_] (first pl))
  (cups-advance-current [_] (->> (concat (rest pl) (list (first pl)))
                                 PLCupRing.)))

(defn destination-index [cr id]
  (or (cups-find cr id)
      (let [id (dec id)
            id (if (neg? id) 9 id)]
        (recur cr id))))

(defn move [cr]
  (let [removed (cups-view cr 1 3)
        cr (cups-remove cr 1 3)
        didx (destination-index cr (dec (cups-current-id cr)))
        cr (cups-insert cr (inc didx) removed)]
    (cups-advance-current cr)))

(defn format-cups [cr]
  (apply str (cups-view cr (inc (cups-find cr 1)) (dec (count (:pl cr))))))

#_(format-cups (nth (->> (PLCupRing. (parse-ring example)) (iterate move)) 100))

#_(format-cups (nth (->> (PLCupRing. (parse-ring input)) (iterate move)) 100))
;;=> 49576328


;; Part 2, more cups...

(defn complete-ring [start-str]
  (let [ids (parse-ring start-str)]
    (concat ids (range (inc (apply max ids)) 1000001))))

#_(take 20 (complete-ring example))
#_(take-last 20 (complete-ring example))

#_(time (take 10 (:pl (nth (iterate move (PLCupRing. (complete-ring input))) 10))))
;; "Elapsed time: 6642.973183 msecs"
;; HA! We'll be done in 80 days.  We need to get at least 10,000 times faster.

;; Let's see -- possibly a mutable, doubly-linked list?

(set! *warn-on-reflection* true)

(defprotocol IDCell
  (get-id [this])
  (get-prev [this])
  (get-next [this])

  (init! [this] "set next and prev to self")
  (remove3! [this] "returns the 3 cells removed")
  (insert-after! [this subring] "inserts subring after this"))

(deftype DCell [^long id, ^:volatile-mutable prev, ^:volatile-mutable next]
  IDCell
  (get-id [this] id)
  (get-prev [this] prev)
  (get-next [this] next)

  (init! [this]
    (set! next this)
    (set! prev this)
    this)

  (remove3! [this]
    (let [^DCell a next
          ^DCell b (.-next a)
          ^DCell c (.-next b)
          ^DCell d (.-next c)]
      ;; fix up main ring
      (set! next d)
      (set! (.-prev d) this)

      ;; fix up subring
      (set! (.-prev a) c)
      (set! (.-next c) a)
      a))

  (insert-after! [this subring]
    (let [^DCell c (.-prev ^DCell subring)]
      (set! (.-prev ^DCell subring) this)
      (set! (.-prev ^DCell next) c)
      (set! (.-next c) next)
      (set! next subring)))

  Object
  (toString [this]
    (str "#♻("
         (str/join " " (concat
                        (->> (iterate #(.-next ^DCell %) this)
                             (take 5)
                             (map #(str (.-id ^DCell %))))
                        '("...")
                        (->> (iterate #(.-prev ^DCell %) this)
                             (take 6)
                             rest reverse (map #(str (.-id ^DCell %))))))
         ")")))

(defn dcell [id made-id!]
  (cond-> (init! (DCell. id nil nil))
    made-id! (made-id! id)))

(defn seq->dcells [coll & [made-id!]]
  (let [current (dcell (first coll) made-id!)
        final (reduce (fn [^DCell prev id]
                        (let [cell (dcell id made-id!)]
                          (insert-after! prev cell)
                          cell))
                      current
                      (rest coll))]
    current))

(defmethod print-method DCell [x writer]
  (.write ^java.io.Writer writer (str x)))

#_(seq->dcells (range 10))

#_(let [cs (-> (seq->dcells (range 10)) get-next get-next)
        sub (remove3! cs)]
    (insert-after! cs sub)
    cs)

(defn cell-moves [start-str movecount]
  (let [^"[Lchouser.day23.DCell;" id->cell (make-array DCell 1000010)
        current (seq->dcells (complete-ring start-str)
                             (fn [cell id] (aset id->cell id cell) cell))]
    (loop [current current, movecount movecount]
      (when (pos? movecount)
        (let [subring (remove3! current)
              dest (loop [dest-id (dec (get-id current))]
                     (let [dest-id (if (<= 0 dest-id)
                                     dest-id
                                     1000000)
                           dest (aget id->cell dest-id)]
                       (if (and dest
                                (not= dest subring)
                                (not= dest (get-next subring))
                                (not= dest (get-prev subring)))
                         dest
                         (recur (dec dest-id)))))]
          (insert-after! dest subring)
          (recur (get-next current) (dec movecount)))))
    (prn (aget id->cell 1))
    (->> (aget id->cell 1) (iterate get-next)
         (map get-id) rest (take 2) (reduce *))))


#_(time (assert (= 149245887792 (cell-moves example 10000000))))
;; #♻(1 934001 159792 508460 761549 ... 113455 131277 496579 907743 161909)
;; "Elapsed time: 4050.854945 msecs"

#_(time (cell-moves input 10000000))
;; #♻(1 760147 673265 160931 577412 ... 392631 647778 614199 940668 807335)
;; "Elapsed time: 4644.793828 msecs"
;;=> 511780369955

