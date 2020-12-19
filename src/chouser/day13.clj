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

