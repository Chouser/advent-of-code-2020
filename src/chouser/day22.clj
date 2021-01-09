(ns chouser.day22
  (:require [chouser.day14 :refer [line-coll let-re counting]]
            [clojure.string :as str]))

;; War!

(def example
  (-> "
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10" (str/split #"\n") rest))

(defn parse-decks [lines]
  (->> lines
       (map #(when (re-matches #"\d+" %) (Long/parseLong %)))
       (partition-by nil?)
       next (take-nth 2)))

(defn trick-winner [trick]
  ;; ignore ties
  (let [win-val (apply max trick)
        [winner & ties] (keep-indexed (fn [i v] (when (= v win-val) i)) trick)]
    winner))

(defn step [deck-queues]
  (let [trick (mapv peek deck-queues)
        decks (mapv pop deck-queues)
        winner (trick-winner trick)]
    (update decks winner into
            (cons (trick winner)
                  (remove nil? (assoc trick winner nil))))))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn war-score [lines]
  (->> lines
       parse-decks
       (map #(into empty-queue %))
       (iterate step) ;; sequence of rounds
       (some #(when-not (every? seq %) %)) ;; until a deck is empty
       (some seq) ;; winning deck
       reverse (map * (next (range))) (reduce +)))

(assert (= 306 (war-score example)))

#_(war-score (line-coll "resources/day22-input.txt"))
;;=> 29764

(defn recombat
  ([decks] (recombat #{} decks))
  ([seen decks]
   ;;(clojure.pprint/pprint {:decks decks})
   (cond
     (some empty? decks)
     {:decks decks
      :winner (first (keep-indexed (fn [i d]
                                     (when (seq d) i))
                                   decks))}

     (seen decks)
     {:decks decks, :winner 0}

     :else
     (let [seen (conj seen decks)
           trick (mapv peek decks)
           decks (mapv pop decks)
           winner (if (some true? (map < (map count decks) trick))
                    (trick-winner trick)
                    (:winner (recombat
                              (mapv (fn [deck n]
                                      (into empty-queue (take n deck)))
                                    decks trick))))]
       (recur seen
              (update decks winner into
                      (cons (trick winner)
                            (remove nil? (assoc trick winner nil)))))))))

(defn recombat-score [lines]
  (->> lines
       parse-decks
       (map #(into empty-queue %))
       recombat
       :decks
       (some seq) ;; winning deck
       reverse (map * (next (range))) (reduce +)))

(assert (= 291 (recombat-score example)))

#_(recombat-score (line-coll "resources/day22-input.txt"))
;;=> 32588
