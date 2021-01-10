(ns chouser.day24
  (:require [chouser.day14 :refer [line-coll counting]]
            [clojure.string :as str]))

;; hextiles!

(def example (-> "
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew" (str/split #"\n") rest))

(defn parse-line [line]
  (re-seq #"[ns]?[ew]" line))

(defn step [[x y] dir]
  (case dir
    "e"  [(inc x) y]
    "w"  [(dec x) y]
    "se" [(if (even? y) x (inc x)) (dec y)]
    "ne" [(if (even? y) x (inc x)) (inc y)]
    "sw" [(if (even? y) (dec x) x) (dec y)]
    "nw" [(if (even? y) (dec x) x) (inc y)]))

(defn reduce-freqs
  ([] (transient {}))
  ([counts] (persistent! counts))
  ([counts x] (assoc! counts x (inc (get counts x 0)))))

(defn count-black-tiles [lines]
  (->> lines
       (transduce (map #(reduce step [0 0] (parse-line %)))
                  reduce-freqs)
       (transduce (comp (map val) (filter odd?))
                  counting)))

(assert (= 10 (count-black-tiles example)))

#_(count-black-tiles (line-coll "resources/day24-input.txt"))
;;=> 322


;; part 2: hexlife!

(defn update-tile [board coord new-self]
  (let [delta (if new-self +1 -1)]
    (reduce #(update-in %1 [(step coord %2) :adj] (fnil + 0) delta)
            (assoc-in board [coord :self] new-self)
            ["e" "w" "se" "ne" "sw" "nw"])))

(defn lifestep [board]
  (reduce
   (fn [new-board [coord {:keys [self adj]}]]
     (cond
       (and self (or (nil? adj) (zero? adj) (< 2 adj)))
       (update-tile new-board coord false)

       (and (not self) (= 2 adj))
       (update-tile new-board coord true)

       :else new-board))
   board board))

(defn print-board [board]
  (let [[[minx maxx] [miny maxy]] (apply map (juxt min max) (keys board))]
    (doseq [y (range miny (inc maxy))]
      (println
       (apply str
              (when (odd? y) " ")
              (map #(if (get-in board [[% y] :self])
                      "# "
                      #_(str (get-in board [[% y] :adj] " ") " ")
                      ". ")
                   (range minx (inc maxx))))))))

(defn build-board [lines]
  (->> lines
       (transduce (map #(reduce step [0 0] (parse-line %)))
                  reduce-freqs)
       (reduce (fn [board [coord flips]]
                 (if (even? flips)
                   board
                   (update-tile board coord true)))
        {})))

#_
(print-board (lifestep (build-board example)))

#_
(->> (iterate lifestep (build-board example))
     (map #(doto % print-board))
     (map #(->> % vals (filter :self) count))
     (take 10))

(defn part2 [lines]
  (->> (nth (iterate lifestep (build-board lines)) 100)
       (transduce (comp (map val) (filter :self))
                  counting)))

(assert (= 2208 (part2 example)))

#_(time (part2 (line-coll "resources/day24-input.txt")))
;; "Elapsed time: 1684.443991 msecs"
;;=> 3831


;; With a transient board, a little faster:

(defn update-tile! [board coord new-self]
  (let [delta (if new-self +1 -1)]
    (reduce #(let [nc (step coord %2)]
               (assoc! %1 nc (update (get %1 nc) :adj (fnil + 0) delta)))
            (assoc! board coord (assoc (get board coord) :self new-self))
            ["e" "w" "se" "ne" "sw" "nw"])))

(defn lifestep [board]
  (persistent!
   (reduce
    (fn [new-board [coord {:keys [self adj]}]]
      (cond
        (and self (or (nil? adj) (zero? adj) (< 2 adj)))
        (update-tile! new-board coord false)

        (and (not self) (= 2 adj))
        (update-tile! new-board coord true)

        :else new-board))
    (transient board) board)))

#_(time (part2 (line-coll "resources/day24-input.txt")))
;; "Elapsed time: 1052.711415 msecs"
;;=> 3831
