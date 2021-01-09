(ns chouser.day20
  (:require [chouser.day14 :refer [line-coll let-re counting]]
            [clojure.string :as str]))

(defn parse-tiles [lines]
  (:tiles
   (reduce
    (fn [state line]
      (let-re [#"Tile (?<id>\d+):|(?<bits>[.#]+)" line]
        (cond
          id (assoc state :id (Long/parseLong id))
          bits (update-in state [:tiles (:id state)] (fnil conj []) bits)
          :else state)))
    {:id nil, :tiles {}} lines)))

;; 01 13 32 20 10 31 23 02
;; 23 02 10 31 32 20 01 13

(defn tile-orients [tile]
  (let [transposed (apply mapv list tile)
        rots (->> [(seq (first tile))
                   (peek transposed)
                   (seq (peek tile))
                   (first transposed)]  ;; rotate:
                  (iterate (fn [[t r b l]] [r (reverse b) l (reverse t)]))
                  (take 4)
                  vec)]
    (into rots ;; flip:
          (map (fn [[t r b l]] [(reverse t) l (reverse b) r]))
          rots)))

#_(tile-orients [[0 1] [3 2]])

(defn index-tiles [tiles]
  (let [tile-orients (into {} (map (fn [[id tile]] [id (tile-orients tile)]))
                           tiles)]
    {:tile-orients tile-orients ;; id->orient->edge
     :edge-tiles ;; [edge edgeIdx]->id->orientIdx
     (reduce
      (fn [m [path v]]
        (update-in m path (fnil conj #{}) v))
      {}
      (for [[id orients] tile-orients
            [orient-idx orient] (map-indexed list orients)
            [edge-idx edge] (map-indexed list orient)]
        [[[edge edge-idx] id] orient-idx]))}))

#_(index-tiles (parse-tiles (line-coll "resources/day20-example1.txt")))

#_(frequencies (map count (vals (index-tiles (parse-tiles (line-coll "resources/day20-input.txt"))))))
;;=> {2 528, 1 96}

;; return a sequence of [id orient] that could be in the edge-idx direction from
;; the given orient
(defn step [idxs orient edge-idx]
  (let [target-ei ({0 2, 2 0, 1 3, 3 1} edge-idx)]
    (for [[id orient-idxs] (get-in idxs [:edge-tiles
                                         [(orient edge-idx) target-ei]])
          orient-idx orient-idxs]
      [id (get-in idxs [:tile-orients id orient-idx])])))

(defn walk [idxs [id orient :as id-orient] seen edge-idx]
  (let [seen (conj seen id) ;; TODO: use only last seen?
        next-id-orients (remove #(seen (first %)) (step idxs orient edge-idx))]
    (when (next next-id-orients)
      (prn :branch! next-id-orients))
    (if (empty? next-id-orients)
      id-orient
      (recur idxs (first next-id-orients) seen edge-idx))))

(defn corners [lines]
  (let [idxs (index-tiles (parse-tiles lines))
        [start-id [start-orient]] (-> idxs :tile-orients first)]
    (for [edge-idx (range 4)]
      (let [corner (walk idxs [start-id start-orient] #{} edge-idx)]
        (first (walk idxs corner #{} (rem (inc edge-idx) 4)))))))

(assert (= 20899048083289
           (apply * (corners (line-coll "resources/day20-example1.txt")))))

#_(apply * (corners (line-coll "resources/day20-input.txt")))
;;=> 63187742854073


;; Part 2: we're going to need to keep track of orient-ids so we can transform
;; original tiles.

;; Returns [id orient-idx] of tile adjacent to the one given edge
(defn stepb [idxs edge-idx [id orient-idx]]
  (let [target-ei ({0 2, 2 0, 1 3, 3 1} edge-idx)
        src-edge (get-in idxs [:tile-orients id orient-idx edge-idx])
        candidates (->> (get-in idxs [:edge-tiles [src-edge target-ei]])
                        (remove #(= id (first %))))
        [next-id orient-idxs] (first candidates)]
    (when (next candidates)
      (prn :branch-a candidates))
    (when (next orient-idxs)
      (prn :branch-b next-id orient-idxs))
    (when next-id
      [next-id (first orient-idxs)])))

(defn path [idxs edge-idx start-ori]
  (take-while identity (iterate #(stepb idxs edge-idx %) start-ori)))

(defn orient-tile [tile orient-idx]
  (let [rot (nth (iterate #(reverse (apply map list %)) tile)
                 (rem orient-idx 4))]
    (if (< orient-idx 4)
      rot
      (map reverse rot)))) ;; flip

(defn trim-tile [tile]
  (->> tile next drop-last
       (map #(->> % next drop-last))))

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(def monster-coords
  (for [[y line] (map-indexed list monster)
        [x c] (map-indexed list line)
        :when (= \# c)]
    [y x]))

;; ugh -- replacement is so annoying, it might be better to skip regex and search myself!
(defn replace-monsters [lines]
  (let [line-len (count (first lines))
        wrap-len (- line-len (count (first monster)))
        ptn (re-pattern (str/join (str ".{" wrap-len "}")
                                  (map #(str/replace % #" " ".") monster)))]

    (->> (str/replace (apply str lines) ptn
                      (fn [match]
                        (apply str "O" (next match))))
         (partition line-len)
         (map #(apply str %)))))

(defn replace-monsters [lines]
  (->> (for [y (range (count lines))
             x (range (count (lines y)))]
         [y x])
       (reduce
        (fn [lines pic-coord]
          (if (some #(not= \# (get-in lines (map + % pic-coord))) monster-coords)
            lines
            (reduce (fn [lines mc]
                      (assoc-in lines (map + mc pic-coord) \@))
                    lines monster-coords)))
        lines)))

(defn count-# [lines]
  (transduce (comp (mapcat seq) (filter #(= \# %))) counting lines))

(defn orient-all-tiles [filename]
  (let [tiles (parse-tiles (line-coll filename))
        idxs (index-tiles tiles)
        [start-id] (-> idxs :tile-orients first)
        whole (->>
               [start-id 3]
               (path idxs 0) last
               (path idxs 3) last
               (path idxs 2)
               (into
                [] (mapcat (fn [tile]
                             (->>
                              (path idxs 1 tile)
                              (map (fn [[id oix]]
                                     (orient-tile (trim-tile (tiles id)) oix)))
                              (apply map #(into [] (mapcat seq) %&)))))))]
    (->>
     (map (fn [i] (replace-monsters (mapv vec (orient-tile whole i))))
          (range 8))
     (apply max-key #(- (count-# %)))
     ((fn [lines]
        [(mapv #(apply str %) lines)
         (count-# lines)])))))

#_(orient-all-tiles "resources/day20-input.txt")


