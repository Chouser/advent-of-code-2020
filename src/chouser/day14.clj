(ns chouser.day14
  (:require [clojure.string :as str]
            [clojure.core.protocols :as p]
            [clojure.java.io :as io]))

;; Update re-let to better support optional match groups:

(def pattern-names
  "Return list of potential group names in regex pattern"
  (memoize (fn pattern-names [pattern]
             (mapv second (re-seq #"\(\?<([a-zA-Z][a-zA-Z0-9]*)>"
                                  (str pattern))))))

(defn re-find-named
  "Like re-find, but returns a map of named groups to the substring they matched."
  [pattern string]
  (let [m (re-matcher pattern string)]
    (when (.find m)
      (into {} (for [n (pattern-names pattern)]
                 [n (try (.group m (str n))
                         (catch IllegalArgumentException ex
                           ;; A capturing group with this name seemed to appear
                           ;; in the regex, but was not found for this
                           ;; particular match. Bind the local to nil:
                           nil))])))))

(defmacro let-re
  "Like `let` but supports regex patterns with named groups on the left, binding
  locals with those group names to the substrings that match on the right."
  [bindings & body]
  `(let ~(vec (mapcat (fn [[binding expr]]
                        (if-not (instance? java.util.regex.Pattern binding)
                          [binding expr]
                          [{:strs (vec (map symbol (pattern-names binding)))}
                           `(re-find-named ~binding ~expr)]))
                      (partition 2 bindings)))
     ~@body))

;; line-coll still seems pretty solid, though supporing seq interface would be
;; nice at times:

(defn line-coll [filename]
  (reify
    p/CollReduce
    (coll-reduce [eof f]
      (with-open [reader (io/reader filename)]
        (reduce f (line-seq reader))))
    (coll-reduce [eof f start]
      (with-open [reader (io/reader filename)]
        (reduce f start (line-seq reader))))
    clojure.lang.IPersistentCollection
    (seq [this]
      (tap> {:msg "line-coll building intermediate vector of entire input"})
      (seq (into [] this)))))

(defonce auto-tap true)
(def default-tap
  (when auto-tap
    (let [nsn (.name *ns*)]
      (remove-tap default-tap)
      (doto (fn [x] (binding [*out* *err*]
                      (println nsn "tap>" (pr-str x))))
        add-tap))))
(defn no-tap []
  (alter-var-root #'auto-tap (constantly false))
  (alter-var-root #'default-tap remove-tap))

(defn counting
  ([] 0)
  ([done] done)
  ([acc elem] (inc acc)))

;; Run silly bit-mask program

(def example
  (-> "
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"
      (str/split #"\n")
      rest))

(defn parse-line [line]
  (let-re [#"(mask = (?<mask>[X01]*))|(mem\[(?<addr>\d+)\] = (?<v>\d+))" line]
    (if mask
      {:set (Long/parseLong (str/replace mask #"X" "0") 2)
       :clear (Long/parseLong (apply str (map {\X 0 \0 1 \1 0} mask)) 2)}
      {:addr (Long/parseLong addr)
       :v (Long/parseLong v)})))

(defn step [state op]
  (if (:set op)
    (assoc state :mask op)
    (assoc-in state [:mem (:addr op)]
              (let [{:keys [set clear]} (:mask state)]
                (bit-xor (bit-or (:v op) set)
                         (bit-and (:v op) clear))))))

(defn sum-mem-after [lines]
  (->> (transduce (map parse-line) (completing step) {} lines)
       :mem vals
       (reduce +)))

(assert (= 165 (sum-mem-after example)))

(sum-mem-after (line-coll "resources/day14-input.txt"))
;;=> 14925946402938

;; Run sillier bit-mask program

(defn step2 [state line]
  (let-re [#"(mask = (?<mask>[X01]*))|(mem\[(?<addr>\d+)\] = (?<v>\d+))" line]
    (if mask
      (let [all-set (Long/parseLong (str/replace mask #"X" "0") 2)
            x-locs (into []
                         (comp
                          (map-indexed (fn [i c]
                                         (when (= \X c) (bit-shift-left 1 i))))
                          (remove nil?))
                         (reverse mask))]
        (assoc state :masks
               (map (fn [f]
                      {:set (bit-or all-set
                                    (transduce
                                     (map-indexed
                                      (fn [i j]
                                        (if (zero? (bit-and f (bit-shift-left 1 i)))
                                          0
                                          j)))
                                     + x-locs))
                       :clear (transduce
                               (map-indexed
                                (fn [i j]
                                  (if (zero? (bit-and f (bit-shift-left 1 i)))
                                    j
                                    0)))
                               + x-locs)})
                    (range (inc (bit-shift-left 1 (count x-locs)))))))
      (reduce
       (fn [state {:keys [set clear]}]
         (let [addr (Long/parseLong addr)
               masked-addr (bit-xor (bit-or addr set)
                                    (bit-and addr clear))]
           (assoc-in state [:mem masked-addr] (Long/parseLong v))))
       state
       (:masks state)))))

;; That works! Let's see if we can tighten it up a bit.

(defn step2 [state line]
  (let-re [#"(mask = (?<mask>[X01]*))|(mem\[(?<addr>\d+)\] = (?<v>\d+))" line]
    (if mask
      (let [all-set (Long/parseLong (str/replace mask #"X" "0") 2)
            x-locs (into [] (keep-indexed
                             (fn [i c] (when (= \X c) (bit-shift-left 1 i))))
                         (reverse mask))]
        (->> (range (inc (bit-shift-left 1 (count x-locs))))
             (map #(->> x-locs
                        (map-indexed
                         (fn [i j]
                           (if (zero? (bit-and % (bit-shift-left 1 i)))
                             [0 j]
                             [j 0])))
                        (concat [[all-set 0]])
                        (apply map bit-or)))
             (assoc state :masks)))
      (reduce
       (fn [state [set clear]]
         (let [addr (Long/parseLong addr)
               masked-addr (bit-xor (bit-or addr set) (bit-and addr clear))]
           (assoc-in state [:mem masked-addr] v)))
       state
       (:masks state)))))

(defn sum-mem-after2 [lines]
  (->> (reduce step2 {} lines)
       :mem vals
       (transduce (map #(Long/parseLong %)) +)))

(def example2
  (-> "
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"
      (str/split #"\n")
      rest))

(assert (= 208 (sum-mem-after2 example2)))

(sum-mem-after2 (line-coll "resources/day14-input.txt"))
;;=> 3706820676200
