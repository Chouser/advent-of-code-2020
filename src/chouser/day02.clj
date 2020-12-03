(ns chouser.day02
  (:require [clojure.java.io :as io]
            [clojure.core.protocols :as p]))

;; Count the passwords that match their requirement

;; I forgot inputs are often specially formatted. Well, this is adapting
;; yesterday's CollReduce to provide a series of strings read from the file,
;; still both eager and progressive, and auto-closing the file when done.
(defn line-coll [filename]
  (reify p/CollReduce
    (coll-reduce [eof f]
      (with-open [reader (io/reader filename)]
        (reduce f (line-seq reader))))
    (coll-reduce [eof f start]
      (with-open [reader (io/reader filename)]
        (reduce f start (line-seq reader))))))

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
                 [n (.group m (str n))])))))

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

(defn line-matches? [line]
  (let-re [#"(?<low>\d+)-(?<high>\d+) (?<chr>.): (?<pw>.*)" line]
    (<= (Long/parseLong low)
        (count (filter #{(first chr)} pw))
        (Long/parseLong high))))

(defn count-matches
  "Like (count (filter line-matches? lines)), but supports CollReduce collection
  instead of sequences."
  [lines]
  (transduce (comp (filter line-matches?)
                   (map (constantly 1)))
             + 0 lines))

(def example ["1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"])

(assert (= 2 (count-matches example)))

(count-matches (line-coll "resources/day02-input.txt"))
;;=> 483


;; Count the passwords that match slightly different requirements

(defn count-filter
  "Like (count (filter pred? coll)), but supports CollReduce collection
  instead of sequences."
  [pred? coll]
  (transduce (comp (filter pred?)
                   (map (constantly 1)))
             + 0 coll))

;; Redo of part 1:
(count-filter line-matches? (line-coll "resources/day02-input.txt"))
;;=> 483

(defn line-matches2? [line]
  (let-re [#"(?<a>\d+)-(?<b>\d+) (?<chr>.): (?<pw>.*)" line
           c (first chr)]
    ;; only return true if one is equal and the other is not
    (not= (= c (nth pw (dec (Long/parseLong a))))
          (= c (nth pw (dec (Long/parseLong b)))))))

(count-filter line-matches2? (line-coll "resources/day02-input.txt"))
;;=> 482
