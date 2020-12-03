(ns chouser.day01
  (:require [clojure.java.io :as io]
            [clojure.core.protocols :as p]))

;; Find a pair of numbers from the set whose sum is 2020

;; I've done a couple Advents before, so I can predict I'll need a way to read
;; stuff from files. I've been occasionally irritated by the lack of a
;; convenient way to read a file full of edn expressions, so I thought I'd try
;; to make something that is abstract enough for reuse, eager (to avoid problems
;; with lazy seqs of files that need to be closed), but that doesn't realize the
;; entire file of forms before processing begins. This clearly needs refinement,
;; and I assume others have written something equivalent or better, but this is
;; what I've got so far:
(defn read-all [filename]
  (reify p/CollReduce
    (coll-reduce [eof f]
      (throw (ex-info "wat" {})))
    (coll-reduce [eof f start]
      (with-open [reader (-> filename io/reader
                             clojure.lang.LineNumberingPushbackReader.)]
        (reduce (fn [acc form]
                  (if (= eof form)
                    (reduced acc)
                    (f acc form)))
                start
                (repeatedly #(read {:eof eof} reader)))))))

;; Pouring input-seq into a set should be O(n * log32(n)), as is walking the set
;; doing a lookup for each entries partner.
(defn multiply-pair [sum-goal input-seq]
  (let [input-set (into (hash-set) input-seq)]
    (some (fn [i]
            (let [partner (- sum-goal i)]
              (when (contains? input-set partner)
                (* i partner))))
          input-set)))

(def example [1721 979 366 299 675 1456])

(assert (= 514579 (multiply-pair 2020 example)))

(multiply-pair 2020 (read-all "resources/day01-input.edn"))
;;=> 252724

;; Find three numbers from the set whose sum is 2020

;; Can't think of how to do this without essentially summing each possible pair
;; before looking up the third entry. So that O(n^2 * log32(n)). Ouch.
(defn multiply-triple [sum-goal input-seq]
  (some (fn [i]
          (when-let [product (multiply-pair (- sum-goal i) input-seq)]
            (* i product)))
        (into [] input-seq)))

(assert (= 241861950 (multiply-triple 2020 example)))

(multiply-triple 2020 (read-all "resources/day01-input.edn"))
;;=> 276912720
