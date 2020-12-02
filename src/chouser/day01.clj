(ns chouser.day01
  (:require [clojure.java.io :as io]))

;; Find a pair of numbers from the set whose sum is 2020

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

(defn multiply-triple [sum-goal input-seq]
  (some (fn [i]
          (when-let [product (multiply-pair (- sum-goal i) input-seq)]
            (* i product)))
        (into [] input-seq)))

(assert (= 241861950 (multiply-triple 2020 example)))

(multiply-triple 2020 (read-all "resources/day01-input.edn"))
;;=> 276912720
