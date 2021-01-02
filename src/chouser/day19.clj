(ns chouser.day19
  (:require [chouser.day14 :refer [line-coll let-re counting]]
            [clojure.string :as str]))

;; Check production rules.

;; This should be easy enough to brute force with backtracking, but wouldn't it
;; be fun to compile the rules into a finite state machine instead?

(defn parse-rule [rule-seq]
  (cond
    (string? rule-seq) (first rule-seq)
    (number? rule-seq) rule-seq
    :else (let [ors (take-nth 2 (partition-by #{'|} rule-seq))]
            (cond
              (not (next rule-seq)) (parse-rule (first rule-seq))
              (next ors) (set (map parse-rule ors))
              :else (mapv parse-rule rule-seq)))))

;; returns map with :rules and :msgs. :rules is a map indexed by rule id.
;; Each rule uses a set to mean alternatives, a vector to mean sequence, strings
;; for literals and ints for rule numbers.
(defn parse-input [lines]
  (reduce
   (fn [state line]
     (let-re [#"(?<id>\d+): (?<rule>.*)" line]
       (cond
         (:msgs state) (update state :msgs conj line)
         (not rule) (assoc state :msgs [])
         :else (assoc-in state [:rules (Long/parseLong id)]
                         (parse-rule (read-string (str \[ rule \])))))))
   {:rules {}}
   lines))

(def example1
  (-> "
0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"" (str/split #"\n") rest))

(def example1-tree {\a {\a {\b {}} \b {\a {}}}})

(defn deep-merge [a b]
  (cond
    (= a b) a
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    :else (prn :merge-conflict a b)))

(defn build-rule-tree [rules [frame & stack]]
  (cond
    (nil? frame) {}
    (int? frame) (recur rules (cons (get rules frame) stack))
    (char? frame) {frame (build-rule-tree rules stack)}
    (set? frame) (transduce
                  (map #(build-rule-tree rules (cons % stack)))
                  (completing deep-merge) {} frame)
    (vector? frame) (if (empty? frame)
                      (recur rules stack)
                      (recur rules (list* (first frame)
                                          (subvec frame 1)
                                          stack)))))

(assert (= {\a {}} (build-rule-tree [#{1 2} \a \b] '(1))))
(assert (= {\a {}, \b {}} (build-rule-tree [#{1 2} \a \b] '(0))))
(assert (= {\a {\b {}}} (build-rule-tree [[\a \b]] '(0))))
(assert (= {\a {\b {}}} (build-rule-tree [[1 2] \a \b] '(0))))
(assert (= example1-tree (build-rule-tree (:rules (parse-input example1)) '(0))))
(assert (= {\a {\b {}}} (build-rule-tree [[1 2] \a \b] '(0))))
(assert (= {\a {}, \b {}, \c {}, \d {}}
           (build-rule-tree [#{1 2} #{\a \b} #{\c \d}] '(0))))

(def example2
  (-> "
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb" (str/split #"\n") rest))

(defn count-matches [lines]
  (let [{:keys [rules msgs]} (parse-input lines)
        rule-tree (build-rule-tree rules '(0))]
    (transduce (filter #(get-in rule-tree %)) counting msgs)))

(assert (= 2 (count-matches example2)))

#_(count-matches (line-coll "resources/day19-input.txt"))
;;=> 109


;; Part 2:
;; Ha! With loops added to the grammer, the approach of compiling all poosible
;; paths into a tree is obviously not going to work.

;; If matches, returns a set of possible remaining unmatched portions of the msg.
;; If no match, returns an empty set.
(defn match [rules rule msg]
  (cond
    (int? rule) (recur rules (rules rule) msg)
    (char? rule) (if (= rule (first msg))
                   (list (rest msg))
                   ())
    (set? rule) (mapcat #(match rules % msg) rule)
    (vector? rule) (reduce
                    (fn [matches subrule]
                      (mapcat #(match rules subrule %) matches))
                    (list msg)
                    rule)))

(defn count-matches [lines]
  (let [{:keys [rules msgs]} (parse-input lines)]
    (transduce (filter #(some empty? (match rules 0 %))) counting msgs)))

(assert (= 2 (count-matches example2)))
#_(count-matches (line-coll "resources/day19-input.txt"))
;;=> 109   ;; So much faster!!

(def rules-patch
  (-> "
8: 42 | 42 8
11: 42 31 | 42 11 31"
      (str/split #"\n") rest
      parse-input :rules))

(defn count-patched-matches [lines]
  (let [{:keys [rules msgs]} (parse-input lines)
        rules (merge rules rules-patch)]
    (transduce (filter #(some empty? (match rules 0 %))) counting msgs)))

#_(count-patched-matches (line-coll "resources/day19-input.txt"))
;;=> 301
