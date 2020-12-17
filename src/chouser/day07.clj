(ns chouser.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [chouser.day02 :refer [line-coll let-re]]))

;; Count bag-nesting options

(def example
  (-> "
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
                (str/split #"\n")
                rest))

(defn parse-rules [lines]
  (reduce
   (fn [rulem line]
     (let-re [#"(?<outer>.*) bags contain (?<inners>.*)" line]
       (assoc rulem outer
              (into {}
                    (for [[_ num inner] (re-seq #"(\d+) ([a-z ]+) bag" inners)]
                      [inner (Long/parseLong num)])))))
   {}
   lines))

(defn outer-options [rules]
  (->> (for [[outer inners] rules
             [inner num] inners]
         {inner #{outer}})
       (apply merge-with into)))

(defn find-outer-options [oopts start]
  (loop [found #{}, todo (list start)]
    (if (empty? todo)
      found
      (let [more (get oopts (peek todo))]
        (recur (into found more)
               (-> todo
                   pop
                   (into (remove found more))))))))

(assert (= 4 (-> example
                 parse-rules
                 outer-options
                 (find-outer-options "shiny gold")
                 count)))

(-> (line-coll "resources/day07-input.txt")
    parse-rules
    outer-options
    (find-outer-options "shiny gold")
    count)
;;=> 197


(def count-inners
  (memoize
   (fn count-inners* [rules outer]
     (inc
      (reduce + (map (fn [[inner num]]
                       (* num (count-inners rules inner)))
                     (get rules outer)))))))

(assert (= 32 (-> example parse-rules (count-inners "shiny gold") dec)))

(-> (line-coll "resources/day07-input.txt")
    parse-rules (count-inners "shiny gold") dec)
;;=> 85324
