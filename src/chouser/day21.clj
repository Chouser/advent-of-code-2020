(ns chouser.day21
  (:require [chouser.day14 :refer [line-coll let-re counting]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example
  (-> "
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
      (str/split #"\n") rest))

(defn parse-line [line]
  (let [words (read-string (str "[" line "]"))]
    {:ings (pop words), :algs (next (peek words))}))

(defn alg-ings [algmap {:keys [ings algs]}]
  (apply merge-with
         set/intersection
         algmap (for [alg algs] {alg (set ings)})))

(defn count-safe-ings [lines]
  (let [entries (into [] (map parse-line) lines)
        possible-alergens (set (mapcat val (reduce alg-ings {} entries)))]
    (transduce (comp (mapcat :ings) (remove possible-alergens))
               counting entries)))

(assert (= 5 (count-safe-ings example)))

#_(count-safe-ings (line-coll "resources/day21-input.txt"))
;;=> 2423

(defn danger [lines]
  (loop [ing-alg {}
         [[alg ings] :as q] (into
                             clojure.lang.PersistentQueue/EMPTY
                             (transduce (map parse-line)
                                        (completing alg-ings)
                                        {} lines))]
    (cond
      (nil? alg) (str/join "," (keys (sort-by val ing-alg)))
      (nil? (next ings)) (recur (assoc ing-alg (first ings) alg) (pop q))
      :else (recur ing-alg (conj (pop q) [alg (remove ing-alg ings)])))))

(assert (= "mxmxvkd,sqjhc,fvjkl" (danger example)))

#_(danger (line-coll "resources/day21-input.txt"))
;;=> "jzzjz,bxkrd,pllzxb,gjddl,xfqnss,dzkb,vspv,dxvsp"
