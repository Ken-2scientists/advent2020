(ns advent2020.day21
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent-utils.core :as u]))


(defn parse-line
  [line-str]
  (let [no-final-paren (subs line-str 0 (dec (count line-str)))
        [ingreds allergens] (str/split no-final-paren #"\(contains ")]
    {:ingredients (set (str/split ingreds #" "))
     :allergens (str/split allergens #", ")}))

(def day21-sample
  (map parse-line
       (str/split
        "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)" #"\n")))

(def day21-input (map parse-line (u/puzzle-input "day21-input.txt")))

(defn allergen-to-ingreds
  [{:keys [ingredients allergens]}]
  (map (fn [a] [a ingredients]) allergens))

(defn allergen-candidates
  [foods]
  (let [candidate-fn (comp (partial reduce set/intersection)
                           (partial map second))]
    (->> foods
         (mapcat allergen-to-ingreds)
         (group-by first)
         (u/fmap candidate-fn))))
