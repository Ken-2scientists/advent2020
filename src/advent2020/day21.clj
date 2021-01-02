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

(defn known-allergens
  [foods]
  (let [candidates (allergen-candidates foods)]
    (apply set/union (vals candidates))))

(defn safe-ingredients
  [foods]
  (let [not-allergen? (complement (known-allergens foods))]
    (filter not-allergen? (mapcat :ingredients foods))))

(defn identify-allergens
  [foods]
  (let [cands (allergen-candidates foods)]
    (loop [candidates cands mapping {}]
      (if (empty? candidates)
        (u/fmap first mapping)
        (let [knowns (into {} (filter #(= 1 (count (val %))) candidates))
              ingreds (apply set/union (vals knowns))]
          (recur (->> (apply dissoc candidates (keys knowns))
                      (u/fmap #(set/difference % ingreds)))
                 (merge mapping knowns)))))))

(defn allergen-sorted-unsafe-ingredients
  [foods]
  (->> (identify-allergens foods)
       (sort-by key)
       (map second)
       (str/join ",")))

(defn day21-part1-soln
  []
  (count (safe-ingredients day21-input)))

(defn day21-part2-soln
  []
  (allergen-sorted-unsafe-ingredients day21-input))