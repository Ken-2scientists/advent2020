(ns advent2020.day07
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day07-sample (str/split
                   "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags." #"\n"))

(defn keywordize
  [adj color]
  (keyword (str/join "-" [adj color])))

(defn bag-description
  [desc]
  (let [[num c1 c2] (str/split (str/trim desc) #"\ ")]
    [(Integer/parseInt num) (keywordize c1 c2)]))

(defn parse-rule
  [rule-str]
  (let [[outer inner] (str/split rule-str #" bags contain ")]
    [(apply keywordize (str/split outer #"\ "))
     (if (str/starts-with? inner "no")
       []
       (map bag-description (str/split inner #",")))]))

(def day07-input
  (->> (u/puzzle-input "day07-input.txt")
       (map parse-rule)))

(defn contained-by
  [[outer inner]]
  (map (fn [itm] [(second itm) outer]) inner))

(defn nesting
  [rules]
  (u/fmap (partial map second) (group-by first (mapcat contained-by rules))))

(defn all-outer-bags
  [rules bag]
  (let [nestings (nesting rules)]
    (loop [next-bags [bag] outer-bags #{}]
      (if (empty? next-bags)
        outer-bags
        (let [outers (mapcat (partial get nestings) next-bags)]
          (recur outers (into outer-bags outers)))))))

(defn day07-part1-soln
  []
  (count (all-outer-bags day07-input :shiny-gold)))