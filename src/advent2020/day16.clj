(ns advent2020.day16
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(defn parse-range
  [range-str]
  (map read-string (str/split range-str #"-")))

(defn parse-rule
  [rule-str]
  (let [[desc-str range-str] (str/split rule-str #": ")
        desc (keyword (str/replace desc-str " " "-"))
        ranges (map parse-range (str/split range-str #" or "))]
    {:desc desc :ranges ranges}))

(defn parse-ticket
  [ticket-str]
  (map read-string (str/split ticket-str #",")))

(defn parse
  [input]
  (let [[rules tickets] (str/split input #"\n\nyour ticket:\n")
        [yours nearby]  (str/split tickets #"\n\nnearby tickets:\n")]
    {:rules (map parse-rule (str/split rules #"\n"))
     :yours (parse-ticket yours)
     :nearby (map parse-ticket (str/split nearby #"\n"))}))

(def day16-sample
  (parse
   "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"))

(def day16-sample2
  (parse
   "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"))

(def day16-input (->>
                  (u/puzzle-input "day16-input.txt")
                  (str/join "\n")
                  parse))

(defn valid-values
  [rules]
  (->> (mapcat :ranges rules)
       (mapcat #(range (first %) (inc (second %))))
       (into #{})))

(defn valid-nearby
  [{:keys [rules nearby]}]
  (let [valid? (valid-values rules)]
    (filter valid? (flatten nearby))))

(defn invalid-nearby
  [{:keys [rules nearby]}]
  (let [valid? (valid-values rules)
        invalid? (complement valid?)]
    (filter invalid? (flatten nearby))))

(defn day16-part1-soln
  []
  (reduce + (invalid-nearby day16-input)))

(defn rule-matchers
  [{:keys [desc ranges]}]
  (fn [coll]
    (let [check (->> ranges
                     (mapcat #(range (first %) (inc (second %))))
                     (into #{}))]
      (when (every? check coll) desc))))

(def mrs (map rule-matchers (:rules day16-sample2)))
(def tst (fn [c] (set (filter some? (map #(% c) mrs)))))
(def cls (apply mapv vector (:nearby day16-sample2)))
(map tst cls)


(defn identify-slots
  [{:keys [rules] :as input}]
  (let [matchers (map rule-matchers rules)
        test (fn [col] (set (filter some? (map #(% col) matchers))))
        cols (apply mapv vector (valid-nearby input))]
    (map test cols)))

(identify-slots day16-input)