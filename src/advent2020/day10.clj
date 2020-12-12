(ns advent2020.day10
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day10-input (map read-string (u/puzzle-input "day10-input.txt")))

(defn freq-steps
  [jolts]
  (let [l (sort (conj jolts 0))
        fs (frequencies (map - (rest l) l))]
    [(get fs 1)
     (inc (get fs 3))]))

(defn combo-mapping
  [run-size]
  (case run-size
    2 2
    3 4
    4 7))

(defn combination-count
  [jolts]
  (let [l (sort (conj jolts 0))
        runs (-> (map - (rest l) l)
                 str/join
                 (str/split #"3"))]
    (->> (map count runs)
         (filter #(> % 1))
         (map combo-mapping)
         (reduce *))))

(defn day10-part1-soln
  []
  (let [freqs (freq-steps day10-input)]
    (reduce * freqs)))

(defn day10-part2-soln
  []
  (combination-count day10-input))