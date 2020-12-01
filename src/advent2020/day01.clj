(ns advent2020.day01
  (:require [advent2020.lib.utils :as u]))

(def day01-input
  (map read-string (u/puzzle-input "day01-input.txt")))

(defn find-pair-that-sums-to-total
  [total candidates]
  (let [candidate-set (set candidates)]
    (->> candidates
         (map (partial - total))
         (filter candidate-set))))

(defn day01-part01-soln
  []
  (apply * (find-pair-that-sums-to-total 2020 day01-input)))

(comment
  (sort day01-input)
  (map (partial - 2020) day01-input)
  (find-pair-that-sums-to-total 2020 day01-input)
  (day01-part01-soln)
  (find-triple-that-sums-to-total 2020 day01-input))
