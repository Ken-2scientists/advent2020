(ns advent2020.day01
  (:require [clojure.math.combinatorics :as combo]
            [advent-utils.core :as u]))

(def day01-input
  (map read-string (u/puzzle-input "day01-input.txt")))

(defn find-pair-that-sums-to-total
  [total candidates]
  (let [candidate-set (set candidates)]
    (->> candidates
         (map (partial - total))
         (filter candidate-set))))

(defn find-triple-that-sums-to-total
  [total candidates]
  (let [candidate-set (->> (combo/cartesian-product candidates candidates)
                           (map (partial apply +))
                           (map (partial - total))
                           set)]
    (->> candidates
         (filter candidate-set))))

(defn day01-part1-soln
  []
  (apply * (find-pair-that-sums-to-total 2020 day01-input)))

(defn day02-part2-soln
  []
  (apply * (find-triple-that-sums-to-total 2020 day01-input)))