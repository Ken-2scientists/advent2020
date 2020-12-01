(ns advent2020.day01
  (:require [clojure.math.combinatorics :as combo]
            [advent2020.lib.utils :as u]))

(def day01-example [1721 979 366 299 675 1456])

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

(defn find-triple-that-sums-to-total
  [total candidates]
  (let [candidate-set (->> (combo/cartesian-product candidates candidates)
                           (map (partial apply +))
                           set)]
    (->> candidates
         (map (partial - total))
         (filter candidate-set)
         (map (partial - total)))))

(defn day02-part02-soln
  []
  (apply * (find-triple-that-sums-to-total 2020 day01-input)))