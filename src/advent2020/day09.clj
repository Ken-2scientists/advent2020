(ns advent2020.day09
  (:require [advent-utils.core :as u]))

(def day09-input (map read-string (u/puzzle-input "day09-input.txt")))

(defn first-non-sum
  [nums window]
  (loop [pos window
         val (nth nums window)
         prev (take window nums)]
    (let [candidates (map (partial - val) prev)]
      (if (empty? (filter (set prev) candidates))
        val
        (recur (inc pos)
               (nth nums (inc pos))
               (take window (drop (- (inc pos) window) nums)))))))

(defn contiguous-range-to-sum
  [nums target-sum]
  (loop [left 0 right 1]
    (let [the-range (take (- right left) (drop left nums))
          range-sum (reduce + the-range)]
      (if (= target-sum range-sum)
        [(apply min the-range) (apply max the-range)]
        (if (< range-sum target-sum)
          (recur left (inc right))
          (recur (inc left) right))))))

(defn day09-part1-soln
  []
  (first-non-sum day09-input 25))

(defn day09-part2-soln
  []
  (reduce + (contiguous-range-to-sum day09-input (day09-part1-soln))))
