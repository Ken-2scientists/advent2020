(ns advent2020.day09
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day09-input (map read-string (u/puzzle-input "day09-input.txt")))

(def day09-sample
  (map read-string
       (str/split
        "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576" #"\n")))

(defn find-first-non-sum
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

(defn find-contiguous-range-to-sum
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
  (find-first-non-sum day09-input 25))

(defn day09-part2-soln
  []
  (reduce + (find-contiguous-range-to-sum day09-input 22406676)))
