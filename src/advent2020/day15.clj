(ns advent2020.day15
  (:require [clojure.string :as str]
            [advent-utils.core :as u]))

(def day15-input (map read-string
                      (-> (u/puzzle-input "day15-input.txt")
                          first
                          (str/split #","))))

(defn rules
  [nums lastnum]
  (let [positions (nums lastnum)
        size      (count positions)]
    (if (= 1 size)
      0
      (- (reduce - (subvec positions (- size 2) size))))))

(defn conjv
  [v x]
  (if (nil? v)
    [x]
    (conj v x)))

(defn memory-seq
  [starters limit]
  (let [start-cnt (count starters)]
    (loop [theseq (vec starters)
           lastnum (last starters)
           nums (zipmap starters (mapv vector (range start-cnt)))
           cnt  start-cnt]
      (if (<= limit cnt)
        (take limit theseq)
        (let [nextnum (rules nums lastnum)]
          (recur (conj theseq nextnum)
                 nextnum
                 (update nums nextnum #(conjv % cnt))
                 (inc cnt)))))))

(defn day15-part1-soln
  []
  (last (memory-seq day15-input 2020)))

(defn day15-part2-soln
  []
  (last (memory-seq day15-input 30000000)))
