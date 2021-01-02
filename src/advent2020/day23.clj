(ns advent2020.day23
  (:require [clojure.string :as str]
            [advent-utils.math :as math]
            [advent-utils.core :as u]))

(def day23-sample [3 8 9 1 2 5 4 6 7])
(def day23-input [1 9 8 7 5 3 4 6 2])

(defn ring-dec
  [x]
  (math/mod-sub 10 x 1))

;; (defn ring-subvec
;;   [v start end]
;;   (if (or (> start 8) (> end 8))
;;     (subvec (vec (concat v v)) start end)
;;     (subvec v start end)))

;; (defn remaining
;;   [cups idx]
;;   (if (< idx 6)
;;     (vec (concat (subvec cups 0 (inc idx))
;;                  (subvec cups (+ idx 4))))
;;     (subvec cups (math/mod-add 9 idx 4) (inc idx))))

(defn destination
  [remaining target]
  (if (remaining target)
    target
    (destination remaining (ring-dec target))))

(defn insert-at
  [v idx coll]
  (vec (concat (subvec v 0 (inc idx))
               coll
               (subvec v (inc idx)))))

(defn move
  [cups]
  (let [target (first cups)
        pick-up (take 3 (rest cups))
        left (vec (concat (list target) (drop 4 cups)))
        dest (destination (set left) (ring-dec target))
        insert-pos (u/index-of dest left)]
    (vec (u/rotate 1 (insert-at left insert-pos pick-up)))))

(defn order-after-moves
  [cups moves]
  (let [final (nth (iterate move cups) moves)
        one-pos (u/index-of 1 final)]
    (->> (u/rotate one-pos final)
         rest
         str/join)))

(defn day23-part1-soln
  []
  (order-after-moves day23-input 100))