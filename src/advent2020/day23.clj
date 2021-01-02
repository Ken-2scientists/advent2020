(ns advent2020.day23
  (:require [clojure.string :as str]
            [advent-utils.core :as u]))

(def day23-sample [3 8 9 1 2 5 4 6 7])
(def day23-input [1 9 8 7 5 3 4 6 2])

(defn init-ring
  "Represents a ring as a simple map, each node is a key, and its next node
   in the ring is its value in the map"
  [input]
  (zipmap input (u/rotate 1 input)))

(defn destination
  [max-label triplet target]
  (if (triplet target)
    (destination max-label triplet (dec target))
    (if (zero? target)
      (destination max-label triplet max-label)
      target)))

(defn move
  [size [ring node]]
  (let [triplet-start (ring node)
        triplet-mid   (ring triplet-start)
        triplet-end   (ring triplet-mid)
        next-node     (ring triplet-end)
        triplet       (set [triplet-start triplet-mid triplet-end])
        target        (destination size triplet (dec node))
        after-target  (ring target)]
    [(assoc ring
            triplet-end after-target
            target      triplet-start
            node        next-node)
     next-node]))

(defn representation
  [[ring node]]
  (let [size (count ring)]
    (take size (iterate ring node))))

(defn n-moves
  [input moves]
  (let [ring  (init-ring input)
        start (first input)
        size  (count input)]
    (nth (iterate (partial move size) [ring start]) moves)))

(defn order-after-moves
  [input moves]
  (let [[final _] (n-moves input moves)]
    (representation [final 1])))

(defn augment-cups
  [input]
  (vec (concat input (range 10 1000001))))

(defn star-cups
  [input moves]
  (let [lotsa-cups (augment-cups input)]
    (take 10 (order-after-moves lotsa-cups moves))))

(defn day23-part1-soln
  []
  (str/join (drop 1 (order-after-moves day23-input 100))))

(defn day23-part2-soln
  []
  (reduce * (take 2 (drop 1 (star-cups day23-input 10000000)))))