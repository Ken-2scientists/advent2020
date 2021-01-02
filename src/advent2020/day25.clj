(ns advent2020.day25
  (:require [advent-utils.math :as math]
            [advent-utils.core :as u]))

(def day25-input (map read-string (u/puzzle-input "day25-input.txt")))

(defn transform
  [loop-size subject-number]
  (math/mod-pow-fast 20201227 subject-number loop-size))

(defn loop-size
  [public-key]
  (some #(when (= public-key (transform % 7)) %) (range 1 80000)))

(defn encryption-key
  [[card-public-key door-public-key]]
  (let [door-loop-size (loop-size door-public-key)]
    (transform door-loop-size card-public-key)))

(defn day25-part1-soln
  []
  (encryption-key day25-input))