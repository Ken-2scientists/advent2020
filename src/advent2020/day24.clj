(ns advent2020.day24
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent-utils.core :as u]))

(defn parse-line
  [line]
  (-> line
      ;; An 'n' or 's' must signal the start of a token, so add a space before
      (str/replace #"([ns])" " $1")
      ;; If there are consecutive e's or w's, add a space between
      (str/replace #"([we])(?=[ew])" "$1 ")
      str/trim
      (str/split #" ")))

(def day24-input (map parse-line (u/puzzle-input "day24-input.txt")))

(def dir->coord {"e"  [1  1  0]
                 "w"  [-1 -1  0]
                 "ne" [0  1  1]
                 "se" [1  0 -1]
                 "nw" [-1  0  1]
                 "sw" [0 -1 -1]})
(def neighbor-coords (vals dir->coord))

(defn neighbors
  [pos]
  (map (partial map + pos) neighbor-coords))

(defn position
  [steps]
  (reduce (partial map +) (map dir->coord steps)))

(defn black-tiles
  [input]
  (->> (map position input)
       frequencies
       (filter (comp odd? second))
       (map first)
       set))

(defn black-neighbor-count
  [black-tiles pos]
  (let [neighs (neighbors pos)]
    (count (filter some? (map black-tiles neighs)))))

(defn nearby-white-tiles
  [black-tiles]
  (set/difference (set (mapcat neighbors black-tiles)) black-tiles))

(defn flip-to-white?
  [black-tiles black-tile]
  (let [cnt (black-neighbor-count black-tiles black-tile)]
    (or (= cnt 0)
        (> cnt 2))))

(defn flip-to-black?
  [black-tiles white-tile]
  (= 2 (black-neighbor-count black-tiles white-tile)))

(defn evolve-tiles
  [black-tiles]
  (let [white-tiles (nearby-white-tiles black-tiles)
        to-remove (filter (partial flip-to-white? black-tiles) black-tiles)
        to-add    (filter (partial flip-to-black? black-tiles) white-tiles)]
    (-> black-tiles
        (set/union (set to-add))
        (set/difference (set to-remove)))))

(defn black-tiles-on-day
  [input day]
  (let [start (black-tiles input)]
    (nth (iterate evolve-tiles start) day)))

(defn day24-part1-soln
  []
  (count (black-tiles day24-input)))

(defn day24-part2-soln
  []
  (count (black-tiles-on-day day24-input 100)))