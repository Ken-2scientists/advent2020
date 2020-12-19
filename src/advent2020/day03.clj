(ns advent2020.day03
  (:require [advent-utils.ascii :as ascii]
            [advent-utils.core :as u]))

(def day03-input (u/puzzle-input "day03-input.txt"))

(defn forest-basemap
  [ascii-lines]
  (let [forest-mapping {\. :space
                        \# :tree}]
    (ascii/ascii->map forest-mapping ascii-lines)))

(defn get-position
  [{:keys [height width grid]} [x y]]
  (let [realx (mod x width)
        realy (mod y height)]
    (get grid [realx realy])))

(defn items-along-slope
  [{:keys [height] :as basemap} [right down]]
  (let [positions (for [step (range 0 (/ height down))]
                    [(* step right) (* step down)])]
    (map (partial get-position basemap) positions)))

(defn trees-along-slope
  [basemap slope]
  (->> (items-along-slope basemap slope)
       (filter #(= :tree %))
       count))

(defn trees-along-slopes
  [basemap slopes]
  (map (partial trees-along-slope basemap) slopes))

(defn day03-part1-soln
  []
  (let [basemap (forest-basemap day03-input)
        slope   [3 1]]
    (trees-along-slope basemap slope)))

(defn day03-part2-soln
  []
  (let [basemap (forest-basemap day03-input)
        slopes  [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (reduce * (trees-along-slopes basemap slopes))))