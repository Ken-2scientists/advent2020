(ns advent2020.day03
  (:require [advent2020.lib.ascii :as ascii]
            [advent2020.lib.utils :as u]))

(def day03-sample "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def day03-input (u/puzzle-input "day03-input.txt"))

(def forest-mapping
  {\. :space
   \# :tree})

(defn forest-base-map
  [ascii-lines]
  (let [height (count ascii-lines)
        width  (count (first ascii-lines))]
    {:height height
     :width  width
     :themap (ascii/ascii->map forest-mapping ascii-lines)}))

(defn get-position
  [{:keys [height width themap]} [x y]]
  (let [realx (mod x width)
        realy (mod y height)]
    (get themap [realx realy])))

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

(defn day03-part1-soln
  []
  (let [basemap (forest-base-map day03-input)]
    (trees-along-slope basemap [3 1])))

(defn trees-along-slopes
  [basemap slopes]
  (map (partial trees-along-slope basemap) slopes))

(defn test1
  []
  (let [basemap (forest-base-map (clojure.string/split day03-sample #"\n"))]
    (trees-along-slopes basemap [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(defn day03-part2-soln
  []
  (let [basemap (forest-base-map day03-input)]
    (reduce * (trees-along-slopes basemap [[1 1] [3 1] [5 1] [7 1] [1 2]]))))