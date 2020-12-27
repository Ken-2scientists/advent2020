(ns advent2020.day20
  (:require [clojure.string :as str]
            [advent-utils.ascii :as ascii]
            [advent-utils.core :as u]))

(defn parse-tile
  [tile-str]
  (let [[header grid] (str/split tile-str #":\n")
        tile-id (read-string (subs header 5 9))]
    [tile-id (ascii/ascii->map
              {\. 0 \# 1}
              (str/split grid #"\n"))]))

(defn parse
  [input]
  (->> (str/split input #"\n\n")
       (map parse-tile)
       (into {})))

(def day20-sample
  (parse
   "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."))

(def day20-input
  (->> (u/puzzle-input "day20-input.txt")
       (str/join "\n")
       parse))

(defn edge-val
  [grid edge-indices]
  (str/join (map grid edge-indices)))

(defn tile-edges
  [[tile-id {:keys [width height grid]}]]
  (let [edge-coords [(map vector (range width)        (repeat 0))
                     (map vector (range width)        (repeat (dec height)))
                     (map vector (repeat 0)           (range height))
                     (map vector (repeat (dec width)) (range height))]]
    (map vector
         [[tile-id :t] [tile-id :b] [tile-id :l] [tile-id :r]]
         (map (partial edge-val grid) edge-coords))))

(defn edge-compare
  [[id1 edge1] [id2 edge2]]
  (cond (= edge1 edge2) [[id1 id2] :match]
        (= edge1 (str/reverse edge2)) [[id1 id2] :rev-match]))

(defn edge-matches
  [all-edges [[tile-id _] :as edge]]
  (let [other-edges (filter #(not= tile-id (first (key %))) all-edges)]
    (map (partial edge-compare edge) other-edges)))

(defn all-edge-matches
  [input]
  (let [edges (into {} (mapcat tile-edges input))]
    (into {} (filter some? (mapcat (partial edge-matches edges) edges)))))

(defn match-counts-per-tile
  [matches]
  (u/fmap count (group-by (comp first ffirst) matches)))

(defn corners
  [input]
  (let [match-counts (match-counts-per-tile (all-edge-matches input))]
    (->> (filter #(= 2 (second %)) match-counts)
         (map first))))

(defn day20-part1-soln
  []
  (reduce * (corners day20-input)))