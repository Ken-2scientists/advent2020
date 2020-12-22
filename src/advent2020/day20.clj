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

(defn edges
  [{:keys [grid]}]
  (let [edge-coords [(map vector (repeat 0) (range 10))
                     (map vector (repeat 9) (range 10))
                     (map vector (range 10) (repeat 0))
                     (map vector (range 10) (repeat 9))]]
    (map (partial map grid) edge-coords)))

(def day20-input
  (->> (u/puzzle-input "day20-input.txt")
       (str/join "\n")
       parse))

(defn edge-compare
  [edge1 edge2]
  (or (= edge1 edge2)
      (= edge1 (reverse edge2))))

(defn edge-match-count
  [all-edges edge]
  (->> all-edges
       (map (partial edge-compare edge))
       (filter identity)
       count))

(defn foo
  [all-tile-edges [tile-id tile-edges]]
  (let [other-edges (->> (u/without-keys all-tile-edges #{tile-id})
                         vals
                         (apply concat))]
    [tile-id
     (->> tile-edges
          (map (partial edge-match-count other-edges))
          (reduce +))]))

(defn tile-matches
  [input]
  (let [tile-edges (u/fmap edges input)]
    (into {} (map (partial foo tile-edges) tile-edges))))

(tile-matches day20-sample)