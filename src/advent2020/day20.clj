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

(defn edge-hash
  [grid edge-indices]
  (let [hash (str/join (map grid edge-indices))]
    (if (>= (compare hash (str/reverse hash)) 0)
      hash
      (str/reverse hash))))

(defn tile-edges
  [[tile-id {:keys [width height grid]}]]
  (let [edge-coords [(map vector (range width)        (repeat 0))
                     (map vector (range width)        (repeat (dec height)))
                     (map vector (repeat 0)           (range height))
                     (map vector (repeat (dec width)) (range height))]]
    (map vector
         [[tile-id :t] [tile-id :b] [tile-id :l] [tile-id :r]]
         (map (partial edge-hash grid) edge-coords))))

(defn tile-edge-map
  [tiles]
  (into {} (mapcat tile-edges tiles)))

(defn edge-pairs
  [tile-edge-map]
  (let [half-matches (->> tile-edge-map
                          (group-by second)
                          (filter #(= 2 (count (val %))))
                          vals
                          (mapv (partial mapv first)))
        all-matches (concat half-matches (mapv (comp vec reverse) half-matches))]
    (into {} all-matches)))

(defn matching-edge-count
  [tiles]
  (->> tiles
       tile-edge-map
       edge-pairs
       (group-by ffirst)
       (u/fmap count)))

(defn corners
  [tiles]
  (let [match-counts (matching-edge-count tiles)]
    (->> (filter #(= 2 (second %)) match-counts)
         (map first))))

(defn day20-part1-soln
  []
  (reduce * (corners day20-input)))

(defn fliph
  [{:keys [width grid] :as tile}]
  (assoc tile :grid (u/kmap (fn [[x y]] [(- (dec width) x) y]) grid)))

(defn flipv
  [{:keys [height grid] :as tile}]
  (assoc tile :grid (u/kmap (fn [[x y]] [x (- (dec height) y)]) grid)))

(defn rotate
  [{:keys [width height grid]}]
  (let [mapping (fn [[x y]] [(- (dec height) y) x])]
    {:width height
     :height width
     :grid (u/kmap mapping grid)}))

;; (filter #(= 1951 ((comp first ffirst) %)) (all-edge-matches day20-sample))
;; 
(def grid-sample
  {:width 2
   :height 4
   :grid {[0 0] :a [1 0] :b
          [0 1] :c [1 1] :d
          [0 2] :e [1 2] :f
          [0 3] :g [1 3] :h}})

;; (defn tile-positions-and-orientations
;;   [input]
;;   (let [corners (corners input)
;;         upper-left (first corners)]))

(defn is-edge?
  [width height [x y]]
  (or (= 0 x)
      (= 0 y)
      (= (dec width) x)
      (= (dec height) y)))

(defn trim-edge
  [{:keys [width height grid]}]
  (let [is-not-edge? (complement (partial is-edge? width height))]
    {:width (- width 2)
     :height (- height 2)
     :grid (into {} (filter (comp is-not-edge? key) grid))}))

(defn combine
  [])

;; (defn final-image
;;   [tiles]
;;   (let [[orientations offsets] (tile-positions-and-orientations tiles)
;;         corrected-tiles (->> tiles
;;                              (u/fmap trim-edge)
;;                              (map orientations)
;;                              (map offsets)
;;                              combine)]))

