(ns advent2020.day20
  (:require [clojure.set :as set]
            [clojure.string :as str]
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

(defn edge
  [grid edge-indices]
  (str/join (map grid edge-indices)))

(defn edge-hash
  [edge]
  (if (>= (compare edge (str/reverse edge)) 0)
    edge
    (str/reverse edge)))

(defn tile-edges
  [[tile-id {:keys [width height grid]}]]
  (let [edge-coords [(map vector (range width)             (repeat 0))
                     (map vector (repeat (dec width))      (range height))
                     (map vector (range (dec width) -1 -1) (repeat (dec height)))
                     (map vector (repeat 0)                (range (dec height) -1 -1))]]
    [tile-id (zipmap '(:n :e :s :w) (map (partial edge grid) edge-coords))]))

(defn tile-edge-map
  [tiles]
  (into {} (map tile-edges tiles)))

(defn index-edge
  [[tile-id edges]]
  (map (fn [[edge-id edge]]
         [[tile-id edge-id] (edge-hash edge)]) edges))

(defn edge-pairs
  [tile-edge-map]
  (let [half-matches (->> tile-edge-map
                          (mapcat index-edge)
                          (into {})
                          (group-by second)
                          (filter #(= 2 (count (val %))))
                          vals
                          (mapv (partial mapv first)))
        all-matches (concat half-matches (mapv (comp vec reverse) half-matches))]
    (into {} all-matches)))

(defn matching-edges
  [tile-edge-map]
  (->> tile-edge-map
       edge-pairs
       (group-by ffirst)))

(defn corners
  [matching-edges]
  (let [match-counts (u/fmap count matching-edges)]
    (->> (filter #(= 2 (second %)) match-counts)
         (map first))))

(defn day20-part1-soln
  []
  (->> day20-input
       tile-edge-map
       matching-edges
       corners
       (reduce *)))

(defn fliph
  [{:keys [width grid] :as tile}]
  (assoc tile :grid (u/kmap (fn [[x y]] [(- (dec width) x) y]) grid)))

(defn fliph-edge
  [{:keys [n e s w]}]
  {:n n
   :e (str/reverse w)
   :s s
   :w (str/reverse e)})

(defn flipv
  [{:keys [height grid] :as tile}]
  (assoc tile :grid (u/kmap (fn [[x y]] [x (- (dec height) y)]) grid)))

(defn flipv-edge
  [{:keys [n e s w]}]
  {:n (str/reverse s)
   :e e
   :s (str/reverse n)
   :w w})

(defn rotate
  [{:keys [width height grid]}]
  (let [mapping (fn [[x y]] [(- (dec height) y) x])]
    {:width height
     :height width
     :grid (u/kmap mapping grid)}))

(defn rotate-edge
  [edge]
  (set/rename-keys edge {:n :e :e :s :s :w :w :n}))

;; (filter #(= 1951 ((comp first ffirst) %)) (all-edge-matches day20-sample))
;; 
(def grid-sample
  {:width 2
   :height 4
   :grid {[0 0] :a [1 0] :b
          [0 1] :c [1 1] :d
          [0 2] :e [1 2] :f
          [0 3] :g [1 3] :h}})

(defn orient
  [desired current])

(defn first-corner-orientation
  [as-is]
  (case as-is
    #{:e :s} [:no-op] nil
    #{:s :w} [:rotate 3]
    #{:w :n} [:rotate 2]
    #{:n :e} [:rotate 1]))

(defn tile-orientations
  [tiles]
  (let [tile-edge-map (tile-edge-map tiles)
        matching-edges (matching-edges tile-edge-map)
        corners (corners matching-edges)
        upper-left (first corners)]
    (set (map (comp second first) (get matching-edges upper-left)))))

(tile-orientations day20-sample)

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

