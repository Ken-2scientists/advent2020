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
  (let [edge-coords [(map vector (range width)        (repeat 0))
                     (map vector (range width)        (repeat (dec height)))
                     (map vector (repeat 0)           (range height))
                     (map vector (repeat (dec width)) (range height))]]
    [tile-id (zipmap '(:n :s :w :e) (map (partial edge grid) edge-coords))]))

(defn tile-edge-map
  [tiles]
  (into {} (map tile-edges tiles)))

(defn index-edge
  [[tile-id edges]]
  (map (fn [[edge-id edge]]
         [(edge-hash edge) [tile-id edge-id]]) edges))

(defn matching-edges
  [tile-edge-map]
  (let [half-matches (->> tile-edge-map
                          (mapcat index-edge)
                          (group-by first)
                          (filter #(= 2 (count (val %))))
                          (u/fmap (partial map second))
                          vals)
        all-matches (concat half-matches (mapv (comp vec reverse) half-matches))
        edge-map    (->> (map (fn [[[a b] [c d]]] [a [b [c d]]]) all-matches)
                         (group-by first)
                         (u/fmap #(into {} (mapv second %))))]
    edge-map))

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
   :e w
   :s s
   :w e})

(defn flipv
  [{:keys [height grid] :as tile}]
  (assoc tile :grid (u/kmap (fn [[x y]] [x (- (dec height) y)]) grid)))

(defn flipv-edge
  [{:keys [n e s w]}]
  {:n s
   :e e
   :s n
   :w w})

(defn rotate
  [{:keys [width height grid]}]
  (let [mapping (fn [[x y]] [(- (dec height) y) x])]
    {:width height
     :height width
     :grid (u/kmap mapping grid)}))

(defn rotate-edge
  [{:keys [n e s w]}]
  {:n (str/reverse w)
   :e n
   :s (str/reverse e)
   :w s})


(def opposite-dir {:n :s :s :n :e :w :w :e})
(defn orthogonal-dir
  [edge-matches [tile dir]]
  (let [options (keys (edge-matches tile))
        orth-dirs (case dir
                    :n #{:e :w}
                    :s #{:e :w}
                    :e #{:n :s}
                    :w #{:n :s})]
    [tile (some orth-dirs options)]))

(defn next-tile
  [edge-matches [tile dir]]
  (get-in edge-matches [tile (opposite-dir dir)]))

(defn tile-row
  [edge-matches [start dir]]
  (take-while some? (iterate (partial next-tile edge-matches) [start (opposite-dir dir)])))

(defn tile-positions
  [edge-matches]
  (let [start        (first (corners edge-matches))
        start-dir    (first (keys (edge-matches start)))
        edge         (map (partial orthogonal-dir edge-matches)
                          (tile-row edge-matches [start start-dir]))
        rows         (count edge)
        ordered-tiles (->> (mapcat (partial tile-row edge-matches) edge)
                           (map first))
        cols         (/ (count ordered-tiles) rows)]
    {:width cols
     :height rows
     :grid (zipmap (for [y (range rows) x (range cols)] [x y])
                   ordered-tiles)}))

(def neighbors {[0 -1] :n
                [1 0] :e
                [0 1] :s
                [-1 0] :w})

(defn desired-edge
  [grid valid-locs [pos tile]]
  (let [n-pos  (->> (map (fn [[loc dir]]
                           [(map + pos loc) dir]) neighbors)
                    (filter (comp valid-locs first)))]
    {tile (into {} (mapv (fn [[loc dir]]
                           [dir [(grid loc) (opposite-dir dir)]])
                         n-pos))}))

(defn desired-edges
  [{:keys [grid]}]
  (let [valid-locs (set (keys grid))]
    (apply merge (map (partial desired-edge grid valid-locs) grid))))

(def baz (desired-edges bar))
baz

(def foo {2729 {1427 :e, 1951 :s, 2971 :n}, 1171 {2473 :n, 1489 :e}, 2971 {1489 :e, 2729 :s}, 2311 {1951 :w, 3079 :e, 1427 :n}, 1489 {1427 :s, 2971 :w, 1171 :e}, 1427 {2473 :e, 2729 :w, 1489 :n, 2311 :s}, 3079 {2473 :s, 2311 :w}, 2473 {1171 :w, 3079 :e, 1427 :s}, 1951 {2311 :e, 2729 :n}})
(def bar {2729 {2971 :n, 1951 :s, 1427 :w}, 1171 {1489 :e, 2473 :s}, 2971 {2729 :s, 1489 :w}, 2311 {1427 :n, 1951 :e, 3079 :w}, 1489 {2971 :e, 1427 :s, 1171 :w}, 1427 {1489 :n, 2729 :e, 2311 :s, 2473 :w}, 3079 {2473 :n, 2311 :e}, 2473 {1171 :n, 1427 :e, 3079 :s}, 1951 {2729 :n, 2311 :w}})

(defn orient
  [current-edges desired-edges]
  (let [tiles (keys current-edges)
        deltas (->> (map (comp set vector)
                         (map current-edges tiles)
                         (map desired-edges tiles))
                    (filter #(> 1 (count %))))]
    (if (empty? deltas)
      [:no-op]
      deltas)))

(orient (first (vals foo)) (first (vals bar)))

(defn simplify-edges
  [edges]
  (u/invert-map (u/fmap first edges)))

(defn orient-tiles
  [current desired]
  (let [c (u/fmap simplify-edges current)
        d (u/fmap simplify-edges desired)
        tiles (keys d)]
    (map orient (map c tiles) (map d tiles))))

((u/fmap simplify-edges foo) 2729)
((u/fmap simplify-edges baz) 2729)


(map (comp set vector)
     (map {1427 :e 1951 :s 2971 :n} [1427 1951 2971])
     (map {2971 :n 1951 :s 1427 :w} [1427 1951 2971]))

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



;; (defn final-image
;;   [tiles]
;;   (let [[orientations offsets] (tile-positions-and-orientations tiles)
;;         corrected-tiles (->> tiles
;;                              (u/fmap trim-edge)
;;                              (map orientations)
;;                              (map offsets)
;;                              combine)]))

