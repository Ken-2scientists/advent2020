(ns advent2020.day20
  (:require [clojure.string :as str]
            [advent-utils.ascii :as ascii]
            [advent-utils.core :as u]))

(def charmap  {\. 0 \# 1})
(def sea-monster-pattern
  (->> (ascii/ascii->map
        charmap
        ["                  # "
         "#    ##    ##    ###"
         " #  #  #  #  #  #   "])
       :grid
       (filter (comp some? second))
       (map first)))

(defn parse-tile
  [tile-str]
  (let [[header grid] (str/split tile-str #":\n")
        tile-id (read-string (subs header 5 9))]
    [tile-id (ascii/ascii->map
              charmap
              (str/split grid #"\n"))]))

(defn parse
  [input]
  (->> (str/split input #"\n\n")
       (map parse-tile)
       (into {})))

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

(defn fliph-edge
  [edges]
  (u/fmap #(if (#{:e :w} %)
             (opposite-dir %)
             %) edges))

(defn flipv-edge
  [edges]
  (u/fmap #(if (#{:n :s} %)
             (opposite-dir %)
             %) edges))

(defn rotate-edge
  [edges]
  (u/fmap {:n :e :e :s :s :w :w :n} edges))

(defn orient
  [current-edges desired-edges]
  (let [tiles (keys current-edges)
        deltas (->> (map (comp set vector)
                         (map current-edges tiles)
                         (map desired-edges tiles))
                    (filter #(> (count %) 1)))
        delta-count (count deltas)]
    (case delta-count
      0 [:no-op]
      1 (if (= #{:e :w} (first deltas))
          [:fliph]
          [:flipv])
      (case (first deltas)
        #{:e :w} [:fliph (orient (fliph-edge current-edges) desired-edges)]
        #{:n :s} [:flipv (orient (flipv-edge current-edges) desired-edges)]
        [:rotate (orient (rotate-edge current-edges) desired-edges)]))))

(defn simplify-edges
  [edges]
  (u/invert-map (u/fmap first edges)))

(defn tile-orientations
  [current desired]
  (let [c (u/fmap simplify-edges current)
        d (u/fmap simplify-edges desired)
        tiles (keys d)]
    (zipmap tiles
            (map (comp flatten orient) (map c tiles) (map d tiles)))))

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

(defn oriented
  [tile command]
  (case command
    :no-op tile
    :fliph (fliph tile)
    :flipv (flipv tile)
    :rotate (rotate tile)))

(defn corrected-tile
  [tiles orientations [[shift-x shift-y] tile-id]]
  (let [tile     (get tiles tile-id)
        commands (get orientations tile-id)]
    (->>
     (reduce oriented tile commands)
     trim-edge
     :grid
     (u/kmap (fn [[x y]]
               [(+ (* 8 shift-x) x -1)
                (+ (* 8 shift-y) y -1)])))))

(defn reassembled-image
  [tiles]
  (let [edge-matches  (matching-edges (tile-edge-map tiles))
        positions     (tile-positions edge-matches)
        desired-edges (desired-edges positions)
        orientations  (tile-orientations edge-matches desired-edges)
        do-everything (partial corrected-tile tiles orientations)
        width         (* 8 (:width positions))
        height        (* 8 (:height positions))]
    {:height height
     :width  width
     :grid (apply merge (map do-everything (:grid positions)))}))

(def all-orientation-cmds
  [[:no-op]
   [:rotate]
   [:rotate :rotate]
   [:rotate :rotate :rotate]
   [:fliph]
   [:fliph :rotate]
   [:fliph :rotate :rotate]
   [:fliph :rotate :rotate :rotate]])

(defn all-orientations
  [tile]
  (map (partial reduce oriented tile) all-orientation-cmds))

(defn is-sea-monster?
  [grid pos]
  (let [positions (map (partial map + pos) sea-monster-pattern)]
    (every? #(= 1 %) (map grid positions))))

(defn sea-monster-count
  [{:keys [width height grid]}]
  (->> (filter (partial is-sea-monster? grid)
               (for [x (range (- width 20))
                     y (range (- height 3))]
                 [x y]))
       count))

(defn sea-roughness
  [tiles]
  (let [image (reassembled-image tiles)
        ones  (count (filter #(= 1 (val %)) (:grid image)))
        sea-monsters (apply max (map sea-monster-count (all-orientations image)))]
    (- ones (* sea-monsters (count sea-monster-pattern)))))

(defn day20-part1-soln
  []
  (->> day20-input
       tile-edge-map
       matching-edges
       corners
       (reduce *)))

(defn day20-part2-soln
  []
  (sea-roughness day20-input))