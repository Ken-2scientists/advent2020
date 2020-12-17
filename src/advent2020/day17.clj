(ns advent2020.day17
  (:require [clojure.string :as str]
            [advent2020.lib.ascii :as ascii]
            [advent2020.lib.utils :as u]))

(defn twod->threed
  [[x y]]
  [x y 0])

(defn initial-slice
  [ascii-lines]
  (let [char-map {\. :inactive
                  \# :active}
        height   (count ascii-lines)
        width    (count (first ascii-lines))]
    {:height height
     :width  width
     :themap (u/kmap twod->threed
                     (ascii/ascii->map char-map ascii-lines))}))

(def day17-sample
  (initial-slice
   (str/split
    ".#.
..#
###" #"\n")))

(def day17-input (initial-slice (u/puzzle-input "day17-input.txt")))

(def adjacent-dirs-3d
  (->> (for [z (range -1 2)
             y (range -1 2)
             x (range -1 2)]
         [x y z])
       (filter #(not= [0 0 0] %))))

(def adjacent-dirs-4d
  (->> (for [w (range -1 2)
             z (range -1 2)
             y (range -1 2)
             x (range -1 2)]
         [x y z w])
       (filter #(not= [0 0 0 0] %))))

(defn adjacent-3d
  [pos]
  (map #(mapv + pos %) adjacent-dirs-3d))

(defn adjacent-4d
  [pos]
  (map #(mapv + pos %) adjacent-dirs-4d))

(defn rules
  [adjacency themap pos]
  (let [state            (get themap pos :inactive)
        active-neighbors (->> (adjacency pos)
                              (map themap)
                              (filter #{:active})
                              count)]
    (case state
      ;;  "If a cube is active 
      ;;  and exactly 2 or 3 of its neighbors are also active, 
      ;;  the cube remains active. 
      ;;  Otherwise, the cube becomes inactive.
      :active (if (or (= 2 active-neighbors)
                      (= 3 active-neighbors))
                :active
                :inactive)
      ;;  If a cube is inactive 
      ;;  but exactly 3 of its neighbors are active, 
      ;;  the cube becomes active. 
      ;;  Otherwise, the cube remains inactive
      :inactive (if (= 3 active-neighbors)
                  :active
                  :inactive))))

(def rules-3d (partial rules adjacent-3d))
(def rules-4d (partial rules adjacent-4d))

;; TODO - come back and make this only expand from
;; known points instead of the entire grid
(defn scan-space-3d
  [height width limit]
  (for [z (range (- limit) (inc limit))
        y (range (- limit) (+ height limit))
        x (range (- limit) (+ width limit))]
    [x y z]))

;; TODO - come back and make this only expand from
;; known points instead of the entire grid
(defn scan-space-4d
  [height width limit]
  (for [w (range (- limit) (inc limit))
        z (range (- limit) (inc limit))
        y (range (- limit) (+ height limit))
        x (range (- limit) (+ width limit))]
    [x y z w]))

(defn evolve-3d
  [{:keys [height width themap]} limit]
  (loop [statemap themap cnt 0]
    (if (= limit cnt)
      statemap
      (let [locs (scan-space-3d height width limit)
            newvals (map (partial rules-3d statemap) locs)]
        (recur (zipmap locs newvals) (inc cnt))))))

(defn evolve-4d
  [{:keys [height width themap]} limit]
  (loop [statemap themap cnt 0]
    (if (= limit cnt)
      statemap
      (let [locs (scan-space-4d height width limit)
            newvals (map (partial rules-4d statemap) locs)]
        (recur (zipmap locs newvals) (inc cnt))))))

(defn day17-part1-soln
  []
  (->> (evolve-3d day17-input 6)
       vals
       (filter #{:active})
       count))

(defn promote-to-4d
  [{:keys [themap] :as input}]
  (let [add-dim (fn [[x y z]] [x y z 0])]
    (assoc input :themap (u/kmap add-dim themap))))

(defn day17-part2-soln
  []
  (->> (evolve-4d (promote-to-4d day17-input) 6)
       vals
       (filter #{:active})
       count))