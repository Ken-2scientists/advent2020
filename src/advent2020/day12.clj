(ns advent2020.day12
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(defn parse
  [cmd]
  (let [dir (subs cmd 0 1)
        amount (read-string (subs cmd 1))]
    [dir amount]))

(def day12-input (map parse (u/puzzle-input "day12-input.txt")))

(def day12-sample
  (map parse
       (str/split
        "F10
N3
F7
R90
F11" #"\n")))

(defn rotate
  [dir heading amount]
  (let [dirs (if (= dir :right)
               [:east :south :west :north]
               [:east :north :west :south])
        start (u/index-of heading dirs)
        shift (case amount
                90  1
                180 2
                270 3)]
    (first (drop (+ start shift) (cycle dirs)))))

(defn forward
  [[x y] heading amount]
  (case heading
    :north [x (+ y amount)]
    :south [x (- y amount)]
    :east  [(+ x amount) y]
    :west  [(- x amount) y]))

(defn exec-cmd
  [{:keys [pos heading] :as state} [dir amount]]
  (let [[x y] pos]
    (case dir
      "N" (assoc state :pos [x (+ y amount)])
      "S" (assoc state :pos [x (- y amount)])
      "E" (assoc state :pos [(+ x amount) y])
      "W" (assoc state :pos [(- x amount) y])
      "L" (assoc state :heading (rotate :left heading amount))
      "R" (assoc state :heading (rotate :right heading amount))
      "F" (assoc state :pos (forward pos heading amount)))))

(def start {:pos [0 0] :waypoint [10 1] :heading :east})

(defn day12-part1-soln
  []
  (->> day12-input
       (reduce exec-cmd start)
       :pos
       (map #(Math/abs %))
       (reduce +)))

(defn rotate2
  [dir [x y] amount]
  (case dir
    :right (case amount
             90 [y (- x)]
             180 [(- x) (- y)]
             270 [(- y) x])
    :left (case amount
            90 [(- y) x]
            180 [(- x) (- y)]
            270 [y (- x)])))

(defn forward2
  [pos waypoint amount]
  (vec (map + pos (map (partial * amount) waypoint))))

(defn exec-cmd2
  [{:keys [pos waypoint] :as state} [dir amount]]
  (let [[x y] waypoint]
    (case dir
      "N" (assoc state :waypoint [x (+ y amount)])
      "S" (assoc state :waypoint [x (- y amount)])
      "E" (assoc state :waypoint [(+ x amount) y])
      "W" (assoc state :waypoint [(- x amount) y])
      "L" (assoc state :waypoint (rotate2 :left waypoint amount))
      "R" (assoc state :waypoint (rotate2 :right waypoint amount))
      "F" (assoc state :pos (forward2 pos waypoint amount)))))

(reduce exec-cmd2 start (take 5 day12-sample))

(defn day12-part1-soln
  []
  (->> day12-input
       (reduce exec-cmd2 start)
       :pos
       (map #(Math/abs %))
       (reduce +)))