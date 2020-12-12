(ns advent2020.day11
  (:require [clojure.string :as str]
            [advent2020.lib.ascii :as ascii]
            [advent2020.lib.utils :as u]))

(defn ferry-seatmap
  [ascii-lines]
  (let [seat-mapping {\. :space
                      \# :occupied
                      \L :seat}
        height       (count ascii-lines)
        width        (count (first ascii-lines))]
    {:height height
     :width  width
     :themap (ascii/ascii->map seat-mapping ascii-lines)}))

(def day11-input (ferry-seatmap (u/puzzle-input "day11-input.txt")))

(def day11-sample
  (ferry-seatmap
   (str/split
    "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL" #"\n")))

(defn adjacent
  [[x y]]
  (->> (for [yy (range (dec y) (+ y 2))
             xx (range (dec x) (+ x 2))]
         [xx yy])
       (filter #(not= [x y] %))))

(defn rules
  [themap pos]
  (let [state (themap pos)
        neighbors (map themap (adjacent pos))]
    (case state
      :seat (if (not (some #{:occupied} neighbors))
              :occupied
              :seat)
      :occupied (if (>= (count (filter #{:occupied} neighbors)) 4)
                  :seat
                  :occupied)
      state)))

(defn apply-rules-until-static
  [rules themap]
  (let [locs (keys themap)]
    (loop [statemap themap]
      (let [nextmap (map (partial rules statemap) locs)]
        (if (= nextmap (vals statemap))
          nextmap
          (recur (zipmap locs nextmap)))))))

(defn day11-part1-soln
  []
  (->> (day11-input :themap)
       (apply-rules-until-static rules)
       (filter #{:occupied})
       count))