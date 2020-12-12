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

(def dirs
  (->> (for [y (range -1 2)
             x (range -1 2)]
         [x y])
       (filter #(not= [0 0] %))))

(defn valid-pos?
  [height width [x y]]
  (and (>= x 0)
       (< x width)
       (>= y 0)
       (< y height)))

(defn sightline
  [height width pos dir]
  (take-while (partial valid-pos? height width)
              (drop 1 (iterate #(mapv + dir %) pos))))

(defn seats
  [themap]
  (map first (filter #(= :seat (val %)) themap)))

(defn first-visible-seat
  [themap sightline]
  (->> sightline
       (filter #(= :seat (themap %)))
       first))

(defn first-visible-seats
  [{:keys [height width themap]} pos]
  (let [sightlines (->> (map (partial sightline height width pos) dirs)
                        (filter u/not-empty?))]
    (filter some?
            (map (partial first-visible-seat themap) sightlines))))

(first-visible-seats day11-sample [2 0])

(def themap (:themap day11-sample))

(seats themap)

(map (partial first-visible-seat themap)
     (filter u/not-empty?))

(seats (:themap day11-sample))

(defn visibility
  [{:keys [themap] :as seatmap}]
  (let [seats (seats themap)]
    (zipmap seats
            (map (partial first-visible-seats seatmap) seats))))

(defn rules2
  [themap visibility pos]
  (let [state (themap pos)
        neighbors (map themap (visibility pos))]
    (case state
      :seat (if (not (some #{:occupied} neighbors))
              :occupied
              :seat)
      :occupied (if (>= (count (filter #{:occupied} neighbors)) 5)
                  :seat
                  :occupied)
      state)))

(defn apply-rules-until-static2
  [rules {:keys [themap] :as seatmap}]
  (let [locs (seats themap)
        vis (visibility seatmap)]
    (loop [statemap themap]
      (let [nextmap (map (partial rules statemap vis) locs)]
        (if (= nextmap (vals statemap))
          nextmap
          (recur (zipmap locs nextmap)))))))

(->> (apply-rules-until-static2 rules2 day11-input)
     (filter #{:occupied})
     count)


(first-visible-seats day11-sample [8 7])
(neighbors day11-sample)

day11-sample

(defn visibile-seat-in-dir
  [themap dir pos])

(defn visible-seats
  [themap pos]
  (let []))