(ns advent2020.day11
  (:require [advent2020.lib.ascii :as ascii]
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

(def dirs
  (->> (for [y (range -1 2)
             x (range -1 2)]
         [x y])
       (filter #(not= [0 0] %))))

(defn seats
  [themap]
  (map first (filter #(= :seat (val %)) themap)))

(defn adjacent
  [pos]
  (map #(mapv + pos %) dirs))

(defn adjacency
  [{:keys [themap]}]
  (let [seats (seats themap)]
    (zipmap seats (map adjacent seats))))

(defn rules
  [limit adjacencies themap pos]
  (let [seat      (themap pos)
        neighbors (map themap (adjacencies pos))]
    (case seat
      :seat (if (not (some #{:occupied} neighbors))
              :occupied
              :seat)
      :occupied (if (>= (count (filter #{:occupied} neighbors)) limit)
                  :seat
                  :occupied)
      seat)))

(defn apply-rules-until-static
  [limit adjacency {:keys [themap] :as seatmap}]
  (let [adjacencies (adjacency seatmap)
        seats       (keys adjacencies)
        apply-rules (partial rules limit adjacencies)]
    (loop [statemap themap]
      (let [nextmap (map (partial apply-rules statemap) seats)]
        (if (= nextmap (vals statemap))
          nextmap
          (recur (zipmap seats nextmap)))))))

(defn occupied-seats-when-static
  [input limit adjacency]
  (->> input
       (apply-rules-until-static limit adjacency)
       (filter #{:occupied})
       count))

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

(defn first-visible-seat
  [themap sightline]
  (->> sightline
       (filter #(= :seat (themap %)))
       first))

(defn first-visible-seats
  [{:keys [height width themap]} pos]
  (let [sightlines (->> (map (partial sightline height width pos) dirs)
                        (filter u/not-empty?))]
    (filter some? (map (partial first-visible-seat themap) sightlines))))

(defn visibility
  [{:keys [themap] :as seatmap}]
  (let [seats (seats themap)]
    (zipmap seats (map (partial first-visible-seats seatmap) seats))))

(defn day11-part1-soln
  []
  (occupied-seats-when-static day11-input 4 adjacency))

(defn day11-part2-soln
  []
  (occupied-seats-when-static day11-input 5 visibility))