(ns advent2020.day13
  (:require [clojure.string :as str]
            [advent-utils.math :as math]
            [advent-utils.core :as u]))

(defn parse
  [input]
  (let [[time bus-str] input
        buses (->> (str/split bus-str #",")
                   (map read-string))]
    {:time (read-string time)
     :buses buses}))

(def day13-input (parse (u/puzzle-input "day13-input.txt")))

(defn next-bus
  [time bus]
  (* bus (inc (quot time bus))))

(defn earliest-bus-and-wait-time
  [{:keys [time buses]}]
  (let [valid-buses    (filter number? buses)
        next-bus-times (zipmap valid-buses (map (partial next-bus time) valid-buses))
        [bus-id bus-time] (apply min-key val next-bus-times)]
    [bus-id (- bus-time time)]))

(defn bus-id-by-wait-time
  [input]
  (reduce * (earliest-bus-and-wait-time input)))

(defn day13-part1-soln
  []
  (bus-id-by-wait-time day13-input))

(defn bus-terms
  "Computes the coefficients [a, b] of a linear formula f = a*x + b
   that determines at what times bus will appear a time shift later
   the main bus. Relies upon main-bus and bus being relatively prime."
  [main-bus [bus shift]]
  [bus (math/mod-div bus (- shift) main-bus)])

(defn earliest-match
  "Computes the coefficients [a b] of a linear formula f = a*x +b
   that correspond with times when the constraints represented by
   the two inputs can both be satisfied.
   
   The logic is that we're looking for values such that
     a1*x + b1 = a2*y + b2
   Solving for y:
     a2*y = (b1 - b2) + a1*x
   Take the mod of a1 on both sides:
     (a2*y) mod a1 = (b1 - b2) mod a1   (using (a1*x) mod a1 = 0)
   So then we can say:
     y = ((b1 - b2) / a2) mod a1
   The first value that represents a match between the two equations is:
     m = (a2 * y + b2)
   Because a1 and a2 are relatively prime, this match will recur with a
   frequency of a1*a2.
   "
  [[a1 b1] [a2 b2]]
  (let [y (math/mod-div a1 (- b1 b2) a2)
        m (+ (* a2 y) b2)]
    [(* a1 a2) m]))

(defn earliest-consecutive-buses
  [{:keys [buses]}]
  (let [main-bus  (first buses)
        positions (->> (map-indexed (fn [i v] [v i]) buses)
                       (filter #(not= 'x (first %)))
                       rest
                       (sort-by first >))
        offsets (map (partial bus-terms main-bus) positions)]
    (* main-bus (second (reduce earliest-match offsets)))))

(defn day13-part2-soln
  []
  (earliest-consecutive-buses day13-input))