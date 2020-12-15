(ns advent2020.day13
  (:require [clojure.string :as str]
            ;[advent2020.lib.math :as math]
            [advent2020.lib.utils :as u]))

(defn parse
  [input]
  (let [[time bus-str] input
        buses (->> (str/split bus-str #",")
                   (map read-string))]
    {:time (read-string time)
     :buses buses}))

(def day13-sample
  (parse
   (str/split
    "939
7,13,x,x,59,x,31,19" #"\n")))

(def day13-input (parse (u/puzzle-input "day13-input.txt")))

(defn earliest-bus-and-wait-time
  [{:keys [time buses]}]
  (let [valid-buses    (filter number? buses)
        next-bus-times (zipmap valid-buses
                               (->> (map #(inc (quot time %)) valid-buses)
                                    (map * valid-buses)))
        [bus-id bus-time] (apply min-key val next-bus-times)]
    [bus-id (- bus-time time)]))

(defn bus-id-by-wait-time
  [input]
  (reduce * (earliest-bus-and-wait-time input)))

(defn day13-part1-soln
  []
  (bus-id-by-wait-time day13-input))