(ns advent2020.day02
  (:require [clojure.string :as str]
            [advent-utils.core :as u]))

(defn parse
  [input]
  (let [segments  (str/split input #" ")
        limits    (str/split (first segments) #"-")
        character (first (second segments))
        password  (last segments)]
    {:min (Integer/parseInt (first limits))
     :max (Integer/parseInt (second limits))
     :char character
     :pass password}))

(def day02-input
  (map parse (u/puzzle-input "day02-input.txt")))

(defn part1-valid?
  [{:keys [min max char pass]}]
  (let [count (get (frequencies pass) char)]
    (and
     (not (nil? count))
     (>= count min)
     (<= count max))))

(defn part2-valid?
  [{:keys [min max char pass]}]
  (let [a (nth pass (dec min))
        b (nth pass (dec max))]
    (or (and (= a char)
             (not (= b char)))
        (and (= b char)
             (not (= a char))))))

(defn day02-part1-soln
  []
  (count (filter part1-valid? day02-input)))

(defn day02-part2-soln
  []
  (count (filter part2-valid? day02-input)))