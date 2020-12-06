(ns advent2020.day06
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day06-input (str/join "\n" (u/puzzle-input "day06-input.txt")))

(defn parse
  [input]
  (->> (str/split input #"\n\n")
       (map #(str/replace % "\n" " "))
       (map #(str/split % #"\ "))))

(defn unique-answers-in-group
  [group]
  (->> group
       (mapcat #(map identity %))
       set))

(defn common-answers-in-group
  [group]
  (->> group
       (map (comp set #(map identity %)))
       (apply set/intersection)))

(defn sum-counts
  [input count-fn]
  (->> (parse input)
       (map (comp count count-fn))
       (reduce +)))

(defn day06-part1-soln
  []
  (sum-counts day06-input unique-answers-in-group))

(defn day06-part2-soln
  []
  (sum-counts day06-input common-answers-in-group))