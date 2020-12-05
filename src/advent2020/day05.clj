(ns advent2020.day05
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day05-input (u/puzzle-input "day05-input.txt"))

(defn seat-id
  [seat]
  (let [bit-string (str/replace seat
                                #"F|B|L|R"
                                {"F" "0"
                                 "B" "1"
                                 "L" "0"
                                 "R" "1"})]
    (read-string (str "2r" bit-string))))

(defn day05-part1-soln
  []
  (apply max (map seat-id day05-input)))

(defn day05-part2-soln
  []
  (let [seat-ids (sort (map seat-id day05-input))]
    (->> (map vector seat-ids (rest seat-ids))
         (filter #(= 2 (- (second %) (first %))))
         ffirst
         inc)))