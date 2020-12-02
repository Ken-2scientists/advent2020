(ns advent2020.day02
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day02-sample ["1-3 a: abcde"
                   "1-3 b: cdefg"
                   "2-9 c: ccccccccc"])

(defn parse
  [input]
  (let [segments (str/split input #" ")
        limits (str/split (first segments) #"-")
        character (first (second segments))
        password (last segments)]
    {:min (Integer/parseInt (first limits))
     :max (Integer/parseInt (second limits))
     :char character
     :pass password}))

(def day02-input
  (map parse (u/puzzle-input "day02-input.txt")))

(defn valid?
  [{:keys [min max char pass]}]
  (let [count (get (frequencies pass) char)]
    (and
     (not (nil? count))
     (>= count min)
     (<= count max))))

(defn day02-part1-soln
  []
  (count (filter valid? day02-input)))

(comment
  (parse (first day02-sample))
  (valid? (parse (first day02-sample)))
  (parse (second day02-sample))
  (valid? (parse (second day02-sample)))
  (valid? (parse (last day02-sample)))

  (take 3 day02-input)
  (frequencies "abcdefab"))