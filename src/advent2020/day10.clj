(ns advent2020.day10
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day10-input (map read-string (u/puzzle-input "day10-input.txt")))

(def day10-sample
  (map read-string
       (str/split
        "16
10
15
5
1
11
7
19
6
12
4" #"\n")))

(def day10-sample2
  (map read-string
       (str/split
        "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3" #"\n")))

(sort (conj day10-sample 0))
(rest (sort (conj day10-sample 0)))

(defn freq-steps
  [jolts]
  (let [l (sort (conj jolts 0))
        fs (frequencies (map - (rest l) l))]
    [(get fs 1)
     (inc (get fs 3))]))

(defn day10-part1-soln
  []
  (let [freqs (freq-steps day10-input)]
    (reduce * freqs)))

(defn combo-mapping
  [run-size]
  (case run-size
    2 2
    3 4
    4 7))

(defn combination-count
  [jolts]
  (let [l (sort (conj jolts 0))
        runs (-> (map - (rest l) l)
                 str/join
                 (str/split #"3"))]
    (->> (map count runs)
         (filter #(> % 1))
         (map combo-mapping)
         (reduce *))))

(defn day10-part2-soln
  []
  (combination-count day10-input))