(ns advent2020.day11-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day11 :as t]))

(def day11-sample
  (t/ferry-seatmap
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

(deftest day11-part1-soln
  (testing "Reproduces the answer for day11, part1"
    (is (= 2222 (t/day11-part1-soln)))))

(deftest day11-part2-soln
  (testing "Reproduces the answer for day11, part2"
    (is (= 2032 (t/day11-part2-soln)))))