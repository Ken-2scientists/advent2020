(ns advent2020.day03-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day03 :as t]))

(def day03-sample "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def sample-basemap (t/forest-basemap (str/split day03-sample #"\n")))

(deftest items-along-slope
  (testing "Correctly determines the items along a slope"
    (is (= '(:space :space :tree :space :tree :tree :space :tree :tree :tree :tree)
           (t/items-along-slope sample-basemap [3 1])))))

(deftest day03-part1-soln
  (testing "Reproduces the answer for day03, part1"
    (is (= 191 (t/day03-part1-soln)))))

(deftest trees-along-slopes
  (testing "Can find the trees in the example along the given slopes"
    (is (= '(2 7 3 4 2)
           (t/trees-along-slopes sample-basemap [[1 1] [3 1] [5 1] [7 1] [1 2]])))))

(deftest day03-part2-soln
  (testing "Reproduces the answer for day03, part2"
    (is (= 1478615040 (t/day03-part2-soln)))))