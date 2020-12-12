(ns advent2020.day12-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day12 :as t]))

(def day12-sample
  (map t/parse
       (str/split
        "F10
N3
F7
R90
F11" #"\n")))

(deftest part1-sample
  (testing "Reproduces the answer for part1 on the sample input"
    (is (= 25 (t/final-distance day12-sample t/exec-cmd)))))

(deftest part2-sample
  (testing "Reproduces the answer for part2 on the sample input"
    (is (= 286 (t/final-distance day12-sample t/exec-cmd2)))))

(deftest day12-part1-soln
  (testing "Reproduces the answer for day12, part1"
    (is (= 1533 (t/day12-part1-soln)))))

(deftest day12-part2-soln
  (testing "Reproduces the answer for day12, part2"
    (is (= 25235 (t/day12-part2-soln)))))