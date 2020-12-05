(ns advent2020.day05-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day05 :as t]))

(def day05-sample1 "FBFBBFFRLR")
(def day05-sample2 "BFFFBBFRRR")
(def day05-sample3 "FFFBBBFRRR")
(def day05-sample4 "BBFFBBFRLL")

(deftest seatid-parser
  (testing "Correctly parses the seat-id from the string")
  (is (= 357 (t/seat-id day05-sample1)))
  (is (= 567 (t/seat-id day05-sample2)))
  (is (= 119 (t/seat-id day05-sample3)))
  (is (= 820 (t/seat-id day05-sample4))))

(deftest day05-part1-soln
  (testing "Reproduces the answer for day05, part1"
    (is (= 858 (t/day05-part1-soln)))))

(deftest day05-part2-soln
  (testing "Reproduces the answer for day05, part2"
    (is (= 557 (t/day05-part2-soln)))))