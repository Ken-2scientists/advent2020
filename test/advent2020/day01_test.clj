(ns advent2020.day01-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day01 :as t]))

(def day01-example [1721 979 366 299 675 1456])

(deftest find-pairs-that-sum
  (testing "Finds pair in a list that sums to 2020"
    (is (= '(299 1721) (t/find-pair-that-sums-to-total 2020 day01-example)))))

(deftest day01-part1-soln
  (testing "Reproduces the answer for day01, part1"
    (is (= 989824 (t/day01-part1-soln)))))

(deftest find-triples-that-sum
  (testing "Finds a triple in a list that sums to 2020"
    (is (= '(979 366 675) (t/find-triple-that-sums-to-total 2020 day01-example)))))

(deftest day01-part2-soln
  (testing "Reproduces the answer for day01, part2"
    (is (= 66432240 (t/day02-part2-soln)))))