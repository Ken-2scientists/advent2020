(ns advent2020.day18-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day18 :as t]))

(def day18-sample1 "1 + 2 * 3 + 4 * 5 + 6")
(def day18-sample2 "1 + (2 * 3) + (4 * (5 + 6))")
(def day18-sample3 "2 * 3 + (4 * 5)")
(def day18-sample4 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
(def day18-sample5
  "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(def day18-sample6
  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(deftest infix-test
  (testing "Can correctly compute infix expressions"
    (is (= 71    (t/infix (t/parse day18-sample1))))
    (is (= 51    (t/infix (t/parse day18-sample2))))
    (is (= 26    (t/infix (t/parse day18-sample3))))
    (is (= 437   (t/infix (t/parse day18-sample4))))
    (is (= 12240 (t/infix (t/parse day18-sample5))))
    (is (= 13632 (t/infix (t/parse day18-sample6))))))

(deftest infix-test2
  (testing "Infix expressions with operator precedence"
    (is (= 231    (t/infix (t/parse2 day18-sample1))))
    (is (= 51     (t/infix (t/parse2 day18-sample2))))
    (is (= 46     (t/infix (t/parse2 day18-sample3))))
    (is (= 1445   (t/infix (t/parse2 day18-sample4))))
    (is (= 669060 (t/infix (t/parse2 day18-sample5))))
    (is (= 23340  (t/infix (t/parse2 day18-sample6))))))

(deftest day18-part1-soln
  (testing "Reproduces the answer for day18, part1"
    (is (= 69490582260 (t/day18-part1-soln)))))

(deftest day18-part2-soln
  (testing "Reproduces the answer for day18, part2"
    (is (= 362464596624526 (t/day18-part2-soln)))))