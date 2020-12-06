(ns advent2020.day06-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day06 :as t]))

(def day06-sample
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest parse-test
  (testing "Correctly parses the input format"
    (is (= '(["abc"] ["a" "b" "c"] ["ab" "ac"] ["a" "a" "a" "a"] ["b"])
           (t/parse day06-sample)))))

(deftest unique-items-in-group
  (testing "Correct identifies the sets of unique items in a group"
    (is (= '(#{\a \b \c} #{\a \b \c} #{\a \b \c} #{\a} #{\b})
           (map t/unique-answers-in-group (t/parse day06-sample))))))

(deftest common-items-in-group
  (testing "Correct identifies the sets of common items in a group"
    (is (= '(#{\a \b \c} #{} #{\a} #{\a} #{\b})
           (map t/common-answers-in-group (t/parse day06-sample))))))

(deftest day06-part1-soln
  (testing "Reproduces the answer for day06, part1"
    (is (= 6170 (t/day06-part1-soln)))))

(deftest day06-part2-soln
  (testing "Reproduces the answer for day06, part2"
    (is (= 2947 (t/day06-part2-soln)))))