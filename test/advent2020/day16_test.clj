(ns advent2020.day16-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day16 :as t]))

(deftest day16-part1-soln
  (testing "Reproduces the answer for day16, part1"
    (is (= 25961 (t/day16-part1-soln)))))

(deftest day16-part2-soln
  (testing "Reproduces the answer for day16, part2"
    (is (= 603409823791 (t/day16-part2-soln)))))