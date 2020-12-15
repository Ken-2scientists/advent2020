(ns advent2020.day13-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day13 :as t]))

(deftest day13-part1-soln
  (testing "Reproduces the answer for day13, part1"
    (is (= 104 (t/day13-part1-soln)))))

;; (deftest day13-part2-soln
;;   (testing "Reproduces the answer for day13, part2"
;;     (is (= 2 (t/day13-part2-soln)))))