(ns advent2020.day14-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day14 :as t]))

(deftest day14-part1-soln
  (testing "Reproduces the answer for day14, part1"
    (is (= 6631883285184 (t/day14-part1-soln)))))

;; (deftest day14-part2-soln
;;   (testing "Reproduces the answer for day14, part2"
;;     (is (= 2 (t/day14-part2-soln)))))