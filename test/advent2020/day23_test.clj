(ns advent2020.day23-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day23 :as t]))

(def day23-sample        [3 8 9 1 2 5 4 6 7])
(def day23-sample-move01 [2 8 9 1 5 4 6 7 3])
(def day23-sample-move02 [5 4 6 7 8 9 1 3 2])
(def day23-sample-move03 [8 9 1 3 4 6 7 2 5])
(def day23-sample-move04 [4 6 7 9 1 3 2 5 8])
(def day23-sample-move10 [8 3 7 4 1 9 2 6 5])

(deftest moves
  (testing "Crab moves"
    (is (= day23-sample-move01 (nth (iterate t/move day23-sample) 1)))
    (is (= day23-sample-move02 (nth (iterate t/move day23-sample) 2)))
    (is (= day23-sample-move03 (nth (iterate t/move day23-sample) 3)))
    (is (= day23-sample-move04 (nth (iterate t/move day23-sample) 4)))
    (is (= day23-sample-move10 (nth (iterate t/move day23-sample) 10)))
    (is (= "92658374" (t/order-after-moves day23-sample 10)))
    (is (= "67384529" (t/order-after-moves day23-sample 100)))))

(deftest day23-part1-soln
  (testing "Reproduces the answer for day23, part1"
    (is (= "62934785" (t/day23-part1-soln)))))

;; (deftest day23-part2-soln
;;   (testing "Reproduces the answer for day23, part2"
;;     (is (= 35070 (t/day23-part2-soln)))))