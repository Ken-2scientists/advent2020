(ns advent2020.day23-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day23 :as t]))

(def day23-sample         [3 8 9 1 2 5 4 6 7])
(def day23-sample-move001 [2 8 9 1 5 4 6 7 3])
(def day23-sample-move002 [5 4 6 7 8 9 1 3 2])
(def day23-sample-move003 [8 9 1 3 4 6 7 2 5])
(def day23-sample-move004 [4 6 7 9 1 3 2 5 8])
(def day23-sample-move010 [8 3 7 4 1 9 2 6 5])
(def day23-sample-move100 [1 6 7 3 8 4 5 2 9])

(deftest moves
  (testing "Crab moves"
    (is (= day23-sample-move001 (t/representation (t/n-moves day23-sample 1))))
    (is (= day23-sample-move002 (t/representation (t/n-moves day23-sample 2))))
    (is (= day23-sample-move003 (t/representation (t/n-moves day23-sample 3))))
    (is (= day23-sample-move004 (t/representation (t/n-moves day23-sample 4))))
    (is (= day23-sample-move010 (t/representation (t/n-moves day23-sample 10))))
    (is (= day23-sample-move100 (t/representation (t/n-moves day23-sample 100))))))

;; Too slow
;; (deftest big-moves
;;   (is (= '(934001 159792) (take 2 (drop 1 (t/star-cups day23-sample 10000000))))))

(deftest day23-part1-soln
  (testing "Reproduces the answer for day23, part1"
    (is (= "62934785" (t/day23-part1-soln)))))

;; Too slow
;; (deftest day23-part2-soln
;;   (testing "Reproduces the answer for day23, part2"
;;     (is (= 693659135400 (t/day23-part2-soln)))))