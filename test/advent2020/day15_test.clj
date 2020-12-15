(ns advent2020.day15-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day15 :as t]))

(def day15-sample1 (map read-string (str/split "0,3,6" #",")))
(def day15-sample2 (map read-string (str/split "1,3,2" #",")))
(def day15-sample3 (map read-string (str/split "2,1,3" #",")))
(def day15-sample4 (map read-string (str/split "1,2,3" #",")))
(def day15-sample5 (map read-string (str/split "2,3,1" #",")))
(def day15-sample6 (map read-string (str/split "3,2,1" #",")))
(def day15-sample7 (map read-string (str/split "3,1,2" #",")))

(deftest find-2020th-number
  (testing "Can find the 2020th number in the sequence for sample data"
    (is (= 436  (last (t/memory-seq day15-sample1 2020))))
    (is (= 1    (last (t/memory-seq day15-sample2 2020))))
    (is (= 10   (last (t/memory-seq day15-sample3 2020))))
    (is (= 27   (last (t/memory-seq day15-sample4 2020))))
    (is (= 78   (last (t/memory-seq day15-sample5 2020))))
    (is (= 438  (last (t/memory-seq day15-sample6 2020))))
    (is (= 1836 (last (t/memory-seq day15-sample7 2020))))))

(deftest day15-part1-soln
  (testing "Reproduces the answer for day15, part1"
    (is (= 1428 (t/day15-part1-soln)))))

; Test is too slow with current implementation to run every time
;; (deftest day15-part2-soln
;;   (testing "Reproduces the answer for day15, part2"
;;     (is (= 3718541 (t/day15-part2-soln)))))