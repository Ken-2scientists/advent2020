(ns advent2020.day13-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day13 :as t]))

(def day13-sample1 (t/parse (str/split "939\n7,13,x,x,59,x,31,19" #"\n")))
(def day13-sample2 {:buses '(17,x,13,19)})
(def day13-sample3 {:buses '(67,7,59,61)})
(def day13-sample4 {:buses '(67,x,7,59,61)})
(def day13-sample5 {:buses '(67,7,x,59,61)})
(def day13-sample6 {:buses '(1789,37,47,1889)})

(deftest earliest-time-for-consecutive-buses
  (testing "Can find the earliest time for consecutive buses to depart"
    (is (= 1068781    (t/earliest-consecutive-buses day13-sample1)))
    (is (= 3417       (t/earliest-consecutive-buses day13-sample2)))
    (is (= 754018     (t/earliest-consecutive-buses day13-sample3)))
    (is (= 779210     (t/earliest-consecutive-buses day13-sample4)))
    (is (= 1261476    (t/earliest-consecutive-buses day13-sample5)))
    (is (= 1202161486 (t/earliest-consecutive-buses day13-sample6)))))

(deftest day13-part1-soln
  (testing "Reproduces the answer for day13, part1"
    (is (= 104 (t/day13-part1-soln)))))

(deftest day13-part2-soln
  (testing "Reproduces the answer for day13, part2"
    (is (= 842186186521918N (t/day13-part2-soln)))))