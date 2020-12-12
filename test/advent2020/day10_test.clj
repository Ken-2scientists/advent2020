(ns advent2020.day10-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day10 :as t]))

(def day10-sample
  (map read-string
       (str/split
        "16
10
15
5
1
11
7
19
6
12
4" #"\n")))

(def day10-sample2
  (map read-string
       (str/split
        "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3" #"\n")))

(deftest jolt-diff-counts
  (testing "Correctly determines the number of 1-jolt and 3-jolt differences"
    (is (= [7 5]   (t/freq-steps day10-sample)))
    (is (= [22 10] (t/freq-steps day10-sample2)))))

(deftest combination-counts
  (testing "Correctly determines the number of unique valid combinations of adapters"
    (is (= 8     (t/combination-count day10-sample)))
    (is (= 19208 (t/combination-count day10-sample2)))))

(deftest day10-part1-soln
  (testing "Reproduces the answer for day10, part1"
    (is (= 2760 (t/day10-part1-soln)))))

(deftest day10-part2-soln
  (testing "Reproduces the answer for day10, part2"
    (is (= 13816758796288 (t/day10-part2-soln)))))