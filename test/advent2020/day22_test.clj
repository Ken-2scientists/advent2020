(ns advent2020.day22-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day22 :as t]))

(def day22-sample
  (t/parse "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"))

(def day22-infinite-sample
  (t/parse "Player 1:
43
19

Player 2:
2
29
14"))

(deftest simple-combat-rules-work
  (testing "Satisfies the rules of basic Combat as defined in part 1"
    (is (= {:player1 [2 6 3 1 9 5]
            :player2 [8 4 7 10]}
           (t/update-combat-round day22-sample)))
    ;; (is (= 29 (count (t/combat-rounds day22-sample))))
    (is (= 306 (t/score (t/combat day22-sample))))))

(deftest no-infinite-repeats
  (testing "Combat game won't loop forever"
    (is (= {:winner :player1, :deck [43 19]}
           (t/combat day22-infinite-sample)))))

(deftest recursive-combat-rules
  (testing "Satisfies the recursive combat rules"
    (is (= {:winner :player2
            :deck [7 5 6 2 4 1 10 8 9 3]}
           (t/recursive-combat day22-sample)))))

(deftest day22-part1-soln
  (testing "Reproduces the answer for day22, part1"
    (is (= 33772 (t/day22-part1-soln)))))

(deftest day22-part2-soln
  (testing "Reproduces the answer for day22, part2"
    (is (= 35070 (t/day22-part2-soln)))))