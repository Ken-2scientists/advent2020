(ns advent2020.day08-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day08 :as t]))

(def day08-sample
  (map t/parse
       (str/split
        "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" #"\n")))


(deftest part1-sample
  (testing "Can return accumulator value at beginning of second loop"
    (is (= 5 (t/acc-value-at-second-loop day08-sample)))))

(deftest part2-sample
  (testing "Returns the accumulator after fixing the invalid instruction"
    (is (= 8 (t/acc-value-for-finite-loop day08-sample)))))

(deftest day08-part1-soln
  (testing "Reproduces the answer for day08, part1"
    (is (= 1451 (t/day08-part1-soln)))))

(deftest day08-part2-soln
  (testing "Reproduces the answer for day08, part2"
    (is (= 1160 (t/day08-part2-soln)))))