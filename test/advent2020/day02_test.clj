(ns advent2020.day02-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day02 :as t]))

(def day02-sample ["1-3 a: abcde"
                   "1-3 b: cdefg"
                   "2-9 c: ccccccccc"])

(deftest parser
  (testing "Testing the parser works correctly on the input"
    (is (= {:min 1 :max 3 :char \a :pass "abcde"}
           (t/parse (first day02-sample))))))

(deftest part1-validator
  (testing "Correctly validates per the rules in part1"
    (is (= true (t/part1-valid? (t/parse (first day02-sample)))))
    (is (= false (t/part1-valid? (t/parse (second day02-sample)))))
    (is (= true (t/part1-valid? (t/parse (last day02-sample)))))))

(deftest part2-validator
  (testing "Correctly validates per the rules in part2"
    (is (= true (t/part2-valid? (t/parse (first day02-sample)))))
    (is (= false (t/part2-valid? (t/parse (second day02-sample)))))
    (is (= false (t/part2-valid? (t/parse (last day02-sample)))))))

(deftest day02-part1-soln
  (testing "Reproduces the answer for day02, part1"
    (is (= 418 (t/day02-part1-soln)))))

(deftest day02-part2-soln
  (testing "Reproduces the answer for day02, part2"
    (is (= 616 (t/day02-part2-soln)))))