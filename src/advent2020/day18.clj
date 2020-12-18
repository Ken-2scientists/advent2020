(ns advent2020.day18
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(defn parse
  [input]
  (read-string (str "(" input ")")))

;; (def day18-sample1 "1 + 2 * 3 + 4 * 5 + 6")
;; (def day18-sample2 "1 + (2 * 3) + (4 * (5 + 6))")
;; (def day18-sample3 "2 * 3 + (4 * 5)")
;; (def day18-sample4 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
;; (def day18-sample5
;;   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
;; (def day18-sample6
;;   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
(def day18-input (map parse (u/puzzle-input "day18-input.txt")))

(defn operize
  [sym]
  (case sym
    + clojure.core/+
    - clojure.core/-
    * clojure.core/*
    / clojure.core//))

(defn infix
  [expr]
  (let [x    (first expr)
        init (if (list? x) (infix x) x)]
    (reduce
     (fn [acc [oper y]]
       (if (list? y)
         ((operize oper) acc (infix y))
         ((operize oper) acc y)))
     init (partition 2 (rest expr)))))

(defn day18-part1-soln
  []
  (reduce + (map infix day18-input)))