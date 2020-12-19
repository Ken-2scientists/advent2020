(ns advent2020.day18
  (:require [clojure.string :as str]
            [advent-utils.core :as u]))

(def day18-input (u/puzzle-input "day18-input.txt"))

(defn parse
  [input]
  (read-string (str "(" input ")")))

(defn parse2
  [input]
  (let [updated (-> input
                    (str/replace "(" "((")
                    (str/replace ")" "))")
                    (str/replace "*" ")*("))]
    (read-string (str "((" updated "))"))))

(defn operize
  [sym]
  (case sym
    + clojure.core/+
    * clojure.core/*))

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
  (reduce + (map (comp infix parse) day18-input)))

(defn day18-part2-soln
  []
  (reduce + (map (comp infix parse2) day18-input)))