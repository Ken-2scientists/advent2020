(ns advent2020.day08
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day08-sample
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse
  [line]
  (let [[op arg] (str/split line #"\ ")]
    [(keyword op) (read-string arg)]))

(def code (map parse (str/split day08-sample #"\n")))
(def day08-input (map parse (u/puzzle-input "day08-input.txt")))

(defn execute
  [{:keys [line acc] :as state} [op arg]]
  (case op
    :nop (assoc state :line (inc line))
    :acc (assoc state :line (inc line)
                :acc (+ acc arg))
    :jmp (assoc state :line (+ line arg))))

(defn acc-value-at-second-loop
  [ops]
  (loop [state {:line 0 :acc 0} lines #{}]
    (if (lines (:line state))
      (:acc state)
      (let [op (nth ops (:line state))
            new-state (execute state op)]
        (recur new-state (conj lines (:line state)))))))

(defn day08-part1-soln
  []
  (acc-value-at-second-loop day08-input))

(defn jmp-or-nop-lines
  [ops]
  (->> ops
       (map-indexed (fn [idx [op _]]
                      (when (#{:jmp :nop} op)
                        idx)))
       (filter some?)))

(defn swap-op
  [ops line]
  (let [[op arg] (nth ops line)
        new-op (if (= op :jmp) :nop :jmp)]
    (assoc ops line [new-op arg])))

(defn variations
  [ops]
  (let [line-changes (jmp-or-nop-lines ops)]
    (map (partial swap-op (vec ops)) line-changes)))

(defn finite-and-infinite-loop
  [ops]
  (let [op-count (count ops)]
    (loop [state {:line 0 :acc 0} lines #{}]
      (if (or (lines (:line state))
              (>= (:line state) op-count))
        (assoc state :finite (>= (:line state) op-count))
        (let [op (nth ops (:line state))
              new-state (execute state op)]
          (recur new-state (conj lines (:line state))))))))

(defn day08-part2-soln
  []
  (->> (variations day08-input)
       (map finite-and-infinite-loop)
       (filter #(:finite %))
       first
       :acc))