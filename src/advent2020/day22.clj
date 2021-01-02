(ns advent2020.day22
  (:require [clojure.string :as str]
            [advent-utils.core :as u]))

(defn parse-cards
  [cards-str]
  (->> (str/split cards-str #"\n")
       rest
       (mapv read-string)))

(defn parse
  [input]
  (let [[player1 player2] (str/split input #"\n\n")]
    {:player1 (parse-cards player1)
     :player2 (parse-cards player2)}))

(def day22-sample
  (parse "Player 1:
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

(def day22-input
  (parse
   (str/join "\n" (u/puzzle-input "day22-input.txt"))))

(defn update-round
  [{:keys [player1 player2]}]
  (let [p1card (first player1)
        p2card (first player2)]
    (if (> p1card p2card)
      {:player1 (conj (subvec player1 1) p1card p2card)
       :player2 (subvec player2 1)}
      {:player1 (subvec player1 1)
       :player2 (conj (subvec player2 1) p2card p1card)})))

(defn game-over?
  [{:keys [player1 player2]}]
  (or (empty? player1) (empty? player2)))

(defn combat-rounds
  [hands]
  (take-while (complement game-over?)
              (iterate update-round hands)))

(defn combat
  [hands]
  (let [rounds (combat-rounds hands)]
    (update-round (last rounds))))

(defn winning-hand
  [{:keys [player1 player2]}]
  (if (empty? player2)
    player1
    player2))

(defn score
  [hand]
  (let [cnt (count hand)]
    (reduce + (map * hand (range cnt 0 -1)))))

(defn combat-score
  [hands]
  (->> (combat hands)
       winning-hand
       score))

(defn day22-part1-soln
  []
  (combat-score day22-input))