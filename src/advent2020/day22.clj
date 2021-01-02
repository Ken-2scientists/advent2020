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

(def day22-input
  (parse
   (str/join "\n" (u/puzzle-input "day22-input.txt"))))

(defn round-results-simple
  [{:keys [player1 player2]}]
  (let [p1card (first player1)
        p2card (first player2)]
    (if (> p1card p2card)
      [:player1 p1card p2card]
      [:player2 p2card p1card])))

(declare recursive-combat)
(defn round-results-recursive
  [{:keys [player1 player2]}]
  (let [p1card (first player1)
        p2card (first player2)
        winner (if (and (<= p1card (count (rest player1)))
                        (<= p2card (count (rest player2))))
                 (-> (recursive-combat {:player1 (subvec player1 1 (inc p1card))
                                        :player2 (subvec player2 1 (inc p2card))})
                     (get :winner))
                 (if (> p1card p2card)
                   :player1
                   :player2))]
    (if (= :player1 winner)
      [:player1 p1card p2card]
      [:player2 p2card p1card])))

(defn update-round
  [round-results {:keys [player1 player2] :as hands}]
  (let [[winner card1 card2] (round-results hands)]
    (if (= :player1 winner)
      {:player1 (conj (subvec player1 1) card1 card2)
       :player2 (subvec player2 1)}
      {:player1 (subvec player1 1)
       :player2 (conj (subvec player2 1) card1 card2)})))

(def update-combat-round
  (partial update-round round-results-simple))

(def update-recursive-combat-round
  (partial update-round round-results-recursive))

(defn game-over?
  [{:keys [player1 player2] :as hands} prev-rounds]
  (or (empty? player1)
      (empty? player2)
      (some? (prev-rounds hands))))

;; (defn combat-rounds
;;   [hands]
;;   (take-while (complement game-over?)
;;               (iterate update-round hands)))
;; (defn combat
;;   [hands]
;;   (let [rounds (combat-rounds hands)]
;;     (update-round (last rounds))))
;;     

(defn game-outcome
  [{:keys [player1 player2]}]
  (cond
    (empty? player2) {:winner :player1 :deck player1}
    (empty? player1) {:winner :player2 :deck player2}
    :else {:winner :player1 :deck player1}))

(defn combat-fn
  [update-fn init-hands]
  (loop [hands init-hands prev-hands #{}]
    (if (game-over? hands prev-hands)
      (game-outcome hands)
      (let [next-hands (update-fn hands)]
        (recur next-hands (conj prev-hands hands))))))

(def combat
  (partial combat-fn update-combat-round))
(def recursive-combat
  (partial combat-fn update-recursive-combat-round))

(defn score
  [{:keys [deck]}]
  (let [cnt (count deck)]
    (reduce + (map * deck (range cnt 0 -1)))))

(defn day22-part1-soln
  []
  (score (combat day22-input)))

(defn day22-part2-soln
  []
  (score (recursive-combat day22-input)))

