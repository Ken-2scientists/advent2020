(ns advent2020.day19
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(defn to-list
  [x]
  (read-string (str "(" x ")")))

(defn parse-rule
  [rule-str]
  (let [[idx rule-txt] (str/split rule-str #": ")
        rule (if (.contains rule-txt "|")
               (map to-list (str/split rule-txt #" \| "))
               (if (str/starts-with? rule-txt "\"")
                 (read-string rule-txt)
                 (to-list rule-txt)))]
    [(read-string idx) rule]))

(defn parse
  [input]
  (let [[rules messages] (str/split input #"\n\n")]
    {:rules (into {} (map parse-rule (str/split rules #"\n")))
     :messages (str/split messages #"\n")}))

(def day19-sample
  (parse
   "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb"))

(def day19-input (parse (str/join "\n" (u/puzzle-input "day19-input.txt"))))
((:rules day19-sample) 0)

(defn resolve-rule
  [rules rule]
  (let [r (rules rule)]
    (if (string? r)
      r
      (if (coll? (first r))
        (str "("
             (str/join (map (partial resolve-rule rules) (first r)))
             "|"
             (str/join (map (partial resolve-rule rules) (second r)))
             ")")
        (str/join (map (partial resolve-rule rules) r))))))

(defn count-matches
  [{:keys [rules messages]}]
  (let [pattern (re-pattern (str "^" (resolve-rule rules 0) "$"))]
    (->> (map (partial re-find pattern) messages)
         (map first)
         (filter some?)
         count)))

(defn day19-part1-soln
  []
  (count-matches day19-input))


(defn dependencies
  [rules]
  (->> (mapcat
        (fn [[k v]]
          (when (coll? v)
            (println v)
            (map
             (fn [i]
               [i k]) (set (flatten v)))))
        rules)
       (group-by first)
       (u/fmap (partial map second))))