(ns advent2020.day04
  (:require [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day04-input (u/puzzle-input "day04-input.txt"))

(def day04-sample
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn split-pair
  [pair]
  (let [[k v] (str/split pair #"\:")]
    [(keyword k) v]))

(defn parse
  [input]
  (->> (str/split input #"\n\n")
       (map #(str/replace % "\n" " "))
       (map #(str/split % #"\ "))
       (map (fn [x]
              (->> x
                   (mapcat split-pair)
                   (apply hash-map))))))

(comment
  (parse day04-sample))
