(ns advent2020.day04
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(def day04-input (str/join "\n" (u/puzzle-input "day04-input.txt")))

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

(def day04-invalid-samples
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def day04-valid-samples
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

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

(defn keys-valid?
  [passport]
  (let [needed #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
        ks     (set (keys passport))]
    (set/subset? needed ks)))

(defn day04-part1-soln
  []
  (->> (parse day04-input)
       (filter keys-valid?)
       count))

(defn byr-valid?
  [{:keys [byr]}]
  (let [yr (read-string byr)]
    (and (= 4 (count byr))
         (number? yr)
         (>= yr 1920)
         (<= yr 2002))))

(defn iyr-valid?
  [{:keys [iyr]}]
  (let [yr (read-string iyr)]
    (and (= 4 (count iyr))
         (number? yr)
         (>= yr 2010)
         (<= yr 2020))))

(defn eyr-valid?
  [{:keys [eyr]}]
  (let [yr (read-string eyr)]
    (and (= 4 (count eyr))
         (number? yr)
         (>= yr 2020)
         (<= yr 2030))))

(defn hgt-valid?
  [{:keys [hgt]}]
  (and (or (str/ends-with? hgt "cm")
           (str/ends-with? hgt "in"))
       (let [len (count hgt)
             num (read-string (subs hgt 0 (- len 2)))
             unit (subs hgt (- len 2) len)]
         (case unit
           "in" (and (number? num)
                     (>= num 59)
                     (<= num 76))
           "cm" (and (number? num)
                     (>= num 150)
                     (<= num 193))))))

(defn hcl-valid?
  [{:keys [hcl]}]
  (some? (re-matches #"^\#[a-f0-9]{6}$" hcl)))

(defn ecl-valid?
  [{:keys [ecl]}]
  (let [valid-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
    (some? (valid-colors ecl))))

(defn pid-valid?
  [{:keys [pid]}]
  (some? (re-matches #"^[0-9]{9}$" pid)))

(def passport-valid?
  (every-pred keys-valid?
              byr-valid?
              iyr-valid?
              eyr-valid?
              hgt-valid?
              hcl-valid?
              ecl-valid?
              pid-valid?))

(comment
  (hgt-valid? {:hgt "190"})
  (hcl-valid? {:hcl "123abc"})
  (ecl-valid? {:ecl "wat"})
  (pid-valid? {:pid "0123456789"})
  (map keys-valid? (parse day04-sample))
  (parse day04-input)
  (day04-part1-soln))
