(ns advent2020.day14
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [advent2020.lib.utils :as u]))

(defn parse-mask
  [mask-str]
  (let [m-str (second (str/split mask-str #" = "))
        override-bits (->> (map-indexed vector m-str)
                          ;;  (filter #(not= \X (second %)))
                           (map (fn
                                  [[idx bit]]
                                  [(read-string (str bit)) (- 35 idx)]))
                           (group-by first)
                           (u/fmap #(map second %)))
        and-mask (reduce bit-clear 68719476735 (override-bits 0))
        or-mask  (reduce bit-set 0 (override-bits 1))]
    (println (keys override-bits))
    {:and-mask and-mask :or-mask or-mask :exes (get override-bits 'X)}))

(defn parse-assign
  [assign-str]
  (let [[addr-str val] (str/split assign-str #" = ")
        len (count addr-str)
        addr (read-string (subs addr-str 4 (dec len)))]
    {:address addr :val (read-string val)}))

(defn parse-line
  [input-line]
  (if (str/starts-with? input-line "mask")
    (parse-mask input-line)
    (parse-assign input-line)))

(def day14-sample
  (map parse-line
       (str/split
        "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0" #"\n")))

(def day14-sample2
  (map parse-line
       (str/split
        "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1" #"\n")))


(def day14-input (map parse-line (u/puzzle-input "day14-input.txt")))

(defn execute
  [instructions]
  (loop [mask {} registers {} inst instructions]
    (if (empty? inst)
      registers
      (let [next-inst (first inst)]
        (if (:and-mask next-inst)
          (recur next-inst registers (rest inst))
          (let [{:keys [address val]} next-inst]
            (recur mask
                   (assoc registers address
                          (->> val
                               (bit-and (:and-mask mask))
                               (bit-or  (:or-mask mask))))
                   (rest inst))))))))

(defn floating-vals
  [exes]
  (->> exes
       (map (partial bit-shift-left 1))
       combo/subsets
       (map (partial apply +))))

(defn addresses
  [{:keys [or-mask exes]} address]
  (map (partial + (bit-or or-mask address)) exes))

(addresses (nth day14-sample2 0) 42)

(defn execute2
  [instructions]
  (loop [mask {} registers {} inst instructions]
    (if (empty? inst)
      registers
      (let [next-inst (first inst)]
        (if (:and-mask next-inst)
          (recur next-inst registers (rest inst))
          (let [{:keys [address val]} next-inst]
            (recur mask
                   (assoc registers address
                          (->> val
                               (bit-and (:and-mask mask))
                               (bit-or  (:or-mask mask))))
                   (rest inst))))))))

(defn register-sum
  [input]
  (reduce + (vals (execute input))))

(defn day14-part1-soln
  []
  (register-sum day14-input))