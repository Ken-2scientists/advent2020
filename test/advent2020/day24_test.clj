(ns advent2020.day24-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [advent2020.day24 :as t]))

(def day24-sample
  (map t/parse-line
       (str/split "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew" #"\n")))

(deftest parser
  (testing "The parser tokenizes the strings correctly"
    (is '("se" "se" "nw" "ne" "ne" "ne" "w" "se" "e" "sw" "w" "sw" "sw" "w" "ne" "ne" "w" "se" "w" "sw")
        (t/parse-line "sesenwnenenewseeswwswswwnenewsewsw"))
    (is '("w" "se" "w" "e" "e" "e" "nw" "ne" "se" "nw" "w" "w" "sw" "ne" "w")
        (t/parse-line "wseweeenwnesenwwwswnew"))))

(deftest identify-black-tiles
  (is (= 10 (count (t/black-tiles day24-sample)))))

(deftest evolution
  (testing "Can correctly apply the neighbor rules to evolve the tiles"
    (is (= 15 (count (t/black-tiles-on-day day24-sample 1))))
    (is (= 12 (count (t/black-tiles-on-day day24-sample 2))))
    (is (= 25 (count (t/black-tiles-on-day day24-sample 3))))
    (is (= 14 (count (t/black-tiles-on-day day24-sample 4))))
    (is (= 23 (count (t/black-tiles-on-day day24-sample 5))))
    (is (= 28 (count (t/black-tiles-on-day day24-sample 6))))
    (is (= 41 (count (t/black-tiles-on-day day24-sample 7))))
    (is (= 37 (count (t/black-tiles-on-day day24-sample 8))))
    (is (= 49 (count (t/black-tiles-on-day day24-sample 9))))
    (is (= 37 (count (t/black-tiles-on-day day24-sample 10))))))

(deftest day24-part1-soln
  (testing "Reproduces the answer for day24, part1"
    (is (= 436 (t/day24-part1-soln)))))

(deftest day24-part2-soln
  (testing "Reproduces the answer for day24, part2"
    (is (= 4133 (t/day24-part2-soln)))))