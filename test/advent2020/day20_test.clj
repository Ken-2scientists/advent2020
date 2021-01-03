(ns advent2020.day20-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2020.day20 :as t]))

(def day20-sample
  (t/parse
   "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."))

(deftest corners
  (testing "Finds the corner tiles"
    (is (= '(1171 2971 3079 1951)
           (t/corners (t/matching-edges (t/tile-edge-map day20-sample)))))))

(def grid-sample1
  {:width 2
   :height 4
   :grid {[0 0] :a [1 0] :b
          [0 1] :c [1 1] :d
          [0 2] :e [1 2] :f
          [0 3] :g [1 3] :h}})

(def grid-sample2
  {:width 3
   :height 3
   :grid {[0 0] :a [1 0] :b [2 0] :c
          [0 1] :d [1 1] :e [2 1] :f
          [0 2] :e [1 2] :f [2 2] :g}})

(deftest flips-and-rotations
  (testing "Horizontal Flips"
    (is (= {:width 2
            :height 4
            :grid {[0 0] :b [1 0] :a
                   [0 1] :d [1 1] :c
                   [0 2] :f [1 2] :e
                   [0 3] :h [1 3] :g}}
           (t/fliph grid-sample1)))
    (is (= grid-sample1 (t/fliph (t/fliph grid-sample1)))))
  (testing "Vertical Flips"
    (is (= {:width 2
            :height 4
            :grid {[0 0] :g [1 0] :h
                   [0 1] :e [1 1] :f
                   [0 2] :c [1 2] :d
                   [0 3] :a [1 3] :b}}
           (t/flipv grid-sample1)))
    (is (= grid-sample1 (t/flipv (t/flipv grid-sample1)))))
  (testing "Rotations"
    (is (= {:width 4
            :height 2
            :grid {[0 0] :g [1 0] :e [2 0] :c [3 0] :a
                   [0 1] :h [1 1] :f [2 1] :d [3 1] :b}}
           (t/rotate grid-sample1)))
    (is (= {:width 2
            :height 4
            :grid {[0 0] :h [1 0] :g
                   [0 1] :f [1 1] :e
                   [0 2] :d [1 2] :c
                   [0 3] :b [1 3] :a}}
           (t/rotate (t/rotate grid-sample1))))
    (is (= {:width 4
            :height 2
            :grid {[0 0] :b [1 0] :d [2 0] :f [3 0] :h
                   [0 1] :a [1 1] :c [2 1] :e [3 1] :g}}
           (t/rotate (t/rotate (t/rotate grid-sample1)))))))

(deftest tile-assembly
  (testing "Trim edges"
    (is (= {:width 1
            :height 1
            :grid {[1 1] :e}}
           (t/trim-edge grid-sample2)))))

(deftest day20-part1-soln
  (testing "Reproduces the answer for day20, part1"
    (is (= 79412832860579 (t/day20-part1-soln)))))


(deftest orientation-rules
  (testing "Correctly determines the necessary flips/rotations"
    (is (= [[:fliph]]
           (t/orient {1427 :e, 1951 :s, 2971 :n}
                     {1427 :w, 1951 :s, 2971 :n})))
    (is (= [[:flipv]]
           (t/orient {2473 :n, 1489 :e}
                     {2473 :s, 1489 :e})))
    (is (= [[:fliph]]
           (t/orient {1489 :e, 2729 :s}
                     {1489 :w, 2729 :s})))
    (is (= [[:fliph]]
           (t/orient {1951 :w, 3079 :e, 1427 :n}
                     {1951 :e, 3079 :w, 1427 :n})))
    (is (= [[:fliph]]
           (t/orient {1427 :s, 2971 :w, 1171 :e}
                     {1427 :s, 2971 :e, 1171 :w})))
    (is (= [[:fliph] [:flipv]]
           (t/orient {2473 :s, 2311 :w}
                     {2473 :n, 2311 :e})))
    (is (= [[:rotate 1] [:fliph]]
           (t/orient {1171 :w, 3079 :e, 1427 :s}
                     {1171 :n, 3079 :s, 1427 :e})))))




;; (deftest day20-part2-soln
;;   (testing "Reproduces the answer for day20, part2"
;;     (is (= 439 (t/day20-part2-soln)))))