(ns advent2020.lib.utils
  (:require [clojure.java.io :as io]))

(defn puzzle-input
  [filename]
  (->> filename
       io/resource
       io/reader
       line-seq))