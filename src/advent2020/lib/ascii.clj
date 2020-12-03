(ns advent2020.lib.ascii)

(defn ascii->map
  [codes lines]
  (let [row-count (count lines)
        col-count (count (first lines))
        symbols (mapcat #(map codes %) lines)]
    (zipmap (for [y (range row-count)
                  x (range col-count)]
              [x y])
            symbols)))
