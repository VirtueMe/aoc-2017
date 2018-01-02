(ns aoc.day3
  (:gen-class))


;; part1

(def manhatten [
  [101   100    99    98    97    96    95    94    93    92    91 ]

  [102    65    64    63    62    61    60    59    58    57    90 ]

  [103    66    37    36    35    34    33    32    31    56    89 ]

  [104    67    38    17    16    15    14    13    30    55    88 ]

  [105    68    39    18     5     4     3    12    29    54    87 ]

  [106    69    40    19     6     1     2    11    28    53    86 ]

  [107    70    41    20     7     8     9    10    27    52    85 ]

  [108    71    42    21    22    23    24    25    26    51    84 ]

  [109    72    43    44    45    46    47    48    49    50    83 ]

  [110    73    74    75    76    77    78    79    80    81    82 ]

  [111   112   113   114   115   116   117   118   119   120   121 ]

  ])


;; part2
(def left [-1, 0])
(def right [1, 0])
(def up [0, -1])
(def down [0, 1])

(def frame [[1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1] [0 1] [1 1]])

(defn calc-pos
  ""
  [list x y]
  (let [[px py] list]
  [(+ x px) (+ y py)]))

(defn calc-positions
  ""
  [coll x y]
  (map #(calc-pos % x y) coll))

(defn create-vec
  ""
  [n item]
  (vec (replicate n item)))

(defn round
  ""
  [n]
  (let [upn (+ 1 (* 2 (- n 1))) nn (* 2 n)]
  (vec (concat [right] (create-vec upn up) (create-vec nn left) (create-vec nn down) (create-vec nn right)))))

(defn rounds
  ""
  [n]
  (reduce concat (map round (take n (iterate inc 1)))))

(def sortedsheet (map sort-sheet spreadsheet))

(defn spiral
  ""
  [n]
  (vec (replicate n (vec (replicate n 0)))))


(defn set-cell
  ""
  [board x y value]
  (assoc board y (assoc (nth board y) x value))
  )

(defn get-board-value
  ""
  [board pos]
  (let [[x y] pos]
    (let [size (count board)]
      (if (and (< y size) (< x size) (> x -1) (> y -1))
        (nth (nth board y) x)
        0))))

(defn get-board-values
  ""
  [board circle]
  (map #(get-board-value board %) circle))

(defn calc-new-value
  ""
  [board x y]
    (apply + (get-board-values board (calc-positions frame x y))))


  ;;(let [[xn yn] (calc-pos right x y)]
  ;;(str size " (" x ", " y ") " (set-cell board xn yn (calc-new-value board xn yn)))

(defn setup-spiral-test
    ""
    [n]
    (let [size (+ 1 (* 2 n))]
      (let [x (/ (- size 1) 2) blank (spiral size)]
        (let [y x board (set-cell blank x x 1)]
          (set-cell (set-cell board (+ x 1) y 1) (+ x 1) (- y 1) 2)
          ))))

(defn setup-spiral
  ""
  [n max]
  (let [size (+ 1 (* 2 n))]
    (let [x (/ (- size 1) 2) blank (spiral size)]
      (let [y x board (set-cell blank x x 1)]
      (loop [list (rounds n) board board x x y y]
        (let [item (first list)]
          (if (empty? list)
            (str board)
            (let [[x y] (calc-pos item x y)]
              (let [value (calc-new-value board x y)]
                (if (> value max)
                  (str value)
                  (recur (rest list) (set-cell board x y value) x y)))
        ))))))))
