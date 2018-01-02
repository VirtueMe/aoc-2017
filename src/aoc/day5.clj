(ns aoc.day5
  (:gen-class))

(use 'aoc.core)

(def maze [0 3 0 1 -3])

(def puzzle5 (vec (map #(Integer. %) (get-lines "day5.txt"))))

(defn i-or-d
  ""
  [value]
  (if (> value 2)
    (dec value)
    (inc value)))

(defn solve-with
  "Solves the maze"
  [input f]
  (loop [step 0 pos 0 maze input]
    (let [value (get maze pos)]
      ;; (println (take 5 maze) " " step " " pos " " pos)
      (if (not (< pos (count maze)))
        (str step)
        (recur (inc step) (+ pos value) (assoc maze pos (f value)))))))
        ;;(assoc maze pos (+ (get maze pos) 1)))))))

(defn solve
  "solve the maze part1"
  [input]
  (solve-with input inc))

(defn solve2
  "solve the maze part2"
  [input]
  (solve-with input i-or-d))
