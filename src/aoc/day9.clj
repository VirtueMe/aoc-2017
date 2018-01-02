(ns aoc.day9
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]))

(def puzzle9 (get-lines "day9.txt"))

(defn garbage-read
  ""
  [input pos]
  (loop [pos pos]
    (let [char (get input pos)]
      (match char
          \! (recur (+ pos 2))
          \> (+ pos 1)
        )
      )))

(defn group-read-pos
  ""
  [input pos]
  input
  )


(defn stream-read
  ""
  [input]
  (if (or (nil? input) (= input ""))
    nil
    (loop [pos 0 result nil current nil]
      (if (= pos (count input))
        result
        (let [char (get input pos)]
          (match char
            \{ (recur (+ pos 1) result (if (nil? current ) [] (conj current [])))
            \} (recur (+ pos 1) (conj result (peek current)) (pop current))
            \! (recur (+ pos 2) result current)
            \< (recur (garbage-read input (+ pos 1)) result current)
            \, {}
            ))))))


(defn group-count
  ""
  [input]
  (if (nil? input)
    0
    (if (empty? input)
      1
      (loop [list input groups 1]
        (if (empty? list)
          groups
          (let [item (first list)]
            (recur (rest list) (+ groups (group-count item)))
            ))))))
