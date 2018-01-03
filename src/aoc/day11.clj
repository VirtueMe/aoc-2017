(ns aoc.day11
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(def puzzle11 (split (first (get-lines "day11.txt")) #","))

(defn h-steps
  ""
  [step]
  (match step
    "n" 0
    "s" 0
    "nw" -1
    "sw" -1
    "ne" 1
    "se" 1
    ))

(defn v-steps
  ""
  [step]
  (match step
    "n" 1
    "s" -1
    "nw" 0
    "sw" -1
    "ne" 1
    "se" 0
    ))

(defn d-steps
  ""
  [step]
  (match step
    "n" 1
    "s" -1
    "nw" 1
    "sw" 0
    "ne" 0
    "se" -1
    ))


(defn calculate-steps
  ""
  [stream]
  (loop [stream stream steps 0 h 0 v 0 d 0 mx 0]
    (let [m-x (apply max (map #(Math/abs %) [h v d]))]
      (let [t-m-x (max mx m-x)]
        ; (println steps " : " (first stream) " (" h "," v "," d ") -> " t-m-x)
        (if (empty? stream)
          {:steps m-x :max t-m-x}
          (if (= steps 0)
            (recur (rest stream) (inc steps) (+ h (h-steps (first stream))) (+ v (v-steps (first stream))) (+ d (d-steps (first stream))) t-m-x)
            (match (first stream)
              "n" (recur (rest stream) (inc steps) h (inc v) (inc d) t-m-x)
              "ne" (recur (rest stream) (inc steps) (inc h) (inc v) d t-m-x)
              "se" (recur (rest stream) (inc steps) (inc h) v (dec d) t-m-x)
              "sw" (recur (rest stream) (dec steps) (dec h) (dec v) d t-m-x)
              "s" (recur (rest stream) steps h (dec v) (dec d) t-m-x)
              "nw" (recur (rest stream) steps (dec h) v (inc d) t-m-x)
              )))))))

(def puzzle11-run (calculate-steps puzzle11))
(def puzzle11-part1 (get puzzle11-run :steps))
(def puzzle11-part2 (get puzzle11-run :max))
