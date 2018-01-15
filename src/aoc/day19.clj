(ns aoc.day19
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))


(def puzzle19-sample-data ["     |          " "     |  +--+    " "     A  |  C    " " F---|----E|--+ " "     |  |  |  D " "     +B-+  +--+ " "                "])

(defn format-data
  ""
  [input]
  (vec (map #(vec (seq %)) input)))

(def puzzle19-sample (format-data puzzle19-sample-data))

(def puzzle19-data (format-data (get-lines "day19.txt")))

(def down [0 1])
(def up [0 -1])
(def right [1 0])
(def left [-1 0])

(def pipe \|)
(def plus \+)
(def minus \-)
(def space \ )

(defn add-dia
  ""
  [pos dir]
  [(+ (pos 0) (dir 0)) (+ (pos 1) (dir 1))]
  )

(defn get-ch
  ""
  [diagram pos]
    ((diagram (pos 1)) (pos 0)))

(defn valid-pos
  ""
  [diagram pos]
  (if (< -1 (pos 0) (count (diagram 0)))
    (if (< -1 (pos 1) (count diagram))
      true
      false)))

(defn follow
  ""
  [diagram]
  (let [start (.indexOf (diagram 0) pipe)]
    (loop [pos [start 0] dir down result [] steps 0]
      ;;(println "pos: " pos)
      (let [next (add-dia pos dir)]
        ;;(println "next: " next)
        (let [ch (get-ch diagram next)]
          ;;(println "ch: " ch)
          (if (.contains [down up] dir)
            (if (= ch minus)
              (recur next dir result (inc steps))
              (if (= ch plus)
                (let [ahead (add-dia next left)]
                  ;;(println "left: " ahead)
                  (if (valid-pos diagram ahead)
                    (let [ch-n (get-ch diagram ahead)]
                      ;;(println "ch-v: " ch-n)
                      (if (= space ch-n)
                        (recur next right result (inc steps))
                        (recur ahead left (if (= minus ch-n) result (conj result ch-n)) (+ steps 2))))
                    (recur next right result (inc steps))))
                (if (= ch pipe)
                  (recur next dir result (inc steps))
                  (if (= ch space)
                    result
                    (recur next dir (conj result ch) (inc steps))))))
            (if (= ch pipe)
              (recur next dir result (inc steps))
              (if (= ch plus)
                (let [ahead (add-dia next up)]
                  ;;(println "up: " ahead (= ahead [11 0]))
                  (if (valid-pos diagram ahead)
                    (let [ch-n (get-ch diagram ahead)]
                      ;;(println "ch-h: " ch-n)
                      (if (= space ch-n)
                          ;;(println next (= next [11 2]))
                        (recur next down result (inc steps))
                        (if (= pipe ch-n)
                          (recur ahead up result (+ steps 2))
                          (recur ahead up (conj result ch-n) (+ steps 2)))))
                    (recur next down result (inc steps))))
                (if (= ch minus)
                  (recur next dir result (inc steps))
                  (if (= ch space)
                    { :result (apply str result) :steps (inc steps) }
                    (recur next dir (conj result ch) (inc steps))))))))))))
