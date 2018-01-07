(ns aoc.day15
  (:gen-class)
  (:require [aoc.core :refer :all]))

(defn get-right-2bytes
  ""
  [n]
  (bit-and n 0xffff))


(defn equals-2bytes
  ""
  [x y]
  (= (get-right-2bytes x) (get-right-2bytes y)))


(defn generate
  ""
  [x y]
  (mod (* x y) 2147483647))

(defn generate-divider
  ""
  [x y & [divider]]
  (let [divider (if (nil? divider) 1 divider)]
    (loop [x (generate x y)]
      (if (= (mod x divider) 0)
        x
        (recur (generate x y))))))
(def million (* 1000 1000))
(def samplesize (* 40 million))
(def factor-a 16807)
(def factor-b 48271)

(def samplesize-part2 (* 5 million))

(defn check-sample
  ""
  [a b & [end divider-a divider-b]]
  (loop [a a b b end (if (nil? end) samplesize end) match 0]
    (if (= end 0)
      match
      (let [res-a (generate-divider a factor-a divider-a) res-b (generate-divider b factor-b divider-b)]
        (recur res-a res-b (dec end) (if (equals-2bytes res-a res-b) (inc match) match))
      ))))

  (def puzzle15-part1-sample-data [65 8921])

  (defn puzzle15-part1-sample
    ""
    []
    (apply check-sample puzzle15-part1-sample-data))

  (defn puzzle15-part2-sample
    ""
    []
    (apply check-sample (conj puzzle15-part1-sample-data samplesize-part2 4 8)))

  (def puzzle15-part1-data [618 814])

  (defn puzzle15-part1
    ""
    []
    (apply check-sample puzzle15-part1-data))

  (defn puzzle15-part2
    ""
    []
    (apply check-sample (conj puzzle15-part1-data samplesize-part2 4 8)))
