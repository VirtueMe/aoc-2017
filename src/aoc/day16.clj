(ns aoc.day16
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(def puzzle16-data (split (first (get-lines "day16.txt")) #","))

(def dancing-programs (vec (map char (take 16 (iterate inc (int \a))))))

(defn spin
  ""
  [n coll]
  (concat (take-last n coll) (take (- (count coll) n) coll)))

(defn exchange
  ""
  [a b coll]
  (let [coll (vec coll)]
    (seq (assoc (assoc coll a (coll b)) b (coll a)))))

(defn partner
  ""
  [a b coll]
  (exchange (.indexOf coll a) (.indexOf coll b) coll))

(defn translate-moves
  ""
  [input]
  (match (first input)
    \p [partner (first (rest input)) (first (take-last 1 (rest input)))]
    \x (let [list (split (subs input 1) #"/")]
          (let [[a b] list]
            [exchange (Integer. a) (Integer. b)]))
    \s [spin (Integer. (subs input 1))]))

(defn create-moves
  ""
  []
  (map translate-moves puzzle16-data))

(defn dance
  ""
  [moves programs]
  (loop [list moves, result programs]
    (if (empty? list)
      result
      (let [move (first list)]
        (recur (rest list) (apply (first move) (conj (vec (rest move)) result)))
          ))))

(def billion (* 1000 1000 1000))

(defn whole-dance
  ""
  [moves programs n]
  (loop [n n result programs]
    (if (= n 0)
    result
    (recur (dec n) (dance moves result)))))

(defn loop-until-equal
  ""
  [moves programs]
  (loop [n 0 result programs]
    (if (and (> n 0) (= result programs))
      n
      (recur (inc n) (dance moves result)))))

(defn puzzle16-part1
  ""
  []
  (apply str (whole-dance (create-moves) dancing-programs 1)))

(defn puzzle16-part2
  ""
  []
  (apply str (whole-dance (create-moves) dancing-programs 40)))
