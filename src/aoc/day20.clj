(ns aoc.day20
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(def puzzle20-sample '( { :p [3 0 0] :v [2 0 0] :a [-1 0 0] } { :p [4 0 0] :v [0 0 0] :a [-2 0 0] }))

(def particle-regex #"p=<(?<px>-?[\d]+),(?<py>-?[\d]+),(?<pz>-?[\d]+)>, v=<(?<vx>-?[\d]+),(?<vy>-?[\d]+),(?<vz>-?[\d]+)>, a=<(?<ax>-?[\d]+),(?<ay>-?[\d]+),(?<az>-?[\d]+)>")

(defn create-particle
  ""
  [input]
  (let [matcher (re-matcher particle-regex input)]
    (if (.matches matcher)
      { :p [(Integer. (.group matcher "px")) (Integer. (.group matcher "py")) (Integer. (.group matcher "pz"))] :v [(Integer. (.group matcher "vx")) (Integer. (.group matcher "vy")) (Integer. (.group matcher "vz"))] :a [(Integer. (.group matcher "ax")) (Integer. (.group matcher "ay")) (Integer. (.group matcher "az"))] }
      {})))

(def puzzle20-data (map create-particle (get-lines "day20.txt")))

(defn move*
  ""
  [particle]
  (-> particle
  (update :v #(vec (map + % (particle :a))))
  (update :p #(vec (map + % (particle :v) (particle :a))))
  (assoc :d (reduce + (map #(Math/abs %) (apply map + (vals (select-keys particle [:p :v :a]))))))))

(defn nearest
  ""
  [input]
  (let [items (flatten (map vals (map #(select-keys % [:d]) input)))]
    (let [low (apply min items)]
      (.indexOf items low))))

(defn collide
  ""
  [input]
  (let [items (frequencies (map first (map vals (map #(select-keys % [:p]) input))))]
    (let [coll (filter #(> (second %) 1) items)]
      (if (empty? coll)
        input
        (loop [list (keys coll) result input]
          (if (empty? list)
            result
            (let [item (first list)]
              (recur (rest list) (filter #(not (= item (% :p))) result)))))))))

(defn calculate-fn
  ""
  [data f]
  (loop [items (map move* data) index -1 steps 0]
    (if (= steps 1000)
      index
      (let [idx (f items)]
        (recur (map move* items) idx (if (= idx index) (inc steps) 1))
        ))))

(defn calculate-nearest
  ""
  [data]
  (calculate-fn data nearest))

(defn calculate-collide
  [data]
  (loop [items (map move* data) index -1 steps 0]
    (if (= steps 1000)
      index
      (let [left (collide items)]
        (recur (map move* left) (count left) (if (= (count left) index) (inc steps) 1)))
        )))
