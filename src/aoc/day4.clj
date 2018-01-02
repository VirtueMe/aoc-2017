(ns aoc.day4
  (:gen-class))

(def passprases (get-lines "passprases.txt"))



(defn check-allowed
  "find if there are duplicates in the list"
  [list]
  (let [coll (distinct list)]
    (= list coll)))


(defn sort-w
  ""
  [words]
  (map sort words))

(defn sort-words
  ""
  [list]
  (map sort-w list))

(defn to-coll
  ""
  [passprase]
  (split passprase #"\s"))

(defn to-colls
  ""
  [lists]
  (map to-coll lists))

(defn only-allowed
  ""
  [list]
  (filter true? list))

(defn calc-allowed
  ""
  [coll]
  (map check-allowed coll))

(def get-passprases (to-colls passprases))
