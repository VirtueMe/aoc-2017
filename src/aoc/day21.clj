(ns aoc.day21
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))

(def puzzle21-data (get-lines "day21.txt"))

(def map-regex #"(?<when>[\.\/\#]+) => (?<then>[\.\/\#]+)")

(defn create-map
  ""
  [input]
  (let [matcher (re-matcher map-regex input)]
    (if (.matches matcher)
      { (mapv vec (mapv seq (split (.group matcher "when") #"/"))) (mapv vec (mapv seq (split (.group matcher "then") #"/"))) }
      {})))


(def puzzle21 (apply merge (map create-map puzzle21-data)))

(def start [[\. \# \.] [\. \. \#] [\# \# \#]])

(defn count-hash
  ""
  [item]
  (count (filter #(= % \#) (flatten item))))

(defn find-candidates
  ""
  [coll item]
  (let [items (keys coll)]
    (let [result (filter #(= (count %) (count item)) items)]
      (let [sum-hash (count-hash item)]
        (filter #(= (count-hash %) sum-hash) result)))))

(defn flip
  ""
  [item]
  (->> item
    (mapv rseq)
    (mapv vec)
    ))

(defn flip-up
  ""
  [item]
  (vec (reverse item)))

(defn rotate2
  ""
  [item]
  (->> item
    (apply mapv vector ,,,)
    (flip ,,,)))

(defn rotate3
  ""
  [item]
  (-> item
    (assoc-in ,,, [0 0] (get-in item [1 0]))
    (assoc-in ,,, [0 1] (get-in item [0 0]))
    (assoc-in ,,, [0 2] (get-in item [0 1]))
    (assoc-in ,,, [1 0] (get-in item [2 0]))
    (assoc-in ,,, [1 2] (get-in item [0 2]))
    (assoc-in ,,, [2 0] (get-in item [2 1]))
    (assoc-in ,,, [2 1] (get-in item [2 2]))
    (assoc-in ,,, [2 2] (get-in item [1 2]))
     ))

(defn rotate3-side
  ""
  [item]
  (-> item
    (rotate3)
    (rotate3)
    (rotate3)
    ))

(defn size
  ""
  [item]
  (let [height (count item)]
    (* height height)))

(defn split-enhancement4
  ""
  [item]
  (let [item (vec (map vec (map #(map vec %) (map #(partition 2 %) item))))]
    [[[(get-in item [0 0]) (get-in item [1 0])] [[(get-in item [0 1]) (get-in item [1 1])]]]
     [[(get-in item [2 0]) (get-in item [3 0])] [[(get-in item [2 1]) (get-in item [3 1])]]]
    ]))

(defn find-enhancement
  ""
  [coll item]
    (let [(if (= (count item) 3) rotate3 rotate2)]
      (loop [steps (take (size item) (iterate inc 0)) item item]
        (if (empty? steps)
          nil
          (let [result (coll item)]
            (if (not (nil? result))
              result
              (recur (rest steps) (f item))))))))
