(ns aoc.day8
  (:gen-class)
  (:require [clojure.core.match :refer [match]]))

(use 'aoc.core)
(use '[clojure.string :only (split)])


(def instructions ["b inc 5 if a > 1" "a inc 1 if b < 5" "c dec -10 if a >= 1" "c inc -20 if c == 10"])

(def abstree (map #(split % #"\s") instructions))

(def puzzle8-input (get-lines "day8.txt"))
(def puzzle8 (map #(split % #"\s") puzzle8-input))



(defn get-register
  ""
  [list name]
  (let [items (filter #(= name (get % 0)) list)]
    (if (empty? items)
      [name 0]
      (first items)
      )))

(defn update-register
  ""
  [list item]
  (let [items (filter #(= (get item 0) (get % 0)) list)]
    (if (empty? items)
      (conj list item)
      (let [index (.indexOf list (first items))]
        (assoc list index item)
        ))))

(defn test-op
  ""
  [op item value]
  (match op
    ">" (> (get item 1) value)
    "<" (< (get item 1) value)
    ">=" (> (get item 1) (dec value))
    "<=" (< (get item 1) (inc value))
    "==" (= (get item 1) value)
    "!=" (not (= (get item 1) value))
    ))

(defn do-op
  ""
  [op item value]
  (match op
    "inc" (+ (get item 1) value)
    "dec" (- (get item 1) value)
    :else value
    ))



(defn find-max-value
  ""
  [instructions]
  (loop [list instructions result []]
    (if (empty? list)
      (let [mx (apply max (map #(get % 1) result))]
        [mx result])
      (let [instruction (first list)]
        ;; (println "1: " instruction)
        (let [testreg (get instruction 4) testop (get instruction 5) testvalue (Integer. (get instruction 6))]
          ;; (println "2: " testreg testvalue)
          (let [testitem (get-register result testreg)]
            (if (test-op testop testitem testvalue)
              (let [reg (get instruction 0) op (get instruction 1) value (Integer. (get instruction 2))]
                ;; (println reg)
                (let [item (get-register result reg)]
                  (recur (rest list) (update-register result (assoc item 1 (do-op op item value))))))
              (recur (rest list) result)
            )))))))
