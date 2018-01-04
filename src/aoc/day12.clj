(ns aoc.day12
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]))


  (def puzzle12-sample-data ["0 <-> 2" "1 <-> 1" "2 <-> 0, 3, 4" "3 <-> 2, 4" "4 <-> 2, 3, 6" "5 <-> 6" "6 <-> 4, 5"])
  (def puzzle12-data (get-lines "day12.txt"))
  ;; (def puzzle12-sample [["0" ["2"]] ["1" ["1"]] ["2" ["0" "3" "4"]] ["3" ["2" "4"]] ["4" ["2" "3" "6"]] ["5" ["6"]] ["6" ["4" "5"]]])

  (def program-regex #"(?<name>[^\s]+)( <-> )?(?<list>.*)?")
  (defn create-programs
    ""
    [input]
    (let [matcher (re-matcher program-regex input)]
      (if (.matches matcher)
              (let [programs (.group matcher "list")]
               [(.group matcher "name") (if (= programs "") [] (split programs  #", "))])
               ["no-name"])
    ))

  (def puzzle12-sample (map create-programs puzzle12-sample-data))
  (def puzzle12 (map create-programs puzzle12-data))


  (defn find-group
    ""
    [name groups]
    (first (filter #(= name (% 0)) groups)))

  (defn count-groups
    ""
    [name groups]
    (let [program (find-group name groups)]
      (loop [list (program 1) found (set [name])]
        ;; (println list " -> "found)
        (if (empty? list)
          { :list found :count (count found) }
          (let [current (find-group (first list) groups)]
            ;; (println (rest list) "-> " (set (current 1)) " >> "(difference found (set (current 1))))
            (recur (distinct (concat (rest list) (difference (set (current 1)) found))) (set (concat found (set [(current 0)])))
          ))))))

  (defn total-groups
    ""
    [groups]
    (loop [list (set (map #(% 0) groups)) total 0]
      (if (empty? list)
        total
        (let [program (first list)]
          (let [res (count-groups program groups)]
            (recur (difference list (res :list)) (inc total))
          )))))

(def puzzle12-part1 ((count-groups "0" puzzle12) :count))
