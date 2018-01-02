(ns aoc.day7
  (:gen-class))


(use 'aoc.core)
(use '[clojure.string :only (split)])

(def circus [["pbga" 66 []] ["xhth" 57 []] ["ebii" 61 []] ["havc" 66 []] ["ktlj" 57 []] ["fwft" 72 ["ktlj" "cntj" "xhth"]] ["qoyq" 66 []] ["padx" 45 ["pbga" "havc" "qoyq"]] ["tknk" 41 ["ugml" "padx" "fwft"]] ["jptl" 61 []] ["ugml" 68 ["gyxo", "ebii", "jptl"]] ["gyxo" 61 []] ["cntj" 57 []]])

(def program-regex #"(?<name>[^\s]+) \((?<weight>[0-9]+)\)( -> )?(?<list>.*)?")

(defn create-program
  ""
  [input]
  (let [matcher (re-matcher program-regex input)]
    (if (.matches matcher)
            (let [programs (.group matcher "list")]
             [(.group matcher "name") (Integer. (.group matcher "weight")) (if (= programs "") [] (split programs  #"[\s,?]+"))])
             ["no-name"])
  ))

(def puzzle7-input (get-lines "day7.txt"))
(def puzzle7 (map create-program puzzle7-input))

(defn holds-programs
  ""
  [item]
  (> (count (get item 2)) 0))

(defn get-children
  ""
  [list]
  (get list 2))

(defn is-root
  ""
  [item list]
  (let [name (get item 0) children (flatten (map get-children list))]
    (= (.indexOf children name) -1)
    ))

(defn all-but
  ""
  [item not-this]
  (let [name (get not-this 0)]
    (not (= name (get item 0)))))

(defn find-root
  ""
  [coll]
  (loop [branches (vec (filter holds-programs coll)) index 0]
    (let [program (get branches index)]
      ;; (println program " " branches)
      (let [programs (filter #(all-but % program) branches)]
      (if (is-root program programs)
        program
        (recur branches (inc index)))))))

(defn get-child
  ""
  [name coll]
  (first (filter #(= name (get % 0)) coll)))

(defn tower-weight
  ""
  [name coll]
  (let [child (get-child name coll)]
    (if (= (get child 2) [])
      (get child 1)
      (let [tower (assoc child 2 (map #(tower-weight % coll) (get child 2)))]
        (+ (get tower 1) (apply + (get tower 2)))
        ))))

(defn create-tower
  ""
  [name coll]
  (let [child (get-child name coll)]
    (let [tower (assoc child 2 (map #(tower-weight % coll) (get child 2)))]
      (let [child-weight (apply + (get tower 2))]
        (conj (conj tower (+ (get tower 1) child-weight)) child-weight)
    ))))

(defn find-weight
  ""
  [coll root]
    (let [children (get root 2)]
      (loop [list children result []]
        (if (empty? list)
          result
          (let [child (first list)]
            (recur (rest list) (conj result (create-tower child coll))
            ))))))


  (defn find-weight-root
    ""
    [coll]
    (find-weight coll (find-root coll)))
