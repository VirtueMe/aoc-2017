(ns aoc.day14
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [aoc.day10 :refer :all]
            [clojure.string :refer [split]]))

(defn bits
  ""
  [char]
  (match char
    \0 0 ;; 0000
    \1 1 ;; 0001
    \2 1 ;; 0010
    \3 2 ;; 0011
    \4 1 ;; 0100
    \5 2 ;; 0101
    \6 2 ;; 0110
    \7 3 ;; 0111
    \8 1 ;; 1000
    \9 2 ;; 1001
    \a 2 ;; 1010
    \b 3 ;; 1011
    \c 2 ;; 1100
    \d 3 ;; 1101
    \e 3 ;; 1110
    \f 4 ;; 1111
    ))

  (defn hashdots
    ""
    [char]
    (match char
      \0 '( "." "." "." ".") ;; 0000
      \1 '( "." "." "." "#") ;; 0001
      \2 '( "." "." "#" ".") ;; 0010
      \3 '( "." "." "#" "#") ;; 0011
      \4 '( "." "#" "." ".") ;; 0100
      \5 '( "." "#" "." "#") ;; 0101
      \6 '( "." "#" "#" ".") ;; 0110
      \7 '( "." "#" "#" "#") ;; 0111
      \8 '( "#" "." "." ".") ;; 1000
      \9 '( "#" "." "." "#") ;; 1001
      \a '( "#" "." "#" ".") ;; 1010
      \b '( "#" "." "#" "#") ;; 1011
      \c '( "#" "#" "." ".") ;; 1100
      \d '( "#" "#" "." "#") ;; 1101
      \e '( "#" "#" "#" ".") ;; 1110
      \f '( "#" "#" "#" "#") ;; 1111
      ))


(defn find-square
  ""
  [key]
  (loop [steps (take 128 (iterate inc 0)) result []]
    (if (empty? steps)
      result
      (let [row (create-hex (create-lengths (str key "-" (str (first steps)))))]
          ;; (println (first steps) " " row)
          (recur (rest steps) (conj result row)))
        )))

(defn calculate-row
  ""
  [row]
  (apply + (map bits (seq (apply str row)))))

(defn create-hash
  ""
  [row]
  (map hashdots (seq (apply str row))))

(defn count-used
  ""
  [grid]
  (apply + (map calculate-row grid)))

(defn hash-grid
  ""
  [grid]
  (map vec (map flatten (map create-hash grid))))

(def left [-1, 0])
(def right [1, 0])
(def up [0, -1])
(def down [0, 1])

(def neigbours [left up right down])

(defn inside
  ""
  [pos]
  (<= 0 pos 127))

(defn mark-regions
  ""
  [grid x y region]
  (loop [grid (assoc-in grid [y x] region) list neigbours]
    ;; (println x ":" y "-> " (first grid))
    (if (empty? list)
      grid
      (let [[nx ny] (first list)]
        (let [px (+ x nx) py (+ y ny)]
          ;; (println px ":" py " -> " (inside px) " = " (inside py))
          (if (and (inside px) (inside py))
            (if (= "#" ((grid py) px))
              (let [result (mark-regions grid px py region)]
                ;; (println px ":" py "->" region)
                (recur result (rest list)))
              (recur grid (rest list)))
            (recur grid (rest list))
              ))))))

(defn find-regions
  ""
  [grid]
  (let [height (count grid) max-x (dec (count (first grid)))]
    (loop [x 0 y 0 regions 0 grid (vec grid)]
      (if (= y height)
        regions
        ;; grid
        (if (= "#" ((grid y) x))
          (let [value (inc regions)]
            (recur (if (= x max-x) 0 (inc x)) (if (= x max-x) (inc y) y) value (mark-regions grid x y value)))
          (recur (if (= x max-x) 0 (inc x)) (if (= x max-x) (inc y) y) regions grid)
            )))))

(defn puzzle14-part1
  ""
  []
  (count-used (find-square "nbysizxe")))

(defn puzzle14-part2
  ""
  []
  (find-regions (hash-grid (find-square "nbysizxe"))))
