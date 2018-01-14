(ns aoc.day18
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]
            [clojure.string :refer [split]]))


(defn set-value
  ""
  [row]
  (if (= 2 (count row))
    row
    (let [item (row 2)]
      (let [value (re-matches #"-?[\d]+" item)]
        (if (nil? value)
          row
          (assoc row 2 (read-string value)))))))

(def puzzle18-sample [["set" "a" 1] ["add" "a" 2] ["mul" "a" "a"] ["mod" "a" 5] ["snd" "a"] ["set" "a" 0] ["rcv" "a"] ["jgz" "a" -1] ["set" "a" 1] ["jgz" "a" -2]])

(def puzzle18-data (vec (map set-value (map #(split % #" ") (get-lines "day18.txt")))))

(defn play-part1
  ""
  [data]
  (loop [registers {} pos 0 sound 0]
    (if (= pos (count data))
      registers
      (let [current (data pos)]
        (let [register (keyword (current 1))]
        (let [result (match (current 0)
          "set" (let [value (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                [(assoc registers register value) (inc pos) sound])
          "add" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                  [(assoc registers register (+ value value2)) (inc pos) sound])
          "mul" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                  [(assoc registers register (* (if (nil? value) 0 value) (if (nil? value2) 0 value2))) (inc pos) sound])
          "mod" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                  [(assoc registers register (mod value value2)) (inc pos) sound])
          "snd" [registers (inc pos) (registers register)]
          "rcv" (if (> (registers register) 0)
                [sound]
                [registers (inc pos) sound])
          "jgz" (if (> (registers register) 0)
                [registers (+ pos (current 2)) sound]
                [registers (inc pos) sound])
          :else [registers (inc pos) sound])]
          (if (= 1 (count result))
            (result 0)
            (recur (result 0) (result 1) (result 2))
            )))))))


(defn create-process
  ""
  [name]
  { :name (str name) :pos 0 :messages [] :registers { :p name } :snd 0 })

(defn seta
  ""
  [process registers pos]
  (assoc process :registers registers :pos pos))

(defn play-part2
  ""
  [data]
  (loop [active (create-process 0) wait (create-process 1) waiting 0 finished 0]
    (print (active :name) " -> " (active :pos))
    (let [pos (active :pos)]
      (if (= pos (count data))
        (if (= finished 1)
          { :current active :wait wait }
          (recur wait active waiting 1))
        (let [current (data pos)]
          (println " " current)
          (let [register (keyword (current 1)) registers (active :registers)]
            (let [result (match (current 0)
              "set" (let [value (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                      ;; (println " = " value)
                      [(seta active (assoc registers register value) (inc pos)) wait])
              "add" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                      [(seta active (assoc registers register (+ value value2)) (inc pos)) wait])
              "mul" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                      [(seta active (assoc registers register (* (if (nil? value) 0 value) (if (nil? value2) 0 value2))) (inc pos)) wait])
              "mod" (let [value (registers register) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                      [(seta active (assoc registers register (mod value value2)) (inc pos)) wait])
              "snd" [(assoc active :pos (inc pos) :snd (inc (active :snd))) (assoc wait :messages (conj (wait :messages) (registers register))) 0]
              "rcv" (if (= 0 (count (active :messages)))
                      (if (= waiting 1)
                        [] ;; both processes
                        [wait active 1])
                      (let [value (first (active :messages))]
                        [(assoc (seta active (assoc registers register value) (inc pos)) :messages (vec (rest (active :messages)))) wait]))
              "jgz" (let [value (if (number? (read-string (current 1))) (read-string (current 1)) (registers register)) value2 (if (string? (current 2)) (registers (keyword (current 2))) (current 2))]
                      ;; (println "jgz" value " -> " (read-string (current 1)) " <> " (string? (read-string (current 1))) " :: " (registers register))
                      ;;(println ": " value " -> " (current 2))
                      [(assoc active :pos (+ pos (if (> value 0) value2 1))) wait]))]
              ;; (print " >> " result)
              (if (= 0 (count result))
                { :current active :wait wait }
                (recur (result 0) (result 1) (if (= 3 (count result)) (result 2) waiting) finished)))))))))
