(ns aoc.day9
  (:gen-class)
  (:require [clojure.core.match :refer [match]]
            [aoc.core :refer :all]))

(def puzzle9 (first (get-lines "day9.txt")))

(defn garbage-read
  ""
  [input pos]
  (loop [pos pos counter 0]
    (let [char (get input pos)]
      (match char
          \! (recur (+ pos 2) counter)
          \> (let [result (+ pos 1)]
              {:pos result :counter counter})
          :else (recur (+ pos 1) (+ counter 1))
        )
      )))

(defn group-read-pos
  ""
  [input pos]
  input
  )


(defn stream-read
  ""
  [input]
  (if (or (nil? input) (empty? input ))
    nil
    (loop [pos 0 result nil current nil counter 0]
      ;; (println pos " : " counter " : ")
      (if (= pos (count input))
        {:counter counter :result result}
        (let [char (get input pos)]
          ;; (println "char: " char)
          (match char
            \{ (recur (+ pos 1) result (conj current []) counter)
            \} (let [curr (peek current) stack (pop current)]
                (recur
                  (+ pos 1)
                  (if (empty? stack)
                    (conj result curr)
                    result)
                  (if (empty? stack)
                    stack
                    (conj (pop stack) (conj (peek stack) curr)))
                  counter))
            \! (recur (+ pos 2) result current counter)
            \< (let [res (garbage-read input (+ pos 1))]
                  (let [newcounter (+ counter (get res :counter))]
                    (recur (get res :pos) result current newcounter)))
            \, (recur (+ pos 1) result current counter)
            :else (recur (+ pos 1) result current counter)
            ))))))

(defn group-count-value
  ""
  [list value]
  ;; (println "-" value)
  (if (empty? list)
    value
    (let [coll (map #(group-count-value % (inc value)) list)]
        ;;(println coll)
        (+ value (apply + coll))
      )))

(defn group-count
  ""
  [input]
  (if (nil? input)
    0
    (group-count-value input 0)))
