(ns aoc.day17
  (:gen-class))

(defn spinlock
  ""
  [x steps]
  (loop [pos 0 step 0 value 1 list [0]]
    ;; (println pos ":" step "->" list)
    (if (= value (inc x))
      ((vec list) (inc pos))
      (let [pos (inc pos) step (inc step)]
        (if (= step steps)
          (let [pos (if (= pos (count list)) 1 (inc pos))]
            (let [[a b] (split-at pos list)]
              ;; (println pos " -> " a " : " b)
              (recur pos 0 (inc value) (concat a [value] b))))
          (recur (if (= pos (count list)) 0 pos) step value list))))))
