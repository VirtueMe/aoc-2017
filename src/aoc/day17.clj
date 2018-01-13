(ns aoc.day17
  (:gen-class))

(defn calc-pos
  [length pos steps]
  (if (< length steps)
    (let [rest (if (= pos 0) steps (- (+ steps pos) length))]
      ;; (println ":: " rest " : " length " - " steps)
      (if (> rest length)
        (mod rest length)
        (if (= rest length)
          0
          rest
            )))
    (let [newpos (+ pos steps)]
      (if (< newpos length)
        newpos
        (- newpos length)
      ))))


(defn spinlock
  ""
  [x steps]
  (loop [pos 0 step 0 value 1 list [0]]
    ;;(println pos " : " value " -> " (count list))
    (if (= value (inc x))
      { :value ((vec list) (inc pos)) :list (take x list) }
      (let [pos (inc pos) step (inc step)]
        (if (= step steps)
          (let [pos (if (= pos (count list)) 1 (inc pos))]
            ;; (println pos " : " value " -> " (count list))
            ;; (println (take 5 list) " : " pos)
            (let [[a b] (split-at pos list)]
              ;; (println pos " -> " a " : " b)
              (recur pos 0 (inc value) (concat a [value] b))))
          (recur (if (= pos (count list)) 0 pos) step value list))))))

(defn spinlock2
  ""
  [x steps]
  (loop [pos 0 value 1 list [0 1]]
    ;;(println pos "->" list)
    (if (= value (inc x))
      { :list (take 2 list) }
      (let [pos (inc (calc-pos value pos steps))]
        ;; (println (assoc list 1 value))
        ;; (let [[a b] (split-at pos list)]
          ;; (println pos " : " value " -> " (count list))
          ;; (println (take (count list) list) " : " pos)
          ;; (println pos " -> " a " : " b)
          ;; (println " - - - - - - - - - - - - ")
          (recur pos (inc value) (if (= pos 1) (assoc list 1 value) list))))))
