(ns aoc.day6
  (:gen-class))

(def banks [0 2 7 0])

(defn redis-banks
  ""
  [memory index value]
  (loop [banks memory index index value value]
    (if (= value 0)
      banks
      (if (= index (count banks))
        (recur banks 0 value)
        (recur (assoc banks index (inc (get banks index))) (inc index) (dec value)
        )))))

(defn redistribute
  ""
  [memory]
  (loop [banks memory history []]
    (if (and (> (count history) 0) (> (count history) (count (distinct history))))
      (str (count history) " " (- (dec (count history)) (.indexOf history banks)))
      (let [value (apply max banks)]
        (let [index (.indexOf banks value)]
          (let [re-banks (redis-banks (assoc banks index 0) (inc index) value)]
            (recur re-banks (conj history re-banks))
          ))))))
