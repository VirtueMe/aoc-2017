(ns aoc.day10
  (:gen-class))

(def puzzle10 (take 256 (iterate inc 0)))

(def inputs [14 58 0 116 179 16 1 104 2 254 167 86 255 55 122 244])
(def byteinputs "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244")
(def suffix [17 31 73 47 23])

(def ascii-inputs (flatten (repeat 64 (concat (map #(int %) byteinputs) suffix))))

(defn do-hash
  ""
  [stream pos len]
  (if (= pos 0)
    (concat (reverse (take len stream)) (drop len stream))
    (let [overflow (if (< (count stream) (+ pos len)) (- (+ pos len) (count stream)) 0)]
      ;(println "> " overflow " l: " len)
      (if (= overflow 0)
        (concat (take pos stream) (reverse (take len (drop pos stream))) (drop (+ pos len) stream))
        (let [reversed (reverse (concat (drop pos stream) (take overflow stream)))]
          ;(println (count reversed))
          (concat (drop (- len overflow) reversed) (drop overflow (take pos stream)) (take (- len overflow) reversed))
          )))))

(defn elves-hash
  ""
  [stream inputs]
  (loop [re-stream stream re-inputs inputs pos 0 skip 0]
    (if (empty? re-inputs)
      re-stream
      (let [len (first re-inputs) position (if (> pos (count re-stream)) (- pos (count re-stream)) pos)]
        ;; do-something with the stream
        ;; (println "- " position " : " len " : " skip)
        (let [result (do-hash re-stream position len)]
          ;; (println ":: " result)
          ;(println (count re-inputs) ": " position " + " len " -> " (count (distinct result)))
          (recur result (rest re-inputs) (+ position len skip) (inc skip))
        )))))

(defn dense-hash
  ""
  [stream]
  (loop [stream stream result nil]
    (if (empty? stream)
      result
      (recur (drop 16 stream) (conj result (apply bit-xor (take 16 stream)))))))

(defn hex-notation
  ""
  [stream]
  (map #(format "%x" %) (take 16 stream)))

(def puzzle10-part1 (apply * (take 2 (elves-hash puzzle10 inputs))))
