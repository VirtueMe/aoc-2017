(ns aoc.day10
  (:gen-class))

(def puzzle10 (take 256 (iterate inc 0)))

(def inputs '(14 58 0 116 179 16 1 104 2 254 167 86 255 55 122 244))
(def byteinputs "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244")
(def suffix '(17 31 73 47 23))

(defn create-lengths
  ""
  [input]
  (flatten
    ;; (repeat 64
      (concat
        (map #(int %) input) suffix)))
    ;; )

(def test-input (create-lengths ""))
(def test-input1 (create-lengths "1,2,3"))
(def test-input2 (create-lengths "1,2,4"))
(def ascii-inputs (create-lengths byteinputs))


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

(defn elves-hash-repeated
  ""
  [stream inputs pos skip]
  (loop [re-stream stream re-inputs inputs pos pos skip skip]
    (if (empty? re-inputs)
      {:stream re-stream :pos pos :skip skip}
      (let [len (first re-inputs)]
        ;; do-something with the stream
        ;; (println "- " position " : " len " : " skip)
        (let [result (do-hash re-stream (mod pos (count stream)) len)]
          ;; (println ":: " result)
          ;(println (count re-inputs) ": " position " + " len " -> " (count (distinct result)))
          (recur result (rest re-inputs) (+ pos len skip) (mod (inc skip) (count stream))))))
        ))

(defn elves-hash
  ""
  [stream inputs & [n]]
  (loop [pos 0 skip 0 result stream n (if (nil? n) 1 n)]
    (if (= n 0)
      result
      (let [res (elves-hash-repeated result inputs pos skip)]
        ;(println (get res :stream))
        (recur (get res :pos) (get res :skip) (get res :stream) (dec n))
        ))))

(defn dense-hash
  ""
  [stream]
  (loop [stream stream result []]
    (if (empty? stream)
      result
      (recur (drop 16 stream) (conj result (apply bit-xor (take 16 stream)))))))

(defn hex-notation
  ""
  [stream]
  (map #(format "%02x" %) (take 16 stream)))

(defn create-hex
  ""
  [key]
  (hex-notation (dense-hash (elves-hash puzzle10 key 64))))

(defn puzzle10-part1
  ""
  []
  (apply * (take 2 (elves-hash puzzle10 inputs))))

(defn puzzle10-part2
  ""
  []
  (apply str (create-hex ascii-inputs)))
