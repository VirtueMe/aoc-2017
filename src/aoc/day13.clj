(ns aoc.day13
  (:gen-class)
  (:require [aoc.core :refer :all]))

  (def firewall-regex #"(?<layer>[\d]+)(: )?(?<range>[\d]+)")

  (defn create-layers
    ""
    [input f]
    (let [matcher (re-matcher firewall-regex input)]
      (if (.matches matcher)
          (let [layer (Integer. (.group matcher "layer")) range (Integer. (.group matcher "range"))]
            ;; (println "> " layer " : " range)
            (f layer range)
            ))))

  (defn create-object
    ""
    [layer range]
    {:id layer :range range :layer layer :pos 0 :caught false :f inc})

  (defn create-hash
        ""
        [layer range]
        [(keyword (str layer)) (create-object layer range)])


  (def puzzle13-sample-data ["0: 3" "1: 2" "4: 4" "6: 4"])
  (def puzzle13-sample (apply hash-map (flatten (map #(create-layers % create-hash) puzzle13-sample-data))))

  (def puzzle13-sample2 (map #(create-layers % create-object) puzzle13-sample-data))
  (def puzzle13-data (get-lines "day13.txt"))
  (def puzzle13 (map #(create-layers % create-object) puzzle13-data))

  (defn add-step-direct
    ""
    [layer]
    (println layer)
    (update layer :pos (if (= (- (layer :range)) (layer :pos)) dec inc)))

  (defn add-step
    ""
    [layer]
      ;; (println layer)
      (update-in layer [1] add-step-direct))

  (defn on-walker
    ""
    [element walker]
    (if (= walker (element :layer)) (assoc element :caught (= (element :pos) 0)) nil))

  (defn move-range
    ""
    [element]
    (let [el (assoc element :pos ((element :f) (element :pos)))]
      (let [pos (el :pos)]
        (assoc el :f (if (= pos 0) inc (if (= pos (dec (el :range))) dec (el :f)))))))

  (defn walk-through
    ""
    [list & [exitonhit]]
    (loop [list list walker 0 result []]
      (if (empty? list)
        (let [success true]
          (println "last: " success)
          (if (true? exitonhit) nil result))
        (let [el (first list)]
          ;; (println el)
          (let [hit (on-walker el walker)]
            (let [moved (map move-range list)]
            (if (nil? hit)
              (recur (vec moved) (inc walker) result)
              (if (and (hit :caught) (or exitonhit false))
                hit
                (recur (vec (rest moved)) (inc walker) (if (false? (hit :caught)) result (conj result hit)))
                  ))))))))

    (defn calc-severity
      ""
      [list]
      (if (empty? list)
      0
      (apply + (map #(* (% :range) (% :layer)) list))))

(defn wait-for-it
  ""
  [list]
  (loop [step 0 list list]
    (let [result (walk-through list true)]
      (println step (if (nil? result) " success " (result :id)))
      (if (nil? result)
        step
        (recur (inc step) (map move-range list))
    ))))

(defn puzzle13-part1
  ""
  []
  (calc-severity (walk-through puzzle13)))

(defn puzzle13-part2
  ""
  []
  (wait-for-it puzzle13))
