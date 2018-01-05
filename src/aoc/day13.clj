(ns aoc.day13
  (:gen-class))

  (def firewall-regex #"(?<layer>[\d]+)(: )?(?<range>[\d]+)")

  (defn create-layers
    ""
    [input]
    (let [matcher (re-matcher firewall-regex input)]
      (if (.matches matcher)
          (let [layer (Integer. (.group matcher "layer")) range (Integer. (.group matcher "range"))]
            (println "> " layer " : " range)
            [(keyword (str layer)) {:range range :layer layer :pos 0 :caught false}]
            ))))

  (def puzzle13-sample-data ["0: 3" "1: 2" "4: 4" "6: 4"])
  (def puzzle13-sample (apply hash-map (flatten (map create-layers puzzle13-sample-data))))

  (defn add-step-direct
    ""
    [layer]
    (println layer)
    (update layer :pos (if (= (- (layer :range)) (layer :pos)) dec inc)))

  (defn add-step
    ""
    [layer]
      (println layer)
      (update-in layer [1] add-step-direct))
