(ns aoc.core
  (:gen-class))

(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(defn get-lines
  "read file into collection"
  [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
