(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest file-length
  (testing "reading file"
    (is (= 512 (count (get-lines "passprases.txt"))))))
