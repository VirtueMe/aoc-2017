(ns aoc.day10-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [aoc.day10 :refer :all]))

(def testpuzzle10 (take 5 (iterate inc 0)))

(def testinputs [3, 4, 1, 5])


(deftest control-number
  (testing "should be equal to"
    (is (= '(3 4) (take 2 (elves-hash testpuzzle10 testinputs))))))
