(ns aoc.day9-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [aoc.day9 :refer :all]))


(deftest one-group-count
  (testing "{} should count as 1"
    (is (= 1 (group-count [])))))

(deftest three-group-count
  (testing "{{{}}} should count as 3"
    (is (= 3 (group-count [[[]]])))))

(deftest three-group-count-other
  (testing "{{}, {}} should count as 3"
    (is (= 3 (group-count [[] []])))))


(deftest six-group-count
  (testing "{{{}, {}, {{}}} should count as 6"
    (is (= 6 (group-count [[[] [] [[]]]])))))

(deftest five-group-count
  (testing "{{},{},{},{}}} should count as 5"
    (is (= 5 (group-count [[] [] [] []])))))
