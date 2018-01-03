(ns aoc.day11-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [aoc.day11 :refer :all]))

(deftest three-steps-away
  (testing "3 steps away"
    (is (= 3 (calculate-steps '("ne" "ne" "ne"))))))

(deftest zero-steps-away
  (testing "0 steps away"
    (is (= 0 (calculate-steps '("ne" "ne" "sw" "sw"))))))

(deftest two-steps-away
  (testing "2 steps away"
    (is (= 2 (calculate-steps '("ne" "ne" "s" "s"))))))

(deftest s-s-sw
  (testing "3 steps away"
    (is (= 3 (calculate-steps '("se" "sw" "se" "sw" "sw"))))))
