(ns aoc.day1-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [aoc.day1 :refer :all]))

(deftest nil-list
  (testing "nil list returns 0"
    (is (= 0 (captcha-sum nil)))))

(deftest empty-list
  (testing "nil list returns 0"
    (is (= 0 (captcha-sum "")))))

(deftest single-element
  (testing "single element returns 0"
    (is (= 0 (captcha-sum "1")))))

(deftest two-different-elements
  (testing "two different numbers will equal 0"
    (is (= 0 (captcha-sum "12")))))

(deftest first-argument
  (testing "first argument of task"
    (is (= 3 (captcha-sum "1122")))))

(deftest second-argument
  (testing "second argument of task"
    (is (= 4 (captcha-sum "1111")))))

(deftest third-argument
  (testing "second argument of task"
    (is (= 0 (captcha-sum "1234")))))

(deftest fourth-argument
  (testing "second argument of task"
    (is (= 9 (captcha-sum "91212129")))))

(deftest ss-min-key
  (testing "min-key first"
    (is (= 32 (apply min-key #(Math/abs %) [790	99	345	1080	32	143	1085	984	553	98	123	97	197	886	125	947]
    )))))
