(ns bank-ocr.core-test
  (:require [clojure.test :refer :all]
            [bank-ocr.core :refer :all]))

(def all-numbers
"    _  _     _  _  _  _  _ 
  | _| _||_||_ |_   ||_||_|
  ||_  _|  | _||_|  ||_| _|")

(def all-numbers-with-fourth-line
"    _  _     _  _  _  _  _ 
  | _| _||_||_ |_   ||_||_|
  ||_  _|  | _||_|  ||_| _|
                           ")

(def all-numbers-top-line-misaligned
"   _  _     _  _  _  _  _  
  | _| _||_||_ |_   ||_||_|
  ||_  _|  | _||_|  ||_| _|
                           ")

(def valid-account-numbers [[7 1 1 1 1 1 1 1 1]
                            [1 2 3 4 5 6 7 8 9]
                            [4 9 0 8 6 7 7 1 5]])

(def invalid-account-numbers [[8 8 8 8 8 8 8 8 8]
                              [4 9 0 0 6 7 7 1 5]
                              [0 1 2 3 4 5 6 7 8]])

(defn split-lines
  [s]
  (clojure.string/split s #"\n"))

(deftest good-input
  (testing "Verify we can parse 123456789, no following empty line."
    (is (= (parse-account-number (split-lines all-numbers))
           '(1 2 3 4 5 6 7 8 9)))))

(deftest good-input-following-line
  (testing "Verify we can parse 123456789, with following empty line."
    (is (= (parse-account-number (split-lines all-numbers-with-fourth-line))
           '(1 2 3 4 5 6 7 8 9)))))

(deftest bad-input-top-line-misaligned
  (testing "Verify we do not parse 123456789 from malformed input"
    (is (= (parse-account-number (split-lines all-numbers-top-line-misaligned))
           '(1 \? \? 4 \? \? \? \? \?)))))

(deftest verify-file-parse-and-write
  (testing "Verify we can read an input file and write the correct results"
    (let [_ (parse-and-write-file (clojure.java.io/resource "user-story-1-testcases"))
          expected-results (slurp (clojure.java.io/resource "user-story-1-expected-output"))
          actual-results (slurp (clojure.java.io/resource "user-story-1-testcases.out"))]
      (is (= expected-results actual-results)))))

(deftest verify-valid-account-numbers
  (testing "Verify checksum agains known valid account numbers"
    (is (apply = true (map check-account-number valid-account-numbers)))))

(deftest verify-invalid-account-numbers
  (testing "Verify checksum agains known valid account numbers"
    (is (apply = false (map check-account-number invalid-account-numbers)))))

(deftest verify-file-parse-and-write-with-bad-input
  (testing "Verify we can read an input file containing malformed input write the correct results"
    (let [_ (parse-and-write-file (clojure.java.io/resource "user-story-3-testcases"))
          expected-results (slurp (clojure.java.io/resource "user-story-3-expected-output"))
          actual-results (slurp (clojure.java.io/resource "user-story-3-testcases.out"))]
      (is (= expected-results actual-results)))))

