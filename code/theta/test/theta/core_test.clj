(ns theta.core-test
  (:require [clojure.test :refer :all]
            [theta.core :refer :all]))

;;constants
(deftest c0-test
  (testing "(lg-eval 'c0)"
    (is (= (lg-eval 'c0) 'c0))))
(deftest c1-test
  (testing "(lg-eval 'c1)"
    (is (= (lg-eval 'c1) 'c1))))
(deftest c2-test
  (testing "(lg-eval 'c2)"
    (is (= (lg-eval 'c2) 'c2))))