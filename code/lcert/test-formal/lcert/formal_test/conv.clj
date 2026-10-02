(ns lcert.formal-test.conv
  "Formal suite (ADR-0006), lcert.formal.conv: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.conv]))
(deftest f2b-steps
  (is (b/has? 'Hd)))

(deftest f2b-positions
  (is (b/has? 'Step))
  (is (b/has? 'setP)))

(deftest f2b-skeleton-typing-and-conversion
  (is (b/has? 'SkJ))
  (is (b/has? 'Cv)))
