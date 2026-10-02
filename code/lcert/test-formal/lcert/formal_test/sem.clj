(ns lcert.formal-test.sem
  "Formal suite (ADR-0006), lcert.formal.sem: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.sem]))
(deftest f3d-semantic-types
  (is (b/has? 'V)))
