(ns lcert.formal-test.p49terminal
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49terminal]))

(deftest terminal-continuation
  (is (b/has? 'p49tc_canonical))
  (is (b/has? 'p49tc_closed))
  (is (b/has? 'p49tc_closed_concrete)))
