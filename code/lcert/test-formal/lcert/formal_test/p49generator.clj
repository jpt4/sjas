(ns lcert.formal-test.p49generator
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49generator]))

(deftest fuel-generator-is-defined
  (is (b/has? 'p49tau))
  (is (b/has? 'p49tau_terminal))
  (is (b/has? 'p49tau_terminal_zero))
  (is (b/has? 'p49tau_nonempty_pos))
  (is (b/has? 'p49tau_zero_nil)))
