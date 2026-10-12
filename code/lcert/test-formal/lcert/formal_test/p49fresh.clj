(ns lcert.formal-test.p49fresh
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49fresh]))

(deftest fresh-hole-bound
  (is (b/has? 'p49c_bound_sound))
  (is (b/has? 'p49slots_bound_sound))
  (is (b/has? 'p49fresh_context))
  (is (b/has? 'p49fresh_slots))
  ;; A context containing hole zero cannot certify zero as a fresh bound.
  (is (b/rejects? '[]
    '(p49c_below (P49C.hole 0) 0)
    '[(omega)])))
