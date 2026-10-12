(ns lcert.formal-test.p49fuel
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49fuel]))

(deftest strict-fuel-decrease
  (is (b/has? 'p49pow3_pos))
  (is (b/has? 'p49pot_leaf_decrease))
  (is (b/has? 'p49pot_node_decrease))
  ;; A binary potential would make the node transition non-strict.
  (is (b/rejects?
    '[j :- Nat]
    '(LT.lt (+ (p49pow3 j) (p49pow3 j))
       (+ (p49pow3 j) (p49pow3 j)))
    '[(omega)])))
