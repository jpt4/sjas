(ns lcert.formal-test.p49slotsunique
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49slotsunique]))

(deftest fuel-slot-invariant
  (is (b/has? 'p49su_unique))
  (is (b/has? 'p49su_node))
  (is (b/has? 'p49su_leaf_fuel))
  (is (b/has? 'p49su_node_fuel)))
