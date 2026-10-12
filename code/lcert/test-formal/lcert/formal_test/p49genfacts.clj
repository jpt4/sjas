(ns lcert.formal-test.p49genfacts
  (:require [clojure.test :refer [deftest is]]
            [lcert.formal.base :as b]
            [lcert.formal.p49genfacts]))

(deftest fresh-child-ids-avoid-pending-slots
  (is (b/has? 'p49gf_beq_small_false))
  (is (b/has? 'p49gf_avoids_of_below))
  (is (b/has? 'p49gf_fresh_avoids))
  (is (b/has? 'p49gf_fresh_succ_avoids))
  (is (b/has? 'p49gf_right_avoids_left_pending)))
