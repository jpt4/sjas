(ns lcert.formal-test.p49all
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.p49all]))

(deftest all-depth-proposition-49
  (is (b/has? 'p49all_slot_step))
  (is (b/has? 'p49all_fuel))
  (is (b/has? 'p49all_prop49))
  (is (b/has? 'p49all_prop49_concrete))
  (is (b/has? 't49))
  (is (b/has? 'prop49))
  (is (b/has? 'prop49_concrete))
  (testing "the closed concrete term inhabits P49 at every depth"
    (is (not (b/rejects? '[k :- Nat]
      '(Rt (Check decCert) (List.nil Exp) (List.nil U)
         (t49 k) (P49 k))
      '[(exact (prop49_concrete k))])))))
