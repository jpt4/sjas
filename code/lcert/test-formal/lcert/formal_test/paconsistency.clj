(ns lcert.formal-test.paconsistency
  (:require [lcert.formal.paconsistency]
            [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]))
(deftest consistency-relative-to-the-metatheory
  (is (b/has? 'pa_consistent_via_lcert))
  (is (not (b/rejects? '[]
    '(Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))
    '[(exact pa_consistent_via_lcert)])))
  (testing "the consistency theorem does not refute PA reflexivity"
    (is (b/rejects? '[] '(Not (PPrv (PF.peq PT.pz PT.pz)))
      '[(exact pa_consistent_via_lcert)]))))
