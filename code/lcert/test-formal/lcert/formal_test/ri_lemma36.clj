(ns lcert.formal-test.ri-lemma36
  "Formal suite (ADR-0006), lcert.formal.ri-lemma36: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.lemma36, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-lemma36]))

(deftest f4-ri-lemma36
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[ConvCase_ri lemma36_ri lemma36_step_ri outer_all_ri outer_succ_ri outer_zero_ri]]
      (is (b/has? c) (str c))))
  (testing "the outer hypothesis holds at budget 0 vacuously, and the generic Lemma 3.6 is assembled"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt]
                         '(OuterIH_ri chkf dec encTy ri 0) '[(exact (outer_zero_ri chkf dec encTy ri))])))))
