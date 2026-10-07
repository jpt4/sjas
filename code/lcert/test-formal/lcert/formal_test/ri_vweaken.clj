(ns lcert.formal-test.ri-vweaken
  "Formal suite (ADR-0006), lcert.formal.ri-vweaken: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.vweaken, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-vweaken]))

(deftest f4-ri-vweaken
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[V_lift2_fam_ri V_lift_fam_ri V_lift_gen_ri V_lift_ri den_succ_var0_ri envOf_sSucc_ri
                envOf_shift_ri vw_Pi_ri vw_Sig_ri vw_T_ri]]
      (is (b/has? c) (str c)))))
