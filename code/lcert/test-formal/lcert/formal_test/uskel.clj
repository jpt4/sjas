(ns lcert.formal-test.uskel
  "Formal suite (ADR-0006), lcert.formal.uskel: requiring the namespace kernel-checks its
  declarations; these tests check that the expected constants exist and
  that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.uskel]))

(deftest f5-uskel
  (testing "substitution and path congruence of the usage skeleton, and transport of E"
    (doseq [c '[usk_lift_var usk_lift up_usk upn_usk usk_of_unit usk_subst usk_subst1 usk_substL
                usk_subst_needs_unit uskCtx uskCtx_nil uskCtx_cons usk_ctx henv_of_usk
                usk_pi_dom usk_pi_cod usk_sig_dom usk_sig_cod
                uchild_tPi0 uchild_tPi1 uchild_tSig0 uchild_tSig1 uchild_tPi uchild_tSig uchild
                usk_step_path usk_step usk_cv_wf usk_cv usk_step_needs_wf
                erel_at erel_subst1 erel_subst_unit erel_cv]]
      (is (b/has? c) (str c))))
  (testing "substituting a type for a variable changes the usage skeleton"
    (is (b/rejects? '[]
                    '(Eq USk (usk (subst1 Exp.tNat (Exp.var 0))) (USk.base Sk.unit))
                    '[(rfl)]))))
