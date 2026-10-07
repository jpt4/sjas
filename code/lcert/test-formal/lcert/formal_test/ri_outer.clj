(ns lcert.formal-test.ri-outer
  "Formal suite (ADR-0006), lcert.formal.ri-outer: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.outer, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-outer]))

(deftest f4-ri-outer
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[F_h1_ri F_insp_ri F_refl_ri OuterIH_ri V_dflt_ri band_false_or base_den_ri bool_and3_true
                chk_true_ri denPrev_stable_ri den_chk_val_ri den_cleaf_ri den_insp_val_ri den_negT_ri
                den_prn_val_ri den_refl_none_ri den_refl_some_ri h1_core_ri h1_lt_arith h1_split_arith
                insp_chk_ri insp_not_ri lt_of_lt_eq refl_pf_ri refl_ph_ri tok_sat_ri]]
      (is (b/has? c) (str c))))
  (testing "reflect's default lies in V(D) for a base type D ≠ 0, never in V(0)"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, n :- Nat, G :- (List Sk), en :- (HEnv G), k :- Nat]
                    '(V_ri chkf dec encTy ri n Exp.tEmpty G en k Sk.unit Unit.unit)
                    '[(exact True.intro)]))))
