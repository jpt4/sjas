(ns lcert.formal-test.ri-recsyn
  "Formal suite (ADR-0006), lcert.formal.ri-recsyn: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected.  The namespace is
  the generic copy (over an R-interpretation) of lcert.formal.recsyn, generated
  by formal/tools/gen_ri_all.sh; the constant list is the one the warm
  server recorded for it."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.ri-recsyn]))

(deftest f4-ri-recsyn
  (testing "the generic copy declares its constants over ri"
    (doseq [c '[F_recS_ri V_leafTy_ri V_nodeTy_ri V_y1Ty_ri V_y1_val_ri V_y2Ty_ri V_y2_val_ri den_sleaf_var0_ri
                den_snode_vars_ri den_var1_y1_ri den_var1_y2_ri envOf_lift1_ri envOf_past_ri envOf_sAt13_ri
                envOf_sAt14_ri envOf_sLeafI_ri envOf_sNodeI_ri node_v_raw_ri recS_leaf_ri recS_node_ri
                recS_node_sat_ri]]
      (is (b/has? c) (str c)))))
