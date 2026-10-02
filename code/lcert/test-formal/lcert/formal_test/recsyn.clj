(ns lcert.formal-test.recsyn
  "Formal suite (ADR-0006), lcert.formal.recsyn: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.recsyn]))
(deftest f3j-recsyn
  (testing "Lemma 3.6, the RecSyn case: motive substitutions, the code induction, F_recS"
    (doseq [c '[sLeafI_consSub subOK_sLeafI den_sleaf_var0 envOf_sLeafI
                skel_var skOf_var nthS_past subOK_past var_shift_succ envOf_lift1 envDrop envOf_past
                sAt_consSub nthS_at1_y1 subOK_sAt13 den_var1_y1 envOf_sAt13
                nthS_at1_y2 subOK_sAt14 den_var1_y2 envOf_sAt14
                nthS_node_lbl nthS_node_c1 nthS_node_c2 nthS_node_tail
                sNodeI_ty sNodeI_sko subOK_sNodeI den_snode_vars envOf_sNodeI
                V_leafTy V_nodeTy V_y1Ty V_y2Ty carTo V_y1_val V_y2_val
                recrec_inv recS_leaf recS_node_sat node_v_raw recS_node F_recS]]
      (is (b/has? c) (str c))))
  (testing "sLeafI sends the label to the leaf code, not to the leaf of its successor"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                      n :- Nat, G :- (List Sk), l :- Nat, en :- (HEnv G)]
                    '(Eq (HEnv (List.cons Sk Sk.syn G))
                         (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sLeafI j))
                                (List.cons Sk Sk.lbl G) (Prod.mk l en))
                         (Prod.mk (Code.sl (Nat.succ l)) en))
                    '[(exact (envOf_sLeafI chkf dec encTy n G l en))])))
  (testing "a leaf labelled NL is not a code in V(Syn)"
    (is (b/rejects? '[] '(Eq Bool (lblOk (Code.sl 100)) Bool.true) '[(rfl)]))))
