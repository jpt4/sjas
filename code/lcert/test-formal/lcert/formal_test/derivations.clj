(ns lcert.formal-test.derivations
  "Formal suite (ADR-0006), lcert.formal.derivations: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.derivations]))
(deftest f2f-runtime-weakening
  (testing "Lemma 2.1: every runtime rule permits insertion at usage zero"
    (doseq [c '[closed_lift closedTy_lift lift_lift_comm lift_comp_ge lift_subst
                lift_subst1 lift_substL hd_lift step_lift cv_lift
                insD insU vadd_insU vscale_insU vadd_ins0 vscale_ins0
                tl_weaken rt_cast rt_cast_t rt_ctx1 rt_ctx2 rt_var rt_weaken]]
      (is (b/has? c) (str c))))
  ;; This instantiation exercises both the index shift and the usage lookup:
  ;; an old token at index 0 moves to index 1 behind a new, unused Bool.
  (let [ps '[chkf :- (=> Code Code Bool)]
        proof '[(exact (rt_weaken chkf (thetaD 1) (thetaU 1) (Exp.var 0) Exp.tDia
                         (Rt.rVar chkf (thetaD 1) (thetaU 1) 0 Exp.tDia U.u1
                           rfl rfl rfl rfl) 0 Exp.tBool))]
        context '(List.cons Exp Exp.tBool (thetaD 1))
        usages '(List.cons U U.u0 (thetaU 1))]
    (testing "the shifted variable retains its original type and usage"
      (is (not (b/rejects? ps (list 'Rt 'chkf context usages '(Exp.var 1) 'Exp.tDia) proof))))
    (testing "omitting the index shift does not prove the original judgment"
      (is (b/rejects? ps (list 'Rt 'chkf context usages '(Exp.var 0) 'Exp.tDia) proof))))
  (testing "closed types may contain bound variables: these stay fixed under lift"
    (is (not (b/rejects? '[]
                         '(= (lift 3 0 (Exp.tPi U.u1 Exp.tBool (Exp.tT (Exp.var 0))))
                             (Exp.tPi U.u1 Exp.tBool (Exp.tT (Exp.var 0))))
                         '[(exact (closedTy_lift (Exp.tPi U.u1 Exp.tBool (Exp.tT (Exp.var 0))) rfl 3 0))]))))
  (testing "closedness matters: lifting an open type changes its free variable"
    (is (b/rejects? '[] '(= (lift 1 0 (Exp.tT (Exp.var 0))) (Exp.tT (Exp.var 0))) '[(rfl)]))))

(deftest f2f-token-blocks
  (testing "Lemma 2.4 helpers: zero padding, closed formation, and repeated weakening"
    (doseq [c '[prefixU prefixU_one insU_prefix insD_theta vscale_one vadd_theta_zero
                vadd_theta_blocks rt_reindex tl_theta_closed rt_theta_prepend rt_theta_append]]
      (is (b/has? c) (str c))))
  (testing "complementary blocks preserve usage 1 for all five tokens"
    (is (not (b/rejects? '[]
                         '(= (vadd (prefixU 2 U.u0 (thetaU 3))
                                   (vscale U.u1 (prefixU 2 U.u1 (vzero 3)))) (thetaU 5))
                         '[(exact (vadd_theta_blocks 2 3))]))))
  (testing "overlapping token blocks cannot be treated as disjoint: 1 + 1 is omega"
    (is (b/rejects? '[] '(= (vadd (thetaU 2) (thetaU 2)) (thetaU 2)) '[(rfl)])))
  (let [ps '[chkf :- (=> Code Code Bool)]
        source '(Rt.rVar chkf (thetaD 2) (thetaU 2) 1 Exp.tDia U.u1 rfl rfl rfl rfl)]
    (testing "prepending unused tokens shifts the old token into the second block"
      (is (not (b/rejects? ps
                           '(Rt chkf (thetaD 5) (prefixU 3 U.u0 (thetaU 2)) (Exp.var 4) Exp.tDia)
                           [(list 'exact (list 'rt_theta_prepend 'chkf 2 '(Exp.var 1) 'Exp.tDia 'rfl source 3))]))))
    (testing "appending unused tokens keeps the old token in the first block"
      (is (not (b/rejects? ps
                           '(Rt chkf (thetaD 5) (prefixU 2 U.u1 (vzero 3)) (Exp.var 1) Exp.tDia)
                           [(list 'exact (list 'rt_theta_append 'chkf 2 '(Exp.var 1) 'Exp.tDia 'rfl source 3))]))))))

(deftest f2f-composition
  (testing "Lemma 2.4 composes the two derivations at the sum of their token budgets"
    (is (b/has? 'lemma24)))
  ;; closedTy checks variable scope on every Exp, including term constructors.
  ;; It cannot replace the Tl formation premise required by rApp.
  (testing "closedness by itself is not type formation"
    (is (not (b/rejects? '[] '(= (closedTy Exp.star) true) '[(rfl)])))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Tl chkf Bool.true (List.nil Exp) Exp.star Exp.tUnit)
                    '[(exact (Tl.fBase chkf (List.nil Exp) Exp.star rfl))]))))
