(ns lcert.formal-test.substitution
  "Formal suite (ADR-0006), lcert.formal.substitution: requiring the namespace
  kernel-checks its declarations; these tests check that the expected
  constants exist and that false variants are rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.substitution]))
(def ^:private den-params
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

(deftest f3h-weakening
  (testing "item 1: weakening (renaming by one insertion), with its lookup and skOf lemmas"
    (doseq [c '[insS insE lookup_ins_above lookup_ins_below nthS_ins_above nthS_ins_below
                skOf_lift skOf_typed denAt_weaken den_eq_denAt den_weaken]]
      (is (b/has? c) (str c))))
  (testing "the inserted value lands at the cut: variable 0 of the extended environment is the new value"
    (is (b/rejects? '[] '(= (lookup (insS 0 Sk.nat (List.cons Sk Sk.nat (List.nil Sk))) 0 Sk.nat
                                    (insE 0 Sk.nat (List.cons Sk Sk.nat (List.nil Sk)) (Prod.mk 5 Unit.unit) 7))
                            5)
                    '[(rfl)])))
  (testing "without lifting the term, inserting a variable changes the denotation"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0 (Exp.var 0) (insS 0 Sk.nat (List.cons Sk Sk.nat (List.nil Sk))) Sk.nat
                             (insE 0 Sk.nat (List.cons Sk Sk.nat (List.nil Sk)) (Prod.mk 5 Unit.unit) 7))
                        (den chkf dec encTy 0 (Exp.var 0) (List.cons Sk Sk.nat (List.nil Sk)) Sk.nat (Prod.mk 5 Unit.unit)))
                    '[(rfl)]))))

(deftest f3h-lemma-3-1-counterexample
  (testing "Lemma 3.1 over skeleton typing alone fails: a branch list substituted in argument position"
    (is (b/has? 'lemma31_skj_counterexample))
    (is (b/has? 'cex31_lhs))
    (is (b/has? 'cex31_rhs)))
  (testing "the two sides of the counterexample really differ"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0
                             (subst (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i))
                                    (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0)))
                             (List.nil Sk) (Sk.arr Sk.lbl Sk.nat) Unit.unit 0)
                        1)
                    '[(rfl)]))))

(deftest f3h-lemma-3-1
  (testing "item 2: Lemma 3.1 for substitutions satisfying SubOK, with skeleton-typing weakening"
    (doseq [c '[skj_weaken envAt envOf_envAt SubTy SubOK subOK_up envAt_lookup envAt_lift envAt_up
                skOf_subst subst_base denAt_subst den_fun_eq lemma31]]
      (is (b/has? c) (str c))))
  (testing "the source term is read in the substitution's environment, not an arbitrary one"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0 (subst (fn [i :- Nat] (inst1 Exp.zero i)) (Exp.var 0)) (List.nil Sk) Sk.nat Unit.unit)
                        (den chkf dec encTy 0 (Exp.var 0) (List.cons Sk Sk.nat (List.nil Sk)) Sk.nat (Prod.mk 1 Unit.unit)))
                    '[(rfl)])))
  (testing "item 3: Lemma 3.3's substitution clause for V"
    (doseq [c '[envOf_up vs_T vs_Pi vs_Sig V_subst_gen V_subst lemma33_subst]]
      (is (b/has? c) (str c))))
  (testing "V of T(b) depends on the environment: T(x) holds at x ↦ tt, not at x ↦ ff"
    (is (b/rejects? den-params
                    '(= (V chkf dec encTy 0 (Exp.tT (Exp.var 0)) (List.cons Sk Sk.bool (List.nil Sk)) (Prod.mk Bool.true Unit.unit) 0 Sk.unit Unit.unit)
                        (V chkf dec encTy 0 (Exp.tT (Exp.var 0)) (List.cons Sk Sk.bool (List.nil Sk)) (Prod.mk Bool.false Unit.unit) 0 Sk.unit Unit.unit))
                    '[(rfl)])))
  (testing "item 4: corollaries for subst1 and substL at their natural environments"
    (doseq [c '[consSub subOK_id subOK_cons envAt_var envOf_id subst1_consSub den_subst1 V_subst1
                instLS instL_instLS substL_instLS appS TyL envL subOK_list envOf_list den_substL V_substL den_substL2]]
      (is (b/has? c) (str c))))
  (testing "the natural environment of subst1 puts the argument's value first"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0 (subst1 (Exp.succ Exp.zero) (Exp.var 0)) (List.nil Sk) Sk.nat Unit.unit)
                        (den chkf dec encTy 0 (Exp.var 0) (List.cons Sk Sk.nat (List.nil Sk)) Sk.nat (Prod.mk 0 Unit.unit)))
                    '[(rfl)])))
  (testing "a branch list is not skOf-faithful, so SubOK excludes the counterexample's substitution"
    (is (b/rejects? '[] '(= (skOf (List.nil Sk) (Exp.bcons (Exp.succ Exp.zero) Exp.bnil)) (Option.some Sk (Sk.arr Sk.lbl Sk.nat)))
                    '[(rfl)]))))
