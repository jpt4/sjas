(ns lcert.formal-test
  "The formal suite (ADR-0006).  Requiring a formal namespace kernel-checks
  every declaration in it; these tests then check that the expected constants
  exist, and that false variants are rejected, so the statements are not
  vacuous."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.usage]))

(deftest f1-usages
  (testing "the laws are kernel-checked constants"
    (doseq [c '[uadd_comm uadd_assoc umul_comm umul_assoc umul_distrib uadd_eq_zero umul_eq_zero
                umul_omega_one]]
      (is (b/has? c) (str c))))
  (testing "false variants are rejected"
    (is (b/rejects? '[] '(= (uadd U.u1 U.u1) U.u1) '[(rfl)]))
    (is (b/rejects? '[x :- U, y :- U] '(= (uadd x y) x) '[(cases x) (all_goals (cases y)) (all_goals (rfl))]))))

(require 'lcert.formal.syntax)
(deftest f1-syntax
  (is (b/has? 'lift_var_below)))

(deftest f1-substitution
  (is (b/has? 'subst1_under_binder))
  (testing "substitution does not reach a bound variable"
    (is (b/rejects? '[u :- Exp, A :- Exp] '(= (subst1 u (Exp.lam U.u1 A (Exp.var 0))) (Exp.lam U.u1 (subst1 u A) u)) '[(rfl)]))))

(require 'lcert.formal.skel)
(deftest f2a-skeletons
  (is (b/has? 'skel_pi))
  (testing "codeOf reads closed canonical codes only"
    (is (b/has? 'codeOf))
    (is (b/rejects? '[] '(= (skel Exp.tNat) Sk.unit) '[(rfl)]))))

(deftest f2a-usage-vectors-compute
  (testing "vadd reduces definitionally, so proofs can compute with usage vectors"
    (is (not (b/rejects? '[y :- (List U)] '(= (vadd (List.nil U) y) (List.nil U)) '[(rfl)])))
    (is (not (b/rejects? '[a :- U, b :- U, x :- (List U), y :- (List U)]
                         '(= (vadd (List.cons U a x) (List.cons U b y)) (List.cons U (uadd a b) (vadd x y))) '[(rfl)]))))
  (testing "and not to something else"
    (is (b/rejects? '[a :- U, x :- (List U)] '(= (vadd (List.cons U a x) (List.nil U)) (List.cons U a x)) '[(rfl)]))))

(require 'lcert.formal.conv)
(deftest f2b-steps
  (is (b/has? 'Hd)))

(deftest f2b-positions
  (is (b/has? 'Step))
  (is (b/has? 'setP)))

(deftest f2b-skeleton-typing-and-conversion
  (is (b/has? 'SkJ))
  (is (b/has? 'Cv)))

(require 'lcert.formal.judgment 'lcert.formal.examples)
(deftest f2c-rule-table
  (testing "the draft's derivation of not is a derivation"
    (is (b/has? 'not_typed)))
  (testing "clean: no branch-list pseudo-type anywhere (App's argument type must be clean)"
    (is (not (b/rejects? '[] '(Eq Bool (clean (Exp.tPi U.u1 Exp.tNat (Exp.tT (Exp.ite Exp.tt Exp.tt Exp.ff)))) Bool.true) '[(rfl)])))
    (is (not (b/rejects? '[P :- Exp, k :- Nat] '(Eq Bool (clean (Exp.tPi U.u1 (Exp.tBrs P k) Exp.tNat)) Bool.false) '[(rfl)])))
    (is (b/rejects? '[P :- Exp, k :- Nat] '(Eq Bool (clean (Exp.tBrs P k)) Bool.true) '[(rfl)])))
  (testing "a usage-0 variable cannot be used at runtime"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.u0 (List.nil U)) (Exp.var 0) Exp.tBool)
                    '[(exact (Rt.rVar chkf (List.cons Exp Exp.tBool (List.nil Exp)) (List.cons U U.u0 (List.nil U)) 0 Exp.tBool U.u0 rfl rfl rfl rfl))]))))

(require 'lcert.formal.carrier)
(deftest f3a-carriers
  (is (b/has? 'car_arr)) (is (b/has? 'henv_cons)) (is (b/has? 'dflt_arr))
  (is (b/rejects? '[] '(= (Car Sk.nat) Bool) '[(rfl)])))

(require 'lcert.formal.den)
(deftest f3c-denotation
  (is (b/has? 'denAt)))

(deftest f3c-denotation-computes
  (is (b/has? 'den_not_tt))
  (is (b/has? 'den_recN_double))
  (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                  '(= (den chkf dec encTy 0 (Exp.app (Exp.lam U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt)) Exp.tt)
                          (List.nil Sk) Sk.bool Unit.unit) Bool.true)
                  '[(rfl)])))

(require 'lcert.formal.sem)
(deftest f3d-semantic-types
  (is (b/has? 'V)))

(require 'lcert.formal.model)
(deftest f3e-statements-and-t2
  (doseq [c '[CheckSpec EnvSat Lemma_3_6 Theorem_1 Corollary_3_7 Theorem_2_H]]
    (is (b/has? c) (str c)))
  (testing "H° cannot be inhabited using r twice"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U))
                         (Exp.node (Exp.var 0) (Exp.lbl 0) (Exp.var 0) (Exp.leaf (Exp.lbl 0))) Exp.tR)
                    '[(exact (Rt.rNode chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) (consU U.u0 (List.nil U))
                               (consU U.u1 (List.nil U)) (consU U.u0 (List.nil U)) (Exp.var 0) (Exp.lbl 0) (Exp.var 0) (Exp.leaf (Exp.lbl 0))
                               (Rt.rVar chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) 0 Exp.tR U.u1 rfl rfl rfl rfl)
                               (Rt.rConst chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0) Exp.tLbl rfl rfl)
                               (Rt.rVar chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) 0 Exp.tR U.u1 rfl rfl rfl rfl)
                               (Rt.rLeaf chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0)
                                  (Rt.rConst chkf (consE Exp.tR (List.nil Exp)) (consU U.u0 (List.nil U)) (Exp.lbl 0) Exp.tLbl rfl rfl))))]))))

(require 'lcert.formal.syntactic)
(deftest f2d-lift-algebra
  (testing "structural lift laws needed by Lemmas 2.1 and 2.5 (by Codex)"
    (doseq [c '[lift_zero lift_comp skel_lift]]
      (is (b/has? c) (str c))))
  (testing "lifting is not the identity at a free variable"
    (is (b/rejects? '[] '(= (lift 1 0 (Exp.var 0)) (Exp.var 0)) '[(rfl)])))
  (testing "composition adds both displacements"
    (is (b/rejects? '[] '(= (lift 2 0 (lift 1 0 (Exp.var 0))) (lift 2 0 (Exp.var 0))) '[(rfl)]))))

(require 'lcert.formal.subst)
(deftest f3f-coercion-and-environment
  (doseq [c '[coe_both coe_self coe_rev envOf envOf_nil]]
    (is (b/has? c) (str c)))
  (is (b/rejects? '[v :- Bool] '(= (coe Sk.bool Sk.bool v) Bool.false) '[(rfl)]))
  (testing "substitution without a typing hypothesis is not an identity"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                    '(= (den chkf dec encTy 0 (subst1 Exp.tt (Exp.var 0)) (List.nil Sk) Sk.bool Unit.unit)
                        (den chkf dec encTy 0 (Exp.var 0) (List.nil Sk) Sk.bool Unit.unit))
                    '[(rfl)]))))

(require 'lcert.formal.mono)
(deftest f3g-monotonicity
  (is (b/has? 'V_mono))
  (testing "footprints cannot be lowered: Vₖ(R) is not contained in V₀(R)"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                    '(V chkf dec encTy 1 Exp.tR (List.nil Sk) Unit.unit 0 Sk.cert (Code.sn 0 (Code.sl 0) (Code.sl 0)))
                    '[(decide)])))
  (testing "Lemma 3.4: base data types across budgets"
    (is (b/has? 'Lemma_3_4)))
  (testing "Lemma 3.4 needs m ≤ k: a one-node certificate is in V₁(R) and provably not in V₀(R)"
    (let [ps '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
          vR (fn [k] (list 'V 'chkf 'dec 'encTy 5 'Exp.tR '(List.nil Sk) 'Unit.unit k 'Sk.cert '(Code.sn 0 (Code.sl 0) (Code.sl 0))))]
      (is (not (b/rejects? ps (vR 1) '[(exact (Nat.le_refl 1))])))
      (is (not (b/rejects? ps (list 'Not (vR 0)) '[(intro h) (have h2 (LE.le 1 0) h) (omega)])))))
  (testing "and T(b) is not a base type: its set depends on the cap"
    (is (b/rejects? '[] '(Eq Bool (isBaseTy (Exp.tT Exp.tt)) Bool.true) '[(rfl)]))))

(require 'lcert.formal.skeletons)
(deftest f2e-lemma-2-5
  (testing "Lemma 2.5: conversion preserves skeletons; derivations are skeleton-typed"
    (doseq [c '[skel_subst step_skel cv_skel lemma25_tl lemma25_tl_term lemma25_tl_type lemma25_rt]]
      (is (b/has? c) (str c))))
  (testing "the hypotheses are necessary (kernel-checked counterexamples)"
    (is (b/has? 'skel_subst_needs_unit))
    (is (b/has? 'step_skel_needs_wf)))
  (testing "a derivation's skeleton is not arbitrary"
    (is (b/rejects? '[] '(SkJ Bool.false (List.nil Sk) Exp.tt Sk.nat) '[(constructor)]))))

(require 'lcert.formal.splitting)
(deftest f3h-lemma-3-5
  (testing "Lemma 3.5: splitting, ω-contexts, 1-contexts, raising the bound"
    (doseq [c '[entry_split EnvSat_split entry_omega EnvSat_omega EnvSat_one EnvSat_mono]]
      (is (b/has? c) (str c))))
  (testing "the 0 summand of a usage-1 entry cannot take its footprint"
    (is (b/rejects? '[j :- Nat, P :- (=> Nat Prop)] '(=> (EntryOK (uadd U.u1 U.u0) j P) (EntryOK U.u0 j P))
                    '[(intro h) (exact rfl)])))
  (testing "a usage-1 entry is not usable at footprint 0: only ω entries are"
    (is (b/rejects? '[j :- Nat, P :- (=> Nat Prop)] '(=> (EntryOK U.u1 j P) (EntryOK U.u1 0 P))
                    '[(intro h) (exact h)]))))

(require 'lcert.formal.unfold)
(deftest f3i-unfolding
  (testing "one step of ⟦·⟧ⁿ at symbolic n, for every constructor"
    (doseq [c '[den_eq_denAt den_succ_eq den_lam_at den_h1_at den_tBrs_at den_refl_at]]
      (is (b/has? c) (str c))))
  (testing "den at a symbolic cap does not compute by itself"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                    '(= (den chkf dec encTy n Exp.tt (List.nil Sk) Sk.bool Unit.unit) Bool.true) '[(rfl)])))
  (testing "but it does after unfolding"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(= (den chkf dec encTy n Exp.tt (List.nil Sk) Sk.bool Unit.unit) Bool.true)
                         '[(rw [(den_tt_at chkf dec encTy n (List.nil Sk) Sk.bool Unit.unit)])])))))

(require 'lcert.formal.fundamental)
(deftest f3j-fundamental-lemma-cases
  (testing "Lemma 3.6, the cases proved so far"
    (doseq [c '[Sound F_const F_succ F_sleaf F_snode F_prn F_chk F_lam0 F_lam1 F_lamw F_lam F_abort F_ite F_leaf F_node F_bnil
                den_app_some F_app1 F_appw F_app sk_transport bool_rec_dep F_elimB]]
      (is (b/has? c) (str c))))
  (testing "a label constant must be below NL: lbl 100 is not in V(Lbl)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tLbl (List.nil Sk) Unit.unit 0 Sk.lbl 100))
                         '[(intro h) (have h2 (LT.lt 100 100) h) (omega)]))))
  (testing "a node costs a token: node ⋆ … at footprint 0 is not in V₀(R)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tR (List.nil Sk) Unit.unit 0 Sk.cert (Code.sn 0 (Code.sl 0) (Code.sl 0))))
                         '[(intro h) (have h2 (LE.le 1 0) h) (omega)])))))

(require 'lcert.formal.substitution)
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

(require 'lcert.formal.skof)
(deftest f3k-skof-completeness
  (testing "skOf is complete on runtime terms at clean types; skeleton-typed expressions are clean"
    (doseq [c '[clean_tPi clean_app skj_clean cv_wf_left skOf_const skOf_rt]]
      (is (b/has? c) (str c))))
  (testing "a branch list has no inferred skeleton, which is why the clean premise is needed"
    (is (not (b/rejects? '[G :- (List Sk)] '(Eq (Option Sk) (skOf G Exp.bnil) (Option.none Sk)) '[(rfl)])))))

(require 'lcert.formal.vweaken)
(deftest f3l-v-weakening
  (testing "weakening for V: V(lift 1 c A) at an inserted environment is V(A)"
    (doseq [c '[vw_T vw_Pi vw_Sig V_lift_gen V_lift]]
      (is (b/has? c) (str c))))
  (testing "the lift matters: T(x) is not T(x) one level up, at an environment where they differ"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                         '(Not (V chkf dec encTy 0 (Exp.tT (Exp.var 0)) (List.cons Sk Sk.bool (List.cons Sk Sk.bool (List.nil Sk)))
                                  (Prod.mk Bool.false (Prod.mk Bool.true Unit.unit)) 0 Sk.unit Unit.unit))
                         '[(intro h) (exact (Bool.noConfusion h))])))))
