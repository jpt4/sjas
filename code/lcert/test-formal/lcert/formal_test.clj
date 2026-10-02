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
      (is (not (b/rejects? ps (vR 1) '[(exact (And.intro (Nat.le_refl 1) rfl))])))
      (is (not (b/rejects? ps (list 'Not (vR 0)) '[(intro h) (have h2 (LE.le 1 0) (And.left h)) (omega)])))))
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
                den_app_some F_app1 F_appw F_app sk_transport bool_rec_dep F_elimB
                WFCtx var_wf entry_nz var_succ var_zero var_sem F_var le_sub_add F_pair1 F_pairw F_pair
                den_letp_some EnvSat_cons le_let le_let0 F_let1 F_letw F_let0 F_let F_app0 F_pair0
                entry_omega_back EnvSat_omega_back V_stepTy den_zero_nat natrec_inv F_recN
                F_caseL bcons_zero bcons_succ sub_eq_zero sub_succ den_lbl_lbl F_bcons
                lblOk_sn cnodes_sn lbl_lt coderec_inv V_l1s skj_lsuc V_lsuc F_itR]]
      (is (b/has? c) (str c))))
  (testing "a label constant must be below NL: lbl 100 is not in V(Lbl)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tLbl (List.nil Sk) Unit.unit 0 Sk.lbl 100))
                         '[(intro h) (have h2 (LT.lt 100 100) h) (omega)]))))
  (testing "certificate labels are below NL: a leaf labelled NL is not in V(R)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tR (List.nil Sk) Unit.unit 5 Sk.cert (Code.sl 100)))
                         '[(intro h) (exact (Bool.noConfusion (And.right h)))]))))
  (testing "a node costs a token: node ⋆ … at footprint 0 is not in V₀(R)"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
                         '(Not (V chkf dec encTy n Exp.tR (List.nil Sk) Unit.unit 0 Sk.cert (Code.sn 0 (Code.sl 0) (Code.sl 0))))
                         '[(intro h) (have h2 (LE.le 1 0) (And.left h)) (omega)])))))

(require 'lcert.formal.recsyn)
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
    (doseq [c '[clean_tPi clean_app skj_clean cv_wf_left skOf_const skOf_rt skOf_tl]]
      (is (b/has? c) (str c))))
  (testing "a branch list has no inferred skeleton, which is why the clean premise is needed"
    (is (not (b/rejects? '[G :- (List Sk)] '(Eq (Option Sk) (skOf G Exp.bnil) (Option.none Sk)) '[(rfl)])))))

(require 'lcert.formal.conversion)
(deftest f3m-conversion
  (testing "conversion records nbr at both endpoints, and lifting preserves it"
    (doseq [c '[cv_nbr_left cv_nbr_right nbrF_lift_var nbrF_lift nbr_lift]]
      (is (b/has? c) (str c))))
  (testing "branch lists are admitted in caseL, but never as application arguments"
    (is (not (b/rejects? '[]
               '(Eq Bool (nbr (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil))) Bool.true)
               '[(rfl)])))
    (is (b/rejects? '[]
           '(Eq Bool (nbr (Exp.app (Exp.var 0) (Exp.bcons Exp.tt Exp.bnil))) Bool.true)
           '[(rfl)])))
  (testing "Lemma 3.2 for the head steps proved so far, and the SkJ-only counterexample"
    (doseq [c '[nbr nbrF skOf_complete skOf_ok skj_inv EquivAt mk_eqv_t mk_eqv_ty
                eqv_den eqv_sko eqv_skel eqv_V
                hd_iteT skof_ite den_iteT den_iteF den_elimT den_elimF den_recNZ
                den_prnL den_prnN den_boolExp den_boolExp_ff den_boolExp_tt
                code_none_ne_some den_sleaf_code codeOf_sleaf_inv
                arr_inj exSk exU exExpC den_lam_arr den_beta_core den_beta
                prod_inj skof_pair den_pair_prod den_betaLet_at den_betaLet_core den_betaLet
                den_itRL den_itRL_nbr V_tTT V_tTF V_tTT_at V_tTF_at hd_tTT hd_tTF
                cex_red_eq cex_den_redex cex_den_contr cex_nbr_ff cex_redex_typed
                cex_den_ne conv_skj_counterexample
                nbr_recN_n nbr_recS_c nbr_snode_a nbr_snode_c1 nbr_snode_c2
                nbr_itR_r nbr_node_d nbr_node_a nbr_caseL_bs nbr_bcons_t nbr_bcons_h
                den_recNS_eval den_recNS den_nthB den_caseLb
                den_recSL_eval den_recSL den_recSN_eval den_recSN
                canonicalNode canonicalNode_some canonicalNode_den
                den_code_sleaf den_snode_code den_code_snode den_codeOf den_delta
                den_dia_unique den_itRN_eval den_itRN cex_no_cv
                skj_not_tT skj_not_tUnit hd_den setP_nil getP_nil den_step_nil
                setKid_ite_0 setKid_ite_1 setKid_ite_2 den_abort_ty den_ite_b]]
      (is (b/has? c) (str c))))
  (testing "the β redex and its contractum are not denoted equally: the application defaults"
    (is (b/rejects? den-params
                    '(= (den chkf dec encTy 0
                             (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool)
                                               (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                                      (Exp.bcons Exp.tt Exp.bnil))
                             (List.nil Sk) Sk.bool Unit.unit)
                        (den chkf dec encTy 0
                             (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil))
                             (List.nil Sk) Sk.bool Unit.unit))
                    '[(rfl)]))))

(require 'lcert.formal.vweaken)
(deftest f3l-v-weakening
  (testing "weakening for V: V(lift 1 c A) at an inserted environment is V(A)"
    (doseq [c '[vw_T vw_Pi vw_Sig V_lift_gen V_lift V_lift_fam V_lift2_fam skj_subst subOK_shift subOK_sSucc envOf_shift envOf_sSucc]]
      (is (b/has? c) (str c))))
  (testing "the lift matters: T(x) is not T(x) one level up, at an environment where they differ"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
                         '(Not (V chkf dec encTy 0 (Exp.tT (Exp.var 0)) (List.cons Sk Sk.bool (List.cons Sk Sk.bool (List.nil Sk)))
                                  (Prod.mk Bool.false (Prod.mk Bool.true Unit.unit)) 0 Sk.unit Unit.unit))
                         '[(intro h) (exact (Bool.noConfusion h))])))))

(require 'lcert.formal.derivations)
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

(require 'lcert.formal.outer)
(deftest f3m-reflect-and-h1
  (testing "the cases of Lemma 3.6 that use the outer induction on the budget"
    (doseq [c '[tokEnvD tok_pair tok_transfer tok_sat wf_theta base_is_base base_leaf base_den chk_true
                den_refl_some denPrev_stable OuterIH F_refl den_negT F_h1 bool_case insp_chk insp_not den_insp_val F_insp]]
      (is (b/has? c) (str c))))
  (testing "the decoded program runs one level down: at cap 0 no budget m < 0 exists"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                      m :- Nat, t :- Exp]
                    '(Eq DenBody (denPrev chkf dec encTy 0 m t) (den chkf dec encTy m t))
                    '[(rfl)]))))

(require 'lcert.formal.lemma36)
(deftest f3n-lemma36-and-main-results
  (testing "Lemma 3.6 assembled, Theorem 1, Corollary 3.7 and Theorem 3 (Conv as a hypothesis)"
    (doseq [c '[ConvCase lemma36_step outer_zero outer_succ outer_all lemma36
                theorem1 bool_false_of cor37_refutation cor37_contradiction theorem3
                ConvAll paper_lemma36 paper_theorem1 paper_cor37 Theorem_2_H1]]
      (is (b/has? c) (str c))))
  (testing "Theorem 1 is not vacuous about derivability: Θ₀ ⊢ ⋆ : 1 is derivable"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                         '(Rt chkf (thetaD 0) (thetaU 0) Exp.star Exp.tUnit)
                         '[(exact (Rt.rConst chkf (List.nil Exp) (List.nil U) Exp.star Exp.tUnit rfl rfl))]))))
  (testing "Bcons now requires its motive's formation: without hP the rule does not apply"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), h :- Exp, t :- Exp,
                      hh :- (Rt chkf (List.nil Exp) (List.nil U) h (subst1 (Exp.lbl 0) Exp.tBool)),
                      ht :- (Rt chkf (List.nil Exp) (List.nil U) t (Exp.tBrs Exp.tBool 1))]
                    '(Rt chkf (List.nil Exp) (List.nil U) (Exp.bcons h t) (Exp.tBrs Exp.tBool 0))
                    '[(exact (Rt.rBcons chkf (List.nil Exp) (List.nil U) Exp.tBool 0 h t hh ht))]))))

(require 'lcert.formal.section4)
(deftest f4-section4
  (testing "Propositions 4.1 and 4.7"
    (doseq [c '[sn_not_le0 leaf_of_cnodes prop41_closed prop41_fun ConP p47term prop47]]
      (is (b/has? c) (str c))))
  (testing "a node is not a leaf: leaf_of_cnodes has content"
    (is (b/rejects? '[] '(Exists (fn [l :- Nat] (Eq Code (Code.sn 0 (Code.sl 0) (Code.sl 0)) (Code.sl l))))
                    '[(exact (leaf_of_cnodes (Code.sn 0 (Code.sl 0) (Code.sl 0)) (Nat.le_refl 0)))])))
  (testing "Con′ ⊸ H° holds but the converse direction's term does not have the swapped type"
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (p47term) (Exp.tPi U.u1 (Hcirc) (ConP)))
                    '[(exact (prop47 chkf))]))))

(require 'lcert.formal.section4b)
(deftest f4-section4b
  (testing "Propositions 4.2, 4.10 and 4.5"
    (doseq [c '[codeTerm lit0 cv_lit prop42 prop410 cert_front prop45_d3 tensor_body prop45_contraction]]
      (is (b/has? c) (str c))))
  (testing "a leaf certificate inhabits □A: labels below L compute, and the checker hypothesis is used"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sl 0) (encTy A)) Bool.true)]
                         '(Rt chkf (thetaD (cnodes (Code.sl 0))) (thetaU (cnodes (Code.sl 0)))
                              (certTerm (Code.sl 0) (codeTerm (encTy A)))
                              (boxTy (codeTerm (encTy A))))
                         '[(exact (prop42 chkf dec encTy A (Code.sl 0) rfl hA hck))]))))
  (testing "a one-node certificate inhabits □A"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sn 1 (Code.sl 2) (Code.sl 3)) (encTy A)) Bool.true)]
                         '(Rt chkf
                              (thetaD (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))))
                              (thetaU (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))))
                              (certTerm (Code.sn 1 (Code.sl 2) (Code.sl 3)) (codeTerm (encTy A)))
                              (boxTy (codeTerm (encTy A))))
                         '[(exact (prop42 chkf dec encTy A (Code.sn 1 (Code.sl 2) (Code.sl 3)) rfl hA hck))]))))
  (testing "a label outside L is not a certificate: lblOk (leaf 100) is not true by computation"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      A :- Exp,
                      hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                      hck :- (Eq Bool (chkf (Code.sl 100) (encTy A)) Bool.true)]
                    '(Rt chkf (thetaD (cnodes (Code.sl 100))) (thetaU (cnodes (Code.sl 100)))
                         (certTerm (Code.sl 100) (codeTerm (encTy A)))
                         (boxTy (codeTerm (encTy A))))
                    '[(exact (prop42 chkf dec encTy A (Code.sl 100) rfl hA hck))])))
  (testing "boxed contraction on a one-node certificate uses both token blocks"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code),
                           A :- Exp,
                           hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                           hck :- (Eq Bool (chkf (Code.sn 1 (Code.sl 2) (Code.sl 3)) (encTy A)) Bool.true)]
                         '(Rt chkf
                              (thetaD (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                         (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))))
                              (thetaU (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                         (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3)))))
                              (Exp.lam U.u1 (boxTy (codeTerm (encTy A)))
                                (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))
                                  (Exp.pair (boxTy (codeTerm (encTy A)))
                                    ((litAt (Code.sn 1 (Code.sl 2) (Code.sl 3)))
                                     (+ (cnodes (Code.sn 1 (Code.sl 2) (Code.sl 3))) 1))
                                    Exp.star)
                                  (Exp.pair (boxTy (codeTerm (encTy A)))
                                    ((litAt (Code.sn 1 (Code.sl 2) (Code.sl 3))) 1) Exp.star)))
                              (Exp.tPi U.u1 (boxTy (codeTerm (encTy A)))
                                (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))))
                         '[(exact (prop45_contraction chkf dec encTy A (Code.sn 1 (Code.sl 2) (Code.sl 3)) rfl hA hck))]))))
  (testing "the same contraction is not derivable by that theorem at budget ‖v‖"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      A :- Exp, v :- Code,
                      hok :- (Eq Bool (lblOk v) Bool.true),
                      hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                      hck :- (Eq Bool (chkf v (encTy A)) Bool.true)]
                    '(Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v))
                         (Exp.lam U.u1 (boxTy (codeTerm (encTy A)))
                           (Exp.pair (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))
                             (Exp.pair (boxTy (codeTerm (encTy A))) ((litAt v) (+ (cnodes v) 1)) Exp.star)
                             (Exp.pair (boxTy (codeTerm (encTy A))) ((litAt v) 1) Exp.star)))
                         (Exp.tPi U.u1 (boxTy (codeTerm (encTy A)))
                           (Exp.tSig U.u1 (boxTy (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))))
                    '[(exact (prop45_contraction chkf dec encTy A v hok hA hck))]))))

(require 'lcert.formal.strengthen)
(deftest f2-strengthening
  (testing "Lemma 2.3 and its infrastructure"
    (doseq [c '[freshF fresh_app fresh_lam fresh_insp zeroUF zero_vadd zero_vscale zero_len zero_nth rt_ucast rt_strengthen]]
      (is (b/has? c) (str c))))
  (let [ps '[chkf :- (=> Code Code Bool)]
        D '(List.cons Exp Exp.tDia (List.cons Exp Exp.tDia (List.nil Exp)))
        us '(List.cons U U.u1 (List.cons U U.u1 (List.nil U)))
        der (list 'Rt.rVar 'chkf D us 0 'Exp.tDia 'U.u1 'rfl 'rfl 'rfl 'rfl)]
    (testing "a token not free in the term is lowered to usage 0"
      (is (not (b/rejects? ps (list 'Rt 'chkf D '(List.cons U U.u1 (List.cons U U.u0 (List.nil U))) '(Exp.var 0) '(lift 1 0 Exp.tDia))
                           [(list 'exact (list 'rt_strengthen 'chkf D us '(Exp.var 0) '(lift 1 0 Exp.tDia) der 1 'rfl))]))))
    (testing "the variable that occurs cannot be lowered: its freshness is false"
      (is (b/rejects? ps (list 'Rt 'chkf D '(List.cons U U.u0 (List.cons U U.u1 (List.nil U))) '(Exp.var 0) '(lift 1 0 Exp.tDia))
                      [(list 'exact (list 'rt_strengthen 'chkf D us '(Exp.var 0) '(lift 1 0 Exp.tDia) der 0 'rfl))])))))

(deftest f3-theorem3-tokens
  (testing "Lemma 2.3 for a mask, and Theorem 3's refinement to the free tokens"
    (doseq [c '[maskUF gsh mask_cons mask_vadd mask_vscale mask_len mask_nth sh_ok mask_var_false rt_mask
                ucnt cntU tok_entry tok_sat_mask theorem3_tokens]]
      (is (b/has? c) (str c))))
  (testing "the count is of the tokens actually free: var 0 in Θ₂ leaves one token"
    (is (not (b/rejects? '[] '(Eq Nat (cntU (maskUF (thetaU 2) (freshF (Exp.var 0)))) 1) '[(rfl)])))
    (is (b/rejects? '[] '(Eq Nat (cntU (maskUF (thetaU 2) (freshF (Exp.var 0)))) 2) '[(rfl)]))))
(require 'lcert.formal.eval)
(deftest f5-evaluator
  (testing "Theorem 4's evaluator: a program evaluates, and the constant cases are adequate"
    (doseq [c '[Eval AppV rel envRel argsOK Adeq Theorem_4
                eval_tt eval_not_tt rlookup_head rlookup_zero rlookup_succ eval_cast
                some_injSk rel_bool_rfl rel_unit_rfl skel_reify rdflt_rel env_at
                adeq_star adeq_tt adeq_ff adeq_zero adeq_lbl adeq_abort adeq_h1
                adeq_succ adeq_var envRel_cons arrCase_arr adeq_lam adeq_app
                adeq_ite adeq_ite_den prodCase_prod adeq_pair adeq_chk
                adeq_bnil adeq_elim adeq_elim_den adeq_prn
                adeq_sleaf adeq_leaf adeq_snode adeq_node adeq_caseL
                bcons_apply_zero bcons_apply_succ adeq_bcons
                adeq_insp_pick adeq_insp adeq_let adeq_iter adeq_recN
                adeq_recs_leaf adeq_recs_node adeq_recs adeq_recS
                adeq_itr_leaf adeq_itr_node adeq_itr adeq_itR
                baseSk rel_budget tokens_rel den_refl_false den_refl_none den_refl_ok_eq
                adeq_refl_no adeq_refl_none adeq_refl_ok]]
      (is (b/has? c) (str c))))
  (testing "tt does not evaluate to ff: the true-constructor is the wrong value"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      n :- Nat, rho :- (List RV)]
                    '(Eval chkf dec encTy n rho Exp.tt (RV.bool Bool.false))
                    '[(exact (Ev.eTT chkf dec encTy n rho))]))))

(require 'lcert.formal.encode)
(deftest f7-encoding
  (testing "the encoding, E5, E1 (injectivity) and CheckSpec's satisfiability"
    (doseq [c '[encE encNat E5 decT roundtrip encE_inj base_enc checkspec_sat]]
      (is (b/has? c) (str c))))
  (testing "⌜0⌝ is c⊥ and ⌜1 ⊸ 0⌝ is neg ⌜1⌝, by computation"
    (is (not (b/rejects? '[] '(Eq Code (encE Exp.tEmpty) (Code.sl 15)) '[(rfl)])))
    (is (not (b/rejects? '[] '(Eq Code (encE (Exp.tPi U.u1 Exp.tUnit Exp.tEmpty)) (Code.sn 25 (Code.sl 16) (Code.sl 15))) '[(rfl)]))))
  (testing "distinct types have distinct codes: the usage is in the label"
    (is (b/rejects? '[] '(Eq Code (encE (Exp.tPi U.u1 Exp.tUnit Exp.tEmpty)) (encE (Exp.tPi U.uw Exp.tUnit Exp.tEmpty))) '[(rfl)]))))
(require 'lcert.formal.convcase)
(deftest f3m-convcase-subst
  (testing "nbr is preserved by substitution, and β's contractum stays skeleton-typed (Lemma 3.2)"
    (doseq [c '[nbrF_var nbrF_anyflag up_pres upn_pres nbrF_subst_var nbrF_subst
                inst1_nbr nbr_subst1 NbrAll instL_nbr instLS_nbr nbr_substL
                skj_subst1 skj_substL skj_beta_core skj_beta]]
      (is (b/has? c) (str c))))
  (testing "a branch list substituted for a variable is not nbr"
    (is (b/rejects? '[]
                    '(Eq Bool (nbr (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.var 0))) Bool.true)
                    '[(rfl)]))))

(deftest f3m-convcase-hd-skj
  (testing "every head step preserves skeleton typing and nbr (Lemma 3.2)"
    (doseq [c '[skj_iteT skj_iteF skj_elimT skj_elimF skj_recNZ skj_recNS skj_recSL
                skj_prnL skj_prnN skj_boolExp skj_delta
                skj_betaLet_at skj_betaLet_core skj_betaLet skj_recSN skj_itRL skj_itRN
                skj_bcons_pick nbr_bcons_pick skj_nthB nbr_nthB skj_caseLb hd_skj]]
      (is (b/has? c) (str c))))
  (testing "nthB of a non-branch-list is not a branch"
    (is (b/rejects? '[]
                    '(Eq (Option Exp) (nthB Exp.tt 0) (Option.some Exp Exp.tt))
                    '[(rfl)]))))

(deftest f3m-convcase-path
  (testing "a path step preserves skOf and the denotation (Lemma 3.2)"
    (doseq [c '[step_nil_pack hd_not_base nbr_hd_flag step_nil_fl
                step_tEmpty step_tUnit step_tBool step_tNat step_tLbl step_tSyn
                step_tDia step_tR step_tT step_tPi step_tSig step_tBrs
                step_var step_star step_tt step_ff step_zero step_lbl step_bnil
                den_succ_cong sk_abort_A
                den_ig_abort_A den_ig_h1_r den_ig_node_d den_coe_sleaf den_coe_leaf
                den_coe_prn den_ite_t den_ite_e
                den_sn_a den_sn_c1 den_sn_c2 den_nd_a den_nd_r1 den_nd_r2 den_chk_c den_chk_d
                step_abort step_succ step_h1 step_sleaf step_leaf step_prn step_ite
                step_snode step_node step_chk
                sk_elimB_P den_el_b den_el_t den_el_e step_elimB
                den_recN_n den_recN_z den_recN_s sk_recN_P step_recN
                den_caseL_a den_caseL_bs sk_caseL_P step_caseL
                den_bcons_h den_bcons_t step_bcons
                den_recS_c den_recS_tl den_recS_tn sk_recS_P step_recS
                den_itR_g den_itR_h den_itR_r sk_itR_X step_itR
                sk_lam_A sk_lam_t den_lam_t step_lam
                sk_app_f den_app_f den_app_u step_app
                sk_pair_S den_pair_a den_pair_b step_pair
                skof_letp sk_letp_C den_letp_p den_letp_t step_letp
                getP_base den_refl_r step_refl_D step_refl
                den_insp_r den_insp_c den_insp_t1 den_insp_t2 sk_insp_X step_insp
                step_pack
                V_tT_den V_tTT_gen V_tTF_gen step_V_tT_nil step_V_tT_b step_V_tT
                V_pi_at V_pi_at_cast V_pi_dom
                pi_no_head V_pi_at_bcst V_pi_cod step_V_pi
                V_sig_at sig_no_head V_sig_at_cast V_sig_dom
                V_sig_at_bcst V_sig_cod step_V_sig
                step_V_tEmpty step_V_tDia step_V_var step_V_tT_pack
                step_V_tPi step_V_tSig step_V_tBrs step_V
                step_V_of_step cv_V F_conv conv_all
                Lemma_3_6_holds Theorem_1_holds Corollary_3_7_holds]]
      (is (b/has? c) (str c))))
  (testing "a childless term has no child to step in"
    (is (b/rejects? '[]
                    '(Eq (Option Exp) (getP (List.cons Nat 0 (List.nil Nat)) Exp.star)
                                      (Option.some Exp Exp.star))
                    '[(rfl)]))))

(require 'lcert.formal.prop410)
(deftest f4-prop410-prime
  (testing "Proposition 4.10′ and its parts: the χ-model's Lemma 3.6, the checker transport, the separation"
    (doseq [c '[noH1 noh_app refl0_val den_refl_dec0 refl0_empty F_refl0 lemma36_noH1
                Agree agree_mono hd_tr trI step_tr cv_tr tl_tr rt_tr
                chiF chi_agree padC pad_nodes pad_ok chi_big ev4 ev5 p410_core prop410_prime]]
      (is (b/has? c) (str c))))
  (testing "H₁ itself is not H₁-free, so the theorem does not cover the term of Theorem 2"
    (is (b/rejects? '[] '(Eq Bool (noH1 (H1term)) Bool.true) '[(rfl)])))
  (testing "χ accepts a code larger than its bound, and agrees with the checker below it"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)] '(Eq Bool (chiF chkf 0 (Code.sl 0) (padC 0)) Bool.true)
                         '[(exact (chi_big chkf 0 (Code.sl 0) (padC 0) (big1 0)))])))))
(require 'lcert.formal.theorem4)
(deftest f5-skof-agree
  (testing "skOf agrees with simple typing off the branch-list spine"
    (doseq [c '[arrCod_arr skOf_lam_none skOf_app_none skOf_bnil_none
                brHead_bnil argsOK_bnil_true argsOK_app_eq
                skOf_brHead br_of_skSome skOf_agree
                argsOK_base argsOK_const match_sk_true argsOK_app_asm
                let_ok_prod argsOK_recs_tn tl_argsOK rt_argsOK
                sk_witness skels_theta baseSk_base closed_of_base skj_along
                eval_only app_arg_sk let_match_some let_body_of lt_le_omega
                adeq_refl_asm
                case_abort case_ite case_elim case_succ case_recN case_caseL
                case_bcons case_snode case_recS case_node case_itR
                case_sleaf case_leaf case_prn case_lam case_pair case_chk
                case_app case_let case_refl case_insp h1_oks case_h1
                adeq_skj adeq_from_below below_zero below_succ adeq_below_all
                theorem4 theorem4_spec corollary51 corollary51_conv corollary51_nodes]]
      (is (b/has? c) (str c))))
  (testing "bnil is simply typable and argsOK, but skOf is none: argsOK alone does not give skOf = some"
    (is (not (b/rejects? '[G :- (List Sk)]
                          '(Eq Bool (argsOK G Exp.bnil) Bool.true)
                          '[(rfl)])))
    (is (b/rejects? '[G :- (List Sk)]
                    '(Eq (Option Sk) (skOf G Exp.bnil) (Option.some Sk (Sk.arr Sk.lbl Sk.unit)))
                    '[(rfl)]))
    (is (b/rejects? '[G :- (List Sk)]
                    '(Eq Bool (argsOK G (Exp.app Exp.bnil Exp.bnil)) Bool.true)
                    '[(rfl)]))
    (is (b/rejects? '[]
                    '(Eq Bool (isBaseTy (Exp.tPi U.u0 Exp.tUnit Exp.tUnit)) Bool.true)
                    '[(rfl)]))
    (is (b/rejects? '[]
                    '(Eq Bool (isBaseTy Exp.tR) Bool.false)
                    '[(rfl)]))))
(require 'lcert.formal.erase)
(deftest f5-erase
  (testing "erasure, the erasing evaluator, and the trace root"
    (doseq [c '[bad_abort bad_h1 bad_H bad_refl_unit bad_star
                isH_empty isH_unit
                usk_empty usk_unit usk_bool usk_t usk_pi0 usk_pi1 usk_sig0 usk_brs
                ebase_unit eve_star ok_star
                er_app0_shape er_pair0_shape er_total
                usk_skel e_star e_tt e_ff e_zero e_lbl ebase_dflt ebase_rel
                erdflt_arr0 erdflt_arrN erdflt_prod0 erdflt_prodN
                erdflt erdflt_ty dflt_cast erel_rv erel_car e_abort e_h1
                usks_nil usks_cons entryE_u0 entryE_u1 entryE_uw
                envE_nil envE_cons envE_cons0 envE_cons1 envE_consw
                usk_hd usk_hd_needs_wf
                nz_uadd nz_umul nz_vadd nz_vscale]]
      (is (b/has? c) (str c))))
  (testing "⋆ is not an abort, and a Π₀ type is not the runtime arrow"
    (is (b/rejects? '[] '(Eq Bool (badNode Exp.star) Bool.true) '[(rfl)]))
    (is (b/rejects? '[X :- Exp, Y :- Exp]
                    '(Eq USk (usk (Exp.tPi U.u0 X Y)) (USk.arrN (usk X) (usk Y)))
                    '[(rfl)]))
    (is (b/rejects? '[xs :- (List U)]
                    '(Eq Bool (nzAt (List.cons U U.u0 xs) 0) Bool.true)
                    '[(rfl)]))
    ;; The product default is (ff, ⋆), not (⋆, ⋆): Σ₀ of E cannot demand ⋆.
    (is (b/rejects? '[]
                    '(Eq RV (rdflt (Sk.prod Sk.bool Sk.unit)) (RV.pair RV.star RV.star))
                    '[(rfl)]))
    ;; Ill-typed β changes the usage skeleton, so usk_hd needs isTy.
    (is (b/rejects? '[]
                    '(Eq USk (usk (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star)) (usk Exp.tBool))
                    '[(rfl)]))))

(require 'lcert.formal.section4c)
(deftest f4-section4c
  (testing "Proposition 4.11: the sharing chain, its denotation, and the node count"
    (doseq [c '[sh4_pow_succ sh4_bush_succ sh4_grow_zero sh4_grow_succ
                sh4_open_zero sh4_open_succ sh4_cn_unfold
                sh4_lenU_cons sh4_lenE_cons sh4_lenU_vzero sh4_len_head
                sh4_vadd_zz sh4_vscale_z sh4_vadd_uw sh4_vadd_u0uw sh4_vscale_uw
                sh4_vadd_node sh4_vadd_app sh4_nthE0 sh4_nthU0 sh4_nonzero_w
                sh4_var sh4_lbl sh4_snode sh4_open_step sh4_open_typed prop411_typed
                sh4_sk_sleaf sh4_sk_snode sh4_den_var0 sh4_den_snode
                sh4_open_den_step sh4_open_den sh4_grow_double sh4_grow_bush prop411_den
                sh4_twice sh4_bush_size sh4_sub_succ sh4_sub_add1 prop411_nodes prop411]]
      (is (b/has? c) (str c))))
  (testing "three doublings are seven internal nodes, and the chain is a Syn term"
    (is (not (b/rejects? '[] '(Eq Nat (cnodes (sh4_bush 3)) 7) '[(exact (prop411_nodes 3))])))
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                         '(Rt chkf (List.nil Exp) (List.nil U) (sh4_cn 2) Exp.tSyn)
                         '[(exact (prop411_typed chkf 2))]))))
  (testing "the leaf chain is not a node, and it is not a term of R"
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code),
                      cap :- Nat]
                    '(Eq Code (den chkf dec encTy cap (sh4_cn 0) (List.nil Sk) Sk.syn Unit.unit)
                            (Code.sn 0 (Code.sl 0) (Code.sl 0)))
                    '[(exact (prop411_den chkf dec encTy cap 0))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (sh4_cn 0) Exp.tR)
                    '[(exact (prop411_typed chkf 0))]))))
  (testing "Proposition 4.12 (1): the doubling recursor, packaged, at budget 0"
    (doseq [c '[sh4_M sh4_motive_step sh4_motive_sub sh4_star_ty sh4_zero_ty sh4_nz1
                sh4_len_nil sh4_lenL sh4_lenC sh4_lenS sh4_lenP sh4_nth_syn sh4_nthu_w
                sh4_lbl_body sh4_code_var sh4_star_body sh4_snode_body sh4_pair_body
                sh4_acc_var sh4_let_step sh4_base sh4_num_typed sh4_formP prop412_typed
                sh4_den_var1 sh4_den_star sh4_sk_var0 sh4_den_acc sh4_fst sh4_snd
                sh4_den_snode1 sh4_den_body sh4_den_step sh4_den_base sh4_den_num
                sh4_den_doubler prop412_den prop412_nodes prop412]]
      (is (b/has? c) (str c))))
  (testing "at 3 the package has seven internal nodes; at 0 it is a leaf, not a node, and not a Syn term"
    (is (not (b/rejects? '[chkf :- (=> Code Code Bool),
                           dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                           encTy :- (=> Exp Code), cap :- Nat]
                         '(Eq Nat (cnodes (Prod.fst (den chkf dec encTy cap (sh4_doubler (sh4_num 3))
                                                          (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)))
                                 7)
                         '[(exact (prop412_nodes chkf dec encTy cap 3))])))
    (is (b/rejects? '[chkf :- (=> Code Code Bool),
                      dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                      encTy :- (=> Exp Code), cap :- Nat]
                    '(Eq (Prod Code Unit)
                         (den chkf dec encTy cap (sh4_doubler (sh4_num 0))
                              (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
                         (Prod.mk (Code.sn 0 (Code.sl 0) (Code.sl 0)) Unit.unit))
                    '[(exact (prop412_den chkf dec encTy cap 0))]))
    (is (b/rejects? '[chkf :- (=> Code Code Bool)]
                    '(Rt chkf (List.nil Exp) (List.nil U) (sh4_doubler (sh4_num 0)) Exp.tSyn)
                    '[(exact (prop412_typed chkf 0))]))))

(require 'lcert.formal.prop434)
(deftest f4-prop43-44
  (testing "Propositions 4.3 and 4.4 (1), with their parts"
    (doseq [c '[TokSize TypeSize cnt_mask_le prop43 cTerm cterm_code box_closed negN negN_size cert_size prop44_1 box_in box_out box_evid prop44_2 prop44_3]]
      (is (b/has? c) (str c))))
  (testing "code terms decode to their codes; ¬¹1 has a one-node code under E5's shape"
    (is (not (b/rejects? '[] '(Eq (Option Code) (codeOf (cTerm (Code.sn 3 (Code.sl 1) (Code.sl 2)))) (Option.some Code (Code.sn 3 (Code.sl 1) (Code.sl 2)))) '[(rfl)])))
    (is (b/rejects? '[] '(Eq (Option Code) (codeOf (cTerm (Code.sl 1))) (Option.some Code (Code.sl 2))) '[(rfl)]))))
