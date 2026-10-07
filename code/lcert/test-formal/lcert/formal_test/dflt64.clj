(ns lcert.formal-test.dflt64
  "Formal suite (ADR-0006, F6.4), lcert.formal.dflt64: the meta content of
  Lemma 6.4 (the default model is sound at a fixed budget, with no induction
  on budgets) and of Corollary 6.5 (a derivation of Con′_ω gives that no code
  checks as a refutation), as instances of the generic R-interpretation chain."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.dflt64]))

(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

(deftest f6-4-default-model
  (testing "the default interpretation, its laws, Corollary 3.7 as PhCons"
    (doseq [c '[riDfltD riDflt_laws riDflt riDflt_pr riDflt_lf riDflt_pf riDflt_phcons]]
      (is (b/has? c) (str c))))
  (testing "Lemma 6.4: the two outer-hypothesis cases without it, the step, and the lemma at one budget"
    (doseq [c '[F_refl_dfl F_h1_dfl lemma64_step lemma64_gen lemma64 lemma64_theta lemma64_concrete]]
      (is (b/has? c) (str c))))
  (testing "Corollary 6.5: den of chk′ x c⊥, from Con′_ω to no refutation, generic and concrete"
    (doseq [c '[den_conchk_ri cor65_dflt cor65_concrete]]
      (is (b/has? c) (str c))))
  (testing "the statements carry no hypothesis on smaller budgets: lemma64_step has no OuterIH argument"
    ;; the generic step needs hout; applying lemma64_step with an extra outer argument is ill-typed
    (is (b/rejects? (into P3 '[hcs :- (CheckSpec chkf dec encTy), n :- Nat, hout :- (OuterIH_ri chkf dec encTy riDflt n),
                               D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)])
                    '(Sound_ri chkf dec encTy riDflt n D us t A)
                    '[(exact (lemma64_step chkf dec encTy riDflt n hcs (ri_laws riDflt) (phcons_of_spec chkf dec encTy hcs riDflt)
                                           (fn [v :- Code] (Eq.refl$1 Bool.false)) hout D us t A der hw))])))
  (testing "the hypothesis riPf ≡ false is needed: at the standard interpretation (riPf ≡ true) it fails"
    (is (b/rejects? (into P3 '[hcs :- (CheckSpec chkf dec encTy), n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                               der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)])
                    '(Sound_ri chkf dec encTy stdRI n D us t A)
                    '[(exact (lemma64_gen chkf dec encTy hcs stdRI (fn [v :- Code] (Eq.refl$1 Bool.false)) n D us t A der hw))])))
  (testing "the default model is not the standard one: at a reflect node they differ (dflt_differs_std_reflect)"
    (doseq [c '[riDflt_ne_std dflt_differs_std_reflect]]
      (is (b/has? c) (str c)))
    (is (b/rejects? [] '(Eq Bool (riPf riDflt (Code.sl 0)) Bool.true) '[(rfl)]))
    (is (not (b/rejects? [] '(Eq Bool (riPf riDflt (Code.sl 0)) Bool.false) '[(rfl)]))))
  (testing "the hypotheses of dflt_differs_std_reflect are satisfiable (the claim is not vacuous): a checker accepting everything, decoding every code to `succ zero`, and the certificate r = leaf 0"
    (is (not (b/rejects? []
      '(Eq Bool (Bool.and (Nat.ble (cnodes (den_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk (Exp.succ Exp.zero) Exp.tNat))))
                                                   (fn [x :- Exp] (Code.sl 0)) stdRI 5 (Exp.leaf (Exp.lbl 0)) (List.nil Sk) Sk.cert Unit.unit)) 5)
                  (Bool.and (riPf stdRI (den_ri (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk (Exp.succ Exp.zero) Exp.tNat))))
                                                (fn [x :- Exp] (Code.sl 0)) stdRI 5 (Exp.leaf (Exp.lbl 0)) (List.nil Sk) Sk.cert Unit.unit))
                            Bool.true)) Bool.true)
      '[(rfl)]))))
  (testing "Corollary 3.7 is what PhCons needs: for the checker that accepts everything, PhCons fails at riDflt"
    (is (not (b/rejects? '[]
                         '(=> (PhCons (fn [a :- Code, b :- Code] Bool.true) (fn [e :- Exp] (Code.sl 0)) riDflt) False)
                         '[(intro h) (exact ((And.left h) (Code.sl 0) (Eq.refl$1 Bool.false) (Eq.refl$1 Bool.true)))])))))
