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
  (testing "CheckSpec is needed (PhCons is Corollary 3.7): no proof of Lemma 6.4 without it"
    (is (b/rejects? (into P3 '[n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                               der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)])
                    '(Sound_ri chkf dec encTy riDflt n D us t A)
                    '[(exact (lemma64 chkf dec encTy _ n D us t A der hw))])))
  (testing "Corollary 6.5 is about codes in L: the unrestricted claim does not follow from the derivation alone"
    (is (b/rejects? (into P3 '[hcs :- (CheckSpec chkf dec encTy), n :- Nat, t :- Exp, der :- (Rt chkf (thetaD n) (thetaU n) t ConOmega), c :- Code])
                    '(Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.false)
                    '[(exact (cor65_dflt chkf dec encTy hcs n t der c _))]))))
