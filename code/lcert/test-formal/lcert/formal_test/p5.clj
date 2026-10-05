(ns lcert.formal-test.p5
  "Formal suite (ADR-0006, F6.5), lcert.formal.p5: Proposition 5 (no budget
  derives Con′_ω) and Proposition 4.8 (no budget derives H° → Con′_ω, nor
  H° ⊸ Con′_ω), at the concrete checker, from the named hypotheses S1, S3,
  Int-6.1, Int-6.4 (nachlass/refinement/P5-hypotheses.md); PA's consistency
  is a theorem of the metatheory (pa.clj, relative consistency), not an
  assumption."
  (:require [clojure.test :refer [deftest is testing]]
            [lcert.formal.base :as b]
            [lcert.formal.p5]))

(def ^:private hyps
  '[conPA :- PF, conL :- PF, EPAw :- (=> PF Prop),
    s1 :- (=> (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz)))) (Not (PPrv conPA))),
    s3 :- (forall [σ PF] (=> (EPAw σ) (PPrv σ))),
    i61 :- (PPrv (PF.pimp conL conPA)),
    i64 :- (forall [n Nat] (forall [t Exp] (=> (Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega) (EPAw conL)))),
    aL :- (forall [ρ (=> Nat Nat)] (Iff (paHolds ρ conL) (forall [c Code] (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)))),
    aPA :- (forall [ρ (=> Nat Nat)] (Iff (paHolds ρ conPA) (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))))])

(deftest f6-prop5
  (testing "the statement, the application lemmas, Propositions 5 and 4.8"
    (doseq [nm '[ConOmega p5Hterm_wk p5Hcirc_form p5Con_form p5Apply_w p5Apply_1 prop5 prop48 prop48_lolli]]
      (is (b/has? nm) (str nm))))
  (testing "Con′_ω is Π(c :ω Syn). T(chk′ c c⊥) → 0, and is not H°"
    (is (not (b/rejects? [] '(Eq Exp ConOmega (Exp.tPi U.uw Exp.tSyn (Exp.tPi U.uw (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty))) '[(rfl)])))
    (is (b/rejects? [] '(Eq Exp ConOmega (Hcirc)) '[(rfl)])))
  (testing "P5 applies at a budget: no refutation-free code consistency at n = 4"
    (is (not (b/rejects? (into hyps '[t :- Exp])
      '(Not (Rt (Check decCert) (thetaD 4) (thetaU 4) t ConOmega))
      '[(exact (prop5 conPA conL EPAw s1 s3 i61 i64 aL aPA 4 t))]))))
  (testing "Proposition 4.8 at a budget, both arrows"
    (is (not (b/rejects? (into hyps '[t :- Exp])
      '(Not (Rt (Check decCert) (thetaD 2) (thetaU 2) t (Exp.tPi U.uw (Hcirc) ConOmega)))
      '[(exact (prop48 conPA conL EPAw s1 s3 i61 i64 aL aPA 2 t))])))
    (is (not (b/rejects? (into hyps '[t :- Exp])
      '(Not (Rt (Check decCert) (thetaD 2) (thetaU 2) t (Exp.tPi U.u1 (Hcirc) ConOmega)))
      '[(exact (prop48_lolli conPA conL EPAw s1 s3 i61 i64 aL aPA 2 t))]))))
  (testing "the application step needs H° as the domain"
    (is (b/rejects? '[chkf :- (=> Code Code Bool), n :- Nat, t :- Exp, hf :- (Rt chkf (thetaD n) (thetaU n) t (Exp.tPi U.uw Exp.tUnit ConOmega))]
      '(Rt chkf (thetaD n) (thetaU n) (Exp.app t (Hterm)) ConOmega)
      '[(exact (p5Apply_w chkf n t hf))]))))
