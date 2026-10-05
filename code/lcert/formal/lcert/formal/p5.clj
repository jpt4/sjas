(ns lcert.formal.p5
  "F6.5 — Proposition 5 and Proposition 4.8, at the concrete checker
  (R4-metatheory.md §6, §4.6; ADR-0006, F6 plan).

  Proposition 5: for no budget n and term t is
      Θₙ ⊢ t :¹ Con′_ω,   Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0,
  derivable, where chk′ denotes F7's concrete checker Check decCert.

  The proof is R4 §6.4's argument. Its λᶜᵉʳᵗ₀ side is formal; its arithmetic
  side enters as named hypotheses, each stated exactly below and justified
  in nachlass/refinement/P5-hypotheses.md (sources for S1 and S3; paper
  proofs for Int-6.1 and Int-6.4). The theorem takes as parameters the
  sentences Con_PA and Con_λ (elements of pa.clj's PF) and E-PA^ω's
  provability on sentences, EPAw:
  - s1   (S1, Gödel II)    PA consistent → PA ⊬ Con_PA;
  - s3   (S3, Kohlenbach)  EPAw σ → PA ⊢ σ;
  - i61  (Int-6.1)         PA ⊢ Con_λ → Con_PA;
  - i64  (Int-6.4)         a derivation of Con′_ω gives EPAw Con_λ;
  - aL, aPA (anchors)      the meaning in ℕ of Con_λ and of Con_PA.
  PA's consistency is not a hypothesis: pa_consistent (pa.clj) proves it in
  the metatheory (a relative consistency result; ADR-0006's trust base).
  The anchors are not used in the deduction; they fix what the two
  parameters mean in ℕ, so that S1 and Int-6.1 are read for the intended
  sentences. What they cannot fix — the *presentation* (naturalness) — is
  stated in P5-hypotheses.md §1 and §4.0.

  Proposition 4.8: no budget derives H° → Con′_ω, nor H° ⊸ Con′_ω. Applied
  to T2's closed inhabitant of H° (weakened into Θₙ), either would derive
  Con′_ω, against Proposition 5. The application step (p5Apply_w,
  p5Apply_1) holds for every checker."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.derivations :refer :all]
            [lcert.formal.pa]
            [lcert.formal.selfjust]))

;; Con′_ω = Π(c :ω Syn). T(chk′ c c⊥) → 0  (R4 §4.6).
(kdef ConOmega Exp (Exp.tPi U.uw Exp.tSyn (Exp.tPi U.uw (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)))

;; ---------------------------------------------------------------------------
;; The application step, for every checker.

;; n usages 0: the usage vector of a closed derivation weakened into Θₙ.
(kdef p5ZU (=> Nat (List U))
  (fn [n :- Nat] (Nat.rec$1 (fn [_ :- Nat] (List U)) (List.nil U) (fn [k :- Nat, ih :- (List U)] (List.cons U U.u0 ih)) n)))

;; T2's closed inhabitant of H°, weakened into Θₙ (Lemma 2.1 at the head,
;; n times; H° and its term are closed, so lifting leaves them unchanged).
(thm p5Hterm_wk [chkf :- (=> Code Code Bool), n :- Nat] (Rt chkf (thetaD n) (p5ZU n) (Hterm) (Hcirc))
  (induction n) (exact (Theorem_2_H chkf))
  (exact (rt_weaken chkf (thetaD n) (p5ZU n) (Hterm) (Hcirc) ih_n 0 Exp.tDia)))

;; The formation of H° and of Con′_ω (in the context of a hypothesis of
;; type H°), built in a concrete context and weakened into Θₙ.
(def ^:private N0 '(List.nil Exp))
(def ^:private D1 (list 'List.cons 'Exp 'Exp.tR N0))
(a/prove-theorem 'p5Hcirc_form0 (lv '[chkf :- (=> Code Code Bool)]) (lv (list 'Tl 'chkf 'Bool.true N0 '(Hcirc) 'Exp.tUnit))
  (lv [(list 'exact (list 'Tl.fPi 'chkf N0 'U.u1 'Exp.tR '(Exp.tPi U.u1 (chkT (Exp.var 0) (cbot)) Exp.tEmpty)
         (list 'Tl.fBase 'chkf N0 'Exp.tR 'rfl)
         (list 'Tl.fPi 'chkf D1 'U.u1 '(chkT (Exp.var 0) (cbot)) 'Exp.tEmpty
           (list 'Tl.fT 'chkf D1 '(Exp.chk (Exp.prn (Exp.var 0)) (cbot))
             (list 'Tl.zChk 'chkf D1 '(Exp.prn (Exp.var 0)) '(cbot)
               (list 'Tl.zPrn 'chkf D1 '(Exp.var 0) (list 'Tl.zVar 'chkf D1 0 'Exp.tR 'rfl))
               (list 'Tl.zSleaf 'chkf D1 '(Exp.lbl 15) (list 'Tl.zConst 'chkf D1 '(Exp.lbl 15) 'Exp.tLbl 'rfl))))
           (list 'Tl.fBase 'chkf (list 'List.cons 'Exp '(chkT (Exp.var 0) (cbot)) D1) 'Exp.tEmpty 'rfl))))]))
(def ^:private H0 (list 'List.cons 'Exp '(Hcirc) N0))
(def ^:private H1 (list 'List.cons 'Exp 'Exp.tSyn H0))
(a/prove-theorem 'p5Con_form0 (lv '[chkf :- (=> Code Code Bool)]) (lv (list 'Tl 'chkf 'Bool.true H0 'ConOmega 'Exp.tUnit))
  (lv [(list 'exact (list 'Tl.fPi 'chkf H0 'U.uw 'Exp.tSyn '(Exp.tPi U.uw (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)
         (list 'Tl.fBase 'chkf H0 'Exp.tSyn 'rfl)
         (list 'Tl.fPi 'chkf H1 'U.uw '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) 'Exp.tEmpty
           (list 'Tl.fT 'chkf H1 '(Exp.chk (Exp.var 0) (cbot))
             (list 'Tl.zChk 'chkf H1 '(Exp.var 0) '(cbot)
               (list 'Tl.zVar 'chkf H1 0 'Exp.tSyn 'rfl)
               (list 'Tl.zSleaf 'chkf H1 '(Exp.lbl 15) (list 'Tl.zConst 'chkf H1 '(Exp.lbl 15) 'Exp.tLbl 'rfl))))
           (list 'Tl.fBase 'chkf (list 'List.cons 'Exp '(Exp.tT (Exp.chk (Exp.var 0) (cbot))) H1) 'Exp.tEmpty 'rfl))))]))
(thm p5Hcirc_form [chkf :- (=> Code Code Bool), n :- Nat] (Tl chkf Bool.true (thetaD n) (Hcirc) Exp.tUnit)
  (induction n) (exact (p5Hcirc_form0 chkf))
  (exact (tl_weaken chkf Bool.true (thetaD n) (Hcirc) Exp.tUnit ih_n 0 Exp.tDia)))
;; (the Θₙ entries go below the hypothesis: weakening at position 1)
(thm p5Con_form [chkf :- (=> Code Code Bool), n :- Nat] (Tl chkf Bool.true (List.cons Exp (Hcirc) (thetaD n)) ConOmega Exp.tUnit)
  (induction n) (exact (p5Con_form0 chkf))
  (exact (tl_weaken chkf Bool.true (List.cons Exp (Hcirc) (thetaD n)) ConOmega Exp.tUnit ih_n 1 Exp.tDia)))

;; Θₙ's usages, plus ρ · (n usages 0), are Θₙ's usages.
(thm p5Use_w [n :- Nat] (Eq (List U) (vadd (thetaU n) (vscale U.uw (p5ZU n))) (thetaU n))
  (induction n) (rfl)
  (have q (Eq (List U) (List.cons U U.u1 (vadd (thetaU n) (vscale U.uw (p5ZU n)))) (List.cons U U.u1 (thetaU n)))
    (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n))
  (exact q))
(thm p5Use_1 [n :- Nat] (Eq (List U) (vadd (thetaU n) (vscale U.u1 (p5ZU n))) (thetaU n))
  (induction n) (rfl)
  (have q (Eq (List U) (List.cons U U.u1 (vadd (thetaU n) (vscale U.u1 (p5ZU n)))) (List.cons U U.u1 (thetaU n)))
    (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n))
  (exact q))

;; A derivation of H° → Con′_ω (or H° ⊸ Con′_ω) in Θₙ, applied to H°'s
;; inhabitant, derives Con′_ω in Θₙ. (subst1 of the closed argument into the
;; closed Con′_ω computes to Con′_ω.)
(doseq [[nm r use] [['p5Apply_w 'U.uw 'p5Use_w] ['p5Apply_1 'U.u1 'p5Use_1]]]
  (a/prove-theorem nm
    (lv ['chkf :- '(=> Code Code Bool) 'n :- 'Nat 't :- 'Exp
         'hf :- (list 'Rt 'chkf '(thetaD n) '(thetaU n) 't (list 'Exp.tPi r '(Hcirc) 'ConOmega))])
    (lv '(Rt chkf (thetaD n) (thetaU n) (Exp.app t (Hterm)) ConOmega))
    (lv [(list 'exact (list 'Eq.mp (list 'congrArg '(fn [us :- (List U)] (Rt chkf (thetaD n) us (Exp.app t (Hterm)) ConOmega)) (list use 'n))
           (list 'Rt.rApp 'chkf '(thetaD n) '(thetaU n) '(p5ZU n) r 't '(Hterm) '(Hcirc) 'ConOmega 'rfl 'hf
                 '(p5Hterm_wk chkf n) '(p5Hcirc_form chkf n) '(p5Con_form chkf n))))])))

;; ---------------------------------------------------------------------------
;; Proposition 5 and Proposition 4.8, from the named hypotheses.

(def ^:private hyps
  '[conPA :- PF, conL :- PF, EPAw :- (=> PF Prop),
    ;; S1: if PA is consistent, PA does not prove Con_PA.
    s1 :- (=> (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz)))) (Not (PPrv conPA))),
    ;; S3: E-PA^ω's arithmetical theorems are PA's.
    s3 :- (forall [σ PF] (=> (EPAw σ) (PPrv σ))),
    ;; Int-6.1: PA proves Con_λ → Con_PA.
    i61 :- (PPrv (PF.pimp conL conPA)),
    ;; Int-6.4: a derivation of Con′_ω makes E-PA^ω prove Con_λ.
    i64 :- (forall [n Nat] (forall [t Exp] (=> (Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega) (EPAw conL)))),
    ;; Anchors: Con_λ says no code checks as a refutation; Con_PA says PA is consistent.
    aL :- (forall [ρ (=> Nat Nat)] (Iff (paHolds ρ conL) (forall [c Code] (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)))),
    aPA :- (forall [ρ (=> Nat Nat)] (Iff (paHolds ρ conPA) (Not (PPrv (PF.peq PT.pz (PT.ps PT.pz))))))])
(def ^:private hyp-args '[conPA conL EPAw s1 s3 i61 i64 aL aPA])

;; R4 §6.4: Int-6.4, S3, Int-6.1 with MP, then S1 with pa_consistent.
(a/prove-theorem 'prop5 (lv hyps)
  (lv '(forall [n Nat] (forall [t Exp] (Not (Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega)))))
  (lv '[(intro n t d)
        (have hE (EPAw conL) (i64 n t d))
        (have hL (PPrv conL) (s3 conL hE))
        (have hP (PPrv conPA) (PPrv.mp conL conPA hL i61))
        (exact (s1 pa_consistent hP))]))

(doseq [[nm r app] [['prop48 'U.uw 'p5Apply_w] ['prop48_lolli 'U.u1 'p5Apply_1]]]
  (a/prove-theorem nm (lv hyps)
    (lv (list 'forall '[n Nat] (list 'forall '[t Exp]
          (list 'Not (list 'Rt '(Check decCert) '(thetaD n) '(thetaU n) 't (list 'Exp.tPi r '(Hcirc) 'ConOmega))))))
    (lv [(list 'intro 'n 't 'd)
         (list 'exact (concat (list 'prop5) hyp-args ['n '(Exp.app t (Hterm)) (list app '(Check decCert) 'n 't 'd)]))])))
