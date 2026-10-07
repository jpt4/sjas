(ns lcert.formal.s52univ
  "F5 — Theorem 5.2, UNIVERSAL form (R4-metatheory.md §5; ADR-0006).

  s52theorem.clj proves the existential theorem: a derivable Θₙ ⊢ t :¹ X has
  A safe run.  R4 §5 says THE evaluation is safe, and Ev is not deterministic
  (the rule eBnil gives the empty branch list any skeleton tag; EvE also runs
  any erasure of a reflected derivation), so existence is weaker.  This file
  proves what R4 states:

    theorem52_all   for every v and every Ev derivation of Eval n (rtokens n) t v
                    from a derivable Θₙ ⊢ t :¹ X, the run is Ok: it enters no
                    abort, H₁ or H node, nested reflect runs included;
    theorem52e_all  the same for EvE, for every erasure e of the derivation;
    theorem52_all_concrete   both at Check decCert, with no hypotheses.

  Proof.  The existential theorem gives an Ok run d₀ (and, with the erasing
  flag, an OkE run S-related to the denotation).  s52det.clj shows that every
  Ev run of the same program is a safe trace equal to d₀ up to bnil tags
  (detOk), and likewise for EvE under the budget hypothesis (detOkE): the
  rules of Ev are syntax- and value-directed, Ok has no apBnil/abort/H₁/H,
  so no tag and no unsafe node can enter a run that agrees with d₀.

  For evalE the budget induction below supplies the hypothesis detOkE needs:
  UE m, the universal statement at budget m, which also records that at a base
  type the value of any run is S-related to the denotation (so two erasures of
  a reflected program return the same value there)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.s52det]
            [lcert.formal.s52theorem]
            [lcert.formal.certenc]))

;; One budget: from the budgets below it.  The existential fundamental
;; property (s52_closed_all, erasing) provides a reference run; detOkE
;; transfers its safety and its base-type value to the arbitrary run.
(thm ue_step
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), n :- Nat,
   below :- (BELOW_E chkf dec encTy n)]
  (UE_E chkf dec encTy n)
  (intro t A hrt e her v hev)
  (have hres := (s52_closed_all chkf dec encTy hcs Bool.true n t A hrt e her))
  (refine' (exT RV _ _ hres _)) (intro v0 pv)
  (have hm := (detOkE_tm chkf dec encTy hcs n (rtokens n) e v0 (And.left pv) below
                (rtokens n) v hev (Eq.refl (flL (rtokens n)))))
  (constructor)
  (exact (And.left hm))
  (intro hbase)
  (exact (s52_base_sim chkf dec encTy n Bool.true (skels (thetaD n)) (tokEnvD n) A hbase v0 v
           (den chkf dec encTy n t (skels (thetaD n)) (skel A) (tokEnvD n))
           (And.right pv) (And.right hm))))

;; The strong induction on the budget, as s52_closed_below_all.
(thm ue_below
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (forall [m Nat] (=> (LT.lt m n) (UE_E chkf dec encTy m))))
  (intro n)
  (induction n)
  (intro m hm) (exact (False.elim (Nat.not_lt_zero m hm)))
  (intro m hm)
  (have hor (Or (Eq Nat m n) (LT.lt m n)) (Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hm)))
  (cases hor)
  (subst h)
  (exact (ue_step chkf dec encTy hcs n ih_n))
  (exact (ih_n m h)))

(thm ue_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), n :- Nat]
  (UE_E chkf dec encTy n)
  (exact (ue_step chkf dec encTy hcs n (ue_below chkf dec encTy hcs n))))

;; Theorem 5.2 for evalₙ, universal form.
(thm theorem52_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (forall [v RV]
    (=> (Eval chkf dec encTy n (rtokens n) t v)
        (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)))
  (intro v hev)
  (have h0 := (theorem52 chkf dec encTy hcs n t X hd))
  (refine' (exT RV _ _ h0 _)) (intro v0 pv)
  (have hm := (detOk_tm chkf dec encTy n (rtokens n) t v0 (And.left pv)
                (rtokens n) v hev (Eq.refl (flL (rtokens n)))))
  (exact (And.left hm)))

;; The paper's instance: X a base data type.
(thm theorem52_all_base
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X),
   hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (forall [v RV]
    (=> (Eval chkf dec encTy n (rtokens n) t v)
        (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)))
  (exact (theorem52_all chkf dec encTy hcs n t X hd)))

;; Theorem 5.2 for evalᴱₙ, universal form: every erasure, every run.
(thm theorem52e_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (forall [e Exp]
    (=> (Er chkf (thetaD n) (thetaU n) t X e)
      (forall [v RV]
        (=> (EvalE chkf dec encTy n (rtokens n) e v)
            (OkE chkf dec encTy n (EvSrc.tm (rtokens n) e) v)))))
  (intro e her v hev)
  (exact (And.left (ue_all chkf dec encTy hcs n t X hd e her v hev))))

;; Both evaluators at the concrete checker, no hypotheses (check_spec).
(thm theorem52_all_concrete
  [n :- Nat, t :- Exp, X :- Exp, hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t X)]
  (And (forall [v RV]
         (=> (Eval (Check decCert) (decOf decCert) encE n (rtokens n) t v)
             (Ok (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) t) v)))
       (forall [e Exp]
         (=> (Er (Check decCert) (thetaD n) (thetaU n) t X e)
           (forall [v RV]
             (=> (EvalE (Check decCert) (decOf decCert) encE n (rtokens n) e v)
                 (OkE (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) e) v))))))
  (constructor)
  (exact (theorem52_all (Check decCert) (decOf decCert) encE (check_spec decCert) n t X hd))
  (exact (theorem52e_all (Check decCert) (decOf decCert) encE (check_spec decCert) n t X hd)))
