(ns lcert.formal.s52theorem
  "F5 — Theorem 5.2 assembled (R4-metatheory.md §5): neither evaluator ever
  reaches abort, H₁ or H.

  s52erased.clj proves the fundamental property of S at every budget, for
  evalₙ (the Tl and Rt inductions, s52assembly.clj) and for evalᴱₙ (the Er
  induction): in the closed token context Θₙ, the term (resp. every erasure
  of the derivation) evaluates under a SAFE trace — Ok, resp. OkE, the
  evaluator's rules with the abort, H₁ and H nodes removed, nested reflect
  runs included — to a value S-related to the denotation.  Here the safe
  trace is read off at the root.

  Statement.  For a derivable Θₙ ⊢ t :¹ X, the program t has an Ok trace
  (and the evaluator Ev, of which Ok is the sub-evaluator, evaluates it).
  The paper states the theorem at a base data type D; the proof does not
  use that, so it is stated for every X, and theorem52_base records the
  paper's instance.  Hypothesis: CheckSpec only, as for Theorems 4 and 4′.

  What \"safe\" means here.  Ok is a derivation of evaluation using only the
  rules other than eAbort, eH1 and eRefl at the empty type: a node of the
  forbidden kinds is not an Ok rule, and every nested program that reflect
  decodes is run by Ok at the smaller budget.  Ev is not proved
  deterministic in this formalization (its rules are syntax- and
  value-directed, but no functionality theorem is stated), so \"the
  evaluation\" is read as: an Ok trace exists, and Ok ⊆ Ev (ok52_eval)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.s52erased]
            [lcert.formal.certenc]))

;; Theorem 5.2 for evalₙ.
(thm theorem52
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (Exists (fn [v :- RV]
    (And (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)
         (Eval chkf dec encTy n (rtokens n) t v))))
  (have hres (Result52 chkf dec encTy n Bool.false (skels (thetaD n)) (tokEnvD n) (rtokens n) t X t)
    (s52_closed_all chkf dec encTy hcs Bool.false n t X hd t (Eq.refl t)))
  (refine' (exT RV _ _ hres _)) (intro v pv)
  (constructor) (exact v)
  (constructor)
  (exact (And.left pv))
  (exact (ok52_eval chkf dec encTy n (EvSrc.tm (rtokens n) t) v (And.left pv))))

;; The paper's instance: X a base data type.
(thm theorem52_base
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X),
   hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (Exists (fn [v :- RV]
    (And (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)
         (Eval chkf dec encTy n (rtokens n) t v))))
  (exact (theorem52 chkf dec encTy hcs n t X hd)))

;; Theorem 5.2 for evalᴱₙ: every erasure of the derivation is safe.
(thm theorem52e
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (forall [e Exp]
    (=> (Er chkf (thetaD n) (thetaU n) t X e)
      (Exists (fn [v :- RV]
        (And (OkE chkf dec encTy n (EvSrc.tm (rtokens n) e) v)
             (EvalE chkf dec encTy n (rtokens n) e v))))))
  (intro e her)
  (have hres (Result52 chkf dec encTy n Bool.true (skels (thetaD n)) (tokEnvD n) (rtokens n) t X e)
    (s52_closed_all chkf dec encTy hcs Bool.true n t X hd e her))
  (refine' (exT RV _ _ hres _)) (intro v pv)
  (constructor) (exact v)
  (constructor)
  (exact (And.left pv))
  (exact (ok52_evalE chkf dec encTy n (EvSrc.tm (rtokens n) e) v (And.left pv))))

;; Both: an erasure exists (er_total), and both evaluators run safely.
(thm theorem52_both
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (And (Exists (fn [v :- RV] (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)))
    (Exists (fn [e :- Exp]
      (And (Er chkf (thetaD n) (thetaU n) t X e)
        (Exists (fn [v :- RV] (OkE chkf dec encTy n (EvSrc.tm (rtokens n) e) v)))))))
  (constructor)
  (refine' (exT RV _ _ (theorem52 chkf dec encTy hcs n t X hd) _)) (intro v pv)
  (constructor) (exact v) (exact (And.left pv))
  (refine' (exT Exp _ _ (er_total chkf (thetaD n) (thetaU n) t X hd) _)) (intro e her)
  (refine' (exT RV _ _ (theorem52e chkf dec encTy hcs n t X hd e her) _)) (intro v pv)
  (constructor) (exact e) (constructor) (exact her)
  (constructor) (exact v) (exact (And.left pv)))

;; Theorem 5.2 at the concrete checker (as selfjust.clj for T1): no
;; hypothesis. Check decCert meets CheckSpec (check_spec).
(thm theorem52_concrete
  [n :- Nat, t :- Exp, X :- Exp, hd :- (Rt (Check decCert) (thetaD n) (thetaU n) t X)]
  (And (Exists (fn [v :- RV] (Ok (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) t) v)))
    (And (forall [e Exp]
           (=> (Er (Check decCert) (thetaD n) (thetaU n) t X e)
             (Exists (fn [v :- RV]
               (OkE (Check decCert) (decOf decCert) encE n (EvSrc.tm (rtokens n) e) v)))))
         (Exists (fn [e :- Exp] (Er (Check decCert) (thetaD n) (thetaU n) t X e)))))
  (have h52 := (theorem52_both (Check decCert) (decOf decCert) encE (check_spec decCert) n t X hd))
  (constructor)
  (exact (And.left h52))
  (constructor)
  (intro e her)
  (refine' (exT RV _ _ (theorem52e (Check decCert) (decOf decCert) encE (check_spec decCert) n t X hd e her) _))
  (intro v pv) (constructor) (exact v) (exact (And.left pv))
  (refine' (exT Exp _ _ (And.right h52) _)) (intro e he) (constructor) (exact e) (exact (And.left he)))
