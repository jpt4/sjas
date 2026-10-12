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
            [lcert.formal.certenc]
            ;; decCert and check_spec (theorem52_concrete); a fresh load of this
            ;; namespace alone needs them, certenc no longer defines decCert
            [lcert.formal.certcanon]))

;; Theorem 5.2 for evalₙ.
(thm theorem52
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (Exists (fn [v :- RV]
    (And (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)
         (Eval chkf dec encTy n (rtokens n) t v))))
  ;; ‹proof elided: lines 42–48 of the full file; kernel-checked›
  )

;; The paper's instance: X a base data type.
(thm theorem52_base
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X),
   hb :- (Eq Bool (isBaseTy X) Bool.true)]
  (Exists (fn [v :- RV]
    (And (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)
         (Eval chkf dec encTy n (rtokens n) t v))))
  ;; ‹proof elided: lines 59–59 of the full file; kernel-checked›
  )

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
  ;; ‹proof elided: lines 71–78 of the full file; kernel-checked›
  )

;; Both: an erasure exists (er_total), and both evaluators run safely.
(thm theorem52_both
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, X :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t X)]
  (And (Exists (fn [v :- RV] (Ok chkf dec encTy n (EvSrc.tm (rtokens n) t) v)))
    (Exists (fn [e :- Exp]
      (And (Er chkf (thetaD n) (thetaU n) t X e)
        (Exists (fn [v :- RV] (OkE chkf dec encTy n (EvSrc.tm (rtokens n) e) v)))))))
  ;; ‹proof elided: lines 89–95 of the full file; kernel-checked›
  )

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
  ;; ‹proof elided: lines 107–114 of the full file; kernel-checked›
  )
