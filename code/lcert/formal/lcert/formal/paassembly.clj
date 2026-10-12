(ns lcert.formal.paassembly
  "F6.3 — assembly rules for the PA translation (R4 §6.2).

  The theorem pa_lcert_of_axioms explicitly assumes typed axiom instances.
  palcert.clj discharges that premise with all templates, including DN, E2
  and IND. pa_close_refutation closes a supplied translation of 0=S0.
  These are meta-level derivation constructions, not internal PA proofs.
  Any resulting consistency corollary is relative to the metatheory."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.paaxioms]))

;; This is the PPrv induction for the desired main theorem. Its axiom
;; premise is explicit here and discharged by palcert.clj.
;; ‹lemma pa_lcert_of_axioms: statement and proof elided, lines 14–30›

;; All free PA variables may alias this one Nat entry. The formula 0=S0
;; is closed, but Good's total assignment cannot live in an empty context.
(kdef paRefuteR (=> Nat Nat) (fn [i :- Nat] 0))
(kdef paRefuteD (List Exp) (List.cons Exp Exp.tNat (List.nil Exp)))
;; ‹lemma paRefute_good: statement and proof elided, lines 36–37›
(kdef paRefuteTerm (=> Exp Exp)
  (fn [e :- Exp]
    (Exp.app (Exp.lam U.uw Exp.tNat
      (Exp.app (paPad (trF paF_COE paRefuteR) paC_COE) e)) Exp.zero)))

;; The final coercion and closing application are unconditional typing
;; rules about a supplied translation of 0=S0, not an inhabitant of it.
;; ‹lemma pa_close_refutation: statement and proof elided, lines 45–59›

;; palcert.clj applies this implication to pa_lcert to prove
;; pa_refutation. No appeal to pa_consistent or paPrv_sound occurs.
;; ‹lemma pa_refutation_of_translation: statement and proof elided, lines 63–71›
