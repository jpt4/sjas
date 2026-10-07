(ns lcert.formal.lemma46a
  "F4 — Lemma 4.6a (R4-metatheory.md §4.5): Lemma 3.6 holds for the phantom
  model ⟦·⟧*, V*.  Design note nachlass/docs-theorem46-design.md §2–§4.

  The generic chain (ri_* namespaces, generated from the standard chain by
  formal/tools/gen_ri.py plus the hand edits of tools/ri_edits.py) proves
  Lemma 3.6 once, for the model over any R-interpretation ri : RInt
  (rint.clj): lemma36_ri.  It takes, besides CheckSpec,
    - RLaws ri: the four laws L1–L4 of the interpretation.  Every RInt
      carries them (ri_laws);
    - PhCons chkf encTy ri: Corollary 3.7 for trees that are not
      phantom-free, which the H₁ and Reflect cases use where a phantom
      blocks the budget descent;
    - the Conv case at every budget, ConvCase_ri, which conv_all_ri gives
      for every interpretation (no CheckSpec needed).

  phcons_of_spec: PhCons holds at EVERY interpretation, given CheckSpec,
  by the standard Corollary 3.7 (cor37_refutation, cor37_contradiction):
  the condition is about the printed codes, and Corollary 3.7 holds of
  every code, at every size.  So the generic lemma needs nothing but
  CheckSpec (lemma36_gen), and

  lemma46a: Lemma 3.6 for the phantom interpretation phRI J σ (phantoms
  ★₀ … ★_{J−1} printing as σ 0 … σ (J−1), whose labels lie in L: PhOk J σ),
  given CheckSpec only.  This is §4.5's Lemma 4.6a.  No encoding fact is
  used beyond CheckSpec; §4.5's \"Lemma 2.8 applies without a phantom\" is the
  law L3 of the interpretation (rl_nodes), proved of phRI in rint.clj.

  lemma36_std: the standard interpretation stdRI is an instance of the same
  generic proof, with PhCons discharged trivially (stdRI_phcons: every tree
  is phantom-free there), so it does not depend on the standard chain's
  Corollary 3.7 — one proof of the fundamental lemma covers both models.
  den_ri at stdRI is the standard den, and V_ri the standard V,
  definitionally (den_ri_std, V_ri_std, by rfl); so is Sound_ri the
  standard Sound (sound_ri_std), and lemma36_via_ri recovers the standard
  Lemma 3.6 (lemma36.clj's statement) from the generic proof alone."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.lemma36 :refer :all]
            [lcert.formal.convcase]
            [lcert.formal.rint]
            [lcert.formal.ri-den]
            [lcert.formal.ri-sem]
            [lcert.formal.ri-model]
            [lcert.formal.ri-fundamental]
            [lcert.formal.ri-lemma36]
            [lcert.formal.ri-convcase]))

;; --- PhCons from Corollary 3.7 ------------------------------------------------------------

;; PhCons at any interpretation: a printed tree is a code, and Corollary 3.7
;; (the standard one, from the standard Lemma 3.6 with the Conv case
;; conv_all) holds of every code.
(thm phcons_of_spec [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                     hcs :- (CheckSpec chkf dec encTy), ri :- RInt]
  (PhCons chkf encTy ri)
  (constructor)
  (intro v hv h)
  (have hf (Eq Bool (chkf (riPr ri v) (encTy Exp.tEmpty)) Bool.false)
    (cor37_refutation chkf dec encTy hcs (conv_all chkf dec encTy hcs) (riPr ri v)))
  (exact (False.elim (Bool.noConfusion (Eq.trans (Eq.symm h) hf))))
  (intro v w d hor h1 h2)
  (exact (cor37_contradiction chkf dec encTy hcs (conv_all chkf dec encTy hcs) (riPr ri v) (riPr ri w) d h1 h2)))

;; --- the generic Lemma 3.6, and its two instances ------------------------------------------

;; Lemma 3.6 over any R-interpretation, given CheckSpec only.
(thm lemma36_gen [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  hcs :- (CheckSpec chkf dec encTy), ri :- RInt,
                  n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound_ri chkf dec encTy ri n D us t A)
  (exact (lemma36_ri chkf dec encTy ri hcs (ri_laws ri) (phcons_of_spec chkf dec encTy hcs ri)
           (conv_all_ri chkf dec encTy ri) n D us t A der hw)))

;; Lemma 4.6a: Lemma 3.6 for the phantom model ⟦·⟧*, V* (phRI J σ), given
;; CheckSpec and that the phantom codes' labels lie in L.
(thm lemma46a [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
               hcs :- (CheckSpec chkf dec encTy), J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg),
               n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound_ri chkf dec encTy (phRI J sg hsg) n D us t A)
  (exact (lemma36_gen chkf dec encTy hcs (phRI J sg hsg) n D us t A der hw)))

;; The standard model as an instance of the generic proof, without the
;; standard Corollary 3.7: at stdRI every tree is phantom-free, so PhCons
;; holds vacuously (stdRI_phcons).
(thm lemma36_std [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  hcs :- (CheckSpec chkf dec encTy),
                  n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound_ri chkf dec encTy stdRI n D us t A)
  (exact (lemma36_ri chkf dec encTy stdRI hcs (ri_laws stdRI) (stdRI_phcons chkf encTy)
           (conv_all_ri chkf dec encTy stdRI) n D us t A der hw)))

;; --- the standard model is the generic model at stdRI ---------------------------------------

;; At stdRI every clause of the generic model is the standard one,
;; definitionally (riLf, riPr the identity, riPf constantly true), so the
;; denotation and the semantic types coincide by rfl, and with them Sound.
(thm den_ri_std [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                 n :- Nat, t :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den_ri chkf dec encTy stdRI n t G s en) (den chkf dec encTy n t G s en))
  (rfl))
(thm V_ri_std [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
               n :- Nat, A :- Exp, G :- (List Sk), en :- (HEnv G), k :- Nat, s :- Sk, v :- (Car s)]
  (Eq Prop (V_ri chkf dec encTy stdRI n A G en k s v) (V chkf dec encTy n A G en k s v))
  (rfl))
(thm sound_ri_std [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp]
  (Eq Prop (Sound_ri chkf dec encTy stdRI n D us t A) (Sound chkf dec encTy n D us t A))
  (rfl))

;; The standard Lemma 3.6 (lemma36.clj's statement), from the generic proof
;; alone: lemma36_std uses lemma36_ri and conv_all_ri, not the standard chain.
(thm lemma36_via_ri [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                     hcs :- (CheckSpec chkf dec encTy),
                     n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound chkf dec encTy n D us t A)
  (exact (lemma36_std chkf dec encTy hcs n D us t A der hw)))
