(ns lcert.formal.dflt64
  "F6.4 — the meta content of Lemma 6.4 (R4-metatheory.md §6.3; ADR-0006,
  F6 plan): the DEFAULT model is sound at a fixed budget, with no induction on
  budgets.

  The default model is the generic R-interpretation (rint.clj) with `print`
  the identity and `riPf` constantly false (riDflt): every tree counts as
  \"not phantom-free\", so ⟦reflect_X r e⟧ is always the default dflt_X
  (den_refl_none_ri), whatever r is; H₁ reads its premises' prints and finds
  them excluded by PhCons.  Over riDflt the model sends exactly what §6.3
  says: reflect_D r e ↦ dflt_D (D = 0 included, where there is no default and
  the case is vacuous: Corollary 3.7, BF₀), H₁ … ↦ 0 (vacuous: BF₁), print ↦
  identity.  (abort_A t ↦ dflt_{skel A} is the standard model's clause.)

  What the generic chain needs, and what it does not.  lemma36_gen
  (lemma46a.clj) already gives Lemma 3.6 at EVERY interpretation, riDflt
  included, but its assembly (ri_lemma36.clj) is an induction on the budget:
  lemma36_step_ri proves Sound at n from OuterIH_ri n, Lemma 3.6 at every
  smaller budget, and outer_all_ri supplies that by induction on n.  Only two
  cases read OuterIH: Reflect and H₁, and only in the BRANCH where the tree
  is phantom-free (refl_pf_ri, h1_core_ri: decode the printed certificate at
  a budget m < n and apply the hypothesis at m).  Over an interpretation
  whose riPf is constantly false that branch is empty, so the outer
  hypothesis is never used.  This namespace shows it:

    F_refl_dfl, F_h1_dfl     the two cases WITHOUT hout, from the phantom
                             branch alone (refl_ph_ri; PhCons);
    lemma64_step             the step lemma with those two cases and no
                             OuterIH — copied from lemma36_step_ri with only
                             those two terms replaced;
    lemma64_gen / lemma64    Sound at ONE budget n, by induction on the
                             derivation only: no hypothesis on smaller
                             budgets, no induction on n.

  That is Lemma 6.4's point: E-PA^ω cannot do the budget-indexed induction,
  and none is done.  The two things Lemma 6.4 does use — Corollary 3.7 (BF₀,
  BF₁: PhCons, phcons_of_spec, which holds at every size) and CheckSpec — are
  hypotheses/facts about the checker, not inductions on n.  (Corollary 3.7's
  own proof, cor37_*, is the generic Lemma 3.6 at the standard model, which
  does the outer induction; in Int-6.4 that is the object-level BF₀/BF₁,
  proved in PA, Lemma 6.3.)  What remains informal is the transcription of
  this argument into E-PA^ω (P5-hypotheses.md §5)."
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
            [lcert.formal.outer :refer :all]
            [lcert.formal.rint]
            [lcert.formal.ri-den]
            [lcert.formal.ri-sem]
            [lcert.formal.ri-model]
            [lcert.formal.ri-fundamental]
            [lcert.formal.ri-outer]
            [lcert.formal.ri-lemma36]
            [lcert.formal.ri-convcase]
            [lcert.formal.lemma46a]
            [lcert.formal.check-spec]
            [lcert.formal.p5]
            [clojure.string :as str]))

;; --- the default interpretation -----------------------------------------------------------

;; print and the leaf label are the identity; riPf is constantly false, so
;; reflect never decodes the certificate (it returns its default).
(kdef riDfltD RIntD (mkRID (fn [l :- Nat] l) (fn [c :- Code] c) (fn [c :- Code] Bool.false)))
;; The laws L1–L4 (rint.clj): L1, L2 as for stdRI; L3 is vacuous (riPf is
;; false); L4: the default certificate sl 0 has its labels in L.
(thm riDflt_laws [] (RLawsD riDfltD)
  (constructor) (intro l) (exact (Eq.refl$1 (Code.sl l)))
  (constructor) (intro l v w) (exact (Eq.refl$1 (Code.sn l v w)))
  (constructor) (intro v hv) (exact (False.elim (Bool.noConfusion hv)))
  (exact (Eq.refl$1 Bool.true)))
(kdef riDflt RInt (Subtype.mk riDfltD riDflt_laws))

;; Its fields, definitionally.
(thm riDflt_pr [c :- Code] (Eq Code (riPr riDflt c) c) (rfl))
(thm riDflt_lf [l :- Nat] (Eq Nat (riLf riDflt l) l) (rfl))
(thm riDflt_pf [c :- Code] (Eq Bool (riPf riDflt c) Bool.false) (rfl))

;; PhCons at riDflt is Corollary 3.7 at every size (BF₀, BF₁): phcons_of_spec
;; (lemma46a.clj) for the printed codes, which are the codes themselves.
(thm riDflt_phcons [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                    hcs :- (CheckSpec chkf dec encTy)]
  (PhCons chkf encTy riDflt)
  (exact (phcons_of_spec chkf dec encTy hcs riDflt)))

;; --- the two cases that used the outer hypothesis, without it ------------------------------

;; The private builders of ri_outer.clj / ri_lemma36.clj.
(defn- pv [s] (var-get (ns-resolve 'lcert.formal.ri-outer s)))
(def ^:private P6 (pv 'P6))
(def ^:private ENV (pv 'ENV))
(def ^:private concl (pv 'concl))
(def ^:private SND (pv 'SND))
(def ^:private ES (pv 'ES))
(def ^:private split-steps (pv 'split-steps))
(def ^:private v1 (pv 'v1))
(def ^:private pv1 (pv 'pv1))
(def ^:private vv (pv 'vv))
(def ^:private ww (pv 'ww))
(def ^:private dc (pv 'dc))
(def ^:private pvv (pv 'pvv))
(def ^:private pww (pv 'pww))
(def ^:private ndc (pv 'ndc))

;; hpa: riPf ri is constantly false (the hypothesis of the default model).
(def ^:private HPA (list 'forall '[v :- Code] '(Eq Bool (riPf ri v) Bool.false)))

;; Reflect, as F_refl_ri up to the last step: e's IH gives the check on the
;; print of ⟦r⟧ at ⌜X⌝; then, riPf being false, the phantom branch refl_ph_ri
;; (⟦reflect⟧ is the default; X = 0 is excluded by PhCons).  hout is not used.
(eval (concat (list 'lcert.formal.base/thm 'F_refl_dfl
  (into P6 (into '[us1 :- (List U), us2 :- (List U), X :- Exp, cd :- Exp, r :- Exp, e :- Exp,
                   hb :- (Eq (Option Exp) (baseCode X) (Option.some Exp cd)),
                   hcs :- (CheckSpec chkf dec encTy),
                   hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri), hpa :- (forall [v Code] (Eq Bool (riPf ri v) Bool.false))]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihe :- (SND 'us2 'e '(chkT r cd))]
                       (conj ENV 'hs :- (ES '(vadd us1 us2) 'k)))))
  (concl 'X '(Exp.refl X r e)))
  (concat
    (split-steps 'hs 'us1 'us2 'k 'k1 'k2 'p)
    ['(have hkn (Nat.le (+ k1 k2) n) (Nat.le_trans (And.left p) hk))
     '(have hk2 (Nat.le k2 n) (Nat.le_trans (Nat.le_add_left k2 k1) hkn))
     '(have ve (V_ri chkf dec encTy ri n (chkT r cd) (skels D) en k2 Sk.unit (den_ri chkf dec encTy ri n e (skels D) Sk.unit en))
        (ihe en k2 hk2 (And.right (And.right p))))
     (list 'have 'hchk0 (list 'Eq 'Bool (list 'chkf pv1 '(den_ri chkf dec encTy ri n cd (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k2 r cd (den_ri chkf dec encTy ri n e (skels D) Sk.unit en) ve))
     '(have hcdv (Eq Code (den_ri chkf dec encTy ri n cd (skels D) Sk.syn en) (encTy X))
        (base_den_ri chkf dec encTy ri n (And.left (And.right hcs)) X cd hb (skels D) en))
     (list 'have 'hchk (list 'Eq 'Bool (list 'chkf pv1 '(encTy X)) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf pv1 'q) 'Bool.true)) 'hcdv) 'hchk0))
     (list 'exact (list 'refl_ph_ri 'chkf 'dec 'encTy 'ri 'n 'D 'X 'cd 'r 'e 'hb 'hrl 'hph 'en 'k 'hk 'hchk (list 'hpa v1)))])))

;; H₁, as F_h1_ri up to the two checks, then PhCons (an accepted contradictory
;; pair, since neither tree is phantom-free).  hout is not used.
(eval (concat (list 'lcert.formal.base/thm 'F_h1_dfl
  (into P6 (into '[us1 :- (List U), us2 :- (List U), us3 :- (List U), us4 :- (List U), us5 :- (List U),
                   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
                   hcs :- (CheckSpec chkf dec encTy),
                   hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri), hpa :- (forall [v Code] (Eq Bool (riPf ri v) Bool.false))]
                 (into ['ihr :- (SND 'us1 'r 'Exp.tR) 'ihs :- (SND 'us2 's 'Exp.tR)
                        'ih1 :- (SND 'us4 'e1 '(chkT r c)) 'ih2 :- (SND 'us5 'e2 '(chkT s (negT c)))]
                       (conj ENV 'hs :- (ES '(vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) 'k)))))
  (concl 'Exp.tEmpty '(Exp.h1 r s c e1 e2)))
  (concat
    (split-steps 'hs 'us1 '(vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5))) 'k 'k1 'm1 'p1)
    (split-steps '(And.right (And.right p1)) 'us2 '(vadd (vscale U.uw us3) (vadd us4 us5)) 'm1 'k2 'm2 'p2)
    (split-steps '(And.right (And.right p2)) '(vscale U.uw us3) '(vadd us4 us5) 'm2 'k3 'm3 'p3)
    (split-steps '(And.right (And.right p3)) 'us4 'us5 'm3 'k4 'k5 'p4)
    ['(have o1 (LE.le (+ k1 m1) k) (And.left p1)) '(have o2 (LE.le (+ k2 m2) m1) (And.left p2))
     '(have o3 (LE.le (+ k3 m3) m2) (And.left p3)) '(have o4 (LE.le (+ k4 k5) m3) (And.left p4))
     '(have hk4 (Nat.le k4 n) (le_chain4 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     '(have hk5 (Nat.le k5 n) (le_chain5 k1 m1 k2 m2 k3 m3 k4 k5 k n o1 o2 o3 o4 hk))
     (list 'have 'hc1 (list 'Eq 'Bool (list 'chkf pvv dc) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k4 r c (den_ri chkf dec encTy ri n e1 (skels D) Sk.unit en) (ih1 en k4 hk4 (And.left (And.right p4)))))
     (list 'have 'hc2a (list 'Eq 'Bool (list 'chkf pww '(den_ri chkf dec encTy ri n (negT c) (skels D) Sk.syn en)) 'Bool.true)
        '(chk_true_ri chkf dec encTy ri n (skels D) en k5 s (negT c) (den_ri chkf dec encTy ri n e2 (skels D) Sk.unit en) (ih2 en k5 hk5 (And.right (And.right p4)))))
     (list 'have 'hc2 (list 'Eq 'Bool (list 'chkf pww ndc) 'Bool.true)
        (list 'Eq.mp (list 'congrArg (list 'fn '[q :- Code] (list 'Eq 'Bool (list 'chkf pww 'q) 'Bool.true)) '(den_negT_ri chkf dec encTy ri n c (skels D) en)) 'hc2a))
     (list 'exact (list 'False.elim (list '(And.right hph) vv ww dc (list 'Or.inl (list 'hpa vv)) 'hc1 'hc2)))])))

;; --- the step lemma without the outer hypothesis --------------------------------------------

;; lemma36_step_ri's case terms, with the Reflect and H₁ entries replaced by
;; the cases above (they differ only in the dropped `hout` and the added `hpa`).
(def ^:private case-terms
  (mapv (fn [s]
          (cond
            (str/starts-with? s "(F_h1_ri")
            (-> s (str/replace "(F_h1_ri" "(F_h1_dfl") (str/replace " hcs hout hrl hph" " hcs hrl hph hpa"))
            (str/starts-with? s "(F_refl_ri")
            (-> s (str/replace "(F_refl_ri" "(F_refl_dfl") (str/replace " hcs hout hrl hph" " hcs hrl hph hpa"))
            :else s))
        (var-get (ns-resolve 'lcert.formal.ri-lemma36 'case-terms))))

;; Lemma 6.4's step (R4 §6.3): at a budget n, every runtime derivation is sound,
;; for an interpretation whose riPf is constantly false.  Hypotheses: CheckSpec,
;; the laws, PhCons (Corollary 3.7 at every size), the Conv case — and NO outer
;; hypothesis on smaller budgets.  By induction on the derivation only.
(a/prove-theorem 'lemma64_step
  (lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt, cap :- Nat,
        hcs :- (CheckSpec chkf dec encTy),
        hrl :- (RLaws ri), hph :- (PhCons chkf encTy ri), hpa :- (forall [v Code] (Eq Bool (riPf ri v) Bool.false)),
        hconv :- (ConvCase_ri chkf dec encTy ri cap),
        D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)])
  '(=> (WFCtx chkf D0) (Sound_ri chkf dec encTy ri cap D0 us0 t0 A0))
  (lv (into ['(induction der)]
            (mapcat (fn [t] ['(intro hw) (list 'exact (read-string t))]) case-terms))))

;; --- Lemma 6.4 (meta content) ---------------------------------------------------------------

;; Sound at one budget, for any interpretation whose riPf is constantly false.
;; The laws are the interpretation's own (ri_laws), PhCons is Corollary 3.7
;; (phcons_of_spec), the Conv case is conv_all_ri.  No OuterIH, no induction on n.
(thm lemma64_gen [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  hcs :- (CheckSpec chkf dec encTy), ri :- RInt, hpa :- (forall [v Code] (Eq Bool (riPf ri v) Bool.false)),
                  n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound_ri chkf dec encTy ri n D us t A)
  (exact (lemma64_step chkf dec encTy ri n hcs (ri_laws ri) (phcons_of_spec chkf dec encTy hcs ri) hpa
           (conv_all_ri chkf dec encTy ri n) D us t A der hw)))

;; Lemma 6.4 at the default model: for every runtime derivation in a
;; well-formed context, at every budget n, the default model's denotation of t
;; is sound.  (Sound_ri at riDflt: for all η ⊨ₖ (D, us), k ≤ n,
;; ⟦t⟧ⁿη ∈ Vⁿₖ(A)η, where ⟦reflect_X r e⟧ = dflt_X and ⟦H₁ …⟧ = 0.)
(thm lemma64 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
              hcs :- (CheckSpec chkf dec encTy),
              n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)]
  (Sound_ri chkf dec encTy riDflt n D us t A)
  (exact (lemma64_gen chkf dec encTy hcs riDflt (fn [v :- Code] (Eq.refl$1 Bool.false)) n D us t A der hw)))

;; The statement the paper uses (Lemma 6.4): Θₙ ⊢ t :¹ A derivable gives
;; ⟦t⟧ ∈ V(A), in the all-token context Θₙ at its token environment, at
;; footprint n (the form of OuterIH_ri at m = n, tok_sat_ri).
(thm lemma64_theta [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                    hcs :- (CheckSpec chkf dec encTy),
                    n :- Nat, t :- Exp, A :- Exp, der :- (Rt chkf (thetaD n) (thetaU n) t A)]
  (V_ri chkf dec encTy riDflt n A (skels (thetaD n)) (tokEnvD n) n (skel A)
        (den_ri chkf dec encTy riDflt n t (skels (thetaD n)) (skel A) (tokEnvD n)))
  (exact (lemma64 chkf dec encTy hcs n (thetaD n) (thetaU n) t A der (wf_theta chkf n)
           (tokEnvD n) n (Nat.le_refl n) (tok_sat_ri chkf dec encTy riDflt n n))))

;; --- Corollary 6.5 (meta content) -----------------------------------------------------------

;; ⟦chk′ x c⊥⟧ at a context whose head is a code c is the check of c against
;; ⌜0⌝ (base_den: ⟦c⊥⟧ = ⌜0⌝ by CheckSpec).
(thm den_conchk_ri [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), ri :- RInt,
                    hcs :- (CheckSpec chkf dec encTy), n :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code]
  (Eq Bool (den_ri chkf dec encTy ri n (Exp.chk (Exp.var 0) (cbot)) (List.cons Sk Sk.syn G) Sk.bool (Prod.mk c en))
           (chkf c (encTy Exp.tEmpty)))
  (rw [(den_chk_val_ri chkf dec encTy ri n (Exp.var 0) (cbot) (List.cons Sk Sk.syn G) (Prod.mk c en))])
  (rw [(den_var_at_ri chkf dec encTy ri n 0 (List.cons Sk Sk.syn G) Sk.syn (Prod.mk c en))])
  (rw [(base_den_ri chkf dec encTy ri n (And.left (And.right hcs)) Exp.tEmpty (cbot) (Eq.refl$1 (Option.some Exp (cbot))) (List.cons Sk Sk.syn G) (Prod.mk c en))]))

;; Corollary 6.5, at the default model: a derivation of Con′_ω at budget n
;; gives that no code in L checks as a refutation (§6.3: V(Con′_ω)(⟦t⟧)
;; unfolds to ∀c (IsSyn c → ∀e (e = 0 ∧ CHECK(c, c⊥) = 1 → ⊥))).  IsSyn c is
;; lblOk c = true, the Syn clause of V.  (cor37_refutation gives the same for
;; every code, without a derivation: it is Corollary 3.7, the BF₀ that
;; Lemma 6.4 itself uses.)
(thm cor65_dflt [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                 hcs :- (CheckSpec chkf dec encTy),
                 n :- Nat, t :- Exp, der :- (Rt chkf (thetaD n) (thetaU n) t ConOmega),
                 c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)]
  (Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.false)
  (have hV := (lemma64_theta chkf dec encTy hcs n t ConOmega der))
  (refine' (bool_false_of (chkf c (encTy Exp.tEmpty)) _))
  (intro h)
  (have hv1 (forall [a Code] (=> (V_ri chkf dec encTy riDflt n Exp.tSyn (skels (thetaD n)) (tokEnvD n) 0 Sk.syn a)
         (V_ri chkf dec encTy riDflt n (Exp.tPi U.uw (Exp.tT (Exp.chk (Exp.var 0) (cbot))) Exp.tEmpty)
               (List.cons Sk Sk.syn (skels (thetaD n))) (Prod.mk a (tokEnvD n)) n
               (Sk.arr Sk.unit (skel Exp.tEmpty))
               ((den_ri chkf dec encTy riDflt n t (skels (thetaD n)) (skel ConOmega) (tokEnvD n)) a))))
    hV)
  (have hv2 := (hv1 c hc))
  (have hv3 (forall [a Unit] (=> (V_ri chkf dec encTy riDflt n (Exp.tT (Exp.chk (Exp.var 0) (cbot)))
                                       (List.cons Sk Sk.syn (skels (thetaD n))) (Prod.mk c (tokEnvD n)) 0 Sk.unit a)
         (V_ri chkf dec encTy riDflt n Exp.tEmpty
               (List.cons Sk Sk.unit (List.cons Sk Sk.syn (skels (thetaD n)))) (Prod.mk a (Prod.mk c (tokEnvD n))) n
               (skel Exp.tEmpty)
               (((den_ri chkf dec encTy riDflt n t (skels (thetaD n)) (skel ConOmega) (tokEnvD n)) c) a))))
    hv2)
  (have hT (V_ri chkf dec encTy riDflt n (Exp.tT (Exp.chk (Exp.var 0) (cbot)))
                 (List.cons Sk Sk.syn (skels (thetaD n))) (Prod.mk c (tokEnvD n)) 0 Sk.unit Unit.unit)
    (Eq.trans (den_conchk_ri chkf dec encTy riDflt hcs n (skels (thetaD n)) (tokEnvD n) c) h))
  (exact (hv3 Unit.unit hT)))

;; Corollary 6.5 at the concrete checker, with no hypotheses (as selfjust.clj
;; for Theorem 2): Check decCert meets CheckSpec (check_spec).
(thm cor65_concrete [n :- Nat, t :- Exp, der :- (Rt (Check decCert) (thetaD n) (thetaU n) t ConOmega),
                     c :- Code, hc :- (Eq Bool (lblOk c) Bool.true)]
  (Eq Bool (Check decCert c (encE Exp.tEmpty)) Bool.false)
  (exact (cor65_dflt (Check decCert) (decOf decCert) encE (check_spec decCert) n t der c hc)))

;; Lemma 6.4 at the concrete checker, with no hypotheses.
(thm lemma64_concrete [n :- Nat, t :- Exp, A :- Exp, der :- (Rt (Check decCert) (thetaD n) (thetaU n) t A)]
  (V_ri (Check decCert) (decOf decCert) encE riDflt n A (skels (thetaD n)) (tokEnvD n) n (skel A)
        (den_ri (Check decCert) (decOf decCert) encE riDflt n t (skels (thetaD n)) (skel A) (tokEnvD n)))
  (exact (lemma64_theta (Check decCert) (decOf decCert) encE (check_spec decCert) n t A der)))
