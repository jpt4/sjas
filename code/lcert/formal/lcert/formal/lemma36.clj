(ns lcert.formal.lemma36
  "F3n — the fundamental lemma (R4-metatheory.md Lemma 3.6), assembled.

  lemma36_step: at a budget n, given the outer hypothesis OuterIH n (Lemma
  3.6 at every smaller budget, for the all-token contexts Θₘ), every runtime
  derivation D ⊢ t :^us A in a well-formed context is sound:
      WFCtx D ⟹ Sound n D us t A
  (for all η ⊨ₖ (D, us) with k ≤ n, ⟦t⟧ⁿη ∈ Vⁿₖ(A)η).  By induction on the
  derivation; each rule is its case lemma (fundamental.clj, outer.clj).  A
  premise under binders is used at the extended context, whose
  well-formedness the rule's formation premises give (WFCtx is a list of
  formation judgments, one per entry).

  outer_all: OuterIH n for every n, by induction on n — OuterIH (n+1) at
  m = n is lemma36_step at n, at the token environment (tok_sat).  This is
  §3.6's argument that the induction is not circular: a certificate is
  decoded at a budget strictly below the one being proved.

  lemma36: Sound n D us t A for every n and every derivation in a
  well-formed context.

  Then Theorem 1 (consistency), Corollary 3.7 (Check accepts no refutation
  and no contradictory pair) and Theorem 3 (certificate size), from Lemma 3.6.

  Two rules enter as hypotheses until their cases are proved: Conv (needs
  Lemma 3.2, conversion invariance) and RecSyn.  Their statements are
  exactly what the induction needs (ConvCase, RecSCase)."
  (:require [ansatz.core :as a]
            [clojure.string :as str]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.derivations]
            [lcert.formal.skeletons]
            [lcert.formal.outer :refer :all]))

(def ^:private PS "chkf dec encTy")

;; The Conv case, as the induction uses it: soundness at A carries over to a
;; formed B convertible with A.
(kdef ConvCase (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)] (=> Nat Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (forall [D (List Exp)] (forall [us (List U)] (forall [t Exp] (forall [A Exp] (forall [B Exp]
      (=> (Rt chkf D us t A) (Tl chkf Bool.true D B Exp.tUnit) (Cv chkf (skels D) A B)
          (Sound chkf dec encTy n D us t A) (Sound chkf dec encTy n D us t B)))))))))

;; The RecSyn case, as the induction uses it (the node branch's IH still
;; asks for its context's well-formedness).
(kdef RecSCase (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)] (=> Nat Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (forall [D (List Exp)] (forall [us1 (List U)] (forall [us2 (List U)] (forall [us3 (List U)]
    (forall [P Exp] (forall [tl Exp] (forall [tn Exp] (forall [c Exp]
      (=> (Rt chkf D us1 c Exp.tSyn) (Tl chkf Bool.true (consE Exp.tSyn D) P Exp.tUnit)
          (Sound chkf dec encTy n D us1 c Exp.tSyn)
          (=> (WFCtx chkf (consE Exp.tLbl D))
              (Sound chkf dec encTy n (consE Exp.tLbl D) (consU U.uw (vscale U.uw us2)) tl (leafTy P)))
          (=> (WFCtx chkf (consE (y2Ty P) (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D))))))
              (Sound chkf dec encTy n (consE (y2Ty P) (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))))
                     (consU U.u1 (consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw us3)))))) tn (nodeTy P)))
          (WFCtx chkf D)
          (Sound chkf dec encTy n D (vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3))) (Exp.recS P tl tn c) (subst1 c P)))))))))))))

;; A case whose lemma takes no environment hypothesis (V of its type is
;; everything at that value): wrap it as Sound.
(defn- wrap [us body]
  (str "(fn [en :- (HEnv (skels D)), kk :- Nat, hkk :- (Nat.le kk cap), hss :- (EnvSat chkf dec encTy cap D " us " en kk)] " body ")"))

;; One proof term per rule of Rt, in constructor order; CAP is the
;; theorem's budget.  hw : WFCtx D; ih_<premise> : WFCtx D′ → Sound … .
(def ^:private case-terms
  (let [C (str PS " cap")
        tR "(And.intro (Tl.fBase chkf D Exp.tR (Eq.refl$1 Bool.true)) hw)"]
    [;; rVar, rConst
     (str "(F_var " C " D us i A r hw hA hu hr)")
     (wrap "us" (str "(F_const " C " D us en kk hkk t A h)"))
     ;; rLam, rApp0, rApp
     (str "(F_lam " C " D us A t B r (ih_ht (And.intro hA hw)))")
     (str "(F_app0 " C " D us f u A B hu hA hB (ih_hf hw))")
     (str "(F_app " C " D us1 us2 f u A B hu hA hB r hr (ih_hf hw) (ih_hu hw))")
     ;; rPair0, rPair, rLet
     (str "(F_pair0 " C " D us A B x y hA hB hx (ih_hy hw))")
     (str "(F_pair " C " D us1 us2 A B x y hA hB hx r hr (ih_hx hw) (ih_hy hw))")
     (str "(F_let " C " D us1 us2 A B C p t hC hA hB r hp (ih_hp hw) (ih_ht (And.intro hB (And.intro hA hw))))")
     ;; rAbort, rConv
     (str "(F_abort " C " D us A t (ih_ht hw))")
     "(hconv D us t A B ht hB hc (ih_ht hw))"
     ;; rIte, rElimB, rSucc, rRecN
     (str "(F_ite " C " D us1 us1 us2 b t e C (ih_hb hw) (ih_ht hw) (ih_he hw))")
     (str "(F_elimB " C " D us1 us1 us2 P b t e hb hP (ih_ht hw) (ih_he hw))")
     (wrap "us" (str "(F_succ " C " D us n en kk hkk)"))
     (str "(F_recN " C " D us1 us1 us2 us3 P z s n hn hP (ih_hz hw) (ih_hs (And.intro hP (And.intro (Tl.fBase chkf D Exp.tNat (Eq.refl$1 Bool.true)) hw))))")
     ;; rCaseL, rBnil, rBcons
     (str "(F_caseL " C " D us1 us1 us2 P x bs hx hP (ih_hx hw) (ih_hb hw))")
     (wrap "us" (str "(F_bnil " C " D us P en kk hkk)"))
     (str "(F_bcons " C " D us us P k h t hP (ih_hh hw) (ih_ht hw))")
     ;; rSleaf, rSnode, rRecS
     (str "(F_sleaf " C " D us x (ih_h hw))")
     (str "(F_snode " C " D us1 us1 us2 us3 x c1 c2 (ih_hx hw) (ih_h1 hw) (ih_h2 hw))")
     "(hrecs D us1 us2 us3 P tl tn c hc hP (ih_hc hw) ih_hl ih_hn hw)"
     ;; rLeaf, rNode, rItR, rPrn
     (str "(F_leaf " C " D us x (ih_h hw))")
     (str "(F_node " C " D us1 us1 us2 us3 us4 d x r1 r2 (ih_hd hw) (ih_hx hw) (ih_h1 hw) (ih_h2 hw))")
     (str "(F_itR " C " D us1 us1 us2 us3 X g h r hX (ih_hg hw) (ih_hh hw) (ih_hr hw))")
     (str "(F_prn " C " D us r (ih_h hw))")
     ;; rChk, rH1, rRefl, rInsp
     (wrap "(vadd us1 us2)" (str "(F_chk " C " D (vadd us1 us2) c d en kk hkk)"))
     (str "(F_h1 " C " D us1 us2 us3 us4 us5 r s c e1 e2 hcs hout (ih_hr hw) (ih_hs hw) (ih_h1 hw) (ih_h2 hw))")
     (str "(F_refl " C " D us1 us2 X cd r e hb hcs hout (ih_hr hw) (ih_he hw))")
     (str "(F_insp " C " D us1 us0 us2 X r c t1 t2 hc hX (ih_hr hw) (ih_h1 (And.intro hF1 " tR ")) (ih_h2 (And.intro hF2 " tR ")))")]))

(a/prove-theorem 'lemma36_step
  (lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat,
        hcs :- (CheckSpec chkf dec encTy), hout :- (OuterIH chkf dec encTy cap),
        hconv :- (ConvCase chkf dec encTy cap), hrecs :- (RecSCase chkf dec encTy cap),
        D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)])
  '(=> (WFCtx chkf D0) (Sound chkf dec encTy cap D0 us0 t0 A0))
  (lv (into ['(induction der)]
            (mapcat (fn [t] ['(intro hw) (list 'exact (read-string t))]) case-terms))))

;; --- the outer induction on the budget, and Lemma 3.6 ------------------------------------

(def ^:private P3 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])
(thm outer_zero [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (OuterIH chkf dec encTy 0)
  (intro m hm) (exact (False.elim (Nat.not_lt_zero m hm))))

;; The trust base (CheckSpec) and, until proved, the Conv and RecSyn cases.
(def ^:private HYP '[hcs :- (CheckSpec chkf dec encTy), hconv :- (forall [n Nat] (ConvCase chkf dec encTy n)),
                     hrecs :- (forall [n Nat] (RecSCase chkf dec encTy n))])
;; outer_succ: OuterIH (n+1) from OuterIH n — at m < n by the hypothesis, at
;; m = n by lemma36_step at the token environment.
(eval (list 'lcert.formal.base/thm 'outer_succ (into P3 HYP)
  '(forall [n Nat] (=> (OuterIH chkf dec encTy n) (OuterIH chkf dec encTy (+ n 1))))
  '(intro n ih m hm t A hd)
  '(have hor (Or (Eq Nat m n) (Nat.lt m n)) (Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hm)))
  '(cases hor)
  '(subst h)
  '(exact (lemma36_step chkf dec encTy n hcs ih (hconv n) (hrecs n) (thetaD n) (thetaU n) t A hd (wf_theta chkf n)
            (tokEnvD n) n (Nat.le_refl n) (tok_sat chkf dec encTy n n)))
  '(exact (ih m h t A hd))))
(eval (list 'lcert.formal.base/thm 'outer_all (into P3 HYP) '(forall [n Nat] (OuterIH chkf dec encTy n))
  '(intro n) '(induction n)
  '(exact (outer_zero chkf dec encTy))
  '(exact (outer_succ chkf dec encTy hcs hconv hrecs n ih_n))))
;; Lemma 3.6.
(eval (list 'lcert.formal.base/thm 'lemma36
  (into (into P3 HYP) '[n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, der :- (Rt chkf D us t A), hw :- (WFCtx chkf D)])
  '(Sound chkf dec encTy n D us t A)
  '(exact (lemma36_step chkf dec encTy n hcs (outer_all chkf dec encTy hcs hconv hrecs n) (hconv n) (hrecs n) D us t A der hw))))

;; --- the main results ------------------------------------------------------------------

(def ^:private PH (into P3 HYP))
;; Theorem 1 (§3.8): no derivable Θₙ ⊢ t :¹ 0.  The all-token environment
;; satisfies Θₙ with footprint n, and Lemma 3.6 puts ⟦t⟧ⁿ in V(0) = ∅.
(eval (list 'lcert.formal.base/thm 'theorem1 (into PH '[n :- Nat, t :- Exp, hd :- (Rt chkf (thetaD n) (thetaU n) t Exp.tEmpty)]) 'False
  '(exact (lemma36 chkf dec encTy hcs hconv hrecs n (thetaD n) (thetaU n) t Exp.tEmpty hd (wf_theta chkf n)
            (tokEnvD n) n (Nat.le_refl n) (tok_sat chkf dec encTy n n)))))
(defn- Cb [cv dv mm tt AA]
  (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'dec cv) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
   (list 'And (list 'Rt 'chkf (list 'thetaD mm) (list 'thetaU mm) tt AA)
   (list 'And (list 'Tl 'chkf 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
   (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
   (list 'And (list 'Eq 'Code (list 'encTy AA) dv) (list 'Nat.lt mm (list 'cnodes cv))))))))
(defn- Cex [cv dv] (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (Cb cv dv 'mm 'tt 'AA))))))))
(defn- gets [q i] (let [f (fn f [x j] (if (zero? j) (list 'And.left x) (f (list 'And.right x) (dec j))))]
                    (if (= i 5) (list 'And.right (list 'And.right (list 'And.right (list 'And.right (list 'And.right q))))) (f q i))))
(thm bool_false_of [b :- Bool] (=> (=> (Eq Bool b Bool.true) False) (Eq Bool b Bool.false))
  (cases b) (all_goals (intro h)) (exact (False.elim (h (Eq.refl$1 Bool.true)))) (exact (Eq.refl$1 Bool.false)))
;; Corollary 3.7 (i): Check(c, c⊥) = ff, for every code c.  Otherwise CheckSpec
;; decodes c to Θₘ ⊢ t : A with ⌜A⌝ = ⌜0⌝; A = 0 by E1 (both closed),
;; contradicting Theorem 1.  bool_false_of: a Boolean that is not true is false.
(eval (list 'lcert.formal.base/thm 'cor37_refutation (into PH '[c :- Code]) '(Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.false)
  '(refine' (bool_false_of (chkf c (encTy Exp.tEmpty)) _))
  '(intro h)
  (list 'have 'X1 (Cex 'c '(encTy Exp.tEmpty)) '((And.left hcs) c (encTy Exp.tEmpty) h))
  '(refine' (exT Nat _ _ X1 _)) '(intro mm hm) '(refine' (exT Exp _ _ hm _)) '(intro tt ht) '(refine' (exT Exp _ _ ht _)) '(intro AA hA)
  (list 'have 'q (Cb 'c '(encTy Exp.tEmpty) 'mm 'tt 'AA) 'hA)
  (list 'have 'eA '(Eq Exp AA Exp.tEmpty) (list '(And.right (And.right (And.right hcs))) 'AA 'Exp.tEmpty (gets 'q 3) '(Eq.refl$1 Bool.true) (gets 'q 4)))
  (list 'exact (list 'theorem1 'chkf 'dec 'encTy 'hcs 'hconv 'hrecs 'mm 'tt
                     (list 'Eq.mp '(congrArg (fn [Z :- Exp] (Rt chkf (thetaD mm) (thetaU mm) tt Z)) eA) (gets 'q 1))))))
;; Corollary 3.7 (ii): no c₁, c₂, d with Check(c₁, d) = Check(c₂, neg d) = tt.
;; Decode both: Θₘ₁ ⊢ t₁ : A with ⌜A⌝ = d, Θₘ₂ ⊢ t₂ : B with ⌜B⌝ = neg d =
;; ⌜A ⊸ 0⌝ (E5), so B = A ⊸ 0 (E1); Lemma 2.4 composes them into a
;; derivable Θₘ₁₊ₘ₂ ⊢ t₂ t₁ : 0, contradicting Theorem 1.  (Cb/Cex/gets: the
;; conjunction CheckSpec gives for an accepted code, and its i-th part.)
(def ^:private nd '(Code.sn 25 d (Code.sl 15)))
(eval (list 'lcert.formal.base/thm 'cor37_contradiction
  (into PH ['c1 :- 'Code, 'c2 :- 'Code, 'd :- 'Code, 'h1 :- '(Eq Bool (chkf c1 d) Bool.true), 'h2 :- (list 'Eq 'Bool (list 'chkf 'c2 nd) 'Bool.true)]) 'False
  (list 'have 'X1 (Cex 'c1 'd) '((And.left hcs) c1 d h1))
  '(refine' (exT Nat _ _ X1 _)) '(intro ma hma) '(refine' (exT Exp _ _ hma _)) '(intro ta hta) '(refine' (exT Exp _ _ hta _)) '(intro Aa hAa)
  (list 'have 'qa (Cb 'c1 'd 'ma 'ta 'Aa) 'hAa)
  (list 'have 'X2 (Cex 'c2 nd) (list '(And.left hcs) 'c2 nd 'h2))
  '(refine' (exT Nat _ _ X2 _)) '(intro mb hmb) '(refine' (exT Exp _ _ hmb _)) '(intro tb htb) '(refine' (exT Exp _ _ htb _)) '(intro Ab hAb)
  (list 'have 'qb (Cb 'c2 nd 'mb 'tb 'Ab) 'hAb)
  (list 'have 'hcl '(Eq Bool (closedTy (Exp.tPi U.u1 Aa Exp.tEmpty)) Bool.true) (list 'andb_intro '(closedTy Aa) 'true (gets 'qa 3) '(Eq.refl$1 Bool.true)))
  (list 'have 'henc '(Eq Code (encTy (Exp.tPi U.u1 Aa Exp.tEmpty)) (encTy Ab))
     (list 'Eq.trans (list '(And.left (And.right (And.right hcs))) 'Aa (gets 'qa 3))
           (list 'Eq.trans (list 'congrArg '(fn [q :- Code] (Code.sn 25 q (Code.sl 15))) (gets 'qa 4)) (list 'Eq.symm (gets 'qb 4)))))
  (list 'have 'eB '(Eq Exp (Exp.tPi U.u1 Aa Exp.tEmpty) Ab) (list '(And.right (And.right (And.right hcs))) '(Exp.tPi U.u1 Aa Exp.tEmpty) 'Ab 'hcl (gets 'qb 3) 'henc))
  (list 'have 'hRtb '(Rt chkf (thetaD mb) (thetaU mb) tb (Exp.tPi U.u1 Aa Exp.tEmpty))
     (list 'Eq.mpr (list 'congrArg '(fn [Z :- Exp] (Rt chkf (thetaD mb) (thetaU mb) tb Z)) 'eB) (gets 'qb 1)))
  (list 'exact (list 'theorem1 'chkf 'dec 'encTy 'hcs 'hconv 'hrecs '(+ ma mb) '(Exp.app (lift ma 0 tb) (lift mb ma ta))
                     (list 'lemma24 'chkf 'ma 'mb 'ta 'tb 'Aa (gets 'qa 3) (gets 'qa 2) (gets 'qa 1) 'hRtb)))))
;; Theorem 3 (§3.7), first claim: a certificate term's value has at most k
;; nodes, for an environment satisfying the context at footprint k (V(R)).
(eval (list 'lcert.formal.base/thm 'theorem3
  (into PH '[n :- Nat, D :- (List Exp), us :- (List U), t :- Exp, der :- (Rt chkf D us t Exp.tR), hw :- (WFCtx chkf D),
             en :- (HEnv (skels D)), k :- Nat, hk :- (Nat.le k n), hs :- (EnvSat chkf dec encTy n D us en k)])
  '(LE.le (cnodes (den chkf dec encTy n t (skels D) Sk.cert en)) k)
  '(exact (And.left (lemma36 chkf dec encTy hcs hconv hrecs n D us t Exp.tR der hw en k hk hs)))))
