(ns lcert.formal.erase
  "F5 — the erasing evaluator evalᴱₙ (R4-metatheory.md §5, Theorem 4′ and
  Theorem 5.2).

  Erasure is a relation, not a function of the term.  Rt is a proposition, so
  a derivation cannot be eliminated into a term: the erased program is data,
  and Prop cannot produce it.  The usage that decides erasure — Π₀ against
  Π₁/Πω, Σ₀ against Σ₁/Σω — lives in the derivation, not in Exp.app or in the
  pair's syntax alone.  Er chkf D us t A e says that some runtime derivation
  of Γ ⊢ t :¹ A erases to e.  It mirrors Rt.  At rApp0 the argument is
  replaced by ⋆; at rPair0 the first component is replaced by ⋆.  Every other
  runtime premise is erased, and type annotations are kept (neither evaluator
  evaluates them).  rConv erases to the premise's erasure.  er_total says the
  relation is total, so it is a function of the derivation up to the choice
  of derivation of one judgment.

  evalᴱ then runs that term with evalₙ's rules.  EvE is Ev with one change:
  when reflect succeeds, the decoded program is erased and run by EvE at the
  smaller budget (the paper's \"treats a program decoded by reflect the same
  way\").  dec returns a term, not a derivation; the Er premise is the
  erasure of a derivation of the judgment CheckSpec decodes.  Two derivations
  of one judgment are not yet proved to erase to one term, so the rule admits
  whichever erasure it is given.  The adequacy proof picks the one er_total
  builds from CheckSpec's derivation.

  Erel is the paper's relation E, by recursion on a usage skeleton.  E does
  not read the terms inside a type: T(b), 0 and 1 are all the unit clause
  (⋆ related to ⋆).  Π₀ relates f ⋆ to φ(α) for every carrier α; Σ₀ forgets
  the first component.  At a base skeleton E is equality of the runtime
  value with the carrier value, which does not depend on the budget.
  erdflt is the defaults paragraph, for every type.  envE is the
  environment half of the fundamental property: usage 0 is unconstrained,
  and usage 1 or ω is E-related.  e_abort and e_h1 evaluate the premises
  and return that related default.  The vacuous reading of those nodes is
  Theorem 5.2, which needs S(0) = ∅ and is not these two lemmas.

  badNode and the trace predicates Ok and OkE are Theorem 5.2's trace.  Ok
  is an evaluation derivation with no abort node, no H₁ node, and no reflect
  at 0 (H): those constructors are absent, or, for reflect, refused by
  isH D = false.  The trace includes every subderivation, every closure
  application, and every program reflect runs.  OkE is the same predicate
  for EvE, so a successful reflect is traced through the erasure it runs."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.eval]
            [lcert.formal.outer]))

;; --- erasure of a runtime derivation -----------------------------------------

;; Er chkf D us t A e: a derivation of Γ ⊢ t :¹ A erases to e.
;; Indices are the judgment and the erased term.  Constructors follow Rt.
(a/inductive Er [chkf (=> Code Code Bool)] :in Prop
  :indices [D (List Exp), us (List U), t Exp, A Exp, e Exp]

  (eVar [D (List Exp)] [us (List U)] [i Nat] [A Exp] [r U]
        [hl (Eq Nat (lenU us) (lenE D))]
        [hA (Eq (Option Exp) (nthE D i) (Option.some Exp A))]
        [hu (Eq (Option U) (nthU us i) (Option.some U r))]
        [hr (Eq Bool (nonzero r) Bool.true)]
        :where [D us (Exp.var i) (lift (+ i 1) 0 A) (Exp.var i)])
  (eConst [D (List Exp)] [us (List U)] [t Exp] [A Exp]
          [hl (Eq Nat (lenU us) (lenE D))]
          [h (Eq Bool (constTyped t A) Bool.true)]
          :where [D us t A t])

  (eLam [D (List Exp)] [us (List U)] [r U] [A Exp] [t Exp] [B Exp] [te Exp]
        [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [ht (Er chkf (consE A D) (consU r us) t B te)]
        :where [D us (Exp.lam r A t) (Exp.tPi r A B) (Exp.lam r A te)])
  ;; Π₀: the argument is not evaluated.  It is replaced by ⋆, whatever u is.
  (eApp0 [D (List Exp)] [us (List U)] [f Exp] [u Exp] [A Exp] [B Exp] [fe Exp]
         [hf (Er chkf D us f (Exp.tPi U.u0 A B) fe)]
         [hu (Tl chkf Bool.false D u A)]
         [hA (Tl chkf Bool.true D A Exp.tUnit)]
         [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
         :where [D us (Exp.app f u) (subst1 u B) (Exp.app fe Exp.star)])
  (eApp [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [f Exp] [u Exp] [A Exp] [B Exp]
        [fe Exp] [ue Exp]
        [hr (Eq Bool (nonzero r) Bool.true)]
        [hf (Er chkf D us1 f (Exp.tPi r A B) fe)]
        [hu (Er chkf D us2 u A ue)]
        [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
        :where [D (vadd us1 (vscale r us2)) (Exp.app f u) (subst1 u B) (Exp.app fe ue)])
  ;; Σ₀: the first component is replaced by ⋆.  The second is erased.
  (ePair0 [D (List Exp)] [us (List U)] [A Exp] [B Exp] [x Exp] [y Exp] [ye Exp]
          [hA (Tl chkf Bool.true D A Exp.tUnit)]
          [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
          [hx (Tl chkf Bool.false D x A)]
          [hy (Er chkf D us y (subst1 x B) ye)]
          :where [D us (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B)
                  (Exp.pair (Exp.tSig U.u0 A B) Exp.star ye)])
  (ePair [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [A Exp] [B Exp] [x Exp] [y Exp]
         [xe Exp] [ye Exp]
         [hr (Eq Bool (nonzero r) Bool.true)]
         [hA (Tl chkf Bool.true D A Exp.tUnit)]
         [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
         [hx (Er chkf D us1 x A xe)]
         [hy (Er chkf D us2 y (subst1 x B) ye)]
         :where [D (vadd (vscale r us1) us2) (Exp.pair (Exp.tSig r A B) x y) (Exp.tSig r A B)
                 (Exp.pair (Exp.tSig r A B) xe ye)])
  (eLet [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [A Exp] [B Exp] [C Exp] [p Exp] [t Exp]
        [pe Exp] [te Exp]
        [hp (Er chkf D us1 p (Exp.tSig r A B) pe)]
        [hC (Tl chkf Bool.true D C Exp.tUnit)]
        [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
        [ht (Er chkf (consE B (consE A D)) (consU U.u1 (consU r us2)) t (lift 2 0 C) te)]
        :where [D (vadd us1 us2) (Exp.letp C p t) C (Exp.letp C pe te)])
  ;; The argument is a runtime premise, so erasure keeps it.  Theorem 5.2
  ;; shows a typed program never reaches the node; erasure does not delete it.
  (eAbort [D (List Exp)] [us (List U)] [A Exp] [t Exp] [te Exp]
          [ht (Er chkf D us t Exp.tEmpty te)]
          [hA (Tl chkf Bool.true D A Exp.tUnit)]
          :where [D us (Exp.abort A t) A (Exp.abort A te)])
  (eConv [D (List Exp)] [us (List U)] [t Exp] [A Exp] [B Exp] [te Exp]
         [ht (Er chkf D us t A te)]
         [hB (Tl chkf Bool.true D B Exp.tUnit)]
         [hc (Cv chkf (skels D) A B)]
         :where [D us t B te])

  (eIte [D (List Exp)] [us1 (List U)] [us2 (List U)] [b Exp] [t Exp] [e Exp] [C Exp]
        [be Exp] [te Exp] [ee Exp]
        [hb (Er chkf D us1 b Exp.tBool be)]
        [ht (Er chkf D us2 t C te)]
        [he (Er chkf D us2 e C ee)]
        :where [D (vadd us1 us2) (Exp.ite b t e) C (Exp.ite be te ee)])
  (eElimB [D (List Exp)] [us1 (List U)] [us2 (List U)] [P Exp] [b Exp] [t Exp] [e Exp]
          [be Exp] [te Exp] [ee Exp]
          [hb (Er chkf D us1 b Exp.tBool be)]
          [hP (Tl chkf Bool.true (consE Exp.tBool D) P Exp.tUnit)]
          [ht (Er chkf D us2 t (subst1 Exp.tt P) te)]
          [he (Er chkf D us2 e (subst1 Exp.ff P) ee)]
          :where [D (vadd us1 us2) (Exp.elimB P b t e) (subst1 b P) (Exp.elimB P be te ee)])
  (eSucc [D (List Exp)] [us (List U)] [n Exp] [ne Exp]
         [h (Er chkf D us n Exp.tNat ne)]
         :where [D us (Exp.succ n) Exp.tNat (Exp.succ ne)])
  (eRecN [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [P Exp] [z Exp] [s Exp] [n Exp]
         [ze Exp] [se Exp] [ne Exp]
         [hn (Er chkf D us1 n Exp.tNat ne)]
         [hP (Tl chkf Bool.true (consE Exp.tNat D) P Exp.tUnit)]
         [hz (Er chkf D us2 z (subst1 Exp.zero P) ze)]
         [hs (Er chkf (consE P (consE Exp.tNat D)) (consU U.u1 (consU U.uw (vscale U.uw us3))) s (stepTy P) se)]
         :where [D (vadd us1 (vadd us2 (vscale U.uw us3))) (Exp.recN P z s n) (subst1 n P)
                 (Exp.recN P ze se ne)])
  (eCaseL [D (List Exp)] [us1 (List U)] [us2 (List U)] [P Exp] [x Exp] [bs Exp]
          [xe Exp] [bse Exp]
          [hx (Er chkf D us1 x Exp.tLbl xe)]
          [hP (Tl chkf Bool.true (consE Exp.tLbl D) P Exp.tUnit)]
          [hb (Er chkf D us2 bs (Exp.tBrs P 0) bse)]
          :where [D (vadd us1 us2) (Exp.caseL P x bs) (subst1 x P) (Exp.caseL P xe bse)])
  (eBnil [D (List Exp)] [us (List U)] [P Exp]
         [hl (Eq Nat (lenU us) (lenE D))]
         :where [D us Exp.bnil (Exp.tBrs P (NL)) Exp.bnil])
  (eBcons [D (List Exp)] [us (List U)] [P Exp] [k Nat] [h Exp] [t Exp] [he Exp] [te Exp]
          [hh (Er chkf D us h (subst1 (Exp.lbl k) P) he)]
          [ht (Er chkf D us t (Exp.tBrs P (+ k 1)) te)]
          [hP (Tl chkf Bool.true (consE Exp.tLbl D) P Exp.tUnit)]
          :where [D us (Exp.bcons h t) (Exp.tBrs P k) (Exp.bcons he te)])
  (eSleaf [D (List Exp)] [us (List U)] [x Exp] [xe Exp]
          [h (Er chkf D us x Exp.tLbl xe)]
          :where [D us (Exp.sleaf x) Exp.tSyn (Exp.sleaf xe)])
  (eSnode [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [x Exp] [c1 Exp] [c2 Exp]
          [xe Exp] [c1e Exp] [c2e Exp]
          [hx (Er chkf D us1 x Exp.tLbl xe)]
          [h1 (Er chkf D us2 c1 Exp.tSyn c1e)]
          [h2 (Er chkf D us3 c2 Exp.tSyn c2e)]
          :where [D (vadd us1 (vadd us2 us3)) (Exp.snode x c1 c2) Exp.tSyn (Exp.snode xe c1e c2e)])
  (eRecS [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [P Exp] [tl Exp] [tn Exp] [c Exp]
         [tle Exp] [tne Exp] [ce Exp]
         [hc (Er chkf D us1 c Exp.tSyn ce)]
         [hP (Tl chkf Bool.true (consE Exp.tSyn D) P Exp.tUnit)]
         [hl (Er chkf (consE Exp.tLbl D) (consU U.uw (vscale U.uw us2)) tl (leafTy P) tle)]
         [hn (Er chkf (consE (y2Ty P) (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))))
                  (consU U.u1 (consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw us3)))))) tn (nodeTy P) tne)]
         [hY1 (Tl chkf Bool.true (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D))) (y1Ty P) Exp.tUnit)]
         [hY2 (Tl chkf Bool.true (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))) (y2Ty P) Exp.tUnit)]
         :where [D (vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3))) (Exp.recS P tl tn c) (subst1 c P)
                 (Exp.recS P tle tne ce)])
  (eLeaf [D (List Exp)] [us (List U)] [x Exp] [xe Exp]
         [h (Er chkf D us x Exp.tLbl xe)]
         :where [D us (Exp.leaf x) Exp.tR (Exp.leaf xe)])
  (eNode [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [us4 (List U)]
         [d Exp] [x Exp] [r1 Exp] [r2 Exp] [de Exp] [xe Exp] [r1e Exp] [r2e Exp]
         [hd (Er chkf D us1 d Exp.tDia de)]
         [hx (Er chkf D us2 x Exp.tLbl xe)]
         [h1 (Er chkf D us3 r1 Exp.tR r1e)]
         [h2 (Er chkf D us4 r2 Exp.tR r2e)]
         :where [D (vadd us1 (vadd us2 (vadd us3 us4))) (Exp.node d x r1 r2) Exp.tR
                 (Exp.node de xe r1e r2e)])
  (eItR [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [X Exp] [g Exp] [h Exp] [r Exp]
        [ge Exp] [he Exp] [re Exp]
        [hX (Tl chkf Bool.true D X Exp.tUnit)]
        [hg (Er chkf D (vscale U.uw us1) g (gTy X) ge)]
        [hh (Er chkf D (vscale U.uw us2) h (hTy X) he)]
        [hr (Er chkf D us3 r Exp.tR re)]
        :where [D (vadd (vscale U.uw us1) (vadd (vscale U.uw us2) us3)) (Exp.itR X g h r) X
                (Exp.itR X ge he re)])
  (ePrn [D (List Exp)] [us (List U)] [r Exp] [re Exp]
        [h (Er chkf D us r Exp.tR re)]
        :where [D us (Exp.prn r) Exp.tSyn (Exp.prn re)])
  (eChk [D (List Exp)] [us1 (List U)] [us2 (List U)] [c Exp] [d Exp] [ce Exp] [de Exp]
        [hc (Er chkf D us1 c Exp.tSyn ce)]
        [hd (Er chkf D us2 d Exp.tSyn de)]
        :where [D (vadd us1 us2) (Exp.chk c d) Exp.tBool (Exp.chk ce de)])
  (eH1 [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [us4 (List U)] [us5 (List U)]
       [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp]
       [re Exp] [se Exp] [ce Exp] [e1e Exp] [e2e Exp]
       [hr (Er chkf D us1 r Exp.tR re)]
       [hs (Er chkf D us2 s Exp.tR se)]
       [hc (Er chkf D (vscale U.uw us3) c Exp.tSyn ce)]
       [h1 (Er chkf D us4 e1 (chkT r c) e1e)]
       [h2 (Er chkf D us5 e2 (chkT s (negT c)) e2e)]
       :where [D (vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) (Exp.h1 r s c e1 e2) Exp.tEmpty
               (Exp.h1 re se ce e1e e2e)])
  ;; The reflect node itself is kept.  The decoded program is erased when
  ;; EvE runs the node, not when the node is erased.
  (eRefl [D (List Exp)] [us1 (List U)] [us2 (List U)] [X Exp] [cd Exp] [r Exp] [e Exp]
         [re Exp] [ee Exp]
         [hb (Eq (Option Exp) (baseCode X) (Option.some Exp cd))]
         [hr (Er chkf D us1 r Exp.tR re)]
         [he (Er chkf D us2 e (chkT r cd) ee)]
         :where [D (vadd us1 us2) (Exp.refl X r e) X (Exp.refl X re ee)])
  (eInsp [D (List Exp)] [us1 (List U)] [us0 (List U)] [us2 (List U)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
         [re Exp] [ce Exp] [t1e Exp] [t2e Exp]
         [hr (Er chkf D us1 r Exp.tR re)]
         [hc (Er chkf D (vscale U.uw us0) c Exp.tSyn ce)]
         [hX (Tl chkf Bool.true D X Exp.tUnit)]
         [hF1 (Tl chkf Bool.true (List.cons Exp Exp.tR D) (chkT (Exp.var 0) (lift 1 0 c)) Exp.tUnit)]
         [hF2 (Tl chkf Bool.true (List.cons Exp Exp.tR D) (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) Exp.tUnit)]
         [h1 (Er chkf (consE (chkT (Exp.var 0) (lift 1 0 c)) (consE Exp.tR D)) (consU U.u1 (consU U.u1 us2)) t1 (lift 2 0 X) t1e)]
         [h2 (Er chkf (consE (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) (consE Exp.tR D))
                  (consU U.u1 (consU U.u1 us2)) t2 (lift 2 0 X) t2e)]
         :where [D (vadd us1 (vadd (vscale U.uw us0) us2)) (Exp.insp X r c t1 t2) X
                 (Exp.insp X re ce t1e t2e)]))

;; --- usage skeletons: what E reads -------------------------------------------

;; USk is the skeleton of a type together with which binders are erased.
;; Base constructors carry an ordinary skeleton.  arr0 / prod0 are Π₀ / Σ₀;
;; arrN / prodN are usage 1 or ω.  Terms that are not types are base unit:
;; E is only applied to types.
(a/inductive USk []
  (base [s Sk])
  (arr0 [d USk] [c USk])
  (arrN [d USk] [c USk])
  (prod0 [d USk] [c USk])
  (prodN [d USk] [c USk]))

(a/defn uskSk [u :- USk] Sk
  (match u
    [(base s) s]
    [(arr0 d c) (Sk.arr (uskSk d) (uskSk c))]
    [(arrN d c) (Sk.arr (uskSk d) (uskSk c))]
    [(prod0 d c) (Sk.prod (uskSk d) (uskSk c))]
    [(prodN d c) (Sk.prod (uskSk d) (uskSk c))]))

;; usk A: the usage skeleton of a type.  T(b) ignores b, so E(T(b)) does not
;; depend on the term inside, and T(tt) and 1 (and T(ff) and 0) agree.
(a/defn usk [A :- Exp] USk
  (match A
    [tEmpty (USk.base Sk.unit)]
    [tUnit (USk.base Sk.unit)]
    [tBool (USk.base Sk.bool)]
    [tNat (USk.base Sk.nat)]
    [tLbl (USk.base Sk.lbl)]
    [tSyn (USk.base Sk.syn)]
    [tDia (USk.base Sk.dia)]
    [tR (USk.base Sk.cert)]
    [(tT b) (USk.base Sk.unit)]
    [(tPi r X Y)
     (match r
       [u0 (USk.arr0 (usk X) (usk Y))]
       [u1 (USk.arrN (usk X) (usk Y))]
       [uw (USk.arrN (usk X) (usk Y))])]
    [(tSig r X Y)
     (match r
       [u0 (USk.prod0 (usk X) (usk Y))]
       [u1 (USk.prodN (usk X) (usk Y))]
       [uw (USk.prodN (usk X) (usk Y))])]
    ;; a branch list denotes a function from labels; its usage is not 0
    [(tBrs P k) (USk.arrN (USk.base Sk.lbl) (usk P))]
    [_ (USk.base Sk.unit)]))

;; H is reflect at 0.  isH is that test, used by the trace predicates.
(a/defn isH [D :- Exp] Bool
  (match D [tEmpty Bool.true] [_ Bool.false]))

;; badNode t: the trace enters an abort, H₁ or H node when it evaluates t.
;; Reflect at a base type other than 0 is not H.
(a/defn badNode [t :- Exp] Bool
  (match t
    [(abort A u) Bool.true]
    [(h1 r s c e1 e2) Bool.true]
    [(refl D r e) (isH D)]
    [_ Bool.false]))

;; --- the erasing evaluator ----------------------------------------------------
;; EvE is evalₙ's relation, except that a successful reflect runs an erasure
;; of the decoded derivation (Er) under EvE at budget m.

(a/inductive EvE [chkf (=> Code Code Bool),
                 dec (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                 encTy (=> Exp Code)]
  :in Prop :indices [n Nat, src EvSrc, v RV]

  ;; variables and constants
  (eVar [n Nat] [rho (List RV)] [i Nat] [v RV]
    [h (Eq (Option RV) (rlookup rho i) (Option.some RV v))]
    :where [n (EvSrc.tm rho (Exp.var i)) v])
  (eStar [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.star) RV.star])
  (eTT [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.tt) (RV.bool Bool.true)])
  (eFF [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.ff) (RV.bool Bool.false)])
  (eZero [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.zero) (RV.nat 0)])
  (eLbl [n Nat] [rho (List RV)] [l Nat]
    :where [n (EvSrc.tm rho (Exp.lbl l)) (RV.lbl l)])

  ;; abort and H₁ evaluate their arguments, then return a default
  (eAbort [n Nat] [rho (List RV)] [A Exp] [t Exp] [vt RV]
    [ht (EvE chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.abort A t)) (rdflt (skel A))])
  (eH1 [n Nat] [rho (List RV)] [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp]
       [vr RV] [vs RV] [vc RV] [v1 RV] [v2 RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) vr)]
    [hs (EvE chkf dec encTy n (EvSrc.tm rho s) vs)]
    [hc (EvE chkf dec encTy n (EvSrc.tm rho c) vc)]
    [h1 (EvE chkf dec encTy n (EvSrc.tm rho e1) v1)]
    [h2 (EvE chkf dec encTy n (EvSrc.tm rho e2) v2)]
    :where [n (EvSrc.tm rho (Exp.h1 r s c e1 e2)) RV.star])

  ;; eliminators: the scrutinee selects one branch (both are not run)
  (eIteT [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (EvE chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eIteF [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (EvE chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eElimT [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (EvE chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])
  (eElimF [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (EvE chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])

  ;; numerals
  (eSucc [n Nat] [rho (List RV)] [m Exp] [k Nat]
    [hm (EvE chkf dec encTy n (EvSrc.tm rho m) (RV.nat k))]
    :where [n (EvSrc.tm rho (Exp.succ m)) (RV.nat (Nat.succ k))])
  ;; recN: the scrutinee is a numeral k; the step runs k times from the base.
  ;; y is variable 0 (the accumulator), x is variable 1 (the predecessor).
  (eRecN [n Nat] [rho (List RV)] [P Exp] [z Exp] [step Exp] [nv Exp]
         [k Nat] [z0 RV] [v RV]
    [hn (EvE chkf dec encTy n (EvSrc.tm rho nv) (RV.nat k))]
    [hz (EvE chkf dec encTy n (EvSrc.tm rho z) z0)]
    [hi (EvE chkf dec encTy n (EvSrc.iter rho step k z0) v)]
    :where [n (EvSrc.tm rho (Exp.recN P z step nv)) v])
  (eIterZ [n Nat] [rho (List RV)] [step Exp] [acc RV]
    :where [n (EvSrc.iter rho step 0 acc) acc])
  (eIterS [n Nat] [rho (List RV)] [step Exp] [k Nat] [acc RV] [mid RV] [out RV]
    [hp (EvE chkf dec encTy n (EvSrc.iter rho step k acc) mid)]
    [hs (EvE chkf dec encTy n
           (EvSrc.tm (List.cons RV mid (List.cons RV (RV.nat k) rho)) step) out)]
    :where [n (EvSrc.iter rho step (Nat.succ k) acc) out])

  ;; labels and branch lists
  (eCaseL [n Nat] [rho (List RV)] [P Exp] [a Exp] [bs Exp] [va RV] [vf RV] [w RV]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho bs) vf)]
    [hp (EvE chkf dec encTy n (EvSrc.ap vf va) w)]
    :where [n (EvSrc.tm rho (Exp.caseL P a bs)) w])
  (eBnil [n Nat] [rho (List RV)] [s Sk]
    :where [n (EvSrc.tm rho Exp.bnil) (RV.bnil s)])
  (eBcons [n Nat] [rho (List RV)] [h Exp] [t Exp] [vh RV] [vt RV]
    [hh (EvE chkf dec encTy n (EvSrc.tm rho h) vh)]
    [ht (EvE chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.bcons h t)) (RV.bcons vh vt)])

  ;; codes
  (eSleaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.sleaf a)) (RV.code (Code.sl l))])
  (eSnode [n Nat] [rho (List RV)] [a Exp] [c1 Exp] [c2 Exp] [l Nat] [x Code] [y Code]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (EvE chkf dec encTy n (EvSrc.tm rho c1) (RV.code x))]
    [h2 (EvE chkf dec encTy n (EvSrc.tm rho c2) (RV.code y))]
    :where [n (EvSrc.tm rho (Exp.snode a c1 c2)) (RV.code (Code.sn l x y))])
  (eRecS [n Nat] [rho (List RV)] [P Exp] [tl Exp] [tn Exp] [c Exp] [cv Code] [v RV]
    [hc (EvE chkf dec encTy n (EvSrc.tm rho c) (RV.code cv))]
    [hr (EvE chkf dec encTy n (EvSrc.recs rho tl tn cv) v)]
    :where [n (EvSrc.tm rho (Exp.recS P tl tn c)) v])
  ;; leaf method: variable 0 is the label.  Node method, innermost first:
  ;; yb, ya, the right code, the left code, the label.
  (eRecSL [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [v RV]
    [h (EvE chkf dec encTy n (EvSrc.tm (List.cons RV (RV.lbl l) rho) tl) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sl l)) v])
  (eRecSN [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [a Code] [b Code]
          [ya RV] [yb RV] [v RV]
    [ha (EvE chkf dec encTy n (EvSrc.recs rho tl tn a) ya)]
    [hb (EvE chkf dec encTy n (EvSrc.recs rho tl tn b) yb)]
    [hs (EvE chkf dec encTy n
           (EvSrc.tm (List.cons RV yb (List.cons RV ya
              (List.cons RV (RV.code b) (List.cons RV (RV.code a)
                (List.cons RV (RV.lbl l) rho))))) tn) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sn l a b)) v])

  ;; certificates.  The token of a node is evaluated and dropped.
  (eLeaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.leaf a)) (RV.cert (Code.sl l))])
  (eNode [n Nat] [rho (List RV)] [d Exp] [a Exp] [r1 Exp] [r2 Exp]
         [vd RV] [l Nat] [x Code] [y Code]
    [hd (EvE chkf dec encTy n (EvSrc.tm rho d) vd)]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (EvE chkf dec encTy n (EvSrc.tm rho r1) (RV.cert x))]
    [h2 (EvE chkf dec encTy n (EvSrc.tm rho r2) (RV.cert y))]
    :where [n (EvSrc.tm rho (Exp.node d a r1 r2)) (RV.cert (Code.sn l x y))])
  (eItR [n Nat] [rho (List RV)] [X Exp] [g Exp] [h Exp] [r Exp]
        [vg RV] [vh RV] [c Code] [w RV]
    [hg (EvE chkf dec encTy n (EvSrc.tm rho g) vg)]
    [hh (EvE chkf dec encTy n (EvSrc.tm rho h) vh)]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [hi (EvE chkf dec encTy n (EvSrc.itr vg vh c) w)]
    :where [n (EvSrc.tm rho (Exp.itR X g h r)) w])
  (eItL [n Nat] [g RV] [h RV] [l Nat] [w RV]
    [ha (EvE chkf dec encTy n (EvSrc.ap g (RV.lbl l)) w)]
    :where [n (EvSrc.itr g h (Code.sl l)) w])
  ;; h is applied to the token, the label, and the two recursive results.
  (eItN [n Nat] [g RV] [h RV] [l Nat] [a Code] [b Code] [ya RV] [yb RV]
        [v1 RV] [v2 RV] [v3 RV] [w RV]
    [ha (EvE chkf dec encTy n (EvSrc.itr g h a) ya)]
    [hb (EvE chkf dec encTy n (EvSrc.itr g h b) yb)]
    [h1 (EvE chkf dec encTy n (EvSrc.ap h RV.token) v1)]
    [h2 (EvE chkf dec encTy n (EvSrc.ap v1 (RV.lbl l)) v2)]
    [h3 (EvE chkf dec encTy n (EvSrc.ap v2 ya) v3)]
    [h4 (EvE chkf dec encTy n (EvSrc.ap v3 yb) w)]
    :where [n (EvSrc.itr g h (Code.sn l a b)) w])
  (ePrn [n Nat] [rho (List RV)] [r Exp] [c Code]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    :where [n (EvSrc.tm rho (Exp.prn r)) (RV.code c)])

  ;; functions and pairs.  λ stops at a closure; application is AppV.
  (eLam [n Nat] [rho (List RV)] [r U] [A Exp] [t Exp]
    :where [n (EvSrc.tm rho (Exp.lam r A t)) (RV.clos rho t)])
  (eApp [n Nat] [rho (List RV)] [f Exp] [u Exp] [vf RV] [vu RV] [w RV]
    [hf (EvE chkf dec encTy n (EvSrc.tm rho f) vf)]
    [hu (EvE chkf dec encTy n (EvSrc.tm rho u) vu)]
    [ha (EvE chkf dec encTy n (EvSrc.ap vf vu) w)]
    :where [n (EvSrc.tm rho (Exp.app f u)) w])
  (ePair [n Nat] [rho (List RV)] [S Exp] [a Exp] [b Exp] [va RV] [vb RV]
    [ha (EvE chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (EvE chkf dec encTy n (EvSrc.tm rho b) vb)]
    :where [n (EvSrc.tm rho (Exp.pair S a b)) (RV.pair va vb)])
  ;; y is variable 0 (the second component), x is variable 1.
  (eLet [n Nat] [rho (List RV)] [C Exp] [p Exp] [t Exp] [va RV] [vb RV] [w RV]
    [hp (EvE chkf dec encTy n (EvSrc.tm rho p) (RV.pair va vb))]
    [ht (EvE chkf dec encTy n
           (EvSrc.tm (List.cons RV vb (List.cons RV va rho)) t) w)]
    :where [n (EvSrc.tm rho (Exp.letp C p t)) w])

  ;; chk′ is chkf on the two codes
  (eChk [n Nat] [rho (List RV)] [c Exp] [d Exp] [cc Code] [dc Code]
    [hc (EvE chkf dec encTy n (EvSrc.tm rho c) (RV.code cc))]
    [hd (EvE chkf dec encTy n (EvSrc.tm rho d) (RV.code dc))]
    :where [n (EvSrc.tm rho (Exp.chk c d)) (RV.bool (chkf cc dc))])

  ;; reflect.  The success rule runs t′ at budget m on m tokens.  The two
  ;; failure rules are the model's Bool.rec / Option.rec defaults: the
  ;; conjunction of the cap test and chkf is false, or it is true and dec
  ;; returns none.  Both evaluate r and e first.
  ;; Success runs an erasure of the decoded judgment, not the raw term.
  ;; dec returns (m, t′, A); Er is the erasure of a derivation of that
  ;; judgment (CheckSpec supplies the derivation, er_total the erasure).
  (eReflOk [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code]
           [m Nat] [t2 Exp] [A Exp] [e2 Exp] [ve RV] [w RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (EvE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.some (Prod Nat (Prod Exp Exp))
              (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A))))]
    [her (Er chkf (thetaD m) (thetaU m) t2 A e2)]
    [ht (EvE chkf dec encTy m (EvSrc.tm (rtokens m) e2) w)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) w])
  (eReflNo [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (EvE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])
  (eReflNone [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (EvE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.none (Prod Nat (Prod Exp Exp))))]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])

  ;; inspect branches on chkf and rebinds the certificate and ⋆
  (eInspT [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (EvE chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.true)]
    [ht (EvE chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t1) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])
  (eInspF [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (EvE chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (EvE chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.false)]
    [ht (EvE chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t2) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])

  ;; application of a runtime value (the clause at an arrow)
  (apClos [n Nat] [rho (List RV)] [body Exp] [arg RV] [w RV]
    [h (EvE chkf dec encTy n (EvSrc.tm (List.cons RV arg rho) body) w)]
    :where [n (EvSrc.ap (RV.clos rho body) arg) w])
  (apBnil [n Nat] [s Sk] [arg RV]
    :where [n (EvSrc.ap (RV.bnil s) arg) (rdflt s)])
  (apBconsZ [n Nat] [h RV] [t RV]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl 0)) h])
  (apBconsS [n Nat] [h RV] [t RV] [k Nat] [w RV]
    [hp (EvE chkf dec encTy n (EvSrc.ap t (RV.lbl k)) w)]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl (Nat.succ k))) w]))


(kdef EvalE
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> Nat (List RV) Exp RV Prop))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, rho :- (List RV), t :- Exp, v :- RV]
    (EvE chkf dec encTy n (EvSrc.tm rho t) v)))

;; --- safe traces (Theorem 5.2) ------------------------------------------------
;; Ok is a trace of evalₙ that never enters abort, H₁ or H.  OkE is the same
;; for evalᴱ.  A value of either predicate is an evaluation derivation: the
;; constructors are the evaluator's rules with those nodes removed.

(a/inductive Ok [chkf (=> Code Code Bool),
                 dec (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                 encTy (=> Exp Code)]
  :in Prop :indices [n Nat, src EvSrc, v RV]

  ;; variables and constants
  (eVar [n Nat] [rho (List RV)] [i Nat] [v RV]
    [h (Eq (Option RV) (rlookup rho i) (Option.some RV v))]
    :where [n (EvSrc.tm rho (Exp.var i)) v])
  (eStar [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.star) RV.star])
  (eTT [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.tt) (RV.bool Bool.true)])
  (eFF [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.ff) (RV.bool Bool.false)])
  (eZero [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.zero) (RV.nat 0)])
  (eLbl [n Nat] [rho (List RV)] [l Nat]
    :where [n (EvSrc.tm rho (Exp.lbl l)) (RV.lbl l)])

  ;; No eAbort and no eH1: a safe trace does not enter those nodes.
  ;; Reflect at 0 (H) is refused by isH = false on each reflect rule.

  ;; eliminators: the scrutinee selects one branch (both are not run)
  (eIteT [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (Ok chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eIteF [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (Ok chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eElimT [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (Ok chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])
  (eElimF [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (Ok chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])

  ;; numerals
  (eSucc [n Nat] [rho (List RV)] [m Exp] [k Nat]
    [hm (Ok chkf dec encTy n (EvSrc.tm rho m) (RV.nat k))]
    :where [n (EvSrc.tm rho (Exp.succ m)) (RV.nat (Nat.succ k))])
  ;; recN: the scrutinee is a numeral k; the step runs k times from the base.
  ;; y is variable 0 (the accumulator), x is variable 1 (the predecessor).
  (eRecN [n Nat] [rho (List RV)] [P Exp] [z Exp] [step Exp] [nv Exp]
         [k Nat] [z0 RV] [v RV]
    [hn (Ok chkf dec encTy n (EvSrc.tm rho nv) (RV.nat k))]
    [hz (Ok chkf dec encTy n (EvSrc.tm rho z) z0)]
    [hi (Ok chkf dec encTy n (EvSrc.iter rho step k z0) v)]
    :where [n (EvSrc.tm rho (Exp.recN P z step nv)) v])
  (eIterZ [n Nat] [rho (List RV)] [step Exp] [acc RV]
    :where [n (EvSrc.iter rho step 0 acc) acc])
  (eIterS [n Nat] [rho (List RV)] [step Exp] [k Nat] [acc RV] [mid RV] [out RV]
    [hp (Ok chkf dec encTy n (EvSrc.iter rho step k acc) mid)]
    [hs (Ok chkf dec encTy n
           (EvSrc.tm (List.cons RV mid (List.cons RV (RV.nat k) rho)) step) out)]
    :where [n (EvSrc.iter rho step (Nat.succ k) acc) out])

  ;; labels and branch lists
  (eCaseL [n Nat] [rho (List RV)] [P Exp] [a Exp] [bs Exp] [va RV] [vf RV] [w RV]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho bs) vf)]
    [hp (Ok chkf dec encTy n (EvSrc.ap vf va) w)]
    :where [n (EvSrc.tm rho (Exp.caseL P a bs)) w])
  (eBnil [n Nat] [rho (List RV)] [s Sk]
    :where [n (EvSrc.tm rho Exp.bnil) (RV.bnil s)])
  (eBcons [n Nat] [rho (List RV)] [h Exp] [t Exp] [vh RV] [vt RV]
    [hh (Ok chkf dec encTy n (EvSrc.tm rho h) vh)]
    [ht (Ok chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.bcons h t)) (RV.bcons vh vt)])

  ;; codes
  (eSleaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.sleaf a)) (RV.code (Code.sl l))])
  (eSnode [n Nat] [rho (List RV)] [a Exp] [c1 Exp] [c2 Exp] [l Nat] [x Code] [y Code]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (Ok chkf dec encTy n (EvSrc.tm rho c1) (RV.code x))]
    [h2 (Ok chkf dec encTy n (EvSrc.tm rho c2) (RV.code y))]
    :where [n (EvSrc.tm rho (Exp.snode a c1 c2)) (RV.code (Code.sn l x y))])
  (eRecS [n Nat] [rho (List RV)] [P Exp] [tl Exp] [tn Exp] [c Exp] [cv Code] [v RV]
    [hc (Ok chkf dec encTy n (EvSrc.tm rho c) (RV.code cv))]
    [hr (Ok chkf dec encTy n (EvSrc.recs rho tl tn cv) v)]
    :where [n (EvSrc.tm rho (Exp.recS P tl tn c)) v])
  ;; leaf method: variable 0 is the label.  Node method, innermost first:
  ;; yb, ya, the right code, the left code, the label.
  (eRecSL [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [v RV]
    [h (Ok chkf dec encTy n (EvSrc.tm (List.cons RV (RV.lbl l) rho) tl) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sl l)) v])
  (eRecSN [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [a Code] [b Code]
          [ya RV] [yb RV] [v RV]
    [ha (Ok chkf dec encTy n (EvSrc.recs rho tl tn a) ya)]
    [hb (Ok chkf dec encTy n (EvSrc.recs rho tl tn b) yb)]
    [hs (Ok chkf dec encTy n
           (EvSrc.tm (List.cons RV yb (List.cons RV ya
              (List.cons RV (RV.code b) (List.cons RV (RV.code a)
                (List.cons RV (RV.lbl l) rho))))) tn) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sn l a b)) v])

  ;; certificates.  The token of a node is evaluated and dropped.
  (eLeaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.leaf a)) (RV.cert (Code.sl l))])
  (eNode [n Nat] [rho (List RV)] [d Exp] [a Exp] [r1 Exp] [r2 Exp]
         [vd RV] [l Nat] [x Code] [y Code]
    [hd (Ok chkf dec encTy n (EvSrc.tm rho d) vd)]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (Ok chkf dec encTy n (EvSrc.tm rho r1) (RV.cert x))]
    [h2 (Ok chkf dec encTy n (EvSrc.tm rho r2) (RV.cert y))]
    :where [n (EvSrc.tm rho (Exp.node d a r1 r2)) (RV.cert (Code.sn l x y))])
  (eItR [n Nat] [rho (List RV)] [X Exp] [g Exp] [h Exp] [r Exp]
        [vg RV] [vh RV] [c Code] [w RV]
    [hg (Ok chkf dec encTy n (EvSrc.tm rho g) vg)]
    [hh (Ok chkf dec encTy n (EvSrc.tm rho h) vh)]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [hi (Ok chkf dec encTy n (EvSrc.itr vg vh c) w)]
    :where [n (EvSrc.tm rho (Exp.itR X g h r)) w])
  (eItL [n Nat] [g RV] [h RV] [l Nat] [w RV]
    [ha (Ok chkf dec encTy n (EvSrc.ap g (RV.lbl l)) w)]
    :where [n (EvSrc.itr g h (Code.sl l)) w])
  ;; h is applied to the token, the label, and the two recursive results.
  (eItN [n Nat] [g RV] [h RV] [l Nat] [a Code] [b Code] [ya RV] [yb RV]
        [v1 RV] [v2 RV] [v3 RV] [w RV]
    [ha (Ok chkf dec encTy n (EvSrc.itr g h a) ya)]
    [hb (Ok chkf dec encTy n (EvSrc.itr g h b) yb)]
    [h1 (Ok chkf dec encTy n (EvSrc.ap h RV.token) v1)]
    [h2 (Ok chkf dec encTy n (EvSrc.ap v1 (RV.lbl l)) v2)]
    [h3 (Ok chkf dec encTy n (EvSrc.ap v2 ya) v3)]
    [h4 (Ok chkf dec encTy n (EvSrc.ap v3 yb) w)]
    :where [n (EvSrc.itr g h (Code.sn l a b)) w])
  (ePrn [n Nat] [rho (List RV)] [r Exp] [c Code]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    :where [n (EvSrc.tm rho (Exp.prn r)) (RV.code c)])

  ;; functions and pairs.  λ stops at a closure; application is AppV.
  (eLam [n Nat] [rho (List RV)] [r U] [A Exp] [t Exp]
    :where [n (EvSrc.tm rho (Exp.lam r A t)) (RV.clos rho t)])
  (eApp [n Nat] [rho (List RV)] [f Exp] [u Exp] [vf RV] [vu RV] [w RV]
    [hf (Ok chkf dec encTy n (EvSrc.tm rho f) vf)]
    [hu (Ok chkf dec encTy n (EvSrc.tm rho u) vu)]
    [ha (Ok chkf dec encTy n (EvSrc.ap vf vu) w)]
    :where [n (EvSrc.tm rho (Exp.app f u)) w])
  (ePair [n Nat] [rho (List RV)] [S Exp] [a Exp] [b Exp] [va RV] [vb RV]
    [ha (Ok chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (Ok chkf dec encTy n (EvSrc.tm rho b) vb)]
    :where [n (EvSrc.tm rho (Exp.pair S a b)) (RV.pair va vb)])
  ;; y is variable 0 (the second component), x is variable 1.
  (eLet [n Nat] [rho (List RV)] [C Exp] [p Exp] [t Exp] [va RV] [vb RV] [w RV]
    [hp (Ok chkf dec encTy n (EvSrc.tm rho p) (RV.pair va vb))]
    [ht (Ok chkf dec encTy n
           (EvSrc.tm (List.cons RV vb (List.cons RV va rho)) t) w)]
    :where [n (EvSrc.tm rho (Exp.letp C p t)) w])

  ;; chk′ is chkf on the two codes
  (eChk [n Nat] [rho (List RV)] [c Exp] [d Exp] [cc Code] [dc Code]
    [hc (Ok chkf dec encTy n (EvSrc.tm rho c) (RV.code cc))]
    [hd (Ok chkf dec encTy n (EvSrc.tm rho d) (RV.code dc))]
    :where [n (EvSrc.tm rho (Exp.chk c d)) (RV.bool (chkf cc dc))])

  ;; reflect.  The success rule runs t′ at budget m on m tokens.  The two
  ;; failure rules are the model's Bool.rec / Option.rec defaults: the
  ;; conjunction of the cap test and chkf is false, or it is true and dec
  ;; returns none.  Both evaluate r and e first.
  (eReflOk [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code]
           [m Nat] [t2 Exp] [A Exp] [ve RV] [w RV]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ok chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.some (Prod Nat (Prod Exp Exp))
              (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A))))]
    [ht (Ok chkf dec encTy m (EvSrc.tm (rtokens m) t2) w)]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) w])
  (eReflNo [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ok chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.false)]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])
  (eReflNone [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (Ok chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.none (Prod Nat (Prod Exp Exp))))]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])

  ;; inspect branches on chkf and rebinds the certificate and ⋆
  (eInspT [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (Ok chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.true)]
    [ht (Ok chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t1) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])
  (eInspF [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (Ok chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (Ok chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.false)]
    [ht (Ok chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t2) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])

  ;; application of a runtime value (the clause at an arrow)
  (apClos [n Nat] [rho (List RV)] [body Exp] [arg RV] [w RV]
    [h (Ok chkf dec encTy n (EvSrc.tm (List.cons RV arg rho) body) w)]
    :where [n (EvSrc.ap (RV.clos rho body) arg) w])
  (apBnil [n Nat] [s Sk] [arg RV]
    :where [n (EvSrc.ap (RV.bnil s) arg) (rdflt s)])
  (apBconsZ [n Nat] [h RV] [t RV]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl 0)) h])
  (apBconsS [n Nat] [h RV] [t RV] [k Nat] [w RV]
    [hp (Ok chkf dec encTy n (EvSrc.ap t (RV.lbl k)) w)]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl (Nat.succ k))) w]))


(a/inductive OkE [chkf (=> Code Code Bool),
                 dec (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                 encTy (=> Exp Code)]
  :in Prop :indices [n Nat, src EvSrc, v RV]

  ;; variables and constants
  (eVar [n Nat] [rho (List RV)] [i Nat] [v RV]
    [h (Eq (Option RV) (rlookup rho i) (Option.some RV v))]
    :where [n (EvSrc.tm rho (Exp.var i)) v])
  (eStar [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.star) RV.star])
  (eTT [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.tt) (RV.bool Bool.true)])
  (eFF [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.ff) (RV.bool Bool.false)])
  (eZero [n Nat] [rho (List RV)]
    :where [n (EvSrc.tm rho Exp.zero) (RV.nat 0)])
  (eLbl [n Nat] [rho (List RV)] [l Nat]
    :where [n (EvSrc.tm rho (Exp.lbl l)) (RV.lbl l)])

  ;; Safe traces of the erasing evaluator: no abort, no H₁.
  ;; Reflect at 0 is H and is refused; a successful reflect runs an erasure.

  ;; eliminators: the scrutinee selects one branch (both are not run)
  (eIteT [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (OkE chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eIteF [n Nat] [rho (List RV)] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (OkE chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.ite b t e)) v])
  (eElimT [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.true))]
    [ht (OkE chkf dec encTy n (EvSrc.tm rho t) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])
  (eElimF [n Nat] [rho (List RV)] [P Exp] [b Exp] [t Exp] [e Exp] [v RV]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho b) (RV.bool Bool.false))]
    [he (OkE chkf dec encTy n (EvSrc.tm rho e) v)]
    :where [n (EvSrc.tm rho (Exp.elimB P b t e)) v])

  ;; numerals
  (eSucc [n Nat] [rho (List RV)] [m Exp] [k Nat]
    [hm (OkE chkf dec encTy n (EvSrc.tm rho m) (RV.nat k))]
    :where [n (EvSrc.tm rho (Exp.succ m)) (RV.nat (Nat.succ k))])
  ;; recN: the scrutinee is a numeral k; the step runs k times from the base.
  ;; y is variable 0 (the accumulator), x is variable 1 (the predecessor).
  (eRecN [n Nat] [rho (List RV)] [P Exp] [z Exp] [step Exp] [nv Exp]
         [k Nat] [z0 RV] [v RV]
    [hn (OkE chkf dec encTy n (EvSrc.tm rho nv) (RV.nat k))]
    [hz (OkE chkf dec encTy n (EvSrc.tm rho z) z0)]
    [hi (OkE chkf dec encTy n (EvSrc.iter rho step k z0) v)]
    :where [n (EvSrc.tm rho (Exp.recN P z step nv)) v])
  (eIterZ [n Nat] [rho (List RV)] [step Exp] [acc RV]
    :where [n (EvSrc.iter rho step 0 acc) acc])
  (eIterS [n Nat] [rho (List RV)] [step Exp] [k Nat] [acc RV] [mid RV] [out RV]
    [hp (OkE chkf dec encTy n (EvSrc.iter rho step k acc) mid)]
    [hs (OkE chkf dec encTy n
           (EvSrc.tm (List.cons RV mid (List.cons RV (RV.nat k) rho)) step) out)]
    :where [n (EvSrc.iter rho step (Nat.succ k) acc) out])

  ;; labels and branch lists
  (eCaseL [n Nat] [rho (List RV)] [P Exp] [a Exp] [bs Exp] [va RV] [vf RV] [w RV]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho bs) vf)]
    [hp (OkE chkf dec encTy n (EvSrc.ap vf va) w)]
    :where [n (EvSrc.tm rho (Exp.caseL P a bs)) w])
  (eBnil [n Nat] [rho (List RV)] [s Sk]
    :where [n (EvSrc.tm rho Exp.bnil) (RV.bnil s)])
  (eBcons [n Nat] [rho (List RV)] [h Exp] [t Exp] [vh RV] [vt RV]
    [hh (OkE chkf dec encTy n (EvSrc.tm rho h) vh)]
    [ht (OkE chkf dec encTy n (EvSrc.tm rho t) vt)]
    :where [n (EvSrc.tm rho (Exp.bcons h t)) (RV.bcons vh vt)])

  ;; codes
  (eSleaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.sleaf a)) (RV.code (Code.sl l))])
  (eSnode [n Nat] [rho (List RV)] [a Exp] [c1 Exp] [c2 Exp] [l Nat] [x Code] [y Code]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (OkE chkf dec encTy n (EvSrc.tm rho c1) (RV.code x))]
    [h2 (OkE chkf dec encTy n (EvSrc.tm rho c2) (RV.code y))]
    :where [n (EvSrc.tm rho (Exp.snode a c1 c2)) (RV.code (Code.sn l x y))])
  (eRecS [n Nat] [rho (List RV)] [P Exp] [tl Exp] [tn Exp] [c Exp] [cv Code] [v RV]
    [hc (OkE chkf dec encTy n (EvSrc.tm rho c) (RV.code cv))]
    [hr (OkE chkf dec encTy n (EvSrc.recs rho tl tn cv) v)]
    :where [n (EvSrc.tm rho (Exp.recS P tl tn c)) v])
  ;; leaf method: variable 0 is the label.  Node method, innermost first:
  ;; yb, ya, the right code, the left code, the label.
  (eRecSL [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [v RV]
    [h (OkE chkf dec encTy n (EvSrc.tm (List.cons RV (RV.lbl l) rho) tl) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sl l)) v])
  (eRecSN [n Nat] [rho (List RV)] [tl Exp] [tn Exp] [l Nat] [a Code] [b Code]
          [ya RV] [yb RV] [v RV]
    [ha (OkE chkf dec encTy n (EvSrc.recs rho tl tn a) ya)]
    [hb (OkE chkf dec encTy n (EvSrc.recs rho tl tn b) yb)]
    [hs (OkE chkf dec encTy n
           (EvSrc.tm (List.cons RV yb (List.cons RV ya
              (List.cons RV (RV.code b) (List.cons RV (RV.code a)
                (List.cons RV (RV.lbl l) rho))))) tn) v)]
    :where [n (EvSrc.recs rho tl tn (Code.sn l a b)) v])

  ;; certificates.  The token of a node is evaluated and dropped.
  (eLeaf [n Nat] [rho (List RV)] [a Exp] [l Nat]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    :where [n (EvSrc.tm rho (Exp.leaf a)) (RV.cert (Code.sl l))])
  (eNode [n Nat] [rho (List RV)] [d Exp] [a Exp] [r1 Exp] [r2 Exp]
         [vd RV] [l Nat] [x Code] [y Code]
    [hd (OkE chkf dec encTy n (EvSrc.tm rho d) vd)]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) (RV.lbl l))]
    [h1 (OkE chkf dec encTy n (EvSrc.tm rho r1) (RV.cert x))]
    [h2 (OkE chkf dec encTy n (EvSrc.tm rho r2) (RV.cert y))]
    :where [n (EvSrc.tm rho (Exp.node d a r1 r2)) (RV.cert (Code.sn l x y))])
  (eItR [n Nat] [rho (List RV)] [X Exp] [g Exp] [h Exp] [r Exp]
        [vg RV] [vh RV] [c Code] [w RV]
    [hg (OkE chkf dec encTy n (EvSrc.tm rho g) vg)]
    [hh (OkE chkf dec encTy n (EvSrc.tm rho h) vh)]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [hi (OkE chkf dec encTy n (EvSrc.itr vg vh c) w)]
    :where [n (EvSrc.tm rho (Exp.itR X g h r)) w])
  (eItL [n Nat] [g RV] [h RV] [l Nat] [w RV]
    [ha (OkE chkf dec encTy n (EvSrc.ap g (RV.lbl l)) w)]
    :where [n (EvSrc.itr g h (Code.sl l)) w])
  ;; h is applied to the token, the label, and the two recursive results.
  (eItN [n Nat] [g RV] [h RV] [l Nat] [a Code] [b Code] [ya RV] [yb RV]
        [v1 RV] [v2 RV] [v3 RV] [w RV]
    [ha (OkE chkf dec encTy n (EvSrc.itr g h a) ya)]
    [hb (OkE chkf dec encTy n (EvSrc.itr g h b) yb)]
    [h1 (OkE chkf dec encTy n (EvSrc.ap h RV.token) v1)]
    [h2 (OkE chkf dec encTy n (EvSrc.ap v1 (RV.lbl l)) v2)]
    [h3 (OkE chkf dec encTy n (EvSrc.ap v2 ya) v3)]
    [h4 (OkE chkf dec encTy n (EvSrc.ap v3 yb) w)]
    :where [n (EvSrc.itr g h (Code.sn l a b)) w])
  (ePrn [n Nat] [rho (List RV)] [r Exp] [c Code]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    :where [n (EvSrc.tm rho (Exp.prn r)) (RV.code c)])

  ;; functions and pairs.  λ stops at a closure; application is AppV.
  (eLam [n Nat] [rho (List RV)] [r U] [A Exp] [t Exp]
    :where [n (EvSrc.tm rho (Exp.lam r A t)) (RV.clos rho t)])
  (eApp [n Nat] [rho (List RV)] [f Exp] [u Exp] [vf RV] [vu RV] [w RV]
    [hf (OkE chkf dec encTy n (EvSrc.tm rho f) vf)]
    [hu (OkE chkf dec encTy n (EvSrc.tm rho u) vu)]
    [ha (OkE chkf dec encTy n (EvSrc.ap vf vu) w)]
    :where [n (EvSrc.tm rho (Exp.app f u)) w])
  (ePair [n Nat] [rho (List RV)] [S Exp] [a Exp] [b Exp] [va RV] [vb RV]
    [ha (OkE chkf dec encTy n (EvSrc.tm rho a) va)]
    [hb (OkE chkf dec encTy n (EvSrc.tm rho b) vb)]
    :where [n (EvSrc.tm rho (Exp.pair S a b)) (RV.pair va vb)])
  ;; y is variable 0 (the second component), x is variable 1.
  (eLet [n Nat] [rho (List RV)] [C Exp] [p Exp] [t Exp] [va RV] [vb RV] [w RV]
    [hp (OkE chkf dec encTy n (EvSrc.tm rho p) (RV.pair va vb))]
    [ht (OkE chkf dec encTy n
           (EvSrc.tm (List.cons RV vb (List.cons RV va rho)) t) w)]
    :where [n (EvSrc.tm rho (Exp.letp C p t)) w])

  ;; chk′ is chkf on the two codes
  (eChk [n Nat] [rho (List RV)] [c Exp] [d Exp] [cc Code] [dc Code]
    [hc (OkE chkf dec encTy n (EvSrc.tm rho c) (RV.code cc))]
    [hd (OkE chkf dec encTy n (EvSrc.tm rho d) (RV.code dc))]
    :where [n (EvSrc.tm rho (Exp.chk c d)) (RV.bool (chkf cc dc))])

  ;; reflect.  The success rule runs t′ at budget m on m tokens.  The two
  ;; failure rules are the model's Bool.rec / Option.rec defaults: the
  ;; conjunction of the cap test and chkf is false, or it is true and dec
  ;; returns none.  Both evaluate r and e first.
  ;; Success runs an erasure of the decoded judgment, not the raw term.
  ;; dec returns (m, t′, A); Er is the erasure of a derivation of that
  ;; judgment (CheckSpec supplies the derivation, er_total the erasure).
  (eReflOk [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code]
           [m Nat] [t2 Exp] [A Exp] [e2 Exp] [ve RV] [w RV]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (OkE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.some (Prod Nat (Prod Exp Exp))
              (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A))))]
    [her (Er chkf (thetaD m) (thetaU m) t2 A e2)]
    [ht (OkE chkf dec encTy m (EvSrc.tm (rtokens m) e2) w)]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) w])
  (eReflNo [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (OkE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.false)]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])
  (eReflNone [n Nat] [rho (List RV)] [D Exp] [r Exp] [e Exp] [c Code] [ve RV]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert c))]
    [he (OkE chkf dec encTy n (EvSrc.tm rho e) ve)]
    [hb (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true)]
    [hd (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
            (Option.none (Prod Nat (Prod Exp Exp))))]
        [hH (Eq Bool (isH D) Bool.false)]
    :where [n (EvSrc.tm rho (Exp.refl D r e)) (rdflt (skel D))])

  ;; inspect branches on chkf and rebinds the certificate and ⋆
  (eInspT [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (OkE chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.true)]
    [ht (OkE chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t1) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])
  (eInspF [n Nat] [rho (List RV)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
          [cv Code] [dv Code] [w RV]
    [hr (OkE chkf dec encTy n (EvSrc.tm rho r) (RV.cert cv))]
    [hc (OkE chkf dec encTy n (EvSrc.tm rho c) (RV.code dv))]
    [hb (Eq Bool (chkf cv dv) Bool.false)]
    [ht (OkE chkf dec encTy n
           (EvSrc.tm (List.cons RV RV.star (List.cons RV (RV.cert cv) rho)) t2) w)]
    :where [n (EvSrc.tm rho (Exp.insp X r c t1 t2)) w])

  ;; application of a runtime value (the clause at an arrow)
  (apClos [n Nat] [rho (List RV)] [body Exp] [arg RV] [w RV]
    [h (OkE chkf dec encTy n (EvSrc.tm (List.cons RV arg rho) body) w)]
    :where [n (EvSrc.ap (RV.clos rho body) arg) w])
  (apBnil [n Nat] [s Sk] [arg RV]
    :where [n (EvSrc.ap (RV.bnil s) arg) (rdflt s)])
  (apBconsZ [n Nat] [h RV] [t RV]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl 0)) h])
  (apBconsS [n Nat] [h RV] [t RV] [k Nat] [w RV]
    [hp (OkE chkf dec encTy n (EvSrc.ap t (RV.lbl k)) w)]
    :where [n (EvSrc.ap (RV.bcons h t) (RV.lbl (Nat.succ k))) w]))


;; ebase s v α: E at a base skeleton.  Equality, as in the paper: a runtime
;; token at ◇, the same Code at Syn and at R, ⋆ at unit (which is 0, 1 and
;; every T(b)).  An arrow or a product is not a base of usk; those clauses
;; are False so a stray USk.base of an arrow is not related.
(kdef ebase (forall [s Sk] (=> RV (Car s) Prop))
  (fn [s :- Sk]
    (Sk.rec$1 (fn [t :- Sk] (=> RV (Car t) Prop))
      (fn [v :- RV, _a :- Unit] (Eq RV v RV.star))
      (fn [v :- RV, a :- Bool] (Eq RV v (RV.bool a)))
      (fn [v :- RV, a :- Nat] (Eq RV v (RV.nat a)))
      (fn [v :- RV, a :- Nat] (Eq RV v (RV.lbl a)))
      (fn [v :- RV, a :- Code] (Eq RV v (RV.code a)))
      (fn [v :- RV, _a :- Unit] (Eq RV v RV.token))
      (fn [v :- RV, a :- Code] (Eq RV v (RV.cert a)))
      (fn [x :- Sk, y :- Sk, _rx :- (=> RV (Car x) Prop), _ry :- (=> RV (Car y) Prop)]
        (fn [_v :- RV, _f :- (Car (Sk.arr x y))] False))
      (fn [x :- Sk, y :- Sk, _rx :- (=> RV (Car x) Prop), _ry :- (=> RV (Car y) Prop)]
        (fn [_v :- RV, _p :- (Car (Sk.prod x y))] False))
      s)))

;; Erel n u v α: the paper's E at usage skeleton u, at budget n.  The
;; application clauses are EvE, so a closure's trace is an erasing evaluation.
(kdef Erel
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [u USk] (=> RV (Car (uskSk u)) Prop))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, u :- USk]
    (USk.rec$1 (fn [w :- USk] (=> RV (Car (uskSk w)) Prop))
      (fn [s :- Sk]
        (fn [v :- RV, a :- (Car (uskSk (USk.base s)))] (ebase s v a)))
      (fn [d :- USk, c :- USk, _rd :- (=> RV (Car (uskSk d)) Prop), rc :- (=> RV (Car (uskSk c)) Prop)]
        (fn [v :- RV, f :- (Car (uskSk (USk.arr0 d c)))]
          (forall [alpha (Car (uskSk d))]
            (Exists (fn [w :- RV]
              (And (EvE chkf dec encTy n (EvSrc.ap v RV.star) w)
                   (rc w (f alpha))))))))
      (fn [d :- USk, c :- USk, rd :- (=> RV (Car (uskSk d)) Prop), rc :- (=> RV (Car (uskSk c)) Prop)]
        (fn [v :- RV, f :- (Car (uskSk (USk.arrN d c)))]
          (forall [arg RV] (forall [alpha (Car (uskSk d))]
            (=> (rd arg alpha)
              (Exists (fn [w :- RV]
                (And (EvE chkf dec encTy n (EvSrc.ap v arg) w)
                     (rc w (f alpha))))))))))
      ;; Σ₀ forgets the first component: any runtime a is related, and the
      ;; second component is E-related to the carrier's second.  The paper
      ;; writes the pair as (⋆, b), which is what erasure produces, but the
      ;; runtime default of a product is (dflt σ, dflt τ) (rdflt), not
      ;; (⋆, dflt τ).  abort of Σ(y :₀ Bool). 1 therefore returns (ff, ⋆).
      ;; Requiring the first component to be ⋆ would make that default
      ;; unrelated, and the defaults paragraph of Theorem 4′ would fail.
      ;; Forgetting it is the reading on which defaults are related.
      (fn [d :- USk, c :- USk, _rd :- (=> RV (Car (uskSk d)) Prop), rc :- (=> RV (Car (uskSk c)) Prop)]
        (fn [v :- RV, p :- (Car (uskSk (USk.prod0 d c)))]
          (Exists (fn [a :- RV] (Exists (fn [b :- RV]
            (And (Eq RV v (RV.pair a b))
                 (rc b (Prod.snd p)))))))))
      (fn [d :- USk, c :- USk, rd :- (=> RV (Car (uskSk d)) Prop), rc :- (=> RV (Car (uskSk c)) Prop)]
        (fn [v :- RV, p :- (Car (uskSk (USk.prodN d c)))]
          (Exists (fn [a :- RV] (Exists (fn [b :- RV]
            (And (Eq RV v (RV.pair a b))
              (And (rd a (Prod.fst p)) (rc b (Prod.snd p))))))))))
      u)))

;; --- checked equations of erasure, E's base, and the trace root ----------

(thm bad_abort [A :- Exp, t :- Exp]
  (Eq Bool (badNode (Exp.abort A t)) Bool.true)
  (rfl))

(thm bad_h1 [r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp]
  (Eq Bool (badNode (Exp.h1 r s c e1 e2)) Bool.true)
  (rfl))

(thm bad_H [r :- Exp, e :- Exp]
  (Eq Bool (badNode (Exp.refl Exp.tEmpty r e)) Bool.true)
  (rfl))

(thm bad_refl_unit [r :- Exp, e :- Exp]
  (Eq Bool (badNode (Exp.refl Exp.tUnit r e)) Bool.false)
  (rfl))

(thm bad_star []
  (Eq Bool (badNode Exp.star) Bool.false)
  (rfl))

(thm isH_empty []
  (Eq Bool (isH Exp.tEmpty) Bool.true)
  (rfl))

(thm isH_unit []
  (Eq Bool (isH Exp.tUnit) Bool.false)
  (rfl))

(thm usk_empty []
  (Eq USk (usk Exp.tEmpty) (USk.base Sk.unit))
  (rfl))

(thm usk_unit []
  (Eq USk (usk Exp.tUnit) (USk.base Sk.unit))
  (rfl))

(thm usk_bool []
  (Eq USk (usk Exp.tBool) (USk.base Sk.bool))
  (rfl))

(thm usk_t [b :- Exp]
  (Eq USk (usk (Exp.tT b)) (USk.base Sk.unit))
  (rfl))

(thm usk_pi0 [X :- Exp, Y :- Exp]
  (Eq USk (usk (Exp.tPi U.u0 X Y)) (USk.arr0 (usk X) (usk Y)))
  (rfl))

(thm usk_pi1 [X :- Exp, Y :- Exp]
  (Eq USk (usk (Exp.tPi U.u1 X Y)) (USk.arrN (usk X) (usk Y)))
  (rfl))

(thm usk_sig0 [X :- Exp, Y :- Exp]
  (Eq USk (usk (Exp.tSig U.u0 X Y)) (USk.prod0 (usk X) (usk Y)))
  (rfl))

(thm usk_brs [P :- Exp, k :- Nat]
  (Eq USk (usk (Exp.tBrs P k)) (USk.arrN (USk.base Sk.lbl) (usk P)))
  (rfl))

(thm ebase_unit [v :- RV]
  (Eq Prop (ebase Sk.unit v Unit.unit) (Eq RV v RV.star))
  (rfl))

(thm eve_star
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, rho :- (List RV)]
  (EvE chkf dec encTy n (EvSrc.tm rho Exp.star) RV.star)
  (exact (EvE.eStar chkf dec encTy n rho)))

(thm ok_star
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, rho :- (List RV)]
  (Ok chkf dec encTy n (EvSrc.tm rho Exp.star) RV.star)
  (exact (Ok.eStar chkf dec encTy n rho)))

;; Π₀ erasure replaces the argument by ⋆ (Theorem 4′, the definition).
(thm er_app0_shape
  [chkf :- (=> Code Code Bool),
   D :- (List Exp), us :- (List U), f :- Exp, u :- Exp, A :- Exp, B :- Exp, fe :- Exp,
   hf :- (Er chkf D us f (Exp.tPi U.u0 A B) fe),
   hu :- (Tl chkf Bool.false D u A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
  (Er chkf D us (Exp.app f u) (subst1 u B) (Exp.app fe Exp.star))
  (exact (Er.eApp0 chkf D us f u A B fe hf hu hA hB)))

;; Σ₀ erasure replaces the first component by ⋆.
(thm er_pair0_shape
  [chkf :- (=> Code Code Bool),
   D :- (List Exp), us :- (List U), A :- Exp, B :- Exp, x :- Exp, y :- Exp, ye :- Exp,
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   hx :- (Tl chkf Bool.false D x A),
   hy :- (Er chkf D us y (subst1 x B) ye)]
  (Er chkf D us (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B)
      (Exp.pair (Exp.tSig U.u0 A B) Exp.star ye))
  (exact (Er.ePair0 chkf D us A B x y ye hA hB hx hy)))

;; Every runtime derivation erases.  Er is therefore a function of the
;; derivation.  The erased term is built by the same rule that typed it:
;; Π₀ arguments and Σ₀ first components become ⋆, and every runtime premise
;; is erased.
(thm er_total
  [chkf :- (=> Code Code Bool),
   D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp,
   der :- (Rt chkf D0 us0 t0 A0)]
  (Exists (fn [e :- Exp] (Er chkf D0 us0 t0 A0 e)))
  (induction der)
  (constructor) (exact (Exp.var i)) (exact (Er.eVar chkf D us i A r hl hA hu hr))
  (constructor) (exact t) (exact (Er.eConst chkf D us t A hl h))
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (constructor) (exact (Exp.lam r A te)) (exact (Er.eLam chkf D us r A t B te hA hte))
  (refine' (exT Exp _ _ ih_hf _)) (intro fe hfe)
  (constructor) (exact (Exp.app fe Exp.star)) (exact (Er.eApp0 chkf D us f u A B fe hfe hu hA hB))
  (refine' (exT Exp _ _ ih_hf _)) (intro fe hfe)
  (refine' (exT Exp _ _ ih_hu _)) (intro ue hue)
  (constructor) (exact (Exp.app fe ue)) (exact (Er.eApp chkf D us1 us2 r f u A B fe ue hr hfe hue hA hB))
  (refine' (exT Exp _ _ ih_hy _)) (intro ye hye)
  (constructor) (exact (Exp.pair (Exp.tSig U.u0 A B) Exp.star ye))
  (exact (Er.ePair0 chkf D us A B x y ye hA hB hx hye))
  (refine' (exT Exp _ _ ih_hx _)) (intro xe hxe)
  (refine' (exT Exp _ _ ih_hy _)) (intro ye hye)
  (constructor) (exact (Exp.pair (Exp.tSig r A B) xe ye))
  (exact (Er.ePair chkf D us1 us2 r A B x y xe ye hr hA hB hxe hye))
  (refine' (exT Exp _ _ ih_hp _)) (intro pe hpe)
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (constructor) (exact (Exp.letp C pe te))
  (exact (Er.eLet chkf D us1 us2 r A B C p t pe te hpe hC hA hB hte))
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (constructor) (exact (Exp.abort A te)) (exact (Er.eAbort chkf D us A t te hte hA))
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (constructor) (exact te) (exact (Er.eConv chkf D us t A B te hte hB hc))
  (refine' (exT Exp _ _ ih_hb _)) (intro be hbe)
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (refine' (exT Exp _ _ ih_he _)) (intro ee hee)
  (constructor) (exact (Exp.ite be te ee)) (exact (Er.eIte chkf D us1 us2 b t e C be te ee hbe hte hee))
  (refine' (exT Exp _ _ ih_hb _)) (intro be hbe)
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (refine' (exT Exp _ _ ih_he _)) (intro ee hee)
  (constructor) (exact (Exp.elimB P be te ee))
  (exact (Er.eElimB chkf D us1 us2 P b t e be te ee hbe hP hte hee))
  (refine' (exT Exp _ _ ih_h _)) (intro ne hne)
  (constructor) (exact (Exp.succ ne)) (exact (Er.eSucc chkf D us n ne hne))
  (refine' (exT Exp _ _ ih_hn _)) (intro ne hne)
  (refine' (exT Exp _ _ ih_hz _)) (intro ze hze)
  (refine' (exT Exp _ _ ih_hs _)) (intro se hse)
  (constructor) (exact (Exp.recN P ze se ne))
  (exact (Er.eRecN chkf D us1 us2 us3 P z s n ze se ne hne hP hze hse))
  (refine' (exT Exp _ _ ih_hx _)) (intro xe hxe)
  (refine' (exT Exp _ _ ih_hb _)) (intro bse hbse)
  (constructor) (exact (Exp.caseL P xe bse))
  (exact (Er.eCaseL chkf D us1 us2 P x bs xe bse hxe hP hbse))
  (constructor) (exact Exp.bnil) (exact (Er.eBnil chkf D us P hl))
  (refine' (exT Exp _ _ ih_hh _)) (intro he hhe)
  (refine' (exT Exp _ _ ih_ht _)) (intro te hte)
  (constructor) (exact (Exp.bcons he te))
  (exact (Er.eBcons chkf D us P k h t he te hhe hte hP))
  (refine' (exT Exp _ _ ih_h _)) (intro xe hxe)
  (constructor) (exact (Exp.sleaf xe)) (exact (Er.eSleaf chkf D us x xe hxe))
  (refine' (exT Exp _ _ ih_hx _)) (intro xe hxe)
  (refine' (exT Exp _ _ ih_h1 _)) (intro c1e hc1)
  (refine' (exT Exp _ _ ih_h2 _)) (intro c2e hc2)
  (constructor) (exact (Exp.snode xe c1e c2e))
  (exact (Er.eSnode chkf D us1 us2 us3 x c1 c2 xe c1e c2e hxe hc1 hc2))
  (refine' (exT Exp _ _ ih_hc _)) (intro ce hce)
  (refine' (exT Exp _ _ ih_hl _)) (intro tle htle)
  (refine' (exT Exp _ _ ih_hn _)) (intro tne htne)
  (constructor) (exact (Exp.recS P tle tne ce))
  (exact (Er.eRecS chkf D us1 us2 us3 P tl tn c tle tne ce hce hP htle htne hY1 hY2))
  (refine' (exT Exp _ _ ih_h _)) (intro xe hxe)
  (constructor) (exact (Exp.leaf xe)) (exact (Er.eLeaf chkf D us x xe hxe))
  (refine' (exT Exp _ _ ih_hd _)) (intro de hde)
  (refine' (exT Exp _ _ ih_hx _)) (intro xe hxe)
  (refine' (exT Exp _ _ ih_h1 _)) (intro r1e hr1)
  (refine' (exT Exp _ _ ih_h2 _)) (intro r2e hr2)
  (constructor) (exact (Exp.node de xe r1e r2e))
  (exact (Er.eNode chkf D us1 us2 us3 us4 d x r1 r2 de xe r1e r2e hde hxe hr1 hr2))
  (refine' (exT Exp _ _ ih_hg _)) (intro ge hge)
  (refine' (exT Exp _ _ ih_hh _)) (intro he hhe)
  (refine' (exT Exp _ _ ih_hr _)) (intro re hre)
  (constructor) (exact (Exp.itR X ge he re))
  (exact (Er.eItR chkf D us1 us2 us3 X g h r ge he re hX hge hhe hre))
  (refine' (exT Exp _ _ ih_h _)) (intro re hre)
  (constructor) (exact (Exp.prn re)) (exact (Er.ePrn chkf D us r re hre))
  (refine' (exT Exp _ _ ih_hc _)) (intro ce hce)
  (refine' (exT Exp _ _ ih_hd _)) (intro de hde)
  (constructor) (exact (Exp.chk ce de)) (exact (Er.eChk chkf D us1 us2 c d ce de hce hde))
  (refine' (exT Exp _ _ ih_hr _)) (intro re hre)
  (refine' (exT Exp _ _ ih_hs _)) (intro se hse)
  (refine' (exT Exp _ _ ih_hc _)) (intro ce hce)
  (refine' (exT Exp _ _ ih_h1 _)) (intro e1e he1)
  (refine' (exT Exp _ _ ih_h2 _)) (intro e2e he2)
  (constructor) (exact (Exp.h1 re se ce e1e e2e))
  (exact (Er.eH1 chkf D us1 us2 us3 us4 us5 r s c e1 e2 re se ce e1e e2e hre hse hce he1 he2))
  (refine' (exT Exp _ _ ih_hr _)) (intro re hre)
  (refine' (exT Exp _ _ ih_he _)) (intro ee hee)
  (constructor) (exact (Exp.refl X re ee))
  (exact (Er.eRefl chkf D us1 us2 X cd r e re ee hb hre hee))
  (refine' (exT Exp _ _ ih_hr _)) (intro re hre)
  (refine' (exT Exp _ _ ih_hc _)) (intro ce hce)
  (refine' (exT Exp _ _ ih_h1 _)) (intro t1e ht1)
  (refine' (exT Exp _ _ ih_h2 _)) (intro t2e ht2)
  (constructor) (exact (Exp.insp X re ce t1e t2e))
  (exact (Er.eInsp chkf D us1 us0 us2 X r c t1 t2 re ce t1e t2e hre hce hX hF1 hF2 ht1 ht2)))

;; usk forgets the usage bit and agrees with skel.  Π and Σ need the
;; induction hypotheses; every other constructor, including T(b), is rfl.
(thm usk_skel [A :- Exp]
  (Eq Sk (uskSk (usk A)) (skel A))
  (induction A)
  (all_goals (try rfl))
  (exact (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) ih_P))
  (cases r)
  (all_goals (exact (Eq.trans
    (congrArg (fn [x :- Sk] (Sk.prod x (uskSk (usk B)))) ih_A)
    (congrArg (fn [y :- Sk] (Sk.prod (skel A) y)) ih_B))))
  (cases r)
  (all_goals (exact (Eq.trans
    (congrArg (fn [x :- Sk] (Sk.arr x (uskSk (usk B)))) ih_A)
    (congrArg (fn [y :- Sk] (Sk.arr (skel A) y)) ih_B)))))

;; Constants evaluate under EvE to the carrier constant, and E at that base
;; skeleton is the equality (Theorem 4′, the constant cases).
(thm e_star
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.star v)
         (Erel chkf dec encTy n (USk.base Sk.unit) v
           (den chkf dec encTy n Exp.star G Sk.unit eta)))))
  (constructor) (exact RV.star)
  (constructor) (exact (EvE.eStar chkf dec encTy n rho)) (rfl))

(thm e_tt
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.tt v)
         (Erel chkf dec encTy n (USk.base Sk.bool) v
           (den chkf dec encTy n Exp.tt G Sk.bool eta)))))
  (rw [(den_tt_at chkf dec encTy n G Sk.bool eta)])
  (rw [(coe_self Sk.bool Bool.true)])
  (constructor) (exact (RV.bool Bool.true))
  (constructor) (exact (EvE.eTT chkf dec encTy n rho)) (rfl))

(thm e_ff
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.ff v)
         (Erel chkf dec encTy n (USk.base Sk.bool) v
           (den chkf dec encTy n Exp.ff G Sk.bool eta)))))
  (rw [(den_ff_at chkf dec encTy n G Sk.bool eta)])
  (rw [(coe_self Sk.bool Bool.false)])
  (constructor) (exact (RV.bool Bool.false))
  (constructor) (exact (EvE.eFF chkf dec encTy n rho)) (rfl))

(thm e_zero
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.zero v)
         (Erel chkf dec encTy n (USk.base Sk.nat) v
           (den chkf dec encTy n Exp.zero G Sk.nat eta)))))
  (rw [(den_zero_at chkf dec encTy n G Sk.nat eta)])
  (rw [(coe_self Sk.nat 0)])
  (constructor) (exact (RV.nat 0))
  (constructor) (exact (EvE.eZero chkf dec encTy n rho)) (rfl))

(thm e_lbl
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, l :- Nat, G :- (List Sk), rho :- (List RV), eta :- (HEnv G)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.lbl l) v)
         (Erel chkf dec encTy n (USk.base Sk.lbl) v
           (den chkf dec encTy n (Exp.lbl l) G Sk.lbl eta)))))
  (rw [(den_lbl_at chkf dec encTy n l G Sk.lbl eta)])
  (rw [(coe_self Sk.lbl l)])
  (constructor) (exact (RV.lbl l))
  (constructor) (exact (EvE.eLbl chkf dec encTy n rho l)) (rfl))

;; At a base skeleton the runtime default is the carrier default (E's base
;; clause).  An arrow or a product is not a base; baseSk is false there.
(thm ebase_dflt [s :- Sk, h :- (Eq Bool (baseSk s) Bool.true)]
  (ebase s (rdflt s) (dflt s))
  (cases s)
  (all_goals (try rfl))
  (exact (Bool.noConfusion h))
  (exact (Bool.noConfusion h)))

;; On a base skeleton E is Theorem 4's relation: both are the same equality,
;; so they do not depend on the budget.  This is why an erasing run and
;; evalₙ can be compared at a base data type.
(thm ebase_rel
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, s :- Sk, v :- RV, a :- (Car s),
   h :- (Eq Bool (baseSk s) Bool.true)]
  (Eq Prop (ebase s v a) (rel chkf dec encTy n s v a))
  (cases s)
  (all_goals (try rfl))
  (exact (Bool.noConfusion h))
  (exact (Bool.noConfusion h)))

;; Restriction, scalar half (Theorem 4′ and Theorem 5.2): a sum is nonzero
;; when either part is, and a product of two nonzero usages is nonzero.
;; uadd_eq_zero / umul_eq_zero are the contrapositives; these are the
;; direction the environment argument uses.
(thm nz_uadd [x :- U, y :- U, h :- (Eq Bool (nonzero x) Bool.true)]
  (Eq Bool (nonzero (uadd x y)) Bool.true)
  (cases x)
  (all_goals (cases y))
  (all_goals (try rfl))
  (exact (Bool.noConfusion h)))

(thm nz_umul [r :- U, x :- U,
              hr :- (Eq Bool (nonzero r) Bool.true),
              hx :- (Eq Bool (nonzero x) Bool.true)]
  (Eq Bool (nonzero (umul r x)) Bool.true)
  (cases r)
  (all_goals (cases x))
  (all_goals (try rfl))
  (exact (Bool.noConfusion hr))
  (exact (Bool.noConfusion hr))
  (exact (Bool.noConfusion hr))
  (exact (Bool.noConfusion hx))
  (exact (Bool.noConfusion hx)))


;; --- restriction on usage vectors (Theorem 4′ / 5.2) ----------------

(thm succ_eq [a :- Nat, b :- Nat, h :- (Eq Nat (Nat.succ a) (Nat.succ b))]
  (Eq Nat a b)
  (cases h)
  (rfl))

(kdef nzAt (=> (List U) Nat Bool)
  (fn [us :- (List U)]
    (List.rec$1$0 U (fn [_ :- (List U)] (=> Nat Bool))
      (fn [_i :- Nat] Bool.false)
      (fn [a :- U, rest :- (List U), ih :- (=> Nat Bool)]
        (fn [i :- Nat]
          (Nat.rec$1 (fn [_ :- Nat] Bool) (nonzero a)
            (fn [j :- Nat, _ :- Bool] (ih j)) i)))
      us)))

(thm lenU_cons [a :- U, xs :- (List U)]
  (Eq Nat (lenU (List.cons U a xs)) (Nat.succ (lenU xs)))
  (rfl))

(thm vadd_cc [a :- U, b :- U, xs :- (List U), ys :- (List U)]
  (Eq (List U) (vadd (List.cons U a xs) (List.cons U b ys))
               (List.cons U (uadd a b) (vadd xs ys)))
  (rfl))

(thm nzAt_zero [a :- U, xs :- (List U)]
  (Eq Bool (nzAt (List.cons U a xs) 0) (nonzero a))
  (rfl))

(thm nzAt_succ [a :- U, xs :- (List U), j :- Nat]
  (Eq Bool (nzAt (List.cons U a xs) (Nat.succ j)) (nzAt xs j))
  (rfl))

(thm nz_vadd_z [a :- U, xs :- (List U), b :- U, ys :- (List U),
                h :- (Eq Bool (nzAt (List.cons U a xs) 0) Bool.true)]
  (Eq Bool (nzAt (vadd (List.cons U a xs) (List.cons U b ys)) 0) Bool.true)
  (rw [nzAt_zero])
  (exact (nz_uadd a b (Eq.trans (Eq.symm (nzAt_zero a xs)) h))))

(thm nz_vadd_s [a :- U, xs :- (List U), b :- U, ys :- (List U), j :- Nat,
                ih :- (=> (Eq Bool (nzAt xs j) Bool.true)
                          (Eq Bool (nzAt (vadd xs ys) j) Bool.true)),
                h :- (Eq Bool (nzAt (List.cons U a xs) (Nat.succ j)) Bool.true)]
  (Eq Bool (nzAt (vadd (List.cons U a xs) (List.cons U b ys)) (Nat.succ j)) Bool.true)
  (rw [nzAt_succ])
  (exact (ih (Eq.trans (Eq.symm (nzAt_succ a xs j)) h))))

(thm nz_vadd_step [a :- U, xs :- (List U), b :- U, ys :- (List U),
                   ih :- (forall [i Nat]
                           (=> (Eq Bool (nzAt xs i) Bool.true)
                               (Eq Bool (nzAt (vadd xs ys) i) Bool.true)))]
  (forall [i Nat]
    (=> (Eq Bool (nzAt (List.cons U a xs) i) Bool.true)
        (Eq Bool (nzAt (vadd (List.cons U a xs) (List.cons U b ys)) i) Bool.true)))
  (intro i h)
  (cases i)
  (exact (nz_vadd_z a xs b ys h))
  (exact (nz_vadd_s a xs b ys n (ih n) h)))

(thm nz_vadd_cons [p :- U, ps :- (List U),
                   ih :- (forall [zs (List U)] (forall [j Nat]
                           (=> (Eq Nat (lenU ps) (lenU zs))
                             (=> (Eq Bool (nzAt ps j) Bool.true)
                                 (Eq Bool (nzAt (vadd ps zs) j) Bool.true))))),
                   ys :- (List U), i :- Nat,
                   hlen :- (Eq Nat (lenU (List.cons U p ps)) (lenU ys)),
                   h :- (Eq Bool (nzAt (List.cons U p ps) i) Bool.true)]
  (Eq Bool (nzAt (vadd (List.cons U p ps) ys) i) Bool.true)
  (cases ys)
  (exact (absurd (Eq.trans (Eq.symm (lenU_cons p ps)) hlen) (Nat.succ_ne_zero (lenU ps))))
  (have ht (Eq Nat (lenU ps) (lenU tail))
    (succ_eq (lenU ps) (lenU tail)
      (Eq.trans (Eq.symm (lenU_cons p ps))
        (Eq.trans hlen (lenU_cons head tail)))))
  (exact (nz_vadd_step p ps head tail
            (fn [j :- Nat, hj :- (Eq Bool (nzAt ps j) Bool.true)] (ih tail j ht hj))
            i h)))

(thm nz_vadd [xs :- (List U)]
  (forall [ys (List U)] (forall [i Nat]
    (=> (Eq Nat (lenU xs) (lenU ys))
      (=> (Eq Bool (nzAt xs i) Bool.true)
          (Eq Bool (nzAt (vadd xs ys) i) Bool.true)))))
  (induction xs)
  (intro ys i hlen h) (exact (Bool.noConfusion h))
  (intro ys i hlen h)
  (exact (nz_vadd_cons head tail ih_tail ys i hlen h)))

(thm vscale_cc [r :- U, a :- U, xs :- (List U)]
  (Eq (List U) (vscale r (List.cons U a xs)) (List.cons U (umul r a) (vscale r xs)))
  (rfl))

(thm nz_vscale_z [r :- U, hr :- (Eq Bool (nonzero r) Bool.true),
                  a :- U, xs :- (List U),
                  h :- (Eq Bool (nzAt (List.cons U a xs) 0) Bool.true)]
  (Eq Bool (nzAt (vscale r (List.cons U a xs)) 0) Bool.true)
  (rw [vscale_cc])
  (rw [nzAt_zero])
  (exact (nz_umul r a hr (Eq.trans (Eq.symm (nzAt_zero a xs)) h))))

(thm nz_vscale_s [r :- U, hr :- (Eq Bool (nonzero r) Bool.true),
                  a :- U, xs :- (List U), j :- Nat,
                  ih :- (=> (Eq Bool (nzAt xs j) Bool.true)
                            (Eq Bool (nzAt (vscale r xs) j) Bool.true)),
                  h :- (Eq Bool (nzAt (List.cons U a xs) (Nat.succ j)) Bool.true)]
  (Eq Bool (nzAt (vscale r (List.cons U a xs)) (Nat.succ j)) Bool.true)
  (rw [vscale_cc])
  (rw [nzAt_succ])
  (exact (ih (Eq.trans (Eq.symm (nzAt_succ a xs j)) h))))

(thm nz_vscale [r :- U, hr :- (Eq Bool (nonzero r) Bool.true), us :- (List U)]
  (forall [i Nat]
    (=> (Eq Bool (nzAt us i) Bool.true)
        (Eq Bool (nzAt (vscale r us) i) Bool.true)))
  (induction us)
  (intro i h) (exact (Bool.noConfusion h))
  (intro i h)
  (cases i)
  (exact (nz_vscale_z r hr head tail h))
  (exact (nz_vscale_s r hr head tail n (ih_tail n) h)))


;; Defaults are related (Theorem 4′, the defaults paragraph).  At a base
;; skeleton this is ebase_dflt.  At Π₀ the default closure is applied to ⋆;
;; at Π₁/Πω it is applied to a related argument and ignores it.  At Σ₀ the
;; first component of the pair of defaults is forgotten; at Σ₁/Σω both
;; components are related.

(thm erdflt_arr0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, d :- USk, c :- USk,
   hc :- (Erel chkf dec encTy n c (rdflt (uskSk c)) (dflt (uskSk c)))]
  (Erel chkf dec encTy n (USk.arr0 d c)
    (rdflt (Sk.arr (uskSk d) (uskSk c)))
    (dflt (Sk.arr (uskSk d) (uskSk c))))
  (intro alpha)
  (constructor)
  (exact (rdflt (skel (reifySk (uskSk c)))))
  (constructor)
  (exact (EvE.apClos chkf dec encTy n (List.nil RV)
           (Exp.abort (reifySk (uskSk c)) Exp.star) RV.star
           (rdflt (skel (reifySk (uskSk c))))
           (EvE.eAbort chkf dec encTy n
             (List.cons RV RV.star (List.nil RV))
             (reifySk (uskSk c)) Exp.star RV.star
             (EvE.eStar chkf dec encTy n
               (List.cons RV RV.star (List.nil RV))))))
  (rw [(dflt_arr (uskSk d) (uskSk c) alpha)])
  (rw [(congrArg rdflt (skel_reify (uskSk c)))])
  (exact hc))

(thm erdflt_arrN
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, d :- USk, c :- USk,
   hc :- (Erel chkf dec encTy n c (rdflt (uskSk c)) (dflt (uskSk c)))]
  (Erel chkf dec encTy n (USk.arrN d c)
    (rdflt (Sk.arr (uskSk d) (uskSk c)))
    (dflt (Sk.arr (uskSk d) (uskSk c))))
  (intro arg) (intro alpha) (intro _harg)
  (constructor)
  (exact (rdflt (skel (reifySk (uskSk c)))))
  (constructor)
  (exact (EvE.apClos chkf dec encTy n (List.nil RV)
           (Exp.abort (reifySk (uskSk c)) Exp.star) arg
           (rdflt (skel (reifySk (uskSk c))))
           (EvE.eAbort chkf dec encTy n
             (List.cons RV arg (List.nil RV))
             (reifySk (uskSk c)) Exp.star RV.star
             (EvE.eStar chkf dec encTy n
               (List.cons RV arg (List.nil RV))))))
  (rw [(dflt_arr (uskSk d) (uskSk c) alpha)])
  (rw [(congrArg rdflt (skel_reify (uskSk c)))])
  (exact hc))

(thm erdflt_prod0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, d :- USk, c :- USk,
   hc :- (Erel chkf dec encTy n c (rdflt (uskSk c)) (dflt (uskSk c)))]
  (Erel chkf dec encTy n (USk.prod0 d c)
    (rdflt (Sk.prod (uskSk d) (uskSk c)))
    (dflt (Sk.prod (uskSk d) (uskSk c))))
  (constructor) (exact (rdflt (uskSk d)))
  (constructor) (exact (rdflt (uskSk c)))
  (constructor) (rfl)
  (exact hc))

(thm erdflt_prodN
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, d :- USk, c :- USk,
   hd :- (Erel chkf dec encTy n d (rdflt (uskSk d)) (dflt (uskSk d))),
   hc :- (Erel chkf dec encTy n c (rdflt (uskSk c)) (dflt (uskSk c)))]
  (Erel chkf dec encTy n (USk.prodN d c)
    (rdflt (Sk.prod (uskSk d) (uskSk c)))
    (dflt (Sk.prod (uskSk d) (uskSk c))))
  (constructor) (exact (rdflt (uskSk d)))
  (constructor) (exact (rdflt (uskSk c)))
  (constructor) (rfl)
  (constructor) (exact hd) (exact hc))

;; The defaults paragraph of Theorem 4′, for every type.  The carrier is
;; Car(uskSk(usk A)), which is where Erel lives, so the four skeleton
;; lemmas apply with no cast.  Bases, T(b) and non-types are the unit (or
;; the matching base) clause and close by rfl.  A branch list is an arrow
;; from labels.  Π₀/Σ₀ use only the codomain hypothesis; Π and Σ at 1 and ω
;; use both, except that an arrow default ignores its argument, so the
;; domain hypothesis is not required there either.
(thm erdflt
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, A :- Exp]
  (Erel chkf dec encTy n (usk A)
    (rdflt (uskSk (usk A)))
    (dflt (uskSk (usk A))))
  (induction A)
  (all_goals (try rfl))
  (exact (erdflt_arrN chkf dec encTy n (USk.base Sk.lbl) (usk P) ih_P))
  (cases r)
  (exact (erdflt_prod0 chkf dec encTy n (usk A) (usk B) ih_B))
  (exact (erdflt_prodN chkf dec encTy n (usk A) (usk B) ih_A ih_B))
  (exact (erdflt_prodN chkf dec encTy n (usk A) (usk B) ih_A ih_B))
  (cases r)
  (exact (erdflt_arr0 chkf dec encTy n (usk A) (usk B) ih_B))
  (exact (erdflt_arrN chkf dec encTy n (usk A) (usk B) ih_B))
  (exact (erdflt_arrN chkf dec encTy n (usk A) (usk B) ih_B)))

;; Transport of a carrier default along a skeleton equation.  cases on the
;; equation, not subst: subst leaves the Eq.mp motive with a free variable.
(thm dflt_cast [s :- Sk, t :- Sk, e :- (Eq Sk s t)]
  (Eq (Car t) (Eq.mp (congrArg Car e) (dflt s)) (dflt t))
  (cases e)
  (rfl))

;; Erel is a predicate of the runtime value and of the carrier value, so an
;; equality of either transports a witness.  subst rewrites the hypothesis.
(thm erel_rv
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, v :- RV, w :- RV, a :- (Car (uskSk u)),
   hv :- (Erel chkf dec encTy n u v a),
   e :- (Eq RV v w)]
  (Erel chkf dec encTy n u w a)
  (subst e)
  (exact hv))

(thm erel_car
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, v :- RV, a :- (Car (uskSk u)), b :- (Car (uskSk u)),
   hv :- (Erel chkf dec encTy n u v a),
   e :- (Eq (Car (uskSk u)) a b)]
  (Erel chkf dec encTy n u v b)
  (subst e)
  (exact hv))

;; The same defaults, stated at skel A, which is the carrier denotation
;; uses.  usk_skel is not definitional at Π, Σ and branch lists, so the
;; carrier value is the cast of dflt(skel A) along that equation.
(thm erdflt_ty
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, A :- Exp]
  (Erel chkf dec encTy n (usk A) (rdflt (skel A))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (dflt (skel A))))
  (exact (erel_car chkf dec encTy n (usk A) (rdflt (skel A))
           (dflt (uskSk (usk A)))
           (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (dflt (skel A)))
           (erel_rv chkf dec encTy n (usk A)
             (rdflt (uskSk (usk A))) (rdflt (skel A))
             (dflt (uskSk (usk A)))
             (erdflt chkf dec encTy n A)
             (congrArg rdflt (usk_skel A)))
           (Eq.symm (dflt_cast (skel A) (uskSk (usk A)) (Eq.symm (usk_skel A)))))))

;; abort (Theorem 4′).  The argument is evaluated and discarded; the result
;; is the runtime default of the annotation, related to the carrier default
;; by erdflt_ty.  This is not vacuous: E(0) relates ⋆ to ⋆, unlike S(0).
(thm e_abort
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk), A :- Exp, t :- Exp,
   rho :- (List RV), eta :- (HEnv G), vt :- RV,
   ht :- (EvalE chkf dec encTy n rho t vt)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.abort A t) v)
         (Erel chkf dec encTy n (usk A) v
           (Eq.mp (congrArg Car (Eq.symm (usk_skel A)))
             (den chkf dec encTy n (Exp.abort A t) G (skel A) eta))))))
  (rw [(den_abort_at chkf dec encTy n A t G (skel A) eta)])
  (constructor) (exact (rdflt (skel A)))
  (constructor) (exact (EvE.eAbort chkf dec encTy n rho A t vt ht))
  (exact (erdflt_ty chkf dec encTy n A)))

;; H₁ returns ⋆, and E at 1 does not read the denotation (Theorem 4′).
;; The five premises are evaluated.  Corollary 3.7, which makes the case
;; vacuous for Theorem 5.2, is not used here.
(thm e_h1
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, G :- (List Sk),
   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   vr :- RV, vs :- RV, vc :- RV, v1 :- RV, v2 :- RV,
   hr :- (EvalE chkf dec encTy n rho r vr),
   hs :- (EvalE chkf dec encTy n rho s vs),
   hc :- (EvalE chkf dec encTy n rho c vc),
   h1 :- (EvalE chkf dec encTy n rho e1 v1),
   h2 :- (EvalE chkf dec encTy n rho e2 v2)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.h1 r s c e1 e2) v)
         (Erel chkf dec encTy n (USk.base Sk.unit) v
           (den chkf dec encTy n (Exp.h1 r s c e1 e2) G Sk.unit eta)))))
  (constructor) (exact RV.star)
  (constructor)
  (exact (EvE.eH1 chkf dec encTy n rho r s c e1 e2 vr vs vc v1 v2 hr hs hc h1 h2))
  (rfl))

;; --- environments for E (Theorem 4′, the fundamental property) ------------
;; usks forgets the usage bit of a usage-skeleton context, innermost first,
;; so HEnv(usks G) is a carrier environment whose head has type
;; Car(uskSk u), the type Erel expects.

(a/defn usks [G :- (List USk)] (List Sk)
  (match G
    [nil (List.nil Sk)]
    [(cons u rest) (List.cons Sk (uskSk u) (usks rest))]))

(thm usks_nil []
  (Eq (List Sk) (usks (List.nil USk)) (List.nil Sk))
  (rfl))

(thm usks_cons [u :- USk, rest :- (List USk)]
  (Eq (List Sk) (usks (List.cons USk u rest)) (List.cons Sk (uskSk u) (usks rest)))
  (rfl))

;; One entry.  Usage 0 is True: the erased program never reads it, so any
;; runtime value is allowed against any carrier value.  Usage 1 and ω are
;; Erel.  (An earlier state required usage-0 entries to be ⋆; review T52-04.)
(kdef entryE
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [r U] (forall [u USk] (=> RV (Car (uskSk u)) Prop)))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, r :- U, u :- USk]
    (U.rec$1 (fn [_ :- U] (=> RV (Car (uskSk u)) Prop))
      (fn [_v :- RV, _a :- (Car (uskSk u))] True)
      (fn [v :- RV, a :- (Car (uskSk u))] (Erel chkf dec encTy n u v a))
      (fn [v :- RV, a :- (Car (uskSk u))] (Erel chkf dec encTy n u v a))
      r)))

(thm entryE_u0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, v :- RV, a :- (Car (uskSk u))]
  (entryE chkf dec encTy n U.u0 u v a)
  (exact True.intro))

(thm entryE_u1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, v :- RV, a :- (Car (uskSk u))]
  (Eq Prop (entryE chkf dec encTy n U.u1 u v a) (Erel chkf dec encTy n u v a))
  (rfl))

(thm entryE_uw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, v :- RV, a :- (Car (uskSk u))]
  (Eq Prop (entryE chkf dec encTy n U.uw u v a) (Erel chkf dec encTy n u v a))
  (rfl))

;; envE n G rs ρ η: the runtime environment ρ is E-related to η along the
;; usage-skeleton context G, with usages rs.  The lists are innermost first.
;; A mismatch of length is not related.  η is an environment for usks G,
;; not for skels of a type context; usk_skel moves one entry, and a lemma
;; for a whole denotation environment is still open.
(kdef envE
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [G (List USk)]
          (=> (List U) (=> (List RV) (HEnv (usks G)) Prop)))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, G :- (List USk)]
    (List.rec$1$0 USk (fn [G :- (List USk)] (=> (List U) (=> (List RV) (HEnv (usks G)) Prop)))
      (fn [rs :- (List U), rho :- (List RV), _e :- Unit]
        (And (Eq (List U) rs (List.nil U)) (Eq (List RV) rho (List.nil RV))))
      (fn [u :- USk, rest :- (List USk),
           ih :- (=> (List U) (=> (List RV) (HEnv (usks rest)) Prop))]
        (fn [rs :- (List U), rho :- (List RV), e :- (HEnv (usks (List.cons USk u rest)))]
          (Exists (fn [r :- U] (Exists (fn [rs2 :- (List U)]
            (Exists (fn [v :- RV] (Exists (fn [rho2 :- (List RV)]
              (And (Eq (List U) rs (List.cons U r rs2))
                (And (Eq (List RV) rho (List.cons RV v rho2))
                  (And (ih rs2 rho2 (Prod.snd e))
                       (entryE chkf dec encTy n r u v (Prod.fst e)))))))))))))))
      G)))

(thm envE_nil
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat]
  (envE chkf dec encTy n (List.nil USk) (List.nil U) (List.nil RV) Unit.unit)
  (constructor) (rfl) (rfl))

(thm envE_cons
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, rest :- (List USk), r :- U,
   rs :- (List U), v :- RV, rho :- (List RV),
   alpha :- (Car (uskSk u)), eta :- (HEnv (usks rest)),
   he :- (entryE chkf dec encTy n r u v alpha),
   ht :- (envE chkf dec encTy n rest rs rho eta)]
  (envE chkf dec encTy n (List.cons USk u rest) (List.cons U r rs)
        (List.cons RV v rho) (Prod.mk alpha eta))
  (constructor) (exact r)
  (constructor) (exact rs)
  (constructor) (exact v)
  (constructor) (exact rho)
  (constructor) (rfl)
  (constructor) (rfl)
  (constructor) (exact ht) (exact he))

;; Usage 0 extends by any runtime value and any carrier value.
(thm envE_cons0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, rest :- (List USk),
   rs :- (List U), v :- RV, rho :- (List RV),
   alpha :- (Car (uskSk u)), eta :- (HEnv (usks rest)),
   ht :- (envE chkf dec encTy n rest rs rho eta)]
  (envE chkf dec encTy n (List.cons USk u rest) (List.cons U U.u0 rs)
        (List.cons RV v rho) (Prod.mk alpha eta))
  (exact (envE_cons chkf dec encTy n u rest U.u0 rs v rho alpha eta
           (entryE_u0 chkf dec encTy n u v alpha) ht)))

;; Usage 1 extends only by an E-related value.  entryE at 1 is Erel.
(thm envE_cons1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, rest :- (List USk),
   rs :- (List U), v :- RV, rho :- (List RV),
   alpha :- (Car (uskSk u)), eta :- (HEnv (usks rest)),
   hv :- (Erel chkf dec encTy n u v alpha),
   ht :- (envE chkf dec encTy n rest rs rho eta)]
  (envE chkf dec encTy n (List.cons USk u rest) (List.cons U U.u1 rs)
        (List.cons RV v rho) (Prod.mk alpha eta))
  (exact (envE_cons chkf dec encTy n u rest U.u1 rs v rho alpha eta hv ht)))

;; Usage ω, the same clause as usage 1.
(thm envE_consw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- USk, rest :- (List USk),
   rs :- (List U), v :- RV, rho :- (List RV),
   alpha :- (Car (uskSk u)), eta :- (HEnv (usks rest)),
   hv :- (Erel chkf dec encTy n u v alpha),
   ht :- (envE chkf dec encTy n rest rs rho eta)]
  (envE chkf dec encTy n (List.cons USk u rest) (List.cons U U.uw rs)
        (List.cons RV v rho) (Prod.mk alpha eta))
  (exact (envE_cons chkf dec encTy n u rest U.uw rs v rho alpha eta hv ht)))

;; A head step of a type preserves the usage skeleton (Theorem 4′, invariance
;; under ≡, the head case).  The only head redexes of shape isTy are
;; T(tt) ⇝ 1 and T(ff) ⇝ 0, and both sides are the unit clause.  Every other
;; head redex is a term, so isTy is false.  The same proof as hd_skel.
;; A step under a path, and therefore a Cv chain, is not yet proved: it is
;; the same argument as step_skel / cv_skel, and it needs the child lemma.
(thm usk_hd
  [chkf :- (=> Code Code Bool), r :- Exp, r2 :- Exp, der :- (Hd chkf r r2)]
  (=> (Eq Bool (isTy r) Bool.true) (Eq USk (usk r2) (usk r)))
  (induction der)
  (all_goals (intro hc))
  (all_goals (first (rfl) (cases hc))))

;; The well-formedness hypothesis is necessary.  β at the ill-formed
;; application (λx:Nat. Bool) ⋆ yields Bool, and usk goes from the wildcard
;; unit clause to Bool.  The same redex as beta_step_example / step_skel_needs_wf.
(thm usk_hd_needs_wf [chkf :- (=> Code Code Bool)]
  (Not (forall [A Exp] (forall [B Exp] (=> (Hd chkf A B) (Eq USk (usk A) (usk B))))))
  (intro h)
  (have hn (Eq USk (usk (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star)) (usk Exp.tBool))
    (h (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star) Exp.tBool
       (Hd.beta chkf U.uw Exp.tNat Exp.tBool Exp.star)))
  (cases hn))
