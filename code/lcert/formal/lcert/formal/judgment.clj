(ns lcert.formal.judgment
  "F2c — the rule table of λᶜᵉʳᵗ₀ (R4-metatheory.md §1.4).

  Two inductive families, both parameterized by the checker `chkf` that
  δ-steps consult:
  - Tl chkf w Δ t A: with w = true, `Δ ⊢ t type` (A unused, tUnit); with
    w = false, the type-level judgment `Δ ⊢ t :⁰ A`.  Type-level premises are
    never runtime ones, so this family stands alone.
  - Rt chkf Δ us t A: the runtime judgment `Γ ⊢ t :¹ A`, where the context Γ
    is the telescope Δ (innermost first) with the usage vector us.

  Contexts in one rule share one telescope, so Γ₁ + Γ₂ and ρΓ are the usage
  vectors vadd us₁ us₂ and vscale ρ us.  A premise at σ(ρ) = 0 is a
  type-level premise; rules whose premise's mode depends on a usage (App,
  Pair) split into a usage-0 case and a runtime case.  Axioms accept any
  usage vector of the right length (the affine reading).

  Codes of types, as the implementation encodes them (lcert.encode): the base
  types are leaves labelled 15–22 (t0, t1, bool, nat, lbl, syn, cert), and
  arrow₁ is label 25.  c⊥ is the leaf t0, and neg c := snode arrow₁ c c⊥.

  caseLbl's branches are typed through the internal pseudo-type tBrs P k,
  for labels k, k+1, … up to NL − 1 = 99 (Ansatz cannot generate recursors
  for a premise quantified over labels)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]))

(a/defn NL [] Nat 100)

(a/defn nthE [D :- (List Exp), i :- Nat] (Option Exp)
  (match D [nil (Option.none Exp)] [(cons x rest) (match i [zero (Option.some Exp x)] [(succ j) (nthE rest j)])]))

(a/defn nthU [us :- (List U), i :- Nat] (Option U)
  (match us [nil (Option.none U)] [(cons x rest) (match i [zero (Option.some U x)] [(succ j) (nthU rest j)])]))

(a/defn skels [D :- (List Exp)] (List Sk)
  (match D [nil (List.nil Sk)] [(cons x rest) (List.cons Sk (skel x) (skels rest))]))

(a/defn nonzero [r :- U] Bool (match r [u0 false] [u1 true] [uw true]))

;; Codes as terms.
(a/defn cLeaf [l :- Nat] Exp (Exp.sleaf (Exp.lbl l)))
(a/defn cbot [] Exp (cLeaf 15))
(a/defn negT [c :- Exp] Exp (Exp.snode (Exp.lbl 25) c (cLeaf 15)))

;; The code, as a term, of a base data type (§1.2: 0, 1, Bool, Nat, Lbl, Syn, R).
(a/defn baseCode [D :- Exp] (Option Exp)
  (match D
    [tEmpty (Option.some Exp (cLeaf 15))] [tUnit (Option.some Exp (cLeaf 16))]
    [tBool (Option.some Exp (cLeaf 17))] [tNat (Option.some Exp (cLeaf 18))]
    [tLbl (Option.some Exp (cLeaf 19))] [tSyn (Option.some Exp (cLeaf 20))]
    [tR (Option.some Exp (cLeaf 22))] [_ (Option.none Exp)]))

(a/defn chkT [r :- Exp, c :- Exp] Exp (Exp.tT (Exp.chk (Exp.prn r) c)))
(a/defn notE [b :- Exp] Exp (Exp.ite b Exp.ff Exp.tt))

;; Motive instances.  P lives under one binder x.
(a/defn sSucc [i :- Nat] Exp (match i [zero (Exp.succ (Exp.var 0))] [(succ j) (Exp.var (+ j 1))]))
(a/defn stepTy [P :- Exp] Exp (lift 1 0 (subst (fn [i :- Nat] (sSucc i)) P)))          ; P[succ x/x] under y
(a/defn sLeafI [i :- Nat] Exp (match i [zero (Exp.sleaf (Exp.var 0))] [(succ j) (Exp.var (+ j 1))]))
(a/defn sNodeI [i :- Nat] Exp (match i [zero (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2))] [(succ j) (Exp.var (+ j 5))]))
(a/defn sAt [k :- Nat, sh :- Nat, i :- Nat] Exp (match i [zero (Exp.var k)] [(succ j) (Exp.var (+ j sh))]))
(a/defn leafTy [P :- Exp] Exp (subst (fn [i :- Nat] (sLeafI i)) P))                  ; P[sleaf a/x] under a
(a/defn nodeTy [P :- Exp] Exp (subst (fn [i :- Nat] (sNodeI i)) P))                  ; P[snode a c1 c2/x] under 5
(a/defn y1Ty [P :- Exp] Exp (subst (fn [i :- Nat] (sAt 1 3 i)) P))                   ; P[c1/x] under c2 c1 a
(a/defn y2Ty [P :- Exp] Exp (subst (fn [i :- Nat] (sAt 1 4 i)) P))                   ; P[c2/x] under y1 c2 c1 a
(a/defn gTy [X :- Exp] Exp (Exp.tPi U.uw Exp.tLbl (lift 1 0 X)))
(a/defn hTy [X :- Exp] Exp
  (Exp.tPi U.u1 Exp.tDia (Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))))))
;; Base types and ◇, formed in any context.
(a/defn isBaseOrDia [X :- Exp] Bool
  (match X [tEmpty true] [tUnit true] [tBool true] [tNat true] [tLbl true] [tSyn true] [tDia true] [tR true] [_ false]))

;; The constants typed by an axiom: ⋆ : 1, tt ff : Bool, zero : Nat, ℓ : Lbl.
(a/defn constTyped [t :- Exp, A :- Exp] Bool
  (match t
    [star (match A [tUnit true] [_ false])]
    [tt (match A [tBool true] [_ false])]
    [ff (match A [tBool true] [_ false])]
    [zero (match A [tNat true] [_ false])]
    [(lbl l) (match A [tLbl (Nat.blt l (NL))] [_ false])]
    [_ false]))

;; clean A: A contains no branch-list pseudo-type tBrs anywhere.  The paper
;; has no such type; the formalization types caseLbl's branch lists through
;; it (the module notes, above).  App₀/App require their function type to be
;; formed (premises hA, hB: those of fPi), hence clean: otherwise a context entry of type
;; Π(y : tBrs P k). B lets a branch list be passed as an argument, where skOf
;; (hence the application clause of ⟦·⟧) has no skeleton for it, and the
;; fundamental lemma fails.  hB is also B's formation, which the
;; substitution lemma needs.  Paper derivations meet it by regularity.
;; Pair₀/Pair likewise take fSig's premises (hA, hB) for their Σ type, and Let
;; for the Σ type it eliminates (its body's context extends by A and B).  The
;; type-level rules zApp, zPair and zLet take the same premises, since
;; type-level arguments are substituted into types (App₀, Pair₀).  Inspect
;; takes the formation of the types its branches' contexts add (hF1, hF2);
;; Bcons the formation of its motive P (hP), as CaseL does, which its case of
;; the fundamental lemma reads (V_subst1 at the label); RecSyn the formation
;; of the two recursive results' types its node branch adds (hY1, hY2).
(a/defn clean [e :- Exp] Bool
  (match e
    [(tT b) (clean b)]
    [(tPi r A B) (Bool.and (clean A) (clean B))]
    [(tSig r A B) (Bool.and (clean A) (clean B))]
    [(abort A t) (Bool.and (clean A) (clean t))]
    [(ite b t e) (Bool.and (clean b) (Bool.and (clean t) (clean e)))]
    [(elimB P b t e) (Bool.and (clean P) (Bool.and (clean b) (Bool.and (clean t) (clean e))))]
    [(succ n) (clean n)]
    [(recN P z s n) (Bool.and (clean P) (Bool.and (clean z) (Bool.and (clean s) (clean n))))]
    [(caseL P a bs) (Bool.and (clean P) (Bool.and (clean a) (clean bs)))]
    [(bcons h t) (Bool.and (clean h) (clean t))]
    [(sleaf a) (clean a)]
    [(snode a c1 c2) (Bool.and (clean a) (Bool.and (clean c1) (clean c2)))]
    [(recS P tl tn c) (Bool.and (clean P) (Bool.and (clean tl) (Bool.and (clean tn) (clean c))))]
    [(leaf a) (clean a)]
    [(node d a r1 r2) (Bool.and (clean d) (Bool.and (clean a) (Bool.and (clean r1) (clean r2))))]
    [(itR X g h r) (Bool.and (clean X) (Bool.and (clean g) (Bool.and (clean h) (clean r))))]
    [(prn r) (clean r)]
    [(lam r A t) (Bool.and (clean A) (clean t))]
    [(app f u) (Bool.and (clean f) (clean u))]
    [(pair S a b) (Bool.and (clean S) (Bool.and (clean a) (clean b)))]
    [(letp C p t) (Bool.and (clean C) (Bool.and (clean p) (clean t)))]
    [(chk c d) (Bool.and (clean c) (clean d))]
    [(h1 r s c e1 e2) (Bool.and (clean r) (Bool.and (clean s) (Bool.and (clean c) (Bool.and (clean e1) (clean e2)))))]
    [(refl D r e) (Bool.and (clean D) (Bool.and (clean r) (clean e)))]
    [(insp X r c t1 t2) (Bool.and (clean X) (Bool.and (clean r) (Bool.and (clean c) (Bool.and (clean t1) (clean t2)))))]
    [(tBrs P k) false]
    [_ true]))

;; ---------------------------------------------------------------------------
;; Formation and type-level typing (Tl).  w = Bool.true: `Δ ⊢ t type`;
;; w = Bool.false: `Δ ⊢ t :⁰ A`.  Every term premise is type-level, and
;; Var⁰ accepts any variable (§1.4, "Type level").

(a/inductive Tl [chkf (=> Code Code Bool)] :in Prop :indices [w Bool, D (List Exp), t Exp, A Exp]
  ;; formation
  (fBase [D (List Exp)] [X Exp] [h (Eq Bool (isBaseOrDia X) Bool.true)] :where [Bool.true D X Exp.tUnit])
  (fT [D (List Exp)] [b Exp] [hb (Tl chkf Bool.false D b Exp.tBool)] :where [Bool.true D (Exp.tT b) Exp.tUnit])
  (fPi [D (List Exp)] [r U] [A Exp] [B Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)]
       [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)] :where [Bool.true D (Exp.tPi r A B) Exp.tUnit])
  (fSig [D (List Exp)] [r U] [A Exp] [B Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)] :where [Bool.true D (Exp.tSig r A B) Exp.tUnit])
  ;; type-level typing
  (zVar [D (List Exp)] [i Nat] [A Exp] [h (Eq (Option Exp) (nthE D i) (Option.some Exp A))]
        :where [Bool.false D (Exp.var i) (lift (+ i 1) 0 A)])
  (zConst [D (List Exp)] [t Exp] [A Exp] [h (Eq Bool (constTyped t A) Bool.true)] :where [Bool.false D t A])
  (zConv [D (List Exp)] [t Exp] [A Exp] [B Exp] [ht (Tl chkf Bool.false D t A)] [hB (Tl chkf Bool.true D B Exp.tUnit)]
         [hc (Cv chkf (skels D) A B)] :where [Bool.false D t B])
  (zLam [D (List Exp)] [r U] [A Exp] [t Exp] [B Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [ht (Tl chkf Bool.false (List.cons Exp A D) t B)] :where [Bool.false D (Exp.lam r A t) (Exp.tPi r A B)])
  (zApp [D (List Exp)] [r U] [f Exp] [u Exp] [A Exp] [B Exp] [hf (Tl chkf Bool.false D f (Exp.tPi r A B))]
        [hu (Tl chkf Bool.false D u A)] [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)] :where [Bool.false D (Exp.app f u) (subst1 u B)])
  (zPair [D (List Exp)] [r U] [A Exp] [B Exp] [x Exp] [y Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)]
         [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
         [hx (Tl chkf Bool.false D x A)] [hy (Tl chkf Bool.false D y (subst1 x B))]
         :where [Bool.false D (Exp.pair (Exp.tSig r A B) x y) (Exp.tSig r A B)])
  (zLet [D (List Exp)] [r U] [A Exp] [B Exp] [C Exp] [p Exp] [t Exp] [hp (Tl chkf Bool.false D p (Exp.tSig r A B))]
        [hC (Tl chkf Bool.true D C Exp.tUnit)] [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
        [ht (Tl chkf Bool.false (List.cons Exp B (List.cons Exp A D)) t (lift 2 0 C))]
        :where [Bool.false D (Exp.letp C p t) C])
  (zAbort [D (List Exp)] [A Exp] [t Exp] [ht (Tl chkf Bool.false D t Exp.tEmpty)] [hA (Tl chkf Bool.true D A Exp.tUnit)]
          :where [Bool.false D (Exp.abort A t) A])
  (zIte [D (List Exp)] [b Exp] [t Exp] [e Exp] [C Exp] [hb (Tl chkf Bool.false D b Exp.tBool)]
        [ht (Tl chkf Bool.false D t C)] [he (Tl chkf Bool.false D e C)] :where [Bool.false D (Exp.ite b t e) C])
  (zElimB [D (List Exp)] [P Exp] [b Exp] [t Exp] [e Exp] [hb (Tl chkf Bool.false D b Exp.tBool)]
          [hP (Tl chkf Bool.true (List.cons Exp Exp.tBool D) P Exp.tUnit)]
          [ht (Tl chkf Bool.false D t (subst1 Exp.tt P))] [he (Tl chkf Bool.false D e (subst1 Exp.ff P))]
          :where [Bool.false D (Exp.elimB P b t e) (subst1 b P)])
  (zSucc [D (List Exp)] [n Exp] [h (Tl chkf Bool.false D n Exp.tNat)] :where [Bool.false D (Exp.succ n) Exp.tNat])
  (zRecN [D (List Exp)] [P Exp] [z Exp] [s Exp] [n Exp] [hn (Tl chkf Bool.false D n Exp.tNat)]
         [hP (Tl chkf Bool.true (List.cons Exp Exp.tNat D) P Exp.tUnit)] [hz (Tl chkf Bool.false D z (subst1 Exp.zero P))]
         [hs (Tl chkf Bool.false (List.cons Exp P (List.cons Exp Exp.tNat D)) s (stepTy P))]
         :where [Bool.false D (Exp.recN P z s n) (subst1 n P)])
  (zCaseL [D (List Exp)] [P Exp] [x Exp] [bs Exp] [hx (Tl chkf Bool.false D x Exp.tLbl)]
          [hP (Tl chkf Bool.true (List.cons Exp Exp.tLbl D) P Exp.tUnit)] [hb (Tl chkf Bool.false D bs (Exp.tBrs P 0))]
          :where [Bool.false D (Exp.caseL P x bs) (subst1 x P)])
  (zBnil [D (List Exp)] [P Exp] :where [Bool.false D Exp.bnil (Exp.tBrs P (NL))])
  (zBcons [D (List Exp)] [P Exp] [k Nat] [h Exp] [t Exp] [hh (Tl chkf Bool.false D h (subst1 (Exp.lbl k) P))]
          [ht (Tl chkf Bool.false D t (Exp.tBrs P (+ k 1)))] :where [Bool.false D (Exp.bcons h t) (Exp.tBrs P k)])
  (zSleaf [D (List Exp)] [x Exp] [h (Tl chkf Bool.false D x Exp.tLbl)] :where [Bool.false D (Exp.sleaf x) Exp.tSyn])
  (zSnode [D (List Exp)] [x Exp] [c1 Exp] [c2 Exp] [hx (Tl chkf Bool.false D x Exp.tLbl)]
          [h1 (Tl chkf Bool.false D c1 Exp.tSyn)] [h2 (Tl chkf Bool.false D c2 Exp.tSyn)]
          :where [Bool.false D (Exp.snode x c1 c2) Exp.tSyn])
  (zRecS [D (List Exp)] [P Exp] [tl Exp] [tn Exp] [c Exp] [hc (Tl chkf Bool.false D c Exp.tSyn)]
         [hP (Tl chkf Bool.true (List.cons Exp Exp.tSyn D) P Exp.tUnit)]
         [hl (Tl chkf Bool.false (List.cons Exp Exp.tLbl D) tl (leafTy P))]
         [hn (Tl chkf Bool.false (List.cons Exp (y2Ty P) (List.cons Exp (y1Ty P) (List.cons Exp Exp.tSyn
                (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))) tn (nodeTy P))]
         :where [Bool.false D (Exp.recS P tl tn c) (subst1 c P)])
  (zLeaf [D (List Exp)] [x Exp] [h (Tl chkf Bool.false D x Exp.tLbl)] :where [Bool.false D (Exp.leaf x) Exp.tR])
  (zNode [D (List Exp)] [d Exp] [x Exp] [r1 Exp] [r2 Exp] [hd (Tl chkf Bool.false D d Exp.tDia)]
         [hx (Tl chkf Bool.false D x Exp.tLbl)] [h1 (Tl chkf Bool.false D r1 Exp.tR)] [h2 (Tl chkf Bool.false D r2 Exp.tR)]
         :where [Bool.false D (Exp.node d x r1 r2) Exp.tR])
  (zItR [D (List Exp)] [X Exp] [g Exp] [h Exp] [r Exp] [hX (Tl chkf Bool.true D X Exp.tUnit)]
        [hg (Tl chkf Bool.false D g (gTy X))] [hh (Tl chkf Bool.false D h (hTy X))] [hr (Tl chkf Bool.false D r Exp.tR)]
        :where [Bool.false D (Exp.itR X g h r) X])
  (zPrn [D (List Exp)] [r Exp] [h (Tl chkf Bool.false D r Exp.tR)] :where [Bool.false D (Exp.prn r) Exp.tSyn])
  (zChk [D (List Exp)] [c Exp] [d Exp] [hc (Tl chkf Bool.false D c Exp.tSyn)] [hd (Tl chkf Bool.false D d Exp.tSyn)]
        :where [Bool.false D (Exp.chk c d) Exp.tBool])
  (zH1 [D (List Exp)] [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp] [hr (Tl chkf Bool.false D r Exp.tR)]
       [hs (Tl chkf Bool.false D s Exp.tR)] [hc (Tl chkf Bool.false D c Exp.tSyn)]
       [h1 (Tl chkf Bool.false D e1 (chkT r c))] [h2 (Tl chkf Bool.false D e2 (chkT s (negT c)))]
       :where [Bool.false D (Exp.h1 r s c e1 e2) Exp.tEmpty])
  (zRefl [D (List Exp)] [X Exp] [cd Exp] [r Exp] [e Exp] [hb (Eq (Option Exp) (baseCode X) (Option.some Exp cd))]
         [hr (Tl chkf Bool.false D r Exp.tR)] [he (Tl chkf Bool.false D e (chkT r cd))]
         :where [Bool.false D (Exp.refl X r e) X])
  (zInsp [D (List Exp)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp] [hr (Tl chkf Bool.false D r Exp.tR)]
         [hc (Tl chkf Bool.false D c Exp.tSyn)] [hX (Tl chkf Bool.true D X Exp.tUnit)]
         [h1 (Tl chkf Bool.false (List.cons Exp (chkT (Exp.var 0) (lift 1 0 c)) (List.cons Exp Exp.tR D)) t1 (lift 2 0 X))]
         [h2 (Tl chkf Bool.false (List.cons Exp (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))
                                             (List.cons Exp Exp.tR D)) t2 (lift 2 0 X))]
         :where [Bool.false D (Exp.insp X r c t1 t2) X]))

;; ---------------------------------------------------------------------------
;; Runtime typing (Rt).  `Rt chkf D us t A` is `Γ ⊢ t :¹ A` with Γ = (D, us).

(a/defn lenU [x :- (List U)] Nat (List.length U x))
(a/defn lenE [x :- (List Exp)] Nat (List.length Exp x))
(a/defn consU [r :- U, x :- (List U)] (List U) (List.cons U r x))
(a/defn consE [A :- Exp, x :- (List Exp)] (List Exp) (List.cons Exp A x))

(a/inductive Rt [chkf (=> Code Code Bool)] :in Prop :indices [D (List Exp), us (List U), t Exp, A Exp]
  ;; axioms: any usages, of the right length
  (rVar [D (List Exp)] [us (List U)] [i Nat] [A Exp] [r U] [hl (Eq Nat (lenU us) (lenE D))]
        [hA (Eq (Option Exp) (nthE D i) (Option.some Exp A))] [hu (Eq (Option U) (nthU us i) (Option.some U r))]
        [hr (Eq Bool (nonzero r) Bool.true)] :where [D us (Exp.var i) (lift (+ i 1) 0 A)])
  (rConst [D (List Exp)] [us (List U)] [t Exp] [A Exp] [hl (Eq Nat (lenU us) (lenE D))]
          [h (Eq Bool (constTyped t A) Bool.true)] :where [D us t A])
  ;; functions and pairs
  (rLam [D (List Exp)] [us (List U)] [r U] [A Exp] [t Exp] [B Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)]
        [ht (Rt chkf (consE A D) (consU r us) t B)] :where [D us (Exp.lam r A t) (Exp.tPi r A B)])
  (rApp0 [D (List Exp)] [us (List U)] [f Exp] [u Exp] [A Exp] [B Exp] [hf (Rt chkf D us f (Exp.tPi U.u0 A B))]
         [hu (Tl chkf Bool.false D u A)] [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)] :where [D us (Exp.app f u) (subst1 u B)])
  (rApp [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [f Exp] [u Exp] [A Exp] [B Exp]
        [hr (Eq Bool (nonzero r) Bool.true)] [hf (Rt chkf D us1 f (Exp.tPi r A B))] [hu (Rt chkf D us2 u A)]
        [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)] :where [D (vadd us1 (vscale r us2)) (Exp.app f u) (subst1 u B)])
  (rPair0 [D (List Exp)] [us (List U)] [A Exp] [B Exp] [x Exp] [y Exp] [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
          [hx (Tl chkf Bool.false D x A)] [hy (Rt chkf D us y (subst1 x B))]
          :where [D us (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B)])
  (rPair [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [A Exp] [B Exp] [x Exp] [y Exp]
         [hr (Eq Bool (nonzero r) Bool.true)] [hA (Tl chkf Bool.true D A Exp.tUnit)]
         [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
         [hx (Rt chkf D us1 x A)] [hy (Rt chkf D us2 y (subst1 x B))]
         :where [D (vadd (vscale r us1) us2) (Exp.pair (Exp.tSig r A B) x y) (Exp.tSig r A B)])
  (rLet [D (List Exp)] [us1 (List U)] [us2 (List U)] [r U] [A Exp] [B Exp] [C Exp] [p Exp] [t Exp]
        [hp (Rt chkf D us1 p (Exp.tSig r A B))] [hC (Tl chkf Bool.true D C Exp.tUnit)]
        [hA (Tl chkf Bool.true D A Exp.tUnit)] [hB (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit)]
        [ht (Rt chkf (consE B (consE A D)) (consU U.u1 (consU r us2)) t (lift 2 0 C))]
        :where [D (vadd us1 us2) (Exp.letp C p t) C])
  (rAbort [D (List Exp)] [us (List U)] [A Exp] [t Exp] [ht (Rt chkf D us t Exp.tEmpty)] [hA (Tl chkf Bool.true D A Exp.tUnit)]
          :where [D us (Exp.abort A t) A])
  (rConv [D (List Exp)] [us (List U)] [t Exp] [A Exp] [B Exp] [ht (Rt chkf D us t A)] [hB (Tl chkf Bool.true D B Exp.tUnit)]
         [hc (Cv chkf (skels D) A B)] :where [D us t B])
  ;; data
  (rIte [D (List Exp)] [us1 (List U)] [us2 (List U)] [b Exp] [t Exp] [e Exp] [C Exp] [hb (Rt chkf D us1 b Exp.tBool)]
        [ht (Rt chkf D us2 t C)] [he (Rt chkf D us2 e C)] :where [D (vadd us1 us2) (Exp.ite b t e) C])
  (rElimB [D (List Exp)] [us1 (List U)] [us2 (List U)] [P Exp] [b Exp] [t Exp] [e Exp] [hb (Rt chkf D us1 b Exp.tBool)]
          [hP (Tl chkf Bool.true (consE Exp.tBool D) P Exp.tUnit)] [ht (Rt chkf D us2 t (subst1 Exp.tt P))]
          [he (Rt chkf D us2 e (subst1 Exp.ff P))] :where [D (vadd us1 us2) (Exp.elimB P b t e) (subst1 b P)])
  (rSucc [D (List Exp)] [us (List U)] [n Exp] [h (Rt chkf D us n Exp.tNat)] :where [D us (Exp.succ n) Exp.tNat])
  (rRecN [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [P Exp] [z Exp] [s Exp] [n Exp]
         [hn (Rt chkf D us1 n Exp.tNat)] [hP (Tl chkf Bool.true (consE Exp.tNat D) P Exp.tUnit)]
         [hz (Rt chkf D us2 z (subst1 Exp.zero P))]
         [hs (Rt chkf (consE P (consE Exp.tNat D)) (consU U.u1 (consU U.uw (vscale U.uw us3))) s (stepTy P))]
         :where [D (vadd us1 (vadd us2 (vscale U.uw us3))) (Exp.recN P z s n) (subst1 n P)])
  (rCaseL [D (List Exp)] [us1 (List U)] [us2 (List U)] [P Exp] [x Exp] [bs Exp] [hx (Rt chkf D us1 x Exp.tLbl)]
          [hP (Tl chkf Bool.true (consE Exp.tLbl D) P Exp.tUnit)] [hb (Rt chkf D us2 bs (Exp.tBrs P 0))]
          :where [D (vadd us1 us2) (Exp.caseL P x bs) (subst1 x P)])
  (rBnil [D (List Exp)] [us (List U)] [P Exp] [hl (Eq Nat (lenU us) (lenE D))] :where [D us Exp.bnil (Exp.tBrs P (NL))])
  (rBcons [D (List Exp)] [us (List U)] [P Exp] [k Nat] [h Exp] [t Exp] [hh (Rt chkf D us h (subst1 (Exp.lbl k) P))]
          [ht (Rt chkf D us t (Exp.tBrs P (+ k 1)))] [hP (Tl chkf Bool.true (consE Exp.tLbl D) P Exp.tUnit)]
          :where [D us (Exp.bcons h t) (Exp.tBrs P k)])
  (rSleaf [D (List Exp)] [us (List U)] [x Exp] [h (Rt chkf D us x Exp.tLbl)] :where [D us (Exp.sleaf x) Exp.tSyn])
  (rSnode [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [x Exp] [c1 Exp] [c2 Exp]
          [hx (Rt chkf D us1 x Exp.tLbl)] [h1 (Rt chkf D us2 c1 Exp.tSyn)] [h2 (Rt chkf D us3 c2 Exp.tSyn)]
          :where [D (vadd us1 (vadd us2 us3)) (Exp.snode x c1 c2) Exp.tSyn])
  (rRecS [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [P Exp] [tl Exp] [tn Exp] [c Exp]
         [hc (Rt chkf D us1 c Exp.tSyn)] [hP (Tl chkf Bool.true (consE Exp.tSyn D) P Exp.tUnit)]
         [hl (Rt chkf (consE Exp.tLbl D) (consU U.uw (vscale U.uw us2)) tl (leafTy P))]
         [hn (Rt chkf (consE (y2Ty P) (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))))
                  (consU U.u1 (consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw us3)))))) tn (nodeTy P))]
         [hY1 (Tl chkf Bool.true (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D))) (y1Ty P) Exp.tUnit)]
         [hY2 (Tl chkf Bool.true (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))) (y2Ty P) Exp.tUnit)]
         :where [D (vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3))) (Exp.recS P tl tn c) (subst1 c P)])
  ;; tokens and certificates
  (rLeaf [D (List Exp)] [us (List U)] [x Exp] [h (Rt chkf D us x Exp.tLbl)] :where [D us (Exp.leaf x) Exp.tR])
  (rNode [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [us4 (List U)] [d Exp] [x Exp] [r1 Exp] [r2 Exp]
         [hd (Rt chkf D us1 d Exp.tDia)] [hx (Rt chkf D us2 x Exp.tLbl)] [h1 (Rt chkf D us3 r1 Exp.tR)]
         [h2 (Rt chkf D us4 r2 Exp.tR)] :where [D (vadd us1 (vadd us2 (vadd us3 us4))) (Exp.node d x r1 r2) Exp.tR])
  (rItR [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [X Exp] [g Exp] [h Exp] [r Exp]
        [hX (Tl chkf Bool.true D X Exp.tUnit)] [hg (Rt chkf D (vscale U.uw us1) g (gTy X))]
        [hh (Rt chkf D (vscale U.uw us2) h (hTy X))] [hr (Rt chkf D us3 r Exp.tR)]
        :where [D (vadd (vscale U.uw us1) (vadd (vscale U.uw us2) us3)) (Exp.itR X g h r) X])
  (rPrn [D (List Exp)] [us (List U)] [r Exp] [h (Rt chkf D us r Exp.tR)] :where [D us (Exp.prn r) Exp.tSyn])
  ;; the checker and the self-reference constants
  (rChk [D (List Exp)] [us1 (List U)] [us2 (List U)] [c Exp] [d Exp] [hc (Rt chkf D us1 c Exp.tSyn)]
        [hd (Rt chkf D us2 d Exp.tSyn)] :where [D (vadd us1 us2) (Exp.chk c d) Exp.tBool])
  (rH1 [D (List Exp)] [us1 (List U)] [us2 (List U)] [us3 (List U)] [us4 (List U)] [us5 (List U)]
       [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp]
       [hr (Rt chkf D us1 r Exp.tR)] [hs (Rt chkf D us2 s Exp.tR)] [hc (Rt chkf D (vscale U.uw us3) c Exp.tSyn)]
       [h1 (Rt chkf D us4 e1 (chkT r c))] [h2 (Rt chkf D us5 e2 (chkT s (negT c)))]
       :where [D (vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) (Exp.h1 r s c e1 e2) Exp.tEmpty])
  (rRefl [D (List Exp)] [us1 (List U)] [us2 (List U)] [X Exp] [cd Exp] [r Exp] [e Exp]
         [hb (Eq (Option Exp) (baseCode X) (Option.some Exp cd))] [hr (Rt chkf D us1 r Exp.tR)]
         [he (Rt chkf D us2 e (chkT r cd))] :where [D (vadd us1 us2) (Exp.refl X r e) X])
  (rInsp [D (List Exp)] [us1 (List U)] [us0 (List U)] [us2 (List U)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]
         [hr (Rt chkf D us1 r Exp.tR)] [hc (Rt chkf D (vscale U.uw us0) c Exp.tSyn)] [hX (Tl chkf Bool.true D X Exp.tUnit)]
         [hF1 (Tl chkf Bool.true (List.cons Exp Exp.tR D) (chkT (Exp.var 0) (lift 1 0 c)) Exp.tUnit)]
         [hF2 (Tl chkf Bool.true (List.cons Exp Exp.tR D) (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) Exp.tUnit)]
         [h1 (Rt chkf (consE (chkT (Exp.var 0) (lift 1 0 c)) (consE Exp.tR D)) (consU U.u1 (consU U.u1 us2)) t1 (lift 2 0 X))]
         [h2 (Rt chkf (consE (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) (consE Exp.tR D))
                  (consU U.u1 (consU U.u1 us2)) t2 (lift 2 0 X))]
         :where [D (vadd us1 (vadd (vscale U.uw us0) us2)) (Exp.insp X r c t1 t2) X]))

;; WFCtx D: the context is well-formed, each entry a formed type over the
;; entries after it (D lists the innermost entry first).  The paper presupposes
;; well-formed contexts; Rt does not enforce it, so Lemma 3.6 states it (its
;; Var case needs the entry's type formed to read V through the lift).
(kdef WFCtx (forall [chkf (=> Code Code Bool)] (=> (List Exp) Prop))
  (fn [chkf :- (=> Code Code Bool), D :- (List Exp)]
    (List.rec$1$0 Exp (fn [_ :- (List Exp)] Prop) True
      (fn [A :- Exp, rest :- (List Exp), ih :- Prop] (And (Tl chkf Bool.true rest A Exp.tUnit) ih)) D)))

;; Θₘ: m tokens, each at usage 1.
(a/defn thetaD [m :- Nat] (List Exp) (match m [zero (List.nil Exp)] [(succ k) (List.cons Exp Exp.tDia (thetaD k))]))
(a/defn thetaU [m :- Nat] (List U) (match m [zero (List.nil U)] [(succ k) (List.cons U U.u1 (thetaU k))]))
