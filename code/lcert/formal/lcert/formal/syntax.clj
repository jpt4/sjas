(ns lcert.formal.syntax
  "F1b — the syntax of λᶜᵉʳᵗ₀ (R4-metatheory.md §§1.2–1.3), deeply embedded.

  One inductive type `Exp` holds types and terms, as the encoding does (§1.6).
  Variables are de Bruijn indices.  Labels are natural numbers (positions in
  the fixed finite set L).  Binders, with the number of variables each binds:
    tPi r A B, tSig r A B, lam r A t   — B, t under 1
    elimB P b t e                     — P under 1 (the motive's x)
    recN P z s n                      — P under 1; s under 2 (x, y)
    caseL P a bs                      — P under 1
    recS P tl tn c                    — P under 1; tl under 1 (a); tn under 5
    letp C p t                        — t under 2 (x, y)
    insp X r c t1 t2                  — t1, t2 under 2 (x, e)
  Branch lists of caseL are encoded in Exp itself (bnil / bcons), so that
  recursion over Exp stays structural."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.usage]))

(a/inductive Exp []
  ;; types (§1.2)
  (tEmpty) (tUnit) (tBool) (tNat) (tLbl) (tSyn) (tDia) (tR)
  (tT [b Exp]) (tPi [r U] [A Exp] [B Exp]) (tSig [r U] [A Exp] [B Exp])
  ;; terms (§1.3)
  (var [i Nat]) (star) (abort [A Exp] [t Exp]) (tt) (ff)
  (ite [b Exp] [t Exp] [e Exp]) (elimB [P Exp] [b Exp] [t Exp] [e Exp])
  (zero) (succ [n Exp]) (recN [P Exp] [z Exp] [s Exp] [n Exp])
  (lbl [l Nat]) (caseL [P Exp] [a Exp] [bs Exp]) (bnil) (bcons [h Exp] [t Exp])
  (sleaf [a Exp]) (snode [a Exp] [c1 Exp] [c2 Exp]) (recS [P Exp] [tl Exp] [tn Exp] [c Exp])
  (leaf [a Exp]) (node [d Exp] [a Exp] [r1 Exp] [r2 Exp]) (itR [X Exp] [g Exp] [h Exp] [r Exp])
  (prn [r Exp])
  (lam [r U] [A Exp] [t Exp]) (app [f Exp] [u Exp]) (pair [S Exp] [a Exp] [b Exp])
  (letp [C Exp] [p Exp] [t Exp])
  (chk [c Exp] [d Exp]) (h1 [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp])
  (refl [D Exp] [r Exp] [e Exp]) (insp [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp]))

;; lift k c e: add k to every variable index ≥ c.  Defined as liftF k e c,
;; recursing on e alone (Ansatz recognizes a recursion as structural only when
;; the other arguments are unchanged, and the cutoff c grows under binders).
(a/defn liftF [k :- Nat, e :- Exp] (=> Nat Exp)
  (match e
    [tEmpty (fn [c :- Nat] Exp.tEmpty)]
    [tUnit (fn [c :- Nat] Exp.tUnit)]
    [tBool (fn [c :- Nat] Exp.tBool)]
    [tNat (fn [c :- Nat] Exp.tNat)]
    [tLbl (fn [c :- Nat] Exp.tLbl)]
    [tSyn (fn [c :- Nat] Exp.tSyn)]
    [tDia (fn [c :- Nat] Exp.tDia)]
    [tR (fn [c :- Nat] Exp.tR)]
    [(tT b) (fn [c :- Nat] (Exp.tT ((liftF k b) c)))]
    [(tPi r A B) (fn [c :- Nat] (Exp.tPi r ((liftF k A) c) ((liftF k B) (+ c 1))))]
    [(tSig r A B) (fn [c :- Nat] (Exp.tSig r ((liftF k A) c) ((liftF k B) (+ c 1))))]
    [(var i) (fn [c :- Nat] (if (< i c) (Exp.var i) (Exp.var (+ i k))))]
    [star (fn [c :- Nat] Exp.star)]
    [(abort A t) (fn [c :- Nat] (Exp.abort ((liftF k A) c) ((liftF k t) c)))]
    [tt (fn [c :- Nat] Exp.tt)]
    [ff (fn [c :- Nat] Exp.ff)]
    [(ite b t e) (fn [c :- Nat] (Exp.ite ((liftF k b) c) ((liftF k t) c) ((liftF k e) c)))]
    [(elimB P b t e) (fn [c :- Nat] (Exp.elimB ((liftF k P) (+ c 1)) ((liftF k b) c) ((liftF k t) c) ((liftF k e) c)))]
    [zero (fn [c :- Nat] Exp.zero)]
    [(succ n) (fn [c :- Nat] (Exp.succ ((liftF k n) c)))]
    [(recN P z s n) (fn [c :- Nat] (Exp.recN ((liftF k P) (+ c 1)) ((liftF k z) c) ((liftF k s) (+ c 2)) ((liftF k n) c)))]
    [(lbl l) (fn [c :- Nat] (Exp.lbl l))]
    [(caseL P x bs) (fn [c :- Nat] (Exp.caseL ((liftF k P) (+ c 1)) ((liftF k x) c) ((liftF k bs) c)))]
    [bnil (fn [c :- Nat] Exp.bnil)]
    [(bcons h t) (fn [c :- Nat] (Exp.bcons ((liftF k h) c) ((liftF k t) c)))]
    [(sleaf x) (fn [c :- Nat] (Exp.sleaf ((liftF k x) c)))]
    [(snode x c1 c2) (fn [c :- Nat] (Exp.snode ((liftF k x) c) ((liftF k c1) c) ((liftF k c2) c)))]
    [(recS P tl tn x) (fn [c :- Nat] (Exp.recS ((liftF k P) (+ c 1)) ((liftF k tl) (+ c 1)) ((liftF k tn) (+ c 5)) ((liftF k x) c)))]
    [(leaf x) (fn [c :- Nat] (Exp.leaf ((liftF k x) c)))]
    [(node d x r1 r2) (fn [c :- Nat] (Exp.node ((liftF k d) c) ((liftF k x) c) ((liftF k r1) c) ((liftF k r2) c)))]
    [(itR X g h r) (fn [c :- Nat] (Exp.itR ((liftF k X) c) ((liftF k g) c) ((liftF k h) c) ((liftF k r) c)))]
    [(prn r) (fn [c :- Nat] (Exp.prn ((liftF k r) c)))]
    [(lam r A t) (fn [c :- Nat] (Exp.lam r ((liftF k A) c) ((liftF k t) (+ c 1))))]
    [(app f u) (fn [c :- Nat] (Exp.app ((liftF k f) c) ((liftF k u) c)))]
    [(pair S x y) (fn [c :- Nat] (Exp.pair ((liftF k S) c) ((liftF k x) c) ((liftF k y) c)))]
    [(letp C p t) (fn [c :- Nat] (Exp.letp ((liftF k C) c) ((liftF k p) c) ((liftF k t) (+ c 2))))]
    [(chk x d) (fn [c :- Nat] (Exp.chk ((liftF k x) c) ((liftF k d) c)))]
    [(h1 r s x e1 e2) (fn [c :- Nat] (Exp.h1 ((liftF k r) c) ((liftF k s) c) ((liftF k x) c) ((liftF k e1) c) ((liftF k e2) c)))]
    [(refl D r e) (fn [c :- Nat] (Exp.refl ((liftF k D) c) ((liftF k r) c) ((liftF k e) c)))]
    [(insp X r x t1 t2) (fn [c :- Nat] (Exp.insp ((liftF k X) c) ((liftF k r) c) ((liftF k x) c) ((liftF k t1) (+ c 2)) ((liftF k t2) (+ c 2))))]))

(a/defn lift [k :- Nat, c :- Nat, e :- Exp] Exp ((liftF k e) c))

;; Parallel substitution.  A substitution is a function σ : Nat → Exp giving
;; the replacement of each variable.  Under a binder it is lifted: variable 0
;; stays, and variable i + 1 becomes σ i lifted by one (up); under n binders,
;; upn n.

(a/defn up [s :- (=> Nat Exp), i :- Nat] Exp
  (match i
    [zero (Exp.var 0)]
    [(succ j) (lift 1 0 (s j))]))

(a/defn upn [n :- Nat, s :- (=> Nat Exp)] (=> Nat Exp)
  (match n
    [zero s]
    [(succ m) (fn [i :- Nat] (up (upn m s) i))]))

(a/defn substF [e :- Exp] (=> (=> Nat Exp) Exp)
  (match e
    [tEmpty (fn [sg :- (=> Nat Exp)] Exp.tEmpty)]
    [tUnit (fn [sg :- (=> Nat Exp)] Exp.tUnit)]
    [tBool (fn [sg :- (=> Nat Exp)] Exp.tBool)]
    [tNat (fn [sg :- (=> Nat Exp)] Exp.tNat)]
    [tLbl (fn [sg :- (=> Nat Exp)] Exp.tLbl)]
    [tSyn (fn [sg :- (=> Nat Exp)] Exp.tSyn)]
    [tDia (fn [sg :- (=> Nat Exp)] Exp.tDia)]
    [tR (fn [sg :- (=> Nat Exp)] Exp.tR)]
    [(tT b) (fn [sg :- (=> Nat Exp)] (Exp.tT ((substF b) sg)))]
    [(tPi r A B) (fn [sg :- (=> Nat Exp)] (Exp.tPi r ((substF A) sg) ((substF B) (upn 1 sg))))]
    [(tSig r A B) (fn [sg :- (=> Nat Exp)] (Exp.tSig r ((substF A) sg) ((substF B) (upn 1 sg))))]
    [(var i) (fn [sg :- (=> Nat Exp)] (sg i))]
    [star (fn [sg :- (=> Nat Exp)] Exp.star)]
    [(abort A t) (fn [sg :- (=> Nat Exp)] (Exp.abort ((substF A) sg) ((substF t) sg)))]
    [tt (fn [sg :- (=> Nat Exp)] Exp.tt)]
    [ff (fn [sg :- (=> Nat Exp)] Exp.ff)]
    [(ite b t e) (fn [sg :- (=> Nat Exp)] (Exp.ite ((substF b) sg) ((substF t) sg) ((substF e) sg)))]
    [(elimB P b t e) (fn [sg :- (=> Nat Exp)] (Exp.elimB ((substF P) (upn 1 sg)) ((substF b) sg) ((substF t) sg) ((substF e) sg)))]
    [zero (fn [sg :- (=> Nat Exp)] Exp.zero)]
    [(succ n) (fn [sg :- (=> Nat Exp)] (Exp.succ ((substF n) sg)))]
    [(recN P z s n) (fn [sg :- (=> Nat Exp)] (Exp.recN ((substF P) (upn 1 sg)) ((substF z) sg) ((substF s) (upn 2 sg)) ((substF n) sg)))]
    [(lbl l) (fn [sg :- (=> Nat Exp)] (Exp.lbl l))]
    [(caseL P x bs) (fn [sg :- (=> Nat Exp)] (Exp.caseL ((substF P) (upn 1 sg)) ((substF x) sg) ((substF bs) sg)))]
    [bnil (fn [sg :- (=> Nat Exp)] Exp.bnil)]
    [(bcons h t) (fn [sg :- (=> Nat Exp)] (Exp.bcons ((substF h) sg) ((substF t) sg)))]
    [(sleaf x) (fn [sg :- (=> Nat Exp)] (Exp.sleaf ((substF x) sg)))]
    [(snode x c1 c2) (fn [sg :- (=> Nat Exp)] (Exp.snode ((substF x) sg) ((substF c1) sg) ((substF c2) sg)))]
    [(recS P tl tn x) (fn [sg :- (=> Nat Exp)] (Exp.recS ((substF P) (upn 1 sg)) ((substF tl) (upn 1 sg)) ((substF tn) (upn 5 sg)) ((substF x) sg)))]
    [(leaf x) (fn [sg :- (=> Nat Exp)] (Exp.leaf ((substF x) sg)))]
    [(node d x r1 r2) (fn [sg :- (=> Nat Exp)] (Exp.node ((substF d) sg) ((substF x) sg) ((substF r1) sg) ((substF r2) sg)))]
    [(itR X g h r) (fn [sg :- (=> Nat Exp)] (Exp.itR ((substF X) sg) ((substF g) sg) ((substF h) sg) ((substF r) sg)))]
    [(prn r) (fn [sg :- (=> Nat Exp)] (Exp.prn ((substF r) sg)))]
    [(lam r A t) (fn [sg :- (=> Nat Exp)] (Exp.lam r ((substF A) sg) ((substF t) (upn 1 sg))))]
    [(app f u) (fn [sg :- (=> Nat Exp)] (Exp.app ((substF f) sg) ((substF u) sg)))]
    [(pair S x y) (fn [sg :- (=> Nat Exp)] (Exp.pair ((substF S) sg) ((substF x) sg) ((substF y) sg)))]
    [(letp C p t) (fn [sg :- (=> Nat Exp)] (Exp.letp ((substF C) sg) ((substF p) sg) ((substF t) (upn 2 sg))))]
    [(chk x d) (fn [sg :- (=> Nat Exp)] (Exp.chk ((substF x) sg) ((substF d) sg)))]
    [(h1 r s x e1 e2) (fn [sg :- (=> Nat Exp)] (Exp.h1 ((substF r) sg) ((substF s) sg) ((substF x) sg) ((substF e1) sg) ((substF e2) sg)))]
    [(refl D r e) (fn [sg :- (=> Nat Exp)] (Exp.refl ((substF D) sg) ((substF r) sg) ((substF e) sg)))]
    [(insp X r x t1 t2) (fn [sg :- (=> Nat Exp)] (Exp.insp ((substF X) sg) ((substF r) sg) ((substF x) sg) ((substF t1) (upn 2 sg)) ((substF t2) (upn 2 sg))))]))


(a/defn subst [s :- (=> Nat Exp), e :- Exp] Exp ((substF e) s))

;; Single substitution t[u/0], as used by β and by B[u/x]: variable 0 becomes
;; u, and variable i + 1 becomes i.
(a/defn inst1 [u :- Exp, i :- Nat] Exp
  (match i
    [zero u]
    [(succ j) (Exp.var j)]))

(a/defn subst1 [u :- Exp, e :- Exp] Exp (subst (fn [i :- Nat] (inst1 u i)) e))

;; Two-variable substitution t[a/0, b/1]... for Let and RecN's step:
;; variable 0 becomes u0, variable 1 becomes u1, and i + 2 becomes i.
(a/defn inst2 [u0 :- Exp, u1 :- Exp, i :- Nat] Exp
  (match i
    [zero u0]
    [(succ j) (inst1 u1 j)]))

(thm lift_var_below [k :- Nat, c :- Nat, i :- Nat, h :- (< i c)] (= (lift k c (Exp.var i)) (Exp.var i))
  (simp [lift liftF])
  (intro hn)
  (exfalso)
  (exact (hn h)))

(thm subst_var [s :- (=> Nat Exp), i :- Nat] (= (subst s (Exp.var i)) (s i))
  (rfl))
(thm subst1_var0 [u :- Exp] (= (subst1 u (Exp.var 0)) u) (rfl))
(thm subst1_under_binder [u :- Exp, A :- Exp] (= (subst1 u (Exp.lam U.u1 A (Exp.var 0))) (Exp.lam U.u1 (subst1 u A) (Exp.var 0)))
  (rfl))

(thm lift_var_above [k :- Nat, c :- Nat, i :- Nat, h :- (<= c i)] (= (lift k c (Exp.var i)) (Exp.var (+ i k)))
  (simp [lift liftF])
  (intro hlt)
  (exfalso)
  (omega))
