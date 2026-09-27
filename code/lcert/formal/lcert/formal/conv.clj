(ns lcert.formal.conv
  "F2b — reduction steps, positions, simple typing and conversion
  (R4-metatheory.md §1.5, Lemma 2.5).

  Head steps (Hd chkf e e'):
    β   (λ(x:ρA).t) u ⇝ t[u/x];  let (x,y) = (a,b) in t ⇝ t[a/x, b/y]
    ι   if, elimBool, recN, caseLbl, recSyn, itR and print on constructor
        forms
    δ   chk′ c d ⇝ Check(c, d), when c and d are closed canonical codes; the
        checker is the parameter chkf
    T   T(tt) ⇝ 1 and T(ff) ⇝ 0
  A step rewrites one head redex at one position: `getP` and `setP` read and
  replace the subterm at a path of child indices.  So one constructor gives the
  compatible closure.

  Simple typing at skeletons (SkTy).  §1.5 takes conversions only between
  skeleton-typed expressions: every intermediate expression of a chain must be
  simply typed at the skeleton of its endpoints (review R4-01).  SkTy is that
  simple typing, over a list of skeletons, the innermost first."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]))

;; Substituting a list: variable i becomes the i-th element, and variables
;; past the end are renumbered down by the list's length.
(a/defn instL [us :- (List Exp), i :- Nat] Exp
  (match us
    [nil (Exp.var i)]
    [(cons u rest) (match i [zero u] [(succ j) (instL rest j)])]))

(a/defn substL [us :- (List Exp), e :- Exp] Exp (subst (fn [i :- Nat] (instL us i)) e))

;; The l-th branch of a caseLbl branch list.
(a/defn nthB [bs :- Exp, l :- Nat] (Option Exp)
  (match bs
    [(bcons h t) (match l [zero (Option.some h)] [(succ m) (nthB t m)])]
    [_ Option.none]))

;; The Boolean constant for a Boolean.
(a/defn boolExp [b :- Bool] Exp (if b Exp.tt Exp.ff))

(a/inductive Hd [chkf (=> Code Code Bool)] :in Prop :indices [e Exp, e2 Exp]
  (beta [r U] [A Exp] [t Exp] [u Exp] :where [(Exp.app (Exp.lam r A t) u) (subst1 u t)])
  (betaLet [C Exp] [S Exp] [x Exp] [y Exp] [t Exp]
    :where [(Exp.letp C (Exp.pair S x y) t) (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)])
  (iteT [t Exp] [e Exp] :where [(Exp.ite Exp.tt t e) t])
  (iteF [t Exp] [e Exp] :where [(Exp.ite Exp.ff t e) e])
  (elimT [P Exp] [t Exp] [e Exp] :where [(Exp.elimB P Exp.tt t e) t])
  (elimF [P Exp] [t Exp] [e Exp] :where [(Exp.elimB P Exp.ff t e) e])
  (recNZ [P Exp] [z Exp] [s Exp] :where [(Exp.recN P z s Exp.zero) z])
  (recNS [P Exp] [z Exp] [s Exp] [n Exp]
    :where [(Exp.recN P z s (Exp.succ n)) (substL (List.cons Exp (Exp.recN P z s n) (List.cons Exp n (List.nil Exp))) s)])
  (caseLb [P Exp] [l Nat] [bs Exp] [b Exp] [h (Eq (Option Exp) (nthB bs l) (Option.some Exp b))]
    :where [(Exp.caseL P (Exp.lbl l) bs) b])
  (recSL [P Exp] [tl Exp] [tn Exp] [x Exp] :where [(Exp.recS P tl tn (Exp.sleaf x)) (subst1 x tl)])
  (recSN [P Exp] [tl Exp] [tn Exp] [x Exp] [c1 Exp] [c2 Exp]
    :where [(Exp.recS P tl tn (Exp.snode x c1 c2))
            (substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
                     (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn)])
  (itRL [X Exp] [g Exp] [h Exp] [x Exp] :where [(Exp.itR X g h (Exp.leaf x)) (Exp.app g x)])
  (itRN [X Exp] [g Exp] [h Exp] [d Exp] [x Exp] [r1 Exp] [r2 Exp]
    :where [(Exp.itR X g h (Exp.node d x r1 r2))
            (Exp.app (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Exp.itR X g h r2))])
  (prnL [x Exp] :where [(Exp.prn (Exp.leaf x)) (Exp.sleaf x)])
  (prnN [d Exp] [x Exp] [r1 Exp] [r2 Exp]
    :where [(Exp.prn (Exp.node d x r1 r2)) (Exp.snode x (Exp.prn r1) (Exp.prn r2))])
  (delta [c Exp] [d Exp] [cc Code] [dc Code] [hc (Eq (Option Code) (codeOf c) (Option.some Code cc))] [hd (Eq (Option Code) (codeOf d) (Option.some Code dc))]
    :where [(Exp.chk c d) (boolExp (chkf cc dc))])
  (tTT :where [(Exp.tT Exp.tt) Exp.tUnit])
  (tTF :where [(Exp.tT Exp.ff) Exp.tEmpty]))

;; --- positions (generated from Exp's constructor table) ---------------

(a/defn child [e :- Exp, i :- Nat] (Option Exp)
  (match e
    [tEmpty (Option.none Exp)]
    [tUnit (Option.none Exp)]
    [tBool (Option.none Exp)]
    [tNat (Option.none Exp)]
    [tLbl (Option.none Exp)]
    [tSyn (Option.none Exp)]
    [tDia (Option.none Exp)]
    [tR (Option.none Exp)]
    [(tT b) (if (= i 0) (Option.some Exp b) (Option.none Exp))]
    [(tPi r A B) (if (= i 0) (Option.some Exp A) (if (= i 1) (Option.some Exp B) (Option.none Exp)))]
    [(tSig r A B) (if (= i 0) (Option.some Exp A) (if (= i 1) (Option.some Exp B) (Option.none Exp)))]
    [(var i) (Option.none Exp)]
    [star (Option.none Exp)]
    [(abort A t) (if (= i 0) (Option.some Exp A) (if (= i 1) (Option.some Exp t) (Option.none Exp)))]
    [tt (Option.none Exp)]
    [ff (Option.none Exp)]
    [(ite b t e) (if (= i 0) (Option.some Exp b) (if (= i 1) (Option.some Exp t) (if (= i 2) (Option.some Exp e) (Option.none Exp))))]
    [(elimB P b t e) (if (= i 0) (Option.some Exp P) (if (= i 1) (Option.some Exp b) (if (= i 2) (Option.some Exp t) (if (= i 3) (Option.some Exp e) (Option.none Exp)))))]
    [zero (Option.none Exp)]
    [(succ n) (if (= i 0) (Option.some Exp n) (Option.none Exp))]
    [(recN P z s n) (if (= i 0) (Option.some Exp P) (if (= i 1) (Option.some Exp z) (if (= i 2) (Option.some Exp s) (if (= i 3) (Option.some Exp n) (Option.none Exp)))))]
    [(lbl l) (Option.none Exp)]
    [(caseL P a bs) (if (= i 0) (Option.some Exp P) (if (= i 1) (Option.some Exp a) (if (= i 2) (Option.some Exp bs) (Option.none Exp))))]
    [bnil (Option.none Exp)]
    [(bcons h t) (if (= i 0) (Option.some Exp h) (if (= i 1) (Option.some Exp t) (Option.none Exp)))]
    [(sleaf a) (if (= i 0) (Option.some Exp a) (Option.none Exp))]
    [(snode a c1 c2) (if (= i 0) (Option.some Exp a) (if (= i 1) (Option.some Exp c1) (if (= i 2) (Option.some Exp c2) (Option.none Exp))))]
    [(recS P tl tn c) (if (= i 0) (Option.some Exp P) (if (= i 1) (Option.some Exp tl) (if (= i 2) (Option.some Exp tn) (if (= i 3) (Option.some Exp c) (Option.none Exp)))))]
    [(leaf a) (if (= i 0) (Option.some Exp a) (Option.none Exp))]
    [(node d a r1 r2) (if (= i 0) (Option.some Exp d) (if (= i 1) (Option.some Exp a) (if (= i 2) (Option.some Exp r1) (if (= i 3) (Option.some Exp r2) (Option.none Exp)))))]
    [(itR X g h r) (if (= i 0) (Option.some Exp X) (if (= i 1) (Option.some Exp g) (if (= i 2) (Option.some Exp h) (if (= i 3) (Option.some Exp r) (Option.none Exp)))))]
    [(prn r) (if (= i 0) (Option.some Exp r) (Option.none Exp))]
    [(lam r A t) (if (= i 0) (Option.some Exp A) (if (= i 1) (Option.some Exp t) (Option.none Exp)))]
    [(app f u) (if (= i 0) (Option.some Exp f) (if (= i 1) (Option.some Exp u) (Option.none Exp)))]
    [(pair S a b) (if (= i 0) (Option.some Exp S) (if (= i 1) (Option.some Exp a) (if (= i 2) (Option.some Exp b) (Option.none Exp))))]
    [(letp C p t) (if (= i 0) (Option.some Exp C) (if (= i 1) (Option.some Exp p) (if (= i 2) (Option.some Exp t) (Option.none Exp))))]
    [(chk c d) (if (= i 0) (Option.some Exp c) (if (= i 1) (Option.some Exp d) (Option.none Exp)))]
    [(h1 r s c e1 e2) (if (= i 0) (Option.some Exp r) (if (= i 1) (Option.some Exp s) (if (= i 2) (Option.some Exp c) (if (= i 3) (Option.some Exp e1) (if (= i 4) (Option.some Exp e2) (Option.none Exp))))))]
    [(refl D r e) (if (= i 0) (Option.some Exp D) (if (= i 1) (Option.some Exp r) (if (= i 2) (Option.some Exp e) (Option.none Exp))))]
    [(insp X r c t1 t2) (if (= i 0) (Option.some Exp X) (if (= i 1) (Option.some Exp r) (if (= i 2) (Option.some Exp c) (if (= i 3) (Option.some Exp t1) (if (= i 4) (Option.some Exp t2) (Option.none Exp))))))]
    [(tBrs P k) (if (= i 0) (Option.some Exp P) (Option.none Exp))]))

(a/defn setKid [e :- Exp, i :- Nat, x :- Exp] Exp
  (match e
    [tEmpty Exp.tEmpty]
    [tUnit Exp.tUnit]
    [tBool Exp.tBool]
    [tNat Exp.tNat]
    [tLbl Exp.tLbl]
    [tSyn Exp.tSyn]
    [tDia Exp.tDia]
    [tR Exp.tR]
    [(tT b) (Exp.tT (if (= i 0) x b))]
    [(tPi r A B) (Exp.tPi r (if (= i 0) x A) (if (= i 1) x B))]
    [(tSig r A B) (Exp.tSig r (if (= i 0) x A) (if (= i 1) x B))]
    [(var i) (Exp.var i)]
    [star Exp.star]
    [(abort A t) (Exp.abort (if (= i 0) x A) (if (= i 1) x t))]
    [tt Exp.tt]
    [ff Exp.ff]
    [(ite b t e) (Exp.ite (if (= i 0) x b) (if (= i 1) x t) (if (= i 2) x e))]
    [(elimB P b t e) (Exp.elimB (if (= i 0) x P) (if (= i 1) x b) (if (= i 2) x t) (if (= i 3) x e))]
    [zero Exp.zero]
    [(succ n) (Exp.succ (if (= i 0) x n))]
    [(recN P z s n) (Exp.recN (if (= i 0) x P) (if (= i 1) x z) (if (= i 2) x s) (if (= i 3) x n))]
    [(lbl l) (Exp.lbl l)]
    [(caseL P a bs) (Exp.caseL (if (= i 0) x P) (if (= i 1) x a) (if (= i 2) x bs))]
    [bnil Exp.bnil]
    [(bcons h t) (Exp.bcons (if (= i 0) x h) (if (= i 1) x t))]
    [(sleaf a) (Exp.sleaf (if (= i 0) x a))]
    [(snode a c1 c2) (Exp.snode (if (= i 0) x a) (if (= i 1) x c1) (if (= i 2) x c2))]
    [(recS P tl tn c) (Exp.recS (if (= i 0) x P) (if (= i 1) x tl) (if (= i 2) x tn) (if (= i 3) x c))]
    [(leaf a) (Exp.leaf (if (= i 0) x a))]
    [(node d a r1 r2) (Exp.node (if (= i 0) x d) (if (= i 1) x a) (if (= i 2) x r1) (if (= i 3) x r2))]
    [(itR X g h r) (Exp.itR (if (= i 0) x X) (if (= i 1) x g) (if (= i 2) x h) (if (= i 3) x r))]
    [(prn r) (Exp.prn (if (= i 0) x r))]
    [(lam r A t) (Exp.lam r (if (= i 0) x A) (if (= i 1) x t))]
    [(app f u) (Exp.app (if (= i 0) x f) (if (= i 1) x u))]
    [(pair S a b) (Exp.pair (if (= i 0) x S) (if (= i 1) x a) (if (= i 2) x b))]
    [(letp C p t) (Exp.letp (if (= i 0) x C) (if (= i 1) x p) (if (= i 2) x t))]
    [(chk c d) (Exp.chk (if (= i 0) x c) (if (= i 1) x d))]
    [(h1 r s c e1 e2) (Exp.h1 (if (= i 0) x r) (if (= i 1) x s) (if (= i 2) x c) (if (= i 3) x e1) (if (= i 4) x e2))]
    [(refl D r e) (Exp.refl (if (= i 0) x D) (if (= i 1) x r) (if (= i 2) x e))]
    [(insp X r c t1 t2) (Exp.insp (if (= i 0) x X) (if (= i 1) x r) (if (= i 2) x c) (if (= i 3) x t1) (if (= i 4) x t2))]
    [(tBrs P k) (Exp.tBrs (if (= i 0) x P) k)]))

;; Paths: getP p e reads the subterm at path p; setP p e x replaces it by x.
;; Both recurse on the path, returning functions of the term.
(a/defn getPF [p :- (List Nat)] (=> Exp (Option Exp))
  (match p
    [nil (fn [e :- Exp] (Option.some Exp e))]
    [(cons i q) (fn [e :- Exp] (match (child e i) [none (Option.none Exp)] [(some c) ((getPF q) c)]))]))

(a/defn setPF [p :- (List Nat)] (=> Exp Exp Exp)
  (match p
    [nil (fn [e :- Exp, x :- Exp] x)]
    [(cons i q) (fn [e :- Exp, x :- Exp]
                  (match (child e i) [none e] [(some c) (setKid e i ((setPF q) c x))]))]))

(a/defn getP [p :- (List Nat), e :- Exp] (Option Exp) ((getPF p) e))
(a/defn setP [p :- (List Nat), e :- Exp, x :- Exp] Exp ((setPF p) e x))

;; One step: a head step at some position.  Defined as an existential (the
;; inductive form trips Ansatz's recursor generator; no induction on steps is
;; needed).
(kdef Step (=> (=> Code Code Bool) Exp Exp Prop)
  (fn [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp]
    (Exists (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
      (And (Eq (Option Exp) (getP p e) (Option.some Exp r))
           (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2))))))))))))

;; --- simple typing at skeletons ----------------------------------------------

(a/defn nthS [G :- (List Sk), i :- Nat] (Option Sk)
  (match G
    [nil (Option.none Sk)]
    [(cons s rest) (match i [zero (Option.some Sk s)] [(succ j) (nthS rest j)])]))

(a/defn sk2 [s :- Sk, t :- Sk, G :- (List Sk)] (List Sk) (List.cons Sk s (List.cons Sk t G)))

;; SkJ Bool.true G A s: A is a skeleton-well-formed type (s is unit, unused).
;; SkJ Bool.false G t s: t is simply typed at skeleton s.  One family, since the
;; two judgments are mutually recursive.
(a/inductive SkJ [] :in Prop :indices [w Bool, G (List Sk), e Exp, s Sk]
  ;; types
  (wEmpty [G (List Sk)] :where [Bool.true G Exp.tEmpty Sk.unit])
  (wUnit [G (List Sk)] :where [Bool.true G Exp.tUnit Sk.unit])
  (wBool [G (List Sk)] :where [Bool.true G Exp.tBool Sk.unit])
  (wNat [G (List Sk)] :where [Bool.true G Exp.tNat Sk.unit])
  (wLbl [G (List Sk)] :where [Bool.true G Exp.tLbl Sk.unit])
  (wSyn [G (List Sk)] :where [Bool.true G Exp.tSyn Sk.unit])
  (wDia [G (List Sk)] :where [Bool.true G Exp.tDia Sk.unit])
  (wR [G (List Sk)] :where [Bool.true G Exp.tR Sk.unit])
  (wT [G (List Sk)] [b Exp] [hb (SkJ Bool.false G b Sk.bool)] :where [Bool.true G (Exp.tT b) Sk.unit])
  (wPi [G (List Sk)] [r U] [A Exp] [B Exp] [hA (SkJ Bool.true G A Sk.unit)]
       [hB (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit)] :where [Bool.true G (Exp.tPi r A B) Sk.unit])
  (wSig [G (List Sk)] [r U] [A Exp] [B Exp] [hA (SkJ Bool.true G A Sk.unit)]
        [hB (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit)] :where [Bool.true G (Exp.tSig r A B) Sk.unit])
  ;; terms
  (sVar [G (List Sk)] [i Nat] [s Sk] [h (Eq (Option Sk) (nthS G i) (Option.some Sk s))] :where [Bool.false G (Exp.var i) s])
  (sStar [G (List Sk)] :where [Bool.false G Exp.star Sk.unit])
  (sAbort [G (List Sk)] [A Exp] [t Exp] [hA (SkJ Bool.true G A Sk.unit)] [ht (SkJ Bool.false G t Sk.unit)]
          :where [Bool.false G (Exp.abort A t) (skel A)])
  (sTT [G (List Sk)] :where [Bool.false G Exp.tt Sk.bool])
  (sFF [G (List Sk)] :where [Bool.false G Exp.ff Sk.bool])
  (sIte [G (List Sk)] [b Exp] [t Exp] [e Exp] [s Sk] [hb (SkJ Bool.false G b Sk.bool)] [ht (SkJ Bool.false G t s)]
        [he (SkJ Bool.false G e s)] :where [Bool.false G (Exp.ite b t e) s])
  (sElimB [G (List Sk)] [P Exp] [b Exp] [t Exp] [e Exp] [hP (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit)]
          [hb (SkJ Bool.false G b Sk.bool)] [ht (SkJ Bool.false G t (skel P))] [he (SkJ Bool.false G e (skel P))]
          :where [Bool.false G (Exp.elimB P b t e) (skel P)])
  (sZero [G (List Sk)] :where [Bool.false G Exp.zero Sk.nat])
  (sSucc [G (List Sk)] [n Exp] [h (SkJ Bool.false G n Sk.nat)] :where [Bool.false G (Exp.succ n) Sk.nat])
  (sRecN [G (List Sk)] [P Exp] [z Exp] [st Exp] [n Exp] [hP (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit)]
         [hz (SkJ Bool.false G z (skel P))] [hs (SkJ Bool.false (sk2 (skel P) Sk.nat G) st (skel P))]
         [hn (SkJ Bool.false G n Sk.nat)] :where [Bool.false G (Exp.recN P z st n) (skel P)])
  (sLbl [G (List Sk)] [l Nat] :where [Bool.false G (Exp.lbl l) Sk.lbl])
  (sCaseL [G (List Sk)] [P Exp] [x Exp] [bs Exp] [hP (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit)]
          [hx (SkJ Bool.false G x Sk.lbl)] [hb (SkJ Bool.false G bs (skel P))] :where [Bool.false G (Exp.caseL P x bs) (skel P)])
  (sBnil [G (List Sk)] [s Sk] :where [Bool.false G Exp.bnil s])
  (sBcons [G (List Sk)] [h Exp] [t Exp] [s Sk] [hh (SkJ Bool.false G h s)] [ht (SkJ Bool.false G t s)]
          :where [Bool.false G (Exp.bcons h t) s])
  (sSleaf [G (List Sk)] [x Exp] [h (SkJ Bool.false G x Sk.lbl)] :where [Bool.false G (Exp.sleaf x) Sk.syn])
  (sSnode [G (List Sk)] [x Exp] [c1 Exp] [c2 Exp] [hx (SkJ Bool.false G x Sk.lbl)] [h1 (SkJ Bool.false G c1 Sk.syn)]
          [h2 (SkJ Bool.false G c2 Sk.syn)] :where [Bool.false G (Exp.snode x c1 c2) Sk.syn])
  (sRecS [G (List Sk)] [P Exp] [tl Exp] [tn Exp] [c Exp] [hP (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
         [hl (SkJ Bool.false (List.cons Sk Sk.lbl G) tl (skel P))]
         [hn (SkJ Bool.false (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn (skel P))]
         [hc (SkJ Bool.false G c Sk.syn)] :where [Bool.false G (Exp.recS P tl tn c) (skel P)])
  (sLeaf [G (List Sk)] [x Exp] [h (SkJ Bool.false G x Sk.lbl)] :where [Bool.false G (Exp.leaf x) Sk.cert])
  (sNode [G (List Sk)] [d Exp] [x Exp] [r1 Exp] [r2 Exp] [hd (SkJ Bool.false G d Sk.dia)] [hx (SkJ Bool.false G x Sk.lbl)]
         [h1 (SkJ Bool.false G r1 Sk.cert)] [h2 (SkJ Bool.false G r2 Sk.cert)] :where [Bool.false G (Exp.node d x r1 r2) Sk.cert])
  (sItR [G (List Sk)] [X Exp] [g Exp] [h Exp] [r Exp] [hX (SkJ Bool.true G X Sk.unit)]
        [hg (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))]
        [hh (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))]
        [hr (SkJ Bool.false G r Sk.cert)] :where [Bool.false G (Exp.itR X g h r) (skel X)])
  (sPrn [G (List Sk)] [r Exp] [h (SkJ Bool.false G r Sk.cert)] :where [Bool.false G (Exp.prn r) Sk.syn])
  (sLam [G (List Sk)] [r U] [A Exp] [t Exp] [s Sk] [hA (SkJ Bool.true G A Sk.unit)]
        [ht (SkJ Bool.false (List.cons Sk (skel A) G) t s)] :where [Bool.false G (Exp.lam r A t) (Sk.arr (skel A) s)])
  (sApp [G (List Sk)] [f Exp] [u Exp] [s Sk] [t Sk] [hf (SkJ Bool.false G f (Sk.arr s t))] [hu (SkJ Bool.false G u s)]
        :where [Bool.false G (Exp.app f u) t])
  (sPair [G (List Sk)] [r U] [A Exp] [B Exp] [x Exp] [y Exp] [hS (SkJ Bool.true G (Exp.tSig r A B) Sk.unit)]
         [hx (SkJ Bool.false G x (skel A))] [hy (SkJ Bool.false G y (skel B))]
         :where [Bool.false G (Exp.pair (Exp.tSig r A B) x y) (Sk.prod (skel A) (skel B))])
  (sLetp [G (List Sk)] [C Exp] [p Exp] [t Exp] [s1 Sk] [s2 Sk] [hC (SkJ Bool.true G C Sk.unit)]
         [hp (SkJ Bool.false G p (Sk.prod s1 s2))] [ht (SkJ Bool.false (sk2 s2 s1 G) t (skel C))]
         :where [Bool.false G (Exp.letp C p t) (skel C)])
  (sChk [G (List Sk)] [c Exp] [d Exp] [hc (SkJ Bool.false G c Sk.syn)] [hd (SkJ Bool.false G d Sk.syn)]
        :where [Bool.false G (Exp.chk c d) Sk.bool])
  (sH1 [G (List Sk)] [r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp] [hr (SkJ Bool.false G r Sk.cert)]
       [hs (SkJ Bool.false G s Sk.cert)] [hc (SkJ Bool.false G c Sk.syn)] [h1 (SkJ Bool.false G e1 Sk.unit)]
       [h2 (SkJ Bool.false G e2 Sk.unit)] :where [Bool.false G (Exp.h1 r s c e1 e2) Sk.unit])
  (sRefl [G (List Sk)] [D Exp] [r Exp] [e Exp] [hD (SkJ Bool.true G D Sk.unit)] [hr (SkJ Bool.false G r Sk.cert)]
         [he (SkJ Bool.false G e Sk.unit)] :where [Bool.false G (Exp.refl D r e) (skel D)])
  (sInsp [G (List Sk)] [X Exp] [r Exp] [c Exp] [t1 Exp] [t2 Exp] [hX (SkJ Bool.true G X Sk.unit)]
         [hr (SkJ Bool.false G r Sk.cert)] [hc (SkJ Bool.false G c Sk.syn)]
         [h1 (SkJ Bool.false (sk2 Sk.unit Sk.cert G) t1 (skel X))] [h2 (SkJ Bool.false (sk2 Sk.unit Sk.cert G) t2 (skel X))]
         :where [Bool.false G (Exp.insp X r c t1 t2) (skel X)]))

;; --- conversion ---------------------------------------------------------------

;; Cv chkf G A B: A and B are joined by a chain of steps, taken in either
;; direction, every element of which is a skeleton-well-formed type in G.
(a/inductive Cv [chkf (=> Code Code Bool), G (List Sk)] :in Prop :indices [A Exp, B Exp]
  (cvRefl [A Exp] [h (SkJ Bool.true G A Sk.unit)] :where [A A])
  (cvFwd [A Exp] [B Exp] [C Exp] [hab (Cv chkf G A B)] [hs (Step chkf B C)] [hc (SkJ Bool.true G C Sk.unit)] :where [A C])
  (cvBwd [A Exp] [B Exp] [C Exp] [hab (Cv chkf G A B)] [hs (Step chkf C B)] [hc (SkJ Bool.true G C Sk.unit)] :where [A C]))
