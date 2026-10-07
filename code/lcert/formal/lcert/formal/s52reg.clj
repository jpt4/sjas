(ns lcert.formal.s52reg
  "Theorem 5.2: regularity of type-level derivations (R4 §5, \"in either mode\").

  The fundamental property of S covers type-level derivations Tl, because
  evalₙ evaluates a type-level argument (App₀, Pair₀).  Its proof needs
  the formation of types at one place only: Bcons.  Tl.zBcons (unlike
  Rt.rBcons) does not record the formation of its motive P, and S's
  head-of-list clause substitutes a label into P (S's substitution fact
  needs P formed).  The induction on Tl therefore carries a regularity
  hypothesis on the derivation's type A:

    Reg G A  =  A is a formed skeleton type (SkJ true G A unit),
                or A is a branch-list pseudo-type tBrs P k whose motive P
                is formed under a label (regB).

  Reg holds at the root of every use (a type-level argument's type is
  formed by the rule's own premise hA) and passes from each rule's
  conclusion to its premises' types: constructor-built types by the
  formation rules of SkJ, substituted and lifted ones by S's companion
  substitution/weakening theorems for SkJ, and the premise types of
  lam, ite, bcons inherited from the conclusion's.  This file proves the
  passing lemmas, one per rule shape; they are about skeleton typing only."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52env]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)

;; regB G e: e is a branch-list pseudo-type whose motive is formed under a
;; label.  By Exp.rec into Prop-valued clauses (the recursive results unused),
;; as InvSkJ.
(kdef! 'regB '(=> (List Sk) Exp Prop)
  (list 'fn '[G :- (List Sk), e :- Exp]
    (concat (list 'Exp.rec$1 '(fn [_ :- Exp] Prop))
            (for [[ctor fs] exp-fields]
              (let [bs (vec (concat (mapcat (fn [[f ty & _]] [f :- ty]) fs)
                                    (mapcat (fn [[f ty & _]] (when (= ty 'Exp) [(symbol (str "i_" f)) :- 'Prop])) fs)))
                    body (if (= ctor 'tBrs) '(SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit) 'False)]
                (if (seq bs) (list 'fn bs body) body)))
            ['e])))

(kdef! 'Reg '(=> (List Sk) Exp Prop)
  '(fn [G :- (List Sk), A :- Exp] (Or (SkJ Bool.true G A Sk.unit) (regB G A))))

(thm reg_formed [G :- (List Sk), A :- Exp, h :- (SkJ Bool.true G A Sk.unit)]
  (Reg G A)
  (exact (Or.inl h)))

(thm reg_brs [G :- (List Sk), P :- Exp, k :- Nat, hP :- (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit)]
  (Reg G (Exp.tBrs P k))
  (exact (Or.inr hP)))

;; A branch-list type is not a formed type, so Reg at tBrs P k is its clause.
(thm reg_brs_inv [G :- (List Sk), P :- Exp, k :- Nat, hr0 :- (Reg G (Exp.tBrs P k))]
  (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit)
  (have hor (Or (SkJ Bool.true G (Exp.tBrs P k) Sk.unit) (regB G (Exp.tBrs P k))) hr0)
  (cases hor)
  (exact (Bool.noConfusion ((skj_isTy Bool.true G (Exp.tBrs P k) Sk.unit h) (Eq.refl Bool.true))))
  (exact h))

;; The body of a regular function type is regular (inversion of SkJ at Π).
(thm reg_lam [G :- (List Sk), r :- U, A :- Exp, B :- Exp, hr0 :- (Reg G (Exp.tPi r A B))]
  (Reg (List.cons Sk (skel A) G) B)
  (have hor (Or (SkJ Bool.true G (Exp.tPi r A B) Sk.unit) (regB G (Exp.tPi r A B))) hr0)
  (cases hor)
  (exact (Or.inl (And.right (And.right (And.right (inv_tPi Bool.true G r A B Sk.unit h))))))
  (exact (False.elim$0 h)))

;; Formation of substituted types: B[u/x] for a typed u (skj_subst1, for types).
(thm reg_subst1 [G :- (List Sk), s :- Sk, B :- Exp, u :- Exp,
                 hB :- (SkJ Bool.true (List.cons Sk s G) B Sk.unit),
                 hu :- (SkJ Bool.false G u s),
                 hk :- (Eq (Option Sk) (skOf G u) (Option.some Sk s))]
  (SkJ Bool.true G (subst1 u B) Sk.unit)
  (have hsub (SkJ Bool.true G (subst (consSub u (fn [j :- Nat] (Exp.var j))) B) Sk.unit)
    (skj_subst Bool.true (List.cons Sk s G) B Sk.unit hB G
      (consSub u (fn [j :- Nat] (Exp.var j)))
      (subOK_cons u s G (fn [j :- Nat] (Exp.var j)) G hu hk (subOK_id G))))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ Bool.true G e Sk.unit)) (Eq.symm (subst1_consSub u B))) hsub)))

;; Two weakenings (Let's body, Inspect's branches).
(thm reg_lift2 [G :- (List Sk), C :- Exp, sa :- Sk, sb :- Sk, hC :- (SkJ Bool.true G C Sk.unit)]
  (SkJ Bool.true (List.cons Sk sb (List.cons Sk sa G)) (lift 2 0 C) Sk.unit)
  (rw [(Eq.symm (lift_comp C 1 1 0))])
  (exact (skj_weaken Bool.true (List.cons Sk sa G) (lift 1 0 C) Sk.unit
           (skj_weaken Bool.true G C Sk.unit hC 0 sa) 0 sb)))

;; RecN's step type, RecSyn's leaf and node types.
(thm reg_stepTy [G :- (List Sk), P :- Exp, hP :- (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit)]
  (SkJ Bool.true (List.cons Sk (skel P) (List.cons Sk Sk.nat G)) (stepTy P) Sk.unit)
  (exact (skj_weaken Bool.true (List.cons Sk Sk.nat G) (subst (fn [j :- Nat] (sSucc j)) P) Sk.unit
           (skj_subst Bool.true (List.cons Sk Sk.nat G) P Sk.unit hP (List.cons Sk Sk.nat G)
             (fn [j :- Nat] (sSucc j)) (subOK_sSucc G))
           0 (skel P))))

(thm reg_leafTy [G :- (List Sk), P :- Exp, hP :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
  (SkJ Bool.true (List.cons Sk Sk.lbl G) (leafTy P) Sk.unit)
  (exact (skj_subst Bool.true (List.cons Sk Sk.syn G) P Sk.unit hP (List.cons Sk Sk.lbl G)
           (fn [i :- Nat] (sLeafI i)) (subOK_sLeafI G))))

(thm reg_nodeTy [G :- (List Sk), P :- Exp, hP :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
  (SkJ Bool.true
    (List.cons Sk (skel (y2Ty P)) (List.cons Sk (skel (y1Ty P))
      (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
    (nodeTy P) Sk.unit)
  (rw [(skel_y2Ty P)])
  (rw [(skel_y1Ty P)])
  (exact (skj_subst Bool.true (List.cons Sk Sk.syn G) P Sk.unit hP
           (List.cons Sk (skel P) (List.cons Sk (skel P)
             (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
           (fn [i :- Nat] (sNodeI i)) (subOK_sNodeI (skel P) G))))

;; ItR's method types.
(thm reg_gTy [G :- (List Sk), X :- Exp, hX :- (SkJ Bool.true G X Sk.unit)]
  (SkJ Bool.true G (gTy X) Sk.unit)
  (exact (SkJ.wPi G U.uw Exp.tLbl (lift 1 0 X) (SkJ.wLbl G)
           (skj_weaken Bool.true G X Sk.unit hX 0 Sk.lbl))))

(thm reg_hTy [G :- (List Sk), X :- Exp, hX :- (SkJ Bool.true G X Sk.unit)]
  (SkJ Bool.true G (hTy X) Sk.unit)
  (have hA2 (SkJ Bool.true (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)) (lift 2 0 X) Sk.unit)
    (reg_lift2 G X Sk.dia Sk.lbl hX))
  (have hA3 (SkJ Bool.true (List.cons Sk (skel (lift 2 0 X)) (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))
              (lift 3 0 X) Sk.unit)
    (Eq.mp (congrArg (fn [z :- Exp] (SkJ Bool.true
                (List.cons Sk (skel (lift 2 0 X)) (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G))) z Sk.unit))
             (lift_comp X 1 2 0))
      (skj_weaken Bool.true (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)) (lift 2 0 X) Sk.unit hA2 0 (skel (lift 2 0 X)))))
  (have hB3 (SkJ Bool.true (List.cons Sk (skel (lift 3 0 X)) (List.cons Sk (skel (lift 2 0 X))
                (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))) (lift 4 0 X) Sk.unit)
    (Eq.mp (congrArg (fn [z :- Exp] (SkJ Bool.true
                (List.cons Sk (skel (lift 3 0 X)) (List.cons Sk (skel (lift 2 0 X))
                  (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))) z Sk.unit))
             (lift_comp X 1 3 0))
      (skj_weaken Bool.true (List.cons Sk (skel (lift 2 0 X)) (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))
        (lift 3 0 X) Sk.unit hA3 0 (skel (lift 3 0 X)))))
  (exact (SkJ.wPi G U.u1 Exp.tDia (Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))))
           (SkJ.wDia G)
           (SkJ.wPi (List.cons Sk Sk.dia G) U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))
             (SkJ.wLbl (List.cons Sk Sk.dia G))
             (SkJ.wPi (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)) U.u1 (lift 2 0 X)
               (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))
               hA2
               (SkJ.wPi (List.cons Sk (skel (lift 2 0 X)) (List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))
                 U.u1 (lift 3 0 X) (lift 4 0 X) hA3 hB3))))))

;; The evidence types of H₁ and Reflect.
(thm reg_chkT [G :- (List Sk), r :- Exp, c :- Exp,
               hr :- (SkJ Bool.false G r Sk.cert), hc :- (SkJ Bool.false G c Sk.syn)]
  (SkJ Bool.true G (chkT r c) Sk.unit)
  (exact (SkJ.wT G (Exp.chk (Exp.prn r) c) (SkJ.sChk G (Exp.prn r) c (SkJ.sPrn G r hr) hc))))

(thm reg_negT [G :- (List Sk), c :- Exp, hc :- (SkJ Bool.false G c Sk.syn)]
  (SkJ Bool.false G (negT c) Sk.syn)
  (exact (SkJ.sSnode G (Exp.lbl 25) c (cLeaf 15) (SkJ.sLbl G 25) hc
           (SkJ.sSleaf G (Exp.lbl 15) (SkJ.sLbl G 15)))))

;; baseCode X is a closed code term: a leaf over a label.  (Local names are
;; chosen not to clash with constructor field names, which `cases` introduces.)
(a/prove-theorem 'reg_baseCode '[X :- Exp]
  '(forall [cd Exp] (forall [G (List Sk)]
     (=> (= (baseCode X) (Option.some Exp cd)) (SkJ Bool.false G cd Sk.syn))))
  (lv (into ['(cases X)]
            (mapcat (fn [[ctor _]]
                      (if-let [n ('{tEmpty 15 tUnit 16 tBool 17 tNat 18 tLbl 19 tSyn 20 tR 22} ctor)]
                        ['(intro cdx Gx hqx)
                         (list 'have 'eqx (list 'Eq 'Exp (list 'cLeaf n) 'cdx)
                           (list 'some_inj (list 'cLeaf n) 'cdx 'hqx))
                         (list 'exact (list 'Eq.mp (list 'congrArg '(fn [z :- Exp] (SkJ Bool.false Gx z Sk.syn)) 'eqx)
                           (list 'SkJ.sSleaf 'Gx (list 'Exp.lbl n) (list 'SkJ.sLbl 'Gx n))))]
                        ['(intro cdx Gx hqx) '(exact (False.elim$0 (none_ne_someE cdx hqx)))]))
                    exp-fields))))
