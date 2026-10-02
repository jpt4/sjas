(ns lcert.formal.check-dt
  "F7 step 2 — data trees for Tl, Rt and Cv, with their claimed conclusions.

  DT contains one constructor per Tl, Rt and Cv rule. Tl/Rt/Cv premise
  derivations are replaced by recursive DT fields; skeleton premises use
  the already defined SkDT; Step premises use StepDT, whose checker is sound.
  This modular representation reuses the 40 SkDT and 18 HdDT constructors.
  All proof-valued side conditions are omitted from data and must be checked
  by the future Tl/Rt/Cv checker. Data has no chkf parameter: delta records
  are interpreted by stepDTCheck at the supplied chkf.

  concl returns the *claimed* conclusion, even for a malformed tree. It is
  not a checker, and no soundness theorem for Tl/Rt/Cv is claimed here.
  A future decoder may fail before producing DT; every constructed DT has
  an explicit conclusion, so concl itself always returns some. The Option
  interface follows ADR-0006 and leaves room for later partial decoding."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.check-skj]))

;; Conversion records a path, its head-rule data, and both whole expressions.
;; Soundness immediately reuses the proved path checker, without assuming
;; that the stored endpoints agree with either the head record or each other.
(a/inductive StepDT []
  (at [p (List Nat)] [head HdDT] [e Exp] [e2 Exp]))
(kdef stepDTSrc (=> StepDT Exp)
  (fn [d :- StepDT] (StepDT.rec$1 (fn [_ :- StepDT] Exp)
    (fn [p :- (List Nat), head :- HdDT, e :- Exp, e2 :- Exp] e) d)))
(kdef stepDTTgt (=> StepDT Exp)
  (fn [d :- StepDT] (StepDT.rec$1 (fn [_ :- StepDT] Exp)
    (fn [p :- (List Nat), head :- HdDT, e :- Exp, e2 :- Exp] e2) d)))
(kdef stepDTCheck (=> (=> Code Code Bool) StepDT Bool)
  (fn [chkf :- (=> Code Code Bool), d :- StepDT]
    (StepDT.rec$1 (fn [_ :- StepDT] Bool)
      (fn [p :- (List Nat), head :- HdDT, e :- Exp, e2 :- Exp]
        (stepCheck chkf p head e e2)) d)))
(thm stepDTCheck_sound [chkf :- (=> Code Code Bool), d :- StepDT]
  (=> (Eq Bool (stepDTCheck chkf d) Bool.true) (Step chkf (stepDTSrc d) (stepDTTgt d)))
  (cases d) (intro accepted) (exact (stepCheck_sound chkf p head e e2 accepted)))

;; Judgment *data*, as distinct from the Prop families Tl, Rt and Cv.
(a/inductive DTJ []
  (tl [w Bool] [D (List Exp)] [e Exp] [A Exp])
  (rt [D (List Exp)] [us (List U)] [e Exp] [A Exp])
  (cv [G (List Sk)] [A Exp] [B Exp]))

;; Tl/Rt signatures copied from judgment.clj. Premise fields keep their
;; original names and order; side conditions remain visible in the table
;; so the next checker can implement exactly those obligations.
(def tl-rules
  '[(fBase [D (List Exp)] [X Exp] [h (Eq Bool (isBaseOrDia X) Bool.true)] :where [Bool.true D X Exp.tUnit])
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
         :where [Bool.false D (Exp.insp X r c t1 t2) X])])

(def rt-rules
  '[(rVar [D (List Exp)] [us (List U)] [i Nat] [A Exp] [r U] [hl (Eq Nat (lenU us) (lenE D))]
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
         :where [D (vadd us1 (vadd (vscale U.uw us0) us2)) (Exp.insp X r c t1 t2) X])])

;; Cv's context was a parameter, so record it explicitly at each node.
;; nbr is a side condition to be checked later, not evidence stored here.
(def cv-rules
  '[(cvRefl [G (List Sk)] [A Exp] [h (SkJ Bool.true G A Sk.unit)]
      [hn (Eq Bool (nbr A) Bool.true)] :where [A A])
    (cvFwd [G (List Sk)] [A Exp] [B Exp] [C Exp] [hab (Cv chkf G A B)]
      [hs (Step chkf B C)] [hc (SkJ Bool.true G C Sk.unit)]
      [hn (Eq Bool (nbr C) Bool.true)] :where [A C])
    (cvBwd [G (List Sk)] [A Exp] [B Exp] [C Exp] [hab (Cv chkf G A B)]
      [hs (Step chkf C B)] [hc (SkJ Bool.true G C Sk.unit)]
      [hn (Eq Bool (nbr C) Bool.true)] :where [A C])])

;; Keep family information beside every original rule signature: it selects
;; the conclusion tag and prevents a type-level/runtime conclusion mixup.
(def dt-rules
  (vec (concat (map #(vector 'Tl %) tl-rules)
               (map #(vector 'Rt %) rt-rules)
               (map #(vector 'Cv %) cv-rules))))

(defn- dt-field-type [ty]
  (cond
    (and (seq? ty) (#{'Tl 'Rt 'Cv} (first ty))) 'DT
    (and (seq? ty) (= 'SkJ (first ty))) 'SkDT
    (and (seq? ty) (= 'Step (first ty))) 'StepDT
    (or (#{'Exp 'Nat 'U 'Bool 'Code 'Sk} ty)
        (#{'(List Exp) '(List U) '(List Sk)} ty)) ty
    :else (throw (ex-info "Unclassified DT field; do not store a proof in data" {:type ty}))))
(defn- dt-fields [rule]
  (vec (for [[x ty :as field] (h/f7-fields rule) :when (not (h/f7-side? field))]
         [x (dt-field-type ty)])))

(eval (list* 'a/inductive 'DT []
  (for [[_ rule] dt-rules] (list* (first rule) (dt-fields rule)))))

;; Read only the constructor and its explicit fields. Premise conclusions
;; and side conditions will be checked separately in the next F7 unit.
;; Explicit recursors avoid the large equation families of a/defn.
(b/kdef! 'concl '(=> DT (Option DTJ))
  (list 'fn '[tree :- DT]
    (apply list 'DT.rec$1 (list 'fn '[_ :- DT] '(Option DTJ))
      (concat
        (for [[family rule] dt-rules]
          (let [fields (dt-fields rule)
                ihs (for [[x ty] fields :when (= ty 'DT)] [(symbol (str "ih_" x)) '(Option DTJ)])
                args (if (= family 'Cv) (cons 'G (last rule)) (last rule))
                tag ({'Tl 'DTJ.tl 'Rt 'DTJ.rt 'Cv 'DTJ.cv} family)]
            (list 'fn (h/f7-params (concat fields ihs))
              (list 'Option.some 'DTJ (apply list tag args))))) ['tree]))))
