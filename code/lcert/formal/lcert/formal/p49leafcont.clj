(ns lcert.formal.p49leafcont
  "Leaf continuation for the concrete Proposition 4.9 telescope."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49peeldepth]
            [lcert.formal.p49telebridgeaux]
            [lcert.formal.p49branches]))

;; This closed default agrees definitionally with the defaults used by the
;; separate formation and naturality modules. Those modules need not be
;; loaded to state the leaf continuation.
(kdef p49lc_rho0 (=> Nat Exp)
  (fn [_ :- Nat] (Exp.sleaf (Exp.lbl 0))))

(kdef p49lc_F (=> P49C (List (Prod Nat Nat)) Nat Exp Exp)
  (fn [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat, c :- Exp]
    (p49tba_F (fn [q :- Nat, x :- Exp] (p49dl q x))
      K slots j p49lc_rho0 c)))

(kdef p49lc_Dleaf (=> Nat (List Exp) (List Exp))
  (fn [j :- Nat, D :- (List Exp)]
    (consE (p49peel_leafA (p49pd_A j))
      (consE Exp.tLbl (consE Exp.tSyn D)))))

(kdef p49lc_Uleaf (=> (List U) (List U))
  (fn [uv :- (List U)]
    (consU U.u1 (consU U.uw (p49peel_Uz uv)))))

(kdef p49lc_Ubranches (=> (List U) (List U))
  (fn [uv :- (List U)] (p49peel_z (p49lc_Uleaf uv))))

(thm p49lc_leaf_len
  [j :- Nat, D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Eq Nat (lenU (p49lc_Uleaf uv)) (lenE (p49lc_Dleaf j D)))
  (exact (congrArg Nat.succ
    (congrArg Nat.succ
      (congrArg Nat.succ
        (Eq.trans (p49peel_zlen uv) hlen))))))

(thm p49lc_label
  [chkf :- (=> Code Code Bool), j :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf (p49lc_Dleaf j D) (p49lc_Uleaf uv)
    (Exp.var 1) Exp.tLbl)
  (exact (Rt.rVar chkf
    (p49lc_Dleaf j D) (p49lc_Uleaf uv)
    1 Exp.tLbl U.uw (p49lc_leaf_len j D uv hlen)
    (Eq.trans
      (nthE.eq_3 (p49peel_leafA (p49pd_A j))
        (consE Exp.tLbl (consE Exp.tSyn D)) 0)
      (nthE.eq_2 Exp.tLbl (consE Exp.tSyn D)))
    (Eq.trans
      (nthU.eq_3 U.u1 (consU U.uw (p49peel_Uz uv)) 0)
      (nthU.eq_2 U.uw (p49peel_Uz uv)))
    rfl)))

(thm p49lc_motive_formed
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, d :- Nat,
   D :- (List Exp),
   hF :- (p49pd_Form chkf (p49lc_F K slots j))]
  (Tl chkf Bool.true (consE Exp.tLbl (p49lc_Dleaf d D))
    (p49lc_F K slots j (Exp.sleaf (Exp.var 0))) Exp.tUnit)
  (exact (hF (consE Exp.tLbl (p49lc_Dleaf d D))
    (Exp.sleaf (Exp.var 0))
    (Tl.zSleaf chkf (consE Exp.tLbl (p49lc_Dleaf d D))
      (Exp.var 0)
      (Tl.zVar chkf (consE Exp.tLbl (p49lc_Dleaf d D))
        0 Exp.tLbl
        (nthE.eq_2 Exp.tLbl (p49lc_Dleaf d D)))))))

;; Substitution into the case motive is the unary family applied to the
;; constructor code. No canonicality or inhabitance is assumed here.
(thm p49lc_subst1_leaf
  [F :- (=> Exp Exp), hS :- (p49tba_Subst F), x :- Exp]
  (Eq Exp
    (subst1 x (F (Exp.sleaf (Exp.var 0))))
    (F (Exp.sleaf x)))
  (exact (hS (fn [i :- Nat] (inst1 x i))
    (Exp.sleaf (Exp.var 0)))))

(thm p49lc_branch_type
  [K :- P49C, slots :- (List (Prod Nat Nat)),
   j :- Nat, l :- Nat,
   hj :- (p49tr_avoids slots j),
   hS :- (p49tba_Subst (p49lc_F K slots j))]
  (Eq Exp
    (subst1 (Exp.lbl l)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0))))
    (p49tele (fn [q :- Nat, x :- Exp] (p49dl q x))
      ((p49crepl K) j (P49C.leaf l)) slots p49lc_rho0))
  (exact (Eq.trans
    (p49lc_subst1_leaf (p49lc_F K slots j) hS (Exp.lbl l))
    (p49tba_leaf_tail
      (fn [q :- Nat, x :- Exp] (p49dl q x))
      K slots j l p49lc_rho0 hj))))

(thm p49lc_result_type
  [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat,
   hL :- (p49tba_Lift (p49lc_F K slots j)),
   hS :- (p49tba_Subst (p49lc_F K slots j))]
  (Eq Exp
    (subst1 (Exp.var 1)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0))))
    (p49peel_leafB (p49lc_F K slots j (Exp.var 0))))
  (exact (Eq.trans
    (p49lc_subst1_leaf (p49lc_F K slots j) hS (Exp.var 1))
    (Eq.symm (p49tba_leafB_eq (p49lc_F K slots j) hL hS)))))

;; The missing all-depth input is explicit: each smaller-potential leaf
;; replacement must already have a runtime witness at zero usage in this
;; extended leaf context. The theorem packages those 100 witnesses into the
;; exact hl premise of p49pd_peel.
(thm p49lc_leaf_cont
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, d :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hj :- (p49tr_avoids slots j),
   hF :- (p49pd_Form chkf (p49lc_F K slots j)),
   hL :- (p49tba_Lift (p49lc_F K slots j)),
   hS :- (p49tba_Subst (p49lc_F K slots j)),
   f :- (=> Nat Exp),
   hf :- (forall [l Nat]
     (=> (Eq Bool (Nat.blt l 100) Bool.true)
       (Rt chkf (p49lc_Dleaf d D) (p49lc_Ubranches uv)
         (f l)
         (p49tele (fn [q :- Nat, x :- Exp] (p49dl q x))
           ((p49crepl K) j (P49C.leaf l)) slots p49lc_rho0))))]
  (Rt chkf (p49lc_Dleaf d D) (p49lc_Uleaf uv)
    (Exp.caseL
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) (p49brs 100 f 0))
    (p49peel_leafB (p49lc_F K slots j (Exp.var 0))))
  (have hP
    (Tl chkf Bool.true (consE Exp.tLbl (p49lc_Dleaf d D))
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0))) Exp.tUnit)
    (p49lc_motive_formed chkf K slots j d D hF))
  (have hbr
    (forall [l Nat]
      (=> (Eq Bool (Nat.blt l 100) Bool.true)
        (Rt chkf (p49lc_Dleaf d D) (p49lc_Ubranches uv)
          (f l)
          (subst1 (Exp.lbl l)
            (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))))))
    (fn [l :- Nat]
      (fn [hlt :- (Eq Bool (Nat.blt l 100) Bool.true)]
        (Eq.mp
          (congrArg
            (fn [A :- Exp]
              (Rt chkf (p49lc_Dleaf d D) (p49lc_Ubranches uv)
                (f l) A))
            (Eq.symm (p49lc_branch_type K slots j l hj hS)))
          (hf l hlt)))))
  (exact (p49peel_cast chkf (p49lc_Dleaf d D)
    (vadd (p49lc_Uleaf uv) (p49lc_Ubranches uv))
    (p49lc_Uleaf uv)
    (Exp.caseL
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) (p49brs 100 f 0))
    (subst1 (Exp.var 1)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0))))
    (p49peel_leafB (p49lc_F K slots j (Exp.var 0)))
    (p49case_typed chkf (p49lc_Dleaf d D)
      (p49lc_Uleaf uv) (p49lc_Ubranches uv)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) f
      (Eq.trans (p49peel_zlen (p49lc_Uleaf uv))
        (p49lc_leaf_len d D uv hlen))
      hP (p49lc_label chkf d D uv hlen) hbr)
    (p49peel_addz (p49lc_Uleaf uv))
    (p49lc_result_type K slots j hL hS))))
