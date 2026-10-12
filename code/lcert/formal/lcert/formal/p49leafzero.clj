(ns lcert.formal.p49leafzero
  "Depth-zero leaf continuation for the all-depth fuel recursion."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49leafcont]
            [lcert.formal.p49peelzero]
            [lcert.formal.p49telelaws]))

(kdef p49lz_Dleaf (=> (List Exp) (List Exp))
  (fn [D :- (List Exp)]
    (consE (p49peel_leafA p49pz_A)
      (consE Exp.tLbl (consE Exp.tSyn D)))))

(thm p49lz_len
  [D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Eq Nat (lenU (p49lc_Uleaf uv)) (lenE (p49lz_Dleaf D)))
  (exact (congrArg Nat.succ
    (congrArg Nat.succ
      (congrArg Nat.succ
        (Eq.trans (p49peel_zlen uv) hlen))))))

(thm p49lz_branch_len
  [D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Eq Nat (lenU (p49lc_Ubranches uv))
    (lenE (p49lz_Dleaf D)))
  (exact (Eq.trans
    (p49peel_zlen (p49lc_Uleaf uv))
    (p49lz_len D uv hlen))))

(thm p49lz_label
  [chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf (p49lz_Dleaf D) (p49lc_Uleaf uv)
    (Exp.var 1) Exp.tLbl)
  (exact (Rt.rVar chkf
    (p49lz_Dleaf D) (p49lc_Uleaf uv)
    1 Exp.tLbl U.uw (p49lz_len D uv hlen)
    (Eq.trans
      (nthE.eq_3 (p49peel_leafA p49pz_A)
        (consE Exp.tLbl (consE Exp.tSyn D)) 0)
      (nthE.eq_2 Exp.tLbl (consE Exp.tSyn D)))
    (Eq.trans
      (nthU.eq_3 U.u1 (consU U.uw (p49peel_Uz uv)) 0)
      (nthU.eq_2 U.uw (p49peel_Uz uv)))
    rfl)))

(thm p49lz_motive_formed
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, D :- (List Exp),
   hF :- (p49pd_Form chkf (p49lc_F K slots j))]
  (Tl chkf Bool.true
    (consE Exp.tLbl (p49lz_Dleaf D))
    (p49lc_F K slots j (Exp.sleaf (Exp.var 0))) Exp.tUnit)
  (exact (hF (consE Exp.tLbl (p49lz_Dleaf D))
    (Exp.sleaf (Exp.var 0))
    (Tl.zSleaf chkf (consE Exp.tLbl (p49lz_Dleaf D))
      (Exp.var 0)
      (Tl.zVar chkf (consE Exp.tLbl (p49lz_Dleaf D))
        0 Exp.tLbl
        (nthE.eq_2 Exp.tLbl (p49lz_Dleaf D)))))))

(thm p49lz_leaf_cont
  [chkf :- (=> Code Code Bool), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hj :- (p49tr_avoids slots j),
   hF :- (p49pd_Form chkf (p49lc_F K slots j)),
   hL :- (p49tba_Lift (p49lc_F K slots j)),
   hS :- (p49tba_Subst (p49lc_F K slots j)),
   f :- (=> Nat Exp),
   hf :- (forall [l Nat]
     (=> (Eq Bool (Nat.blt l 100) Bool.true)
       (Rt chkf (p49lz_Dleaf D) (p49lc_Ubranches uv)
         (f l)
         (p49tele (fn [q :- Nat, x :- Exp] (p49dl q x))
           ((p49crepl K) j (P49C.leaf l)) slots p49lc_rho0))))]
  (Rt chkf (p49lz_Dleaf D) (p49lc_Uleaf uv)
    (Exp.caseL
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) (p49brs 100 f 0))
    (p49peel_leafB (p49lc_F K slots j (Exp.var 0))))
  (have hbr
    (forall [l Nat]
      (=> (Eq Bool (Nat.blt l 100) Bool.true)
        (Rt chkf (p49lz_Dleaf D) (p49lc_Ubranches uv)
          (f l)
          (subst1 (Exp.lbl l)
            (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))))))
    (fn [l :- Nat]
      (fn [hlt :- (Eq Bool (Nat.blt l 100) Bool.true)]
        (Eq.mp
          (congrArg
            (fn [A :- Exp]
              (Rt chkf (p49lz_Dleaf D) (p49lc_Ubranches uv)
                (f l) A))
            (Eq.symm (p49lc_branch_type K slots j l hj hS)))
          (hf l hlt)))))
  (exact (p49peel_cast chkf (p49lz_Dleaf D)
    (vadd (p49lc_Uleaf uv) (p49lc_Ubranches uv))
    (p49lc_Uleaf uv)
    (Exp.caseL
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) (p49brs 100 f 0))
    (subst1 (Exp.var 1)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0))))
    (p49peel_leafB (p49lc_F K slots j (Exp.var 0)))
    (p49case_typed chkf (p49lz_Dleaf D)
      (p49lc_Uleaf uv) (p49lc_Ubranches uv)
      (p49lc_F K slots j (Exp.sleaf (Exp.var 0)))
      (Exp.var 1) f
      (p49lz_branch_len D uv hlen)
      (p49lz_motive_formed chkf K slots j D hF)
      (p49lz_label chkf D uv hlen) hbr)
    (p49peel_addz (p49lc_Uleaf uv))
    (p49lc_result_type K slots j hL hS))))
