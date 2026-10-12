(ns lcert.formal.p49terminal
  "Terminal telescope continuation from canonical-code readback."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49zero]
            [lcert.formal.p49telelaws]
            [lcert.formal.p49teleformbridge]
            [lcert.formal.p49contextfacts]))

(kdef p49tc_term (=> Exp Exp)
  (fn [c :- Exp]
    (Exp.lam U.u1
      (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
      (Exp.var 0))))

(thm p49tc_plug_lift [K :- P49C]
  (Eq Exp (lift 1 0 (p49plug K p49tl_rho0))
    (p49plug K p49tl_rho0))
  (exact (Eq.trans
    (p49plug_lift K 1 0 p49tl_rho0)
    (congrArg
      (fn [rho :- (=> Nat Exp)] (p49plug K rho))
      (funext (fn [i :- Nat] (p49tl_rho0_lift i 1 0)))))))

(thm p49tc_plug_nbr [K :- P49C]
  (Eq Bool (nbr (p49plug K p49tl_rho0)) Bool.true)
  (induction K)
  (rfl)
  (rfl)
  (exact (andb_intro
    (nbr (Exp.lbl l))
    (Bool.and (nbr (p49plug a p49tl_rho0))
      (nbr (p49plug b p49tl_rho0)))
    (nbr_lbl_tt l)
    (andb_intro
      (nbr (p49plug a p49tl_rho0))
      (nbr (p49plug b p49tl_rho0)) ih_a ih_b))))

;; The final checker witness itself is linear. Its code must be canonical,
;; so delta has a concrete source code to feed to Corollary 3.7.
(thm p49tc_canonical
  [chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
   c :- Exp, cc :- Code,
   hlen :- (Eq Nat (lenU uv) (lenE D)),
   hc :- (Tl chkf Bool.false D c Exp.tSyn),
   hcl :- (Eq Exp (lift 1 0 c) c),
   hnbr :- (Eq Bool (nbr c) Bool.true),
   hEext :- (Tl chkf Bool.true
     (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
     (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) Exp.tUnit),
   hcode :- (Eq (Option Code) (codeOf c) (Option.some Code cc)),
   hf :- (Eq Bool (chkf cc (Code.sl 15)) Bool.false)]
  (Rt chkf D uv (p49tc_term c)
    (Exp.tPi U.u1
      (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
      Exp.tEmpty))
  (have hbot
    (Tl chkf Bool.false D (Exp.sleaf (Exp.lbl 15)) Exp.tSyn)
    (Tl.zSleaf chkf D (Exp.lbl 15)
      (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl (Eq.refl Bool.true))))
  (have hchk
    (Tl chkf Bool.false D
      (Exp.chk c (Exp.sleaf (Exp.lbl 15))) Exp.tBool)
    (Tl.zChk chkf D c (Exp.sleaf (Exp.lbl 15)) hc hbot))
  (have hE
    (Tl chkf Bool.true D
      (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) Exp.tUnit)
    (Tl.fT chkf D (Exp.chk c (Exp.sleaf (Exp.lbl 15))) hchk))
  (have hA
    (Eq Exp
      (lift 1 0 (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))))
      (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))))
    (congrArg
      (fn [e :- Exp]
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))) hcl))
  (have hnA
    (Eq Bool
      (nbr (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))))
      Bool.true)
    (Eq.trans
      (nbr_tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
      (Eq.trans
        (nbr_chk c (Exp.sleaf (Exp.lbl 15)))
        (andb_intro (nbr c) (nbr (Exp.sleaf (Exp.lbl 15)))
          hnbr
          (Eq.trans (nbr_sleaf (Exp.lbl 15))
            (nbr_lbl_tt 15))))))
  (exact (Rt.rLam chkf D uv U.u1
    (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
    (Exp.var 0) Exp.tEmpty hE
    (Rt.rConv chkf
      (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
      (consU U.u1 uv)
      (Exp.var 0)
      (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
      Exp.tEmpty
      (Eq.mp
        (congrArg
          (fn [A :- Exp]
            (Rt chkf
              (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
              (consU U.u1 uv) (Exp.var 0) A)) hA)
        (Rt.rVar chkf
          (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
          (consU U.u1 uv) 0
          (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) U.u1
          (congrArg Nat.succ hlen)
          (nthE.eq_2
            (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
          (nthU.eq_2 U.u1 uv) (Eq.refl Bool.true)))
      (Tl.fBase chkf
        (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
        Exp.tEmpty (Eq.refl Bool.true))
      (Cv.cvFwd chkf
        (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D))
        (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
        (Exp.tT Exp.ff) Exp.tEmpty
        (Cv.cvFwd chkf
          (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D))
          (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
          (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
          (Exp.tT Exp.ff)
          (Cv.cvRefl chkf
            (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D))
            (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15))))
            (lemma25_tl_type chkf
              (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)
              (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) hEext)
            hnA)
          (p49z_step_T chkf
            (Exp.chk c (Exp.sleaf (Exp.lbl 15))) Exp.ff
            (p49z_delta chkf c cc hcode hf))
          (SkJ.wT
            (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D))
            Exp.ff
            (SkJ.sFF
              (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D))))
          (Eq.refl Bool.true))
        (sh4_step_hd chkf (Exp.tT Exp.ff) Exp.tEmpty
          (Hd.tTF chkf))
        (SkJ.wEmpty
          (skels (consE (Exp.tT (Exp.chk c (Exp.sleaf (Exp.lbl 15)))) D)))
        (Eq.refl Bool.true))))))

(thm p49tc_closed
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
   K :- P49C, hK :- (Eq Bool (p49c_lblok K) Bool.true),
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt chkf D uv
    (p49tc_term (p49plug K p49tl_rho0))
    (p49tele
      (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.nil (Prod Nat Nat)) p49tl_rho0))
  (have hc
    (Tl chkf Bool.false D (p49plug K p49tl_rho0) Exp.tSyn)
    (p49plug_typed chkf D p49tl_rho0
      (fn [i :- Nat] (p49tf_rho0_typed chkf D i)) K hK))
  (have hEext
    (Tl chkf Bool.true
      (consE
        (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
          (Exp.sleaf (Exp.lbl 15)))) D)
      (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
        (Exp.sleaf (Exp.lbl 15)))) Exp.tUnit)
    (Tl.fT chkf
      (consE
        (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
          (Exp.sleaf (Exp.lbl 15)))) D)
      (Exp.chk (p49plug K p49tl_rho0)
        (Exp.sleaf (Exp.lbl 15)))
      (Tl.zChk chkf
        (consE
          (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
            (Exp.sleaf (Exp.lbl 15)))) D)
        (p49plug K p49tl_rho0) (Exp.sleaf (Exp.lbl 15))
        (p49plug_typed chkf
          (consE
            (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
              (Exp.sleaf (Exp.lbl 15)))) D)
          p49tl_rho0
          (fn [i :- Nat]
            (p49tf_rho0_typed chkf
              (consE
                (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
                  (Exp.sleaf (Exp.lbl 15)))) D) i))
          K hK)
        (Tl.zSleaf chkf
          (consE
            (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
              (Exp.sleaf (Exp.lbl 15)))) D)
          (Exp.lbl 15)
          (Tl.zConst chkf
            (consE
              (Exp.tT (Exp.chk (p49plug K p49tl_rho0)
                (Exp.sleaf (Exp.lbl 15)))) D)
            (Exp.lbl 15) Exp.tLbl rfl)))))
  (have hfalse
    (Eq Bool
      (chkf (p49ccode K (fn [_ :- Nat] (Code.sl 0)))
        (Code.sl 15)) Bool.false)
    (Eq.trans
      (congrArg
        (fn [d :- Code]
          (chkf (p49ccode K (fn [_ :- Nat] (Code.sl 0))) d))
        (Eq.symm (p49z_checker_empty chkf dec encTy hcs)))
      (cor37_refutation chkf dec encTy hcs
        (conv_all chkf dec encTy hcs)
        (p49ccode K (fn [_ :- Nat] (Code.sl 0))))))
  (exact (p49tc_canonical chkf D uv
    (p49plug K p49tl_rho0)
    (p49ccode K (fn [_ :- Nat] (Code.sl 0)))
    hlen hc (p49tc_plug_lift K) (p49tc_plug_nbr K) hEext
    (p49plug_codeOf K (fn [_ :- Nat] (Code.sl 0))) hfalse)))

(thm p49tc_closed_concrete
  [K :- P49C, hK :- (Eq Bool (p49c_lblok K) Bool.true),
   D :- (List Exp), uv :- (List U),
   hlen :- (Eq Nat (lenU uv) (lenE D))]
  (Rt (Check decCert) D uv
    (p49tc_term (p49plug K p49tl_rho0))
    (p49tele
      (fn [j :- Nat, c :- Exp] (p49dl j c))
      K (List.nil (Prod Nat Nat)) p49tl_rho0))
  (exact (p49tc_closed
    (Check decCert) (decOf decCert) encE (check_spec decCert)
    K hK D uv hlen)))
