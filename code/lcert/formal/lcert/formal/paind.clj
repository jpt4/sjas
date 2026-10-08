(ns lcert.formal.paind
  "F6.3 — packaged induction for arbitrary translated formulas (R4 §6.2)."
  (:require [ansatz.core :as a]
            [clojure.walk :as w]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.paquant]
            [lcert.formal.paprop :as pp]))

;; Local packaging operations for R4's !A = Sigma(p:omega A).1.
;; The consumed package has usage ONE. Its first projection is available
;; at omega; the Unit component occupies the second, usage-one slot.
(kdef paBang (=> Exp Exp) (fn [A :- Exp] (Exp.tSig U.uw A Exp.tUnit)))
(thm paBang_form [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp,
                  hA :- (Tl chkf Bool.true D A Exp.tUnit)]
  (Tl chkf Bool.true D (paBang A) Exp.tUnit)
  (exact (Tl.fSig chkf D U.uw A Exp.tUnit hA
    (Tl.fBase chkf (consE A D) Exp.tUnit rfl))))
(thm paBang_intro [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, e :- Exp,
                   hA :- (Tl chkf Bool.true D A Exp.tUnit),
                   he :- (Rt chkf D (allW D) e A)]
  (Rt chkf D (allW D) (Exp.pair (paBang A) e Exp.star) (paBang A))
  (exact (rt_cast chkf D (vadd (vscale U.uw (allW D)) (allW D)) (allW D)
    (Exp.pair (paBang A) e Exp.star) (paBang A) (paBang A)
    (Rt.rPair chkf D (allW D) (allW D) U.uw A Exp.tUnit e Exp.star rfl hA
      (Tl.fBase chkf (consE A D) Exp.tUnit rfl) he
      (Rt.rConst chkf D (allW D) Exp.star Exp.tUnit (paAllW_len D) rfl))
    (Eq.trans (congrArg (fn [us :- (List U)] (vadd us (allW D))) (paAllW_scale D U.uw rfl))
      (paAllW_add D)) rfl)))
(thm paBang_project [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, e :- Exp,
                     hA :- (Tl chkf Bool.true D A Exp.tUnit),
                     he :- (Rt chkf D (allW D) e (paBang A))]
  (Rt chkf D (allW D) (Exp.letp A e (Exp.var 1)) A)
  (exact (rt_cast chkf D (vadd (allW D) (allW D)) (allW D)
    (Exp.letp A e (Exp.var 1)) A A
    (Rt.rLet chkf D (allW D) (allW D) U.uw A Exp.tUnit A e (Exp.var 1)
      he hA hA (Tl.fBase chkf (consE A D) Exp.tUnit rfl)
      (Rt.rVar chkf (consE Exp.tUnit (consE A D)) (consU U.u1 (consU U.uw (allW D)))
        1 A U.uw (congrArg (fn [n :- Nat] (Nat.succ (Nat.succ n))) (paAllW_len D))
        (Eq.trans (nthE.eq_3 Exp.tUnit (consE A D) 0) (nthE.eq_2 A D))
        (Eq.trans (nthU.eq_3 U.u1 (consU U.uw (allW D)) 0) (nthU.eq_2 U.uw (allW D))) rfl))
    (paAllW_add D) rfl)))

;; Moving an extra package underneath the recovered proof commutes with
;; its existing binder shift. This is syntax equality, not conversion.
(thm paBang_lifts [B :- Exp]
  (Eq Exp (lift 1 0 (lift 1 1 (lift 1 0 B))) (lift 2 0 (lift 1 0 B)))
  (exact (Eq.trans (congrArg (lift 1 0) (lift_lift_comm B 1 0 0))
    (lift_comp (lift 1 0 B) 1 1 0))))

;; Map a supplied open proof A |- B over a package. Both weakening
;; insertions have usage zero. The star accounts for the Unit slot; the
;; scrutinee accounts for the only use of the package itself.
(let [D 'D, A 'A, B 'B, bang #(list 'paBang %), L #(list 'lift 1 0 %)
      ce #(list 'consE %1 %2), cu #(list 'consU %1 %2), aw #(list 'allW %)
      X (bang A), DX (ce X D), AX (L A), BX (L B), C (bang BX)
      Dp (ce AX DX), Du (ce 'Exp.tUnit Dp)
      tailu (cu 'U.u0 (aw D)), eup (cu 'U.uw tailu)
      ue (cu 'U.u0 eup), staru (cu 'U.u1 (cu 'U.u0 (cu 'U.u0 (aw D))))
      bodyu (cu 'U.u1 eup), scru (cu 'U.u1 (aw D))
      e1 '(lift 1 1 e), e2 (L e1)
      h1 (list 'rt_weaken 'chkf (ce A D) (aw (ce A D)) 'e (L B) 'he 1 X)
      h2 (list 'rt_weaken 'chkf Dp eup e1 '(lift 1 1 (lift 1 0 B)) h1 0 'Exp.tUnit)
      hh (list 'rt_cast 'chkf Du ue ue e2 '(lift 1 0 (lift 1 1 (lift 1 0 B))) '(lift 2 0 (lift 1 0 B)) h2 'rfl '(paBang_lifts B))
      hAX (list 'tl_weaken 'chkf 'Bool.true D A 'Exp.tUnit 'hA 0 X)
      hBX (list 'tl_weaken 'chkf 'Bool.true D B 'Exp.tUnit 'hB 0 X)
      hbodyA0 (list 'tl_weaken 'chkf 'Bool.true Dp (L BX) 'Exp.tUnit
               (list 'tl_weaken 'chkf 'Bool.true DX BX 'Exp.tUnit hBX 0 AX) 0 'Exp.tUnit)
      hbodyA (list 'tl_cast3 'chkf 'Bool.true Du (L (L BX)) '(lift 2 0 (lift 1 0 B)) 'Exp.tUnit 'Exp.tUnit hbodyA0 (list 'lift_comp BX 1 1 0) 'rfl)
      pair (list 'Exp.pair '(lift 2 0 (paBang (lift 1 0 B))) e2 'Exp.star)
      hpair (list 'Rt.rPair 'chkf Du ue staru 'U.uw '(lift 2 0 (lift 1 0 B)) 'Exp.tUnit e2 'Exp.star 'rfl
              hbodyA (list 'Tl.fBase 'chkf (ce '(lift 2 0 (lift 1 0 B)) Du) 'Exp.tUnit 'rfl) hh
              (list 'Rt.rConst 'chkf Du staru 'Exp.star 'Exp.tUnit
                '(congrArg (fn [n :- Nat] (Nat.succ (Nat.succ (Nat.succ n)))) (paAllW_len D)) 'rfl))
      pu '(Eq.trans (congrArg (fn [us :- (List U)] (consU U.u1 (consU U.uw (consU U.u0 (vadd us (allW D)))))) (paAllW_scale D U.uw rfl))
             (congrArg (fn [us :- (List U)] (consU U.u1 (consU U.uw (consU U.u0 us)))) (paAllW_add D)))
      hb (list 'rt_cast 'chkf Du (list 'vadd (list 'vscale 'U.uw ue) staru) bodyu pair '(lift 2 0 (paBang (lift 1 0 B))) '(lift 2 0 (paBang (lift 1 0 B))) hpair pu 'rfl)
      hscr (list 'Rt.rVar 'chkf DX scru 0 X 'U.u1 '(congrArg Nat.succ (paAllW_len D)) (list 'nthE.eq_2 X D) (list 'nthU.eq_2 'U.u1 (aw D)) 'rfl)
      term (list 'Exp.letp C '(Exp.var 0) pair)
      proof (list 'Rt.rLet 'chkf DX scru tailu 'U.uw AX 'Exp.tUnit C '(Exp.var 0) pair
              hscr (list 'paBang_form 'chkf DX BX hBX) hAX (list 'Tl.fBase 'chkf Dp 'Exp.tUnit 'rfl) hb)]
  (b/kdef! 'paBang_map '(=> Exp Exp Exp Exp) (list 'fn '[A :- Exp, B :- Exp, e :- Exp] term))
  (a/prove-theorem 'paBang_map_rt
    '[chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, B :- Exp, e :- Exp,
      hA :- (Tl chkf Bool.true D A Exp.tUnit), hB :- (Tl chkf Bool.true D B Exp.tUnit),
      he :- (Rt chkf (consE A D) (allW (consE A D)) e (lift 1 0 B))]
    (list 'Rt 'chkf DX scru '(paBang_map A B e) C)
    [(list 'exact (list 'rt_cast 'chkf DX (list 'vadd scru tailu) scru term C C proof
                   '(congrArg (consU U.u1) (paAllW_add D)) 'rfl))]))

;; Opening a universal at the predecessor beneath one proof binder.
(thm paInd_open_agree [ρ :- (=> Nat Nat), i :- Nat]
  (Eq Exp (inst1 (Exp.var 1) (trUpR (trSucR (trSucR ρ)) i))
          (trT (PT.pv i) (trSucR (trUpR ρ))))
  (cases i) (rfl) (rfl))
(thm paInd_open_eq [p :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (subst1 (Exp.var 1) (trF p (trUpR (trSucR (trSucR ρ)))))
          (trF p (trSucR (trUpR ρ))))
  (exact (Eq.trans
    (trF_subst p (fn [j :- Nat] (inst1 (Exp.var 1) j))
      (trUpR (trSucR (trSucR ρ))) PT.pv (trSucR (trUpR ρ)) (paInd_open_agree ρ))
    (congrArg (fn [q :- PF] (trF q (trSucR (trUpR ρ)))) (paSbF_id p)))))

(defn- tr [p r] (list 'trF p r))
(defn- su [r] (list 'trSucR r))
(defn- lift-eq [p r n]
  (if (= n 1) (list 'trF_lift1 p r)
    (list 'Eq.trans (list 'Eq.symm (list 'lift_comp (tr p r) 1 (dec n) 0))
      (list 'Eq.trans (list 'congrArg '(lift 1 0) (lift-eq p r (dec n)))
        (list 'trF_lift1 p (nth (iterate su r) (dec n)))))))

;; IND, with the exact packaged term of R4 section 6.2. The Clojure
;; emitter only abbreviates telescopes and explicit rule applications;
;; p, D and rho remain arbitrary parameters of the kernel theorem.
(let [ce #(list 'consE %1 %2), cu #(list 'consU %1 %2), aw #(list 'allW %)
      up #(list 'trUpR %), bang #(list 'paBang %), L #(list 'lift 1 0 %)
      r0 'ρ, r1 (su r0), r2 (su r1), r3 (su r2), r4 (su r3), r5 (su r4)
      p0 '(paSbF (paInst PT.pz) p), ps '(paSbF paSuc p)
      q (list 'PF.pimp 'p ps), F (list 'PF.pall q)
      A0 (tr p0 r0), A1 (tr F r1)
      D1 (ce A0 'D), D2 (ce A1 D1), D3 (ce 'Exp.tNat D2)
      Dk (ce 'Exp.tNat D3), P (tr 'p (up r3)), Dp (ce P Dk)
      S (su (up r3)), B (list 'subst '(fn [i :- Nat] (sSucc i)) P)
      g1 (list 'paGood_suc 'D r0 'hg A0), g2 (list 'paGood_suc D1 r1 g1 A1)
      g3 (list 'paGood_suc D2 r2 g2 'Exp.tNat)
      gk (list 'paGood_up D3 r3 g3), gp (list 'paGood_suc Dk (up r3) gk P)
      gi4 (list 'paGood_suc D3 r3 g3 'Exp.tNat), gi5 (list 'paGood_suc Dk r4 gi4 P)
      hP (list 'trF_form 'chkf 'p Dk (up r3) gk)
      hB (list 'tl_cast3 'chkf 'Bool.true Dk (tr ps (up r3)) B 'Exp.tUnit 'Exp.tUnit
             (list 'trF_form 'chkf ps Dk (up r3) gk) (list 'Eq.symm (list 'trF_suc 'p r3)) 'rfl)
      ;; b, at index 2, supplies the zero instance in the recursor context.
      hb0 (list 'paHyp_allW 'chkf D3 2 A0 (tr p0 r3)
              (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D2 1)
                (list 'Eq.trans (list 'nthE.eq_3 A1 D1 0) (list 'nthE.eq_2 A0 'D)))
              (lift-eq p0 r0 3))
      Z (list 'subst1 'Exp.zero P)
      hb (list 'rt_cast 'chkf D3 (aw D3) (aw D3) '(Exp.var 2) (tr p0 r3) Z hb0 'rfl
             (list 'Eq.symm (list 'trF_inst 'p 'PT.pz r3)))
      hzform (list 'tl_cast3 'chkf 'Bool.true D3 (tr p0 r3) Z 'Exp.tUnit 'Exp.tUnit
                 (list 'trF_form 'chkf p0 D3 r3 g3) (list 'Eq.symm (list 'trF_inst 'p 'PT.pz r3)) 'rfl)
      z (list 'Exp.pair (bang Z) '(Exp.var 2) 'Exp.star)
      hz (list 'paBang_intro 'chkf D3 Z '(Exp.var 2) hzform hb)
      ;; st, at index 3 under p and k, is opened at k=var1.
      lookup-st (list 'Eq.trans (list 'nthE.eq_3 P Dk 2)
                  (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D3 1)
                    (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D2 0) (list 'nthE.eq_2 A1 D1))))
      hst (list 'paHyp_allW 'chkf Dp 3 A1 (tr F r5) lookup-st (lift-eq F r1 4))
      opened '(Exp.app (Exp.var 3) (Exp.var 1))
      hopen (list 'rt_cast 'chkf Dp (list 'vadd (aw Dp) (list 'vscale 'U.uw (aw Dp))) (aw Dp)
              opened (list 'subst1 '(Exp.var 1) (tr q (up r5))) (tr q S)
              (list 'Rt.rApp 'chkf Dp (aw Dp) (aw Dp) 'U.uw '(Exp.var 3) '(Exp.var 1)
                'Exp.tNat (tr q (up r5)) 'rfl hst
                (list 'trT_rt 'chkf '(PT.pv 0) Dp S gp)
                (list 'Tl.fBase 'chkf Dp 'Exp.tNat 'rfl)
                (list 'trF_form 'chkf q (ce 'Exp.tNat Dp) (up r5)
                  (list 'paGood_up Dp r5 gi5)))
              (list 'paAllW_app Dp 'U.uw 'rfl) (list 'paInd_open_eq q r3))
      hp (list 'paHyp_allW 'chkf Dp 0 P (tr 'p S) (list 'nthE.eq_2 P Dk) (list 'trF_lift1 'p (up r3)))
      step '(Exp.app (Exp.app (Exp.var 3) (Exp.var 1)) (Exp.var 0))
      hstep0 (list 'pa_mp 'chkf 'p ps Dp S gp opened '(Exp.var 0) hopen hp)
      hstep (list 'rt_cast 'chkf Dp (aw Dp) (aw Dp) step (tr ps S) (L B) hstep0 'rfl
              (list 'Eq.symm (list 'Eq.trans (list 'congrArg '(lift 1 0) (list 'trF_suc 'p r3))
                (list 'trF_lift1 ps (up r3)))))
      s (list 'paBang_map P B step)
      Dstep (ce (bang P) Dk), usstep (cu 'U.u1 (aw Dk))
      hs (list 'rt_cast 'chkf Dstep usstep (cu 'U.u1 (cu 'U.uw (list 'vscale 'U.uw (aw D3))))
           s (bang (L B)) (list 'stepTy (bang P))
           (list 'paBang_map_rt 'chkf Dk P B step hP hB hstep)
           (list 'congrArg '(fn [us :- (List U)] (consU U.u1 (consU U.uw us)))
             (list 'Eq.symm (list 'paAllW_scale D3 'U.uw 'rfl))) 'rfl)
      n '(Exp.var 0), hn (list 'Rt.rVar 'chkf D3 (aw D3) 0 'Exp.tNat 'U.uw (list 'paAllW_len D3)
                          (list 'nthE.eq_2 'Exp.tNat D2) (list 'nthU.eq_2 'U.uw (aw D2)) 'rfl)
      rec (list 'Exp.recN (bang P) z s n), RecTy (list 'subst1 n (bang P))
      hrec (list 'rt_cast 'chkf D3 (list 'vadd (aw D3) (list 'vadd (aw D3) (list 'vscale 'U.uw (aw D3)))) (aw D3)
             rec RecTy RecTy (list 'Rt.rRecN 'chkf D3 (aw D3) (aw D3) (aw D3) (bang P) z s n hn
                              (list 'paBang_form 'chkf Dk P hP) hz hs)
             (list 'Eq.trans (list 'congrArg (list 'vadd (aw D3)) (list 'paAllW_app D3 'U.uw 'rfl))
               (list 'paAllW_add D3)) 'rfl)
      R (tr 'p (up r2)), ru (up r2), gu (list 'paGood_up D2 r2 g2)
      hrec2 (list 'rt_cast 'chkf D3 (aw D3) (aw D3) rec RecTy (bang R) hrec 'rfl
              (list 'congrArg 'paBang (list 'trF_open_var 'p r2)))
      body (list 'Exp.letp R rec '(Exp.var 1))
      hbody (list 'paBang_project 'chkf D3 R rec (list 'trF_form 'chkf 'p D3 ru gu) hrec2)
      en (list 'Exp.lam 'U.uw 'Exp.tNat body), hen (list 'pa_generalize 'chkf 'p D2 r2 body hbody)
      es (list 'Exp.lam 'U.uw A1 en), hes (list 'Rt.rLam 'chkf D1 (aw D1) 'U.uw A1 en (tr '(PF.pall p) r2)
                                           (list 'trF_form 'chkf F D1 r1 g1) hen)
      e (list 'Exp.lam 'U.uw A0 es), he (list 'Rt.rLam 'chkf 'D '(allW D) 'U.uw A0 es
                                        (tr (list 'PF.pimp F '(PF.pall p)) r1)
                                        (list 'trF_form 'chkf p0 'D r0 'hg) hes)]
  (a/prove-theorem 'pa_ax_ind
    '[chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
    (list 'Exists (list 'fn '[e :- Exp] (list 'Rt 'chkf 'D '(allW D) 'e
      (tr (list 'PF.pimp p0 (list 'PF.pimp F '(PF.pall p))) r0))))
    ['(constructor) (list 'exact e) (list 'exact he)]))
