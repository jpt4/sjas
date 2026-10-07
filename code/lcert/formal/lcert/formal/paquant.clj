(ns lcert.formal.paquant
  "F6.3 — the A5 quantifier template of R4 §6.2."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.paprop]))

;; Identity substitution only on translated formulas, obtained from patr's
;; existing substitution theorem and the PA syntactic identity theorem.
(thm trF_identity_subst [p :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (subst Exp.var (trF p ρ)) (trF p ρ))
  (exact (Eq.trans
    (trF_subst p Exp.var ρ PT.pv ρ (fn [i :- Nat] (Eq.refl (Exp.var (ρ i)))))
    (congrArg (fn [q :- PF] (trF q ρ)) (paSbF_id p)))))

;; A5's "not free" condition is represented by paSbF paShT. The two
;; translations agree after a Nat binder versus a proof binder is added.
(thm trF_shift [p :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (trF (paSbF paShT p) (trUpR ρ)) (trF p (trSucR ρ)))
  (exact (Eq.symm (Eq.trans
    (Eq.symm (trF_identity_subst p (trSucR ρ)))
    (trF_subst p Exp.var (trSucR ρ) paShT (trUpR ρ)
      (fn [i :- Nat] (Eq.refl (Exp.var (Nat.succ (ρ i))))))))))

(thm paOpen_agree [ρ :- (=> Nat Nat), i :- Nat]
  (Eq Exp (inst1 (Exp.var 0) (trUpR (trSucR ρ) i)) (trT (PT.pv i) (trUpR ρ)))
  (cases i) (rfl) (rfl))
(thm trF_open_var [p :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (subst1 (Exp.var 0) (trF p (trUpR (trSucR ρ)))) (trF p (trUpR ρ)))
  (exact (Eq.trans
    (trF_subst p (fn [j :- Nat] (inst1 (Exp.var 0) j))
      (trUpR (trSucR ρ)) PT.pv (trUpR ρ) (paOpen_agree ρ))
    (congrArg (fn [q :- PF] (trF q (trUpR ρ))) (paSbF_id p)))))

;; Universal elimination specifically at the newly bound Nat variable.
;; Both environments on the extended telescope are Good: trSucR ignores
;; the new Nat, whereas trUpR assigns it to PA variable zero.
(thm pa_open [chkf :- (=> Code Code Bool), p :- PF, D :- (List Exp),
              ρ :- (=> Nat Nat), hg :- (Good D ρ), f :- Exp,
              hf :- (Rt chkf (List.cons Exp Exp.tNat D)
                      (allW (List.cons Exp Exp.tNat D)) f
                      (trF (PF.pall p) (trSucR ρ)))]
  (Rt chkf (List.cons Exp Exp.tNat D) (allW (List.cons Exp Exp.tNat D))
    (Exp.app f (Exp.var 0)) (trF p (trUpR ρ)))
  (exact (rt_cast chkf (List.cons Exp Exp.tNat D)
    (vadd (allW (List.cons Exp Exp.tNat D))
          (vscale U.uw (allW (List.cons Exp Exp.tNat D))))
    (allW (List.cons Exp Exp.tNat D)) (Exp.app f (Exp.var 0))
    (subst1 (Exp.var 0) (trF p (trUpR (trSucR ρ)))) (trF p (trUpR ρ))
    (Rt.rApp chkf (List.cons Exp Exp.tNat D)
      (allW (List.cons Exp Exp.tNat D)) (allW (List.cons Exp Exp.tNat D)) U.uw
      f (Exp.var 0) Exp.tNat (trF p (trUpR (trSucR ρ))) (Eq.refl Bool.true) hf
      (trT_rt chkf (PT.pv 0) (List.cons Exp Exp.tNat D) (trUpR ρ) (paGood_up D ρ hg))
      (Tl.fBase chkf (List.cons Exp Exp.tNat D) Exp.tNat (Eq.refl Bool.true))
      (trF_form chkf p (List.cons Exp Exp.tNat (List.cons Exp Exp.tNat D))
        (trUpR (trSucR ρ))
        (paGood_up (List.cons Exp Exp.tNat D) (trSucR ρ) (paGood_suc D ρ hg Exp.tNat))))
    (paAllW_app (List.cons Exp Exp.tNat D) U.uw (Eq.refl Bool.true))
    (trF_open_var p ρ))))

;; A5: lambda f a n. f n a. This emitter only abbreviates repeated
;; telescopes; its output applies the displayed typing rules directly.
(let [tr (fn [p rho] (list 'trF p rho))
      con (fn [A D] (list 'List.cons 'Exp A D))
      aw (fn [D] (list 'allW D))
      r1 '(trSucR ρ), r2 (list 'trSucR r1), r3 (list 'trSucR r2)
      ru (list 'trUpR r2)
      shifted '(paSbF paShT p)
      body (list 'PF.pimp shifted 'q), F (list 'PF.pall body)
      A (tr F 'ρ), B (tr 'p r1)
      D1 (con A 'D), D2 (con B D1), D3 (con 'Exp.tNat D2)
      g1 (list 'paGood_suc 'D 'ρ 'hg A)
      g2 (list 'paGood_suc D1 r1 g1 B)
      gu (list 'paGood_up D2 r2 g2)
      f '(Exp.var 2), v '(Exp.var 1)
      lf (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D2 1)
           (list 'Eq.trans (list 'nthE.eq_3 B D1 0) (list 'nthE.eq_2 A 'D)))
      la (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D2 0) (list 'nthE.eq_2 B D1))
      hf (list 'paHyp_allW 'chkf D3 2 A (tr F r3) lf (list 'trF_lift3 F 'ρ))
      ha (list 'paHyp_allW 'chkf D3 1 B (tr shifted ru) la
           (list 'Eq.trans (list 'trF_lift2 'p r1) (list 'Eq.symm (list 'trF_shift 'p r2))))
      opened (list 'Exp.app f '(Exp.var 0))
      hopen (list 'pa_open 'chkf body D2 r2 g2 f hf)
      e (list 'Exp.app opened v)
      he (list 'pa_mp 'chkf shifted 'q D3 ru gu opened v hopen ha)
      en (list 'Exp.lam 'U.uw 'Exp.tNat e)
      hn (list 'pa_generalize 'chkf 'q D2 r2 e he)
      ea (list 'Exp.lam 'U.uw B en)
      ha-lam (list 'Rt.rLam 'chkf D1 (aw D1) 'U.uw B en (tr '(PF.pall q) r2)
               (list 'trF_form 'chkf 'p D1 r1 g1) hn)
      ef (list 'Exp.lam 'U.uw A ea)
      h (list 'Rt.rLam 'chkf 'D '(allW D) 'U.uw A ea (tr '(PF.pimp p (PF.pall q)) r1)
          (list 'trF_form 'chkf F 'D 'ρ 'hg) ha-lam)
      phi (list 'PF.pimp F '(PF.pimp p (PF.pall q)))]
  (a/prove-theorem 'pa_ax_a5
    '[chkf :- (=> Code Code Bool), p :- PF, q :- PF,
      D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
    (list 'Rt 'chkf 'D '(allW D) ef (tr phi 'ρ)) [(list 'exact h)]))
