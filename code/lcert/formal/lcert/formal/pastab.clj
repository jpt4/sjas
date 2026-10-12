(ns lcert.formal.pastab
  "F6.3 — stability of every translated PA formula (R4 §6.2, DN).

  The induction constructs object-calculus derivations at allW in every
  Good environment. It does not appeal to PA semantic soundness."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.paaxioms]
            [lcert.formal.paprop :as pp]))
(kdef paDN (=> PF PF)
  (fn [p :- PF] (PF.pimp (PF.pimp (PF.pimp p PF.pbot) PF.pbot) p)))
(kdef paStable (=> (=> Code Code Bool) PF Prop)
  (fn [chkf :- (=> Code Code Bool), p :- PF]
    (forall [D (List Exp)] (forall [ρ (=> Nat Nat)] (=> (Good D ρ)
      (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF (paDN p) ρ)))))))))

;; Instantiating the first argument under the second universal binder
;; shifts it. The second instantiation cancels exactly that shift.
(thm pa_stab_eq [chkf :- (=> Code Code Bool), s :- PT, t :- PT,
                 D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
  (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF (paDN (PF.peq s t)) ρ))))
  (have h (Rt chkf D (allW D)
    (Exp.app (Exp.app (paPad (trF paF_STABEQ ρ) paC_STABEQ) (trT s ρ)) (trT t ρ))
    (trF (paDN (PF.peq (paSbT (paInst t) (paSbT paShT s)) t)) ρ))
    (pa_instantiate chkf
      (paDN (PF.peq (paSbT paShT s) (PT.pv 0))) t D ρ hg
      (Exp.app (paPad (trF paF_STABEQ ρ) paC_STABEQ) (trT s ρ))
      (pa_instantiate chkf
        (PF.pall (paDN (PF.peq (PT.pv 1) (PT.pv 0)))) s D ρ hg
        (paPad (trF paF_STABEQ ρ) paC_STABEQ) (paC_STABEQ_at chkf D ρ))))
  (constructor)
  (exact (Exp.app (Exp.app (paPad (trF paF_STABEQ ρ) paC_STABEQ) (trT s ρ)) (trT t ρ)))
  (exact (rt_cast chkf D (allW D) (allW D)
    (Exp.app (Exp.app (paPad (trF paF_STABEQ ρ) paC_STABEQ) (trT s ρ)) (trT t ρ))
    (trF (paDN (PF.peq (paSbT (paInst t) (paSbT paShT s)) t)) ρ)
    (trF (paDN (PF.peq s t)) ρ) h rfl
    (congrArg (fn [v :- PT] (trF (paDN (PF.peq v t)) ρ)) (paSbT_inst_shift s t)))))
(thm pa_stab_bot [chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat)]
  (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF (paDN PF.pbot) ρ))))
  (constructor) (exact (paPad (trF paF_STABBOT ρ) paC_STABBOT))
  (exact (paC_STABBOT_at chkf D ρ)))

(defn- imp [p q] (list 'PF.pimp p q))
(defn- neg [p] (imp p 'PF.pbot))
(defn- dn [p] (imp (neg (neg p)) p))
(defn- tr [p r] (list 'trF p r))
(defn- con [A D] (list 'List.cons 'Exp A D))
(defn- aw [D] (list 'allW D))
(defn- suc [r] (list 'trSucR r))
(defn- lift-eq [p r n]
  (if (= n 1) (list 'trF_lift1 p r)
    (list 'Eq.trans (list 'Eq.symm (list 'lift_comp (tr p r) 1 (dec n) 0))
      (list 'Eq.trans (list 'congrArg '(lift 1 0) (lift-eq p r (dec n)))
        (list 'trF_lift1 p (nth (iterate suc r) (dec n)))))))
(defn- hyp [ctx i]
  (let [{:keys [D rho stack]} ctx
        {p :p A :A born :rho} (nth stack i)]
    {:p p :e (list 'Exp.var i)
     :proof (list 'paHyp_allW 'chkf D i A (tr p rho)
              (#'pp/lookup-proof D stack i) (lift-eq p born (inc i)))}))

;; Implication: lambda k a. STAB(q) (lambda nb. k (lambda f. nb (f a))).
;; Select STAB(q) at the context after k and a, before constructing nb.
(let [p 'p, q 'q, F (imp p q), initial {:D 'D :rho 'ρ :good 'hg :stack []}
      ck (#'pp/extend-proof initial (neg (neg F)))
      ca (#'pp/extend-proof ck p)
      result (#'pp/lam initial (neg (neg F))
        (fn [_] (#'pp/lam ck p
          (fn [_] (#'pp/app ca {:p (dn q) :e 's :proof 'hs}
            (#'pp/lam ca (neg q)
              (fn [cn] (#'pp/app cn (hyp cn 2)
                (#'pp/lam cn F
                  (fn [cf] (#'pp/app cf (hyp cf 1)
                    (#'pp/app cf (hyp cf 0) (hyp cf 2)))))))))))))
      goal (list 'Exists (list 'fn '[e :- Exp] (list 'Rt 'chkf 'D '(allW D) 'e (tr (dn F) 'ρ))))]
  (a/prove-theorem 'pa_stab_imp
    '[chkf :- (=> Code Code Bool), p :- PF, q :- PF, ih :- (paStable chkf q),
      D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)] goal
    [(list 'refine' (list 'exists_elimE '_ '_ (list 'ih (:D ca) (:rho ca) (:good ca)) '_))
     '(intro s hs) '(constructor) (list 'exact (:e result)) (list 'exact (:proof result))]))

;; In the forall case, f is beneath k, the Nat, nb and f itself. Its
;; argument is Nat variable 2. These pointwise equations track precisely
;; the two proof binders inserted after the Nat binder.
(thm paStab_open_agree [ρ :- (=> Nat Nat), i :- Nat]
  (Eq Exp
    (inst1 (Exp.var 2) (trUpR (trSucR (trSucR (trSucR (trSucR ρ)))) i))
    (trT (PT.pv i) (trSucR (trSucR (trUpR (trSucR ρ))))))
  (cases i) (rfl) (rfl))
(thm paStab_open_eq [p :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp
    (subst1 (Exp.var 2) (trF p (trUpR (trSucR (trSucR (trSucR (trSucR ρ)))))))
    (trF p (trSucR (trSucR (trUpR (trSucR ρ))))))
  (exact (Eq.trans
    (trF_subst p (fn [j :- Nat] (inst1 (Exp.var 2) j))
      (trUpR (trSucR (trSucR (trSucR (trSucR ρ))))) PT.pv
      (trSucR (trSucR (trUpR (trSucR ρ)))) (paStab_open_agree ρ))
    (congrArg (fn [v :- PF] (trF v (trSucR (trSucR (trUpR (trSucR ρ)))))) (paSbF_id p)))))

(let [r0 'ρ, r1 (suc r0), r2 (suc r1), r3 (suc r2), r4 (suc r3)
      ru (list 'trUpR r1), ru1 (suc ru), ru2 (suc ru1)
      F '(PF.pall p), K (neg (neg F)), A (tr K r0)
      D1 (con A 'D), D2 (con 'Exp.tNat D1), B (tr (neg 'p) ru)
      D3 (con B D2), C (tr F r3), D4 (con C D3)
      g1 (list 'paGood_suc 'D r0 'hg A)
      g2 (list 'paGood_suc D1 r1 g1 'Exp.tNat)
      g3 (list 'paGood_suc D2 r2 g2 B)
      g4 (list 'paGood_suc D3 r3 g3 C)
      gu (list 'paGood_up D1 r1 g1)
      gu1 (list 'paGood_suc D2 ru gu B)
      gu2 (list 'paGood_suc D3 ru1 gu1 C)
      hf (list 'paHyp_allW 'chkf D4 0 C (tr F r4) (list 'nthE.eq_2 C D3) (lift-eq F r3 1))
      hn (list 'paHyp_allW 'chkf D4 1 B (tr (neg 'p) ru2)
           (list 'Eq.trans (list 'nthE.eq_3 C D3 0) (list 'nthE.eq_2 B D2)) (lift-eq (neg 'p) ru 2))
      hk (list 'paHyp_allW 'chkf D3 2 A (tr K r3)
           (list 'Eq.trans (list 'nthE.eq_3 B D2 1)
             (list 'Eq.trans (list 'nthE.eq_3 'Exp.tNat D1 0) (list 'nthE.eq_2 A 'D)))
           (lift-eq K r0 3))
      opened '(Exp.app (Exp.var 0) (Exp.var 2))
      hopen (list 'rt_cast 'chkf D4 (list 'vadd (aw D4) (list 'vscale 'U.uw (aw D4))) (aw D4)
              opened (list 'subst1 '(Exp.var 2) (tr 'p (list 'trUpR r4))) (tr 'p ru2)
              (list 'Rt.rApp 'chkf D4 (aw D4) (aw D4) 'U.uw '(Exp.var 0) '(Exp.var 2)
                'Exp.tNat (tr 'p (list 'trUpR r4)) 'rfl hf
                (list 'trT_rt 'chkf '(PT.pv 0) D4 ru2 gu2)
                (list 'Tl.fBase 'chkf D4 'Exp.tNat 'rfl)
                (list 'trF_form 'chkf 'p (con 'Exp.tNat D4) (list 'trUpR r4)
                  (list 'paGood_up D4 r4 g4)))
              (list 'paAllW_app D4 'U.uw 'rfl) '(paStab_open_eq p ρ))
      inn (list 'Exp.app '(Exp.var 1) opened)
      hinn (list 'pa_mp 'chkf 'p 'PF.pbot D4 ru2 gu2 '(Exp.var 1) opened hn hopen)
      ef (list 'Exp.lam 'U.uw C inn)
      hef (list 'Rt.rLam 'chkf D3 (aw D3) 'U.uw C inn 'Exp.tEmpty
            (list 'trF_form 'chkf F D3 r3 g3) hinn)
      ek (list 'Exp.app '(Exp.var 2) ef)
      hek (list 'pa_mp 'chkf (neg F) 'PF.pbot D3 r3 g3 '(Exp.var 2) ef hk hef)
      en (list 'Exp.lam 'U.uw B ek)
      hen (list 'Rt.rLam 'chkf D2 (aw D2) 'U.uw B ek 'Exp.tEmpty
            (list 'trF_form 'chkf (neg 'p) D2 ru gu) hek)
      ep (list 'Exp.app 's en)
      hep (list 'pa_mp 'chkf (neg (neg 'p)) 'p D2 ru gu 's en 'hs hen)
      ex (list 'Exp.lam 'U.uw 'Exp.tNat ep)
      hex (list 'pa_generalize 'chkf 'p D1 r1 ep hep)
      e (list 'Exp.lam 'U.uw A ex)
      he (list 'Rt.rLam 'chkf 'D '(allW D) 'U.uw A ex (tr F r1)
           (list 'trF_form 'chkf K 'D r0 'hg) hex)]
  (a/prove-theorem 'pa_stab_all
    '[chkf :- (=> Code Code Bool), p :- PF, ih :- (paStable chkf p),
      D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
    (list 'Exists (list 'fn '[e :- Exp] (list 'Rt 'chkf 'D '(allW D) 'e (tr (dn F) r0))))
    [(list 'refine' (list 'exists_elimE '_ '_ (list 'ih D2 ru gu) '_))
     '(intro s hs) '(constructor) (list 'exact e) (list 'exact he)]))

(thm pa_ax_dn [chkf :- (=> Code Code Bool), f0 :- PF]
  (paStable chkf f0)
  (induction f0)
  (intro D ρ hg) (exact (pa_stab_eq chkf t u D ρ hg))
  (intro D ρ hg) (exact (pa_stab_bot chkf D ρ))
  (intro D ρ hg) (exact (pa_stab_imp chkf p q ih_q D ρ hg))
  (intro D ρ hg) (exact (pa_stab_all chkf p ih_p D ρ hg)))
