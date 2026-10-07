(ns lcert.formal.paaxioms
  "F6.3 — arbitrary-environment E1 and Q1–Q6 templates (R4 §6.2)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.paprop]
            [lcert.formal.paclosed :as pc]
            [lcert.formal.dtgen :as g]))

;; A closed constant weakened from nil uses allZ. Give it an allW
;; derivation by applying an identity whose ambient vector is allW.
;; This uses the existing rVar rule's allowed unused omega entries.
(kdef paPad (=> Exp Exp Exp)
  (fn [A :- Exp, e :- Exp] (Exp.app (Exp.lam U.uw A (Exp.var 0)) e)))
(thm paAllW_zero_app [D :- (List Exp)]
  (Eq (List U) (vadd (allW D) (vscale U.uw (allZ D))) (allW D))
  (induction D) (rfl)
  (have e (Eq (List U)
    (List.cons U U.uw (vadd (allW tail) (vscale U.uw (allZ tail))))
    (List.cons U U.uw (allW tail)))
    (congrArg (fn [xs :- (List U)] (List.cons U U.uw xs)) ih_tail)) (exact e))
(thm pa_pad_closed [chkf :- (=> Code Code Bool), D :- (List Exp), e :- Exp, A :- Exp,
                   he :- (Eq Exp (lift 1 0 e) e), hA :- (Eq Exp (lift 1 0 A) A),
                   hs :- (Eq Exp (subst1 e A) A),
                   hf :- (Tl chkf Bool.true (List.nil Exp) A Exp.tUnit),
                   ht :- (Rt chkf (List.nil Exp) (List.nil U) e A)]
  (Rt chkf D (allW D) (paPad A e) A)
  (exact (rt_cast chkf D (vadd (allW D) (vscale U.uw (allZ D))) (allW D)
    (paPad A e) (subst1 e A) A
    (Rt.rApp chkf D (allW D) (allZ D) U.uw (Exp.lam U.uw A (Exp.var 0)) e A A
      (Eq.refl Bool.true)
      (Rt.rLam chkf D (allW D) U.uw A (Exp.var 0) A
        (paClosed_tl chkf Bool.true A Exp.tUnit hA rfl hf D)
        (paHyp_allW chkf (List.cons Exp A D) 0 A A (nthE.eq_2 A D) hA))
      (paClosed_rt chkf e A he hA ht D)
      (paClosed_tl chkf Bool.true A Exp.tUnit hA rfl hf D)
      (paClosed_tl chkf Bool.true A Exp.tUnit hA rfl hf (List.cons Exp A D)))
    (paAllW_zero_app D) hs)))

;; Formation of each closed type, then its padded constant at allW in an
;; arbitrary telescope. rho is arbitrary because these formulas are closed.
(doseq [[nm formula _] pc/combinators]
  (let [term (symbol (str "paC_" nm)), fname (symbol (str "paF_" nm))
        A (list 'trF fname 'ρ), form-name (symbol (str term "_form"))
        ty (g/pa-formula pc/constants formula identity)]
    (binding [g/*names* pc/names]
      (let [[defs tree] (g/share (str "paaxioms_form_" nm) 30 (g/tl-form [] ty))]
        (doseq [[n sort body] defs] (b/kdef! n sort body))
        (a/prove-theorem form-name '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
          (list 'Tl 'chkf 'Bool.true '(List.nil Exp) A 'Exp.tUnit)
          [(list 'exact (list 'dtCheck_sound 'chkf tree
             (list 'DTJ.tl 'Bool.true '(List.nil Exp) A 'Exp.tUnit) 'rfl))])))
    (a/prove-theorem (symbol (str term "_at"))
      '[chkf :- (=> Code Code Bool), D :- (List Exp), ρ :- (=> Nat Nat)]
      (list 'Rt 'chkf 'D '(allW D) (list 'paPad A term) A)
      [(list 'exact (list 'pa_pad_closed 'chkf 'D term A 'rfl 'rfl 'rfl
         (list form-name 'chkf 'ρ) (list (symbol (str term "_rt")) 'chkf 'ρ)))])))

;; Each instance below is an exact PAx constructor's formula. In the
;; binary cases the first instantiation goes under the remaining forall
;; through paUp; the second removes that binder. These are concrete PA
;; variable indices, so the final formula equations reduce definitionally.
(def axiom-instances
  '[[pa_ax_e1 REFL [(PT.pv i)] (PF.peq (PT.pv i) (PT.pv i))]
    [pa_ax_q1 Q1 [(PT.pv 0)] (PF.pimp (PF.peq (PT.ps (PT.pv 0)) PT.pz) PF.pbot)]
    [pa_ax_q2 Q2 [(PT.pv 0) (PT.pv 1)]
     (PF.pimp (PF.peq (PT.ps (PT.pv 0)) (PT.ps (PT.pv 1))) (PF.peq (PT.pv 0) (PT.pv 1)))]
    [pa_ax_q3 Q3 [(PT.pv 0)] (PF.peq (PT.padd (PT.pv 0) PT.pz) (PT.pv 0))]
    [pa_ax_q4 Q4 [(PT.pv 0) (PT.pv 1)]
     (PF.peq (PT.padd (PT.pv 0) (PT.ps (PT.pv 1))) (PT.ps (PT.padd (PT.pv 0) (PT.pv 1))))]
    [pa_ax_q5 Q5 [(PT.pv 0)] (PF.peq (PT.pmul (PT.pv 0) PT.pz) PT.pz)]
    [pa_ax_q6 Q6 [(PT.pv 0) (PT.pv 1)]
     (PF.peq (PT.pmul (PT.pv 0) (PT.ps (PT.pv 1)))
             (PT.padd (PT.pmul (PT.pv 0) (PT.pv 1)) (PT.pv 0)))]] )

(doseq [[theorem nm args formula] axiom-instances]
  (let [closed-formula (second (first (filter #(= nm (first %)) pc/combinators)))
        term (symbol (str "paC_" nm)), fname (symbol (str "paF_" nm))
        e0 (list 'paPad (list 'trF fname 'ρ) term)
        p0 (list (symbol (str term "_at")) 'chkf 'D 'ρ)
        p1 (second closed-formula), t1 (first args)
        e1 (list 'Exp.app e0 (list 'trT t1 'ρ))
        h1 (list 'pa_instantiate 'chkf p1 t1 'D 'ρ 'hg e0 p0)
        [e h] (if (= 1 (count args)) [e1 h1]
                (let [p2 (list 'paSbF (list 'paUp (list 'paInst t1)) (second p1))
                      t2 (second args)]
                  [(list 'Exp.app e1 (list 'trT t2 'ρ))
                   (list 'pa_instantiate 'chkf p2 t2 'D 'ρ 'hg e1 h1)]))
        params (into (if (= theorem 'pa_ax_e1)
                       '[chkf :- (=> Code Code Bool), i :- Nat]
                       '[chkf :- (=> Code Code Bool)])
                     '[D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)])]
    (a/prove-theorem theorem params
      (list 'Rt 'chkf 'D '(allW D) e (list 'trF formula 'ρ)) [(list 'exact h)])))
