(ns lcert.formal.paclosed
  "F6.3 — closed proof combinators from R4 §6.2, checked by reflection.

  REFL, successor congruence, atomic and bottom stability, the six closed
  arithmetic axiom templates, and COE each inhabit the trF translation of
  their stated closed PA formula, at nil and for every checker and rho.
  Each proof uses dtCheck_sound with a kernel-checked computation.

  COE only converts supplied evidence of 0 = S0 into Empty. No evidence of
  that equation is constructed, and no consistency claim is made here.
  The full PA proof translation and its relative-consistency corollary
  still require the remaining schematic templates and assembly."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b]
            [lcert.formal.patyp :as pt]
            [lcert.formal.dtgen :as g]))

;; Each row fixes both a closed H_PA formula and its concrete proof term.
;; The theorem's conclusion uses patr's trF, independently of dtgen's
;; translation; reflection therefore also checks that the two agree.
(def combinators
  '[[REFL
     (PF.pall (PF.peq (PT.pv 0) (PT.pv 0)))
     (λ :uw [x Nat] (recN [z (T (ap EQ z z))] ⋆ [k y y] x))]
    [CONGS
     (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0))
                (PF.peq (PT.ps (PT.pv 1)) (PT.ps (PT.pv 0))))))
     (λ :uw [a Nat] (λ :uw [b Nat] (λ :uw [e (T (ap EQ a b))] e)))]
    [STABEQ
     (PF.pall (PF.pall
       (PF.pimp (PF.pimp (PF.pimp (PF.peq (PT.pv 1) (PT.pv 0)) PF.pbot) PF.pbot)
                (PF.peq (PT.pv 1) (PT.pv 0)))))
     (λ :uw [a Nat] (λ :uw [b Nat]
       (elimB [z (Π :uw [k (Π :uw [n (Π :uw [w (T z)] Empty)] Empty)] (T z))]
         (ap EQ a b)
         (λ :uw [k (Π :uw [n (Π :uw [w (T tt)] Empty)] Empty)] ⋆)
         (λ :uw [k (Π :uw [n (Π :uw [w (T ff)] Empty)] Empty)]
           (ap k (λ :uw [w (T ff)] w))))))]
    [STABBOT
     (PF.pimp (PF.pimp (PF.pimp PF.pbot PF.pbot) PF.pbot) PF.pbot)
     (λ :uw [k (Π :uw [n (Π :uw [w Empty] Empty)] Empty)]
       (ap k (λ :uw [w Empty] w)))]
    [Q1
     (PF.pall (PF.pimp (PF.peq (PT.ps (PT.pv 0)) PT.pz) PF.pbot))
     (λ :uw [a Nat] (λ :uw [e (T (ap EQ (succ a) zero))] e))]
    [Q2
     (PF.pall (PF.pall
       (PF.pimp (PF.peq (PT.ps (PT.pv 1)) (PT.ps (PT.pv 0)))
                (PF.peq (PT.pv 1) (PT.pv 0)))))
     (λ :uw [a Nat] (λ :uw [b Nat]
       (λ :uw [e (T (ap EQ (succ a) (succ b)))] e)))]
    [Q3
     (PF.pall (PF.peq (PT.padd (PT.pv 0) PT.pz) (PT.pv 0)))
     (λ :uw [a Nat] (ap REFL a))]
    [Q4
     (PF.pall (PF.pall
       (PF.peq (PT.padd (PT.pv 1) (PT.ps (PT.pv 0)))
               (PT.ps (PT.padd (PT.pv 1) (PT.pv 0))))))
     (λ :uw [a Nat] (λ :uw [b Nat] (ap REFL (succ (ap PLUS a b)))))]
    [Q5
     (PF.pall (PF.peq (PT.pmul (PT.pv 0) PT.pz) PT.pz))
     (λ :uw [a Nat] (ap REFL zero))]
    [Q6
     (PF.pall (PF.pall
       (PF.peq (PT.pmul (PT.pv 1) (PT.ps (PT.pv 0)))
               (PT.padd (PT.pmul (PT.pv 1) (PT.pv 0)) (PT.pv 1)))))
     (λ :uw [a Nat] (λ :uw [b Nat] (ap REFL (ap PLUS (ap TIMES a b) a))))]
    [COE
     (PF.pimp (PF.peq PT.pz (PT.ps PT.pz)) PF.pbot)
     (λ :uw [e (T (ap EQ zero (succ zero)))] e)]])

(def constants
  (reduce (fn [cs [nm _ form]] (assoc cs nm (g/compile-term cs form)))
    pt/constants combinators))
(def names
  (into pt/names (for [[nm _ _] combinators]
    [(constants nm) (symbol (str "paC_" nm))])))

(doseq [[nm formula _] combinators]
  (let [term (symbol (str "paC_" nm)), fname (symbol (str "paF_" nm))
        theorem (symbol (str term "_rt"))]
    (b/kdef! fname 'PF formula)
    ;; Do not fold the term to its own name before defining it.
    (binding [g/*names* (dissoc names (constants nm))]
      (b/kdef! term 'Exp (g/sx (constants nm))))
    (binding [g/*names* names]
      (let [ty (g/pa-formula constants formula identity)
            tree (g/rt [] [] (constants nm) ty)
            [defs tree] (g/share (str "paclosed_" nm) 30 tree)
            A (list 'trF fname 'ρ)
            J (list 'DTJ.rt '(List.nil Exp) '(List.nil U) term A)]
        (doseq [[name sort body] defs] (b/kdef! name sort body))
        (a/prove-theorem theorem '[chkf :- (=> Code Code Bool), ρ :- (=> Nat Nat)]
          (list 'Rt 'chkf '(List.nil Exp) '(List.nil U) term A)
          [(list 'exact (list 'dtCheck_sound 'chkf tree J '(Eq.refl Bool.true)))])))))
