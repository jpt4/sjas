(ns lcert.formal.paclosure
  "A05: finite-support closure of the PA formula translation in R4 §6.2.

  The functions are definitions in the checked calculus.  A support bound
  counts de Bruijn indices that may occur free; under a PA quantifier the
  bound drops by one.  Closing with that many Nat binders is the bounded
  form of the paper's ordered universal closure."
  (:require [lcert.formal.base :refer [kdef thm]]
            [lcert.formal.patr]
            [lcert.formal.paform]))

;; Nat.max is not exported by Ansatz's Init snapshot.  Addition after
;; truncated subtraction computes the same maximum on natural numbers.
(kdef paMax (=> Nat Nat Nat)
  (fn [a :- Nat, b :- Nat] (Nat.add a (Nat.sub b a))))

;; One greater than the largest possible free variable index in a PA term.
;; The maximum of two supports preserves a bound for both subterms.
(kdef paSupportT (=> PT Nat)
  (fn [t :- PT]
    (PT.rec$1 (fn [_ :- PT] Nat)
      (fn [i :- Nat] (Nat.succ i))
      0
      (fn [a :- PT, ia :- Nat] ia)
      (fn [a :- PT, b :- PT, ia :- Nat, ib :- Nat] (paMax ia ib))
      (fn [a :- PT, b :- PT, ia :- Nat, ib :- Nat] (paMax ia ib))
      t)))

;; PA's forall binds index zero, so its free-variable support is the body
;; support with that binder removed.  Implication combines both supports.
(kdef paSupportF (=> PF Nat)
  (fn [p :- PF]
    (PF.rec$1 (fn [_ :- PF] Nat)
      (fn [a :- PT, b :- PT] (paMax (paSupportT a) (paSupportT b)))
      0
      (fn [a :- PF, b :- PF, ia :- Nat, ib :- Nat] (paMax ia ib))
      (fn [a :- PF, ia :- Nat] (Nat.sub ia 1))
      p)))

;; A formula translated under n Nat entries uses precisely the n inner
;; binders in the closed type.  The declaration is a function, so the same
;; closure used by proofs is available to extracted code.
(kdef paCloseN (=> Nat Exp Exp)
  (fn [n :- Nat, A :- Exp]
    ((Nat.rec$1 (fn [_ :- Nat] (=> Exp Exp))
       (fn [B :- Exp] B)
       (fn [k :- Nat, ih :- (=> Exp Exp)]
         (fn [B :- Exp] (ih (Exp.tPi U.uw Exp.tNat B))))
       n) A)))

(kdef paCloseF (=> PF Exp)
  (fn [p :- PF]
    (paCloseN (paSupportF p) (trF p (fn [i :- Nat] i)))))

;; The context is ordered from the innermost Nat entry outward.  Closing a
;; judgment removes the head entry first, and its usage is omega.
(kdef paNatD (=> Nat (List Exp))
  (fn [n :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (List Exp))
      (List.nil Exp)
      (fn [k :- Nat, ih :- (List Exp)] (List.cons Exp Exp.tNat ih))
      n)))

(kdef paCloseLam (=> Nat Exp Exp)
  (fn [n :- Nat, e :- Exp]
    ((Nat.rec$1 (fn [_ :- Nat] (=> Exp Exp))
       (fn [t :- Exp] t)
       (fn [k :- Nat, ih :- (=> Exp Exp)]
         (fn [t :- Exp] (ih (Exp.lam U.uw Exp.tNat t))))
       n) e)))

(thm paClose_intro [chkf :- (=> Code Code Bool), n :- Nat]
  (forall [e Exp] (forall [A Exp]
    (=> (Rt chkf (paNatD n) (allW (paNatD n)) e A)
        (Rt chkf (List.nil Exp) (List.nil U)
          (paCloseLam n e) (paCloseN n A)))))
  (induction n)
  (intro e A he)
  (exact he)
  (intro e A he)
  (have hl (Rt chkf (paNatD n) (allW (paNatD n))
             (Exp.lam U.uw Exp.tNat e) (Exp.tPi U.uw Exp.tNat A))
    (Rt.rLam chkf (paNatD n) (allW (paNatD n)) U.uw Exp.tNat e A
      (Tl.fBase chkf (paNatD n) Exp.tNat rfl) he))
  (exact (ih_n (Exp.lam U.uw Exp.tNat e)
           (Exp.tPi U.uw Exp.tNat A) hl)))
