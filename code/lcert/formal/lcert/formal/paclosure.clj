(ns lcert.formal.paclosure
  "A05: finite-support closure of the PA formula translation in R4 §6.2.

  The functions are definitions in the checked calculus.  A support bound
  counts de Bruijn indices that may occur free; under a PA quantifier the
  bound drops by one.  Closing with that many Nat binders is the bounded
  form of the paper's ordered universal closure."
  (:require [lcert.formal.base :refer [kdef thm]]
            [lcert.formal.patr]
            [lcert.formal.paform]
            [lcert.formal.palcert]))

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

;; These two inequalities justify restricting an agreement hypothesis from
;; a compound syntax node to either child.  Nat.sub is truncated subtraction.
(thm paMax_left [a :- Nat, b :- Nat]
  (LE.le a (paMax a b))
  (unfold paMax)
  (exact (Nat.le_add_right a (Nat.sub b a))))

(thm paMax_right [a :- Nat, b :- Nat]
  (LE.le b (paMax a b))
  (have hor (Or (LE.le a b) (LE.le b a)) (Nat.le_total a b))
  (cases hor)
  (have hs (Eq Nat (+ (Nat.sub b a) a) b) (Nat.sub_add_cancel h))
  (have hc (Eq Nat (+ a (Nat.sub b a)) (+ (Nat.sub b a) a))
    (Nat.add_comm a (Nat.sub b a)))
  (have q (Eq Nat (paMax a b) b) (Eq.trans (Eq.trans rfl hc) hs))
  (exact (Eq.mpr (congrArg (fn [x :- Nat] (LE.le b x)) q)
           (Nat.le_refl b)))
  (have q (Eq Nat (paMax a b) a)
    (Eq.trans
      (congrArg (fn [x :- Nat] (+ a x)) (Nat.sub_eq_zero_of_le h))
      (Nat.add_zero a)))
  (exact (Eq.mpr (congrArg (fn [x :- Nat] (LE.le b x)) q) h)))

(kdef paAgree (=> Nat (=> Nat Nat) (=> Nat Nat) Prop)
  (fn [n :- Nat, ρ :- (=> Nat Nat), σ :- (=> Nat Nat)]
    (forall [i Nat] (=> (LT.lt i n) (Eq Nat (ρ i) (σ i))))))

;; The translated PA term reads only the indices below its computed bound.
;; Each binary constructor projects agreement through the two paMax bounds.
(thm trT_support [t :- PT]
  (forall [ρ (=> Nat Nat)] (forall [σ (=> Nat Nat)]
    (=> (paAgree (paSupportT t) ρ σ)
        (Eq Exp (trT t ρ) (trT t σ)))))
  (induction t)
  (intro ρ σ h)
  (exact (congrArg Exp.var (h i (Nat.lt_succ_self i))))
  (intro ρ σ h) (rfl)
  (intro ρ σ h)
  (exact (congrArg Exp.succ (ih_t ρ σ h)))
  (intro ρ σ h)
  (have ht (paAgree (paSupportT t) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT t))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_left (paSupportT t) (paSupportT u))))))
  (have hu (paAgree (paSupportT u) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT u))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_right (paSupportT t) (paSupportT u))))))
  (have et (Eq Exp (trT t ρ) (trT t σ)) (ih_t ρ σ ht))
  (have eu (Eq Exp (trT u ρ) (trT u σ)) (ih_u ρ σ hu))
  (exact (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trPLUS x))) et) eu))
  (intro ρ σ h)
  (have ht (paAgree (paSupportT t) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT t))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_left (paSupportT t) (paSupportT u))))))
  (have hu (paAgree (paSupportT u) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT u))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_right (paSupportT t) (paSupportT u))))))
  (have et (Eq Exp (trT t ρ) (trT t σ)) (ih_t ρ σ ht))
  (have eu (Eq Exp (trT u ρ) (trT u σ)) (ih_u ρ σ hu))
  (exact (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trTIMES x))) et) eu)))

(thm paSupp_pred [s :- Nat, k :- Nat,
  h :- (LT.lt (Nat.succ k) s)]
  (LT.lt k (Nat.sub s 1))
  (omega))

(thm paAgree_up [s :- Nat, ρ :- (=> Nat Nat), σ :- (=> Nat Nat),
  h :- (paAgree (Nat.sub s 1) ρ σ)]
  (paAgree s (trUpR ρ) (trUpR σ))
  (intro i hi)
  (cases i)
  (rfl)
  (exact (congrArg Nat.succ (h n (paSupp_pred s n hi)))))

;; In the forall case, index zero is the bound Nat variable and each
;; successor index refers to one of the prior free variables.
(thm trF_support [p :- PF]
  (forall [ρ (=> Nat Nat)] (forall [σ (=> Nat Nat)]
    (=> (paAgree (paSupportF p) ρ σ)
        (Eq Exp (trF p ρ) (trF p σ)))))
  (induction p)
  (intro ρ σ h)
  (have ht (paAgree (paSupportT t) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT t))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_left (paSupportT t) (paSupportT u))))))
  (have hu (paAgree (paSupportT u) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportT u))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_right (paSupportT t) (paSupportT u))))))
  (have et (Eq Exp (trT t ρ) (trT t σ)) (trT_support t ρ σ ht))
  (have eu (Eq Exp (trT u ρ) (trT u σ)) (trT_support u ρ σ hu))
  (exact (congrArg Exp.tT
    (congr (congrArg (fn [x :- Exp] (Exp.app (Exp.app trEQ x))) et) eu)))
  (intro ρ σ h) (rfl)
  (intro ρ σ h)
  (have hp (paAgree (paSupportF p) ρ σ)
    (fn [i :- Nat, hi :- (LT.lt i (paSupportF p))]
      (h i (Nat.lt_of_lt_of_le hi
             (paMax_left (paSupportF p) (paSupportF q))))))
  (have hq (paAgree (paSupportF q) (trSucR ρ) (trSucR σ))
    (fn [i :- Nat, hi :- (LT.lt i (paSupportF q))]
      (congrArg Nat.succ
        (h i (Nat.lt_of_lt_of_le hi
               (paMax_right (paSupportF p) (paSupportF q)))))))
  (have ep (Eq Exp (trF p ρ) (trF p σ)) (ih_p ρ σ hp))
  (have eq (Eq Exp (trF q (trSucR ρ)) (trF q (trSucR σ)))
    (ih_q (trSucR ρ) (trSucR σ) hq))
  (exact (congr (congrArg (fn [x :- Exp] (Exp.tPi U.uw x)) ep) eq))
  (intro ρ σ h)
  (have hp (paAgree (paSupportF p) (trUpR ρ) (trUpR σ))
    (paAgree_up (paSupportF p) ρ σ h))
  (exact (congrArg (fn [x :- Exp] (Exp.tPi U.uw Exp.tNat x))
           (ih_p (trUpR ρ) (trUpR σ) hp))))

;; A finite nonempty Nat context gives the total Good assignment needed by
;; pa_lcert: indices within the context keep their identity; all remaining
;; indices alias its first Nat entry.  The support theorem removes those
;; aliases from formulas that never mention them.
(thm paNatD_nth [k :- Nat]
  (forall [i Nat] (=> (LT.lt i k)
    (Eq (Option Exp) (nthE (paNatD k) i)
      (Option.some Exp Exp.tNat))))
  (induction k)
  (intro i hi)
  (exact (False.elim (Nat.not_lt_zero i hi)))
  (intro i hi)
  (exact ((Nat.rec$0
    (fn [j :- Nat] (=> (LT.lt j (Nat.succ n))
      (Eq (Option Exp) (nthE (paNatD (Nat.succ n)) j)
        (Option.some Exp Exp.tNat))))
    (fn [hz :- (LT.lt 0 (Nat.succ n))]
      (nthE.eq_2 Exp.tNat (paNatD n)))
    (fn [j :- Nat,
         rec :- (=> (LT.lt j (Nat.succ n))
           (Eq (Option Exp) (nthE (paNatD (Nat.succ n)) j)
             (Option.some Exp Exp.tNat)))]
      (fn [hs :- (LT.lt (Nat.succ j) (Nat.succ n))]
        (Eq.trans (nthE.eq_3 Exp.tNat (paNatD n) j)
          (ih_n j (Nat.lt_of_succ_lt_succ hs)))))
    i) hi)))

(kdef paEnv (=> Nat (=> Nat Nat))
  (fn [k :- Nat, i :- Nat]
    (Bool.rec$1 (fn [_ :- Bool] Nat) 0 i (Nat.blt i k))))

(thm paEnv_agree [k :- Nat, i :- Nat, hi :- (LT.lt i k)]
  (Eq Nat (paEnv k i) i)
  (have hb (Eq Bool (Nat.blt i k) Bool.true)
    (Eq.mpr (Nat.blt_eq i k) hi))
  (unfold paEnv)
  (rw [hb]))

(thm paEnv_good [k :- Nat, hk :- (LT.lt 0 k)]
  (Good (paNatD k) (paEnv k))
  (intro i)
  (have hor (Or (LT.lt i k) (LE.le k i)) (Nat.lt_or_ge i k))
  (cases hor)
  (have he (Eq Nat (paEnv k i) i) (paEnv_agree k i h))
  (exact (Eq.mpr
    (congrArg (fn [j :- Nat]
      (Eq (Option Exp) (nthE (paNatD k) j)
        (Option.some Exp Exp.tNat))) he)
    (paNatD_nth k i h)))
  (have hb (Eq Bool (Nat.blt i k) Bool.false)
    (bfalse (Nat.blt i k)
      (fn [ht :- (Eq Bool (Nat.blt i k) Bool.true)]
        (Nat.lt_irrefl i
          (Nat.lt_of_lt_of_le (Eq.mp (Nat.blt_eq i k) ht) h)))))
  (have he (Eq Nat (paEnv k i) 0) (Eq.trans rfl
    (congrArg (fn [bb :- Bool]
      (Bool.rec$1 (fn [_ :- Bool] Nat) 0 i bb)) hb)))
  (exact (Eq.mpr
    (congrArg (fn [j :- Nat]
      (Eq (Option Exp) (nthE (paNatD k) j)
        (Option.some Exp Exp.tNat))) he)
    (paNatD_nth k 0 hk))))

;; For a positive support bound, pa_lcert's total Good assignment can live
;; in the finite context.  trF_support removes the fallback aliases, and
;; paClose_intro abstracts exactly the k Nat entries.
(thm pa_finite_lcert_pos
  [chkf :- (=> Code Code Bool), p :- PF, hp :- (PPrv p),
   k :- Nat, hk :- (LT.lt 0 k), hs :- (LE.le (paSupportF p) k)]
  (Exists (fn [e :- Exp]
    (Rt chkf (List.nil Exp) (List.nil U) e
      (paCloseN k (trF p (fn [i :- Nat] i))))))
  (have hag (paAgree (paSupportF p) (paEnv k) (fn [i :- Nat] i))
    (fn [i :- Nat, hi :- (LT.lt i (paSupportF p))]
      (paEnv_agree k i (Nat.lt_of_lt_of_le hi hs))))
  (have heq (Eq Exp (trF p (paEnv k)) (trF p (fn [i :- Nat] i)))
    (trF_support p (paEnv k) (fn [i :- Nat] i) hag))
  (refine' (exists_elimE _ _
    (pa_lcert chkf p hp (paNatD k) (paEnv k) (paEnv_good k hk)) _))
  (intro e he)
  (have ht (Rt chkf (paNatD k) (allW (paNatD k)) e
             (trF p (fn [i :- Nat] i)))
    (Eq.mp (congrArg (fn [A :- Exp]
      (Rt chkf (paNatD k) (allW (paNatD k)) e A)) heq) he))
  (constructor)
  (exact (paCloseLam k e))
  (exact (paClose_intro chkf k e (trF p (fn [i :- Nat] i)) ht)))
