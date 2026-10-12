(ns lcert.formal.p49branches
  "Finite label elimination for arbitrary motives and branch families."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.judgment]))

;; Build n successive branches beginning at label k.  The recursive
;; accumulator is itself a function of the starting label, so the Nat
;; recursor can advance that label at every constructor step.
(kdef p49brs (=> Nat (=> (=> Nat Exp) (=> Nat Exp)))
  (fn [n :- Nat]
    (fn [f :- (=> Nat Exp)]
      (fn [k :- Nat]
        ((Nat.rec$1 (fn [_ :- Nat] (=> Nat Exp))
           (fn [_ :- Nat] Exp.bnil)
           (fn [_ :- Nat, tail :- (=> Nat Exp)]
             (fn [j :- Nat] (Exp.bcons (f j) (tail (+ j 1)))))
           n) k)))))

;; Small Presburger facts keep arithmetic out of the proof-term position of
;; `have`, where Ansatz expects an explicit term rather than a tactic.
(thm p49brs_zero_offset
  [k :- Nat, hkn :- (Eq Nat (+ k 0) 100)]
  (Eq Nat k 100)
  (omega))

(thm p49brs_head_bound
  [k :- Nat, n :- Nat, hkn :- (Eq Nat (+ k (+ n 1)) 100)]
  (LT.lt k 100)
  (omega))

(thm p49brs_tail_offset
  [k :- Nat, n :- Nat, hkn :- (Eq Nat (+ k (+ n 1)) 100)]
  (Eq Nat (+ (+ k 1) n) 100)
  (omega))

;; If k+n=100, the generated list has exactly the branch derivation
;; required by Rt.tBrs at k.  The inductive hypothesis is generalized over
;; both the offset and its arithmetic side condition.
(thm p49brs_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), P :- Exp,
   f :- (=> Nat Exp), hlen :- (Eq Nat (lenU us) (lenE D)),
   hP :- (Tl chkf Bool.true (List.cons Exp Exp.tLbl D) P Exp.tUnit),
   hf :- (forall [l Nat]
          (=> (Eq Bool (Nat.blt l 100) Bool.true)
              (Rt chkf D us (f l) (subst1 (Exp.lbl l) P))))]
  (forall [n Nat]
    (forall [k Nat]
      (=> (Eq Nat (+ k n) 100)
          (Rt chkf D us (p49brs n f k) (Exp.tBrs P k)))))
  (intro n) (intro k) (intro hkn)
  (revert hkn) (revert k)
  (induction n)
  (intro k) (intro hkn)
  (have hk (Eq Nat k 100) (p49brs_zero_offset k hkn))
  (subst hk)
  (exact (Rt.rBnil chkf D us P hlen))
  (intro k) (intro hkn)
  (have hlt (LT.lt k 100) (p49brs_head_bound k n hkn))
  (have hfk (Eq Bool (Nat.blt k 100) Bool.true)
    (Eq.mpr (Nat.blt_eq k 100) hlt))
  (have htail (Eq Nat (+ (+ k 1) n) 100)
    (p49brs_tail_offset k n hkn))
  (exact (Rt.rBcons chkf D us P k (f k) (p49brs n f (+ k 1))
    (hf k hfk) (ih_n (+ k 1) htail) hP)))

;; The full 0..99 family is the branch premise of the left label rule.
(thm p49case_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp),
   us1 :- (List U), us2 :- (List U), P :- Exp, x :- Exp,
   f :- (=> Nat Exp),
   hlen :- (Eq Nat (lenU us2) (lenE D)),
   hP :- (Tl chkf Bool.true (List.cons Exp Exp.tLbl D) P Exp.tUnit),
   hx :- (Rt chkf D us1 x Exp.tLbl),
   hf :- (forall [l Nat]
          (=> (Eq Bool (Nat.blt l 100) Bool.true)
              (Rt chkf D us2 (f l) (subst1 (Exp.lbl l) P))))]
  (Rt chkf D (vadd us1 us2) (Exp.caseL P x (p49brs 100 f 0))
      (subst1 x P))
  (exact (Rt.rCaseL chkf D us1 us2 P x (p49brs 100 f 0) hx hP
    (p49brs_typed chkf D us2 P f hlen hP hf 100 0 rfl))))
