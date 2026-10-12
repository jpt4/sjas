(ns lcert.formal.p49generator
  "Finite fuel term generator for the all-depth Proposition 4.9 candidate."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49terminal]
            [lcert.formal.p49leafcont]
            [lcert.formal.p49nodemotive]
            [lcert.formal.p49peelzero]
            [lcert.formal.p49fuel]))

;; The predecessor recursion is shared by all 100 label branches. The
;; generated node branch applies the recursive four-binder telescope to
;; the live left code/evidence (indices 6,1) and right code/evidence (5,0).
(kdef p49tau_core (=> Nat P49C (List (Prod Nat Nat)) Exp)
  (fn [fuel :- Nat]
    (Nat.rec$1
       (fn [_ :- Nat] (=> P49C (List (Prod Nat Nat)) Exp))
       (fn [baseK :- P49C,
            unusedSlots :- (List (Prod Nat Nat))]
         (p49tc_term (p49plug baseK p49tl_rho0)))
       (fn [_ :- Nat,
            ih :- (=> P49C (List (Prod Nat Nat)) Exp)]
         (fn [ctx :- P49C, todo :- (List (Prod Nat Nat))]
           (List.rec$1$0 (Prod Nat Nat)
              (fn [_ :- (List (Prod Nat Nat))] Exp)
              (p49tc_term (p49plug ctx p49tl_rho0))
              (fn [slot :- (Prod Nat Nat),
                   tail :- (List (Prod Nat Nat)), unusedTail :- Exp]
                (Nat.rec$1 (fn [_ :- Nat] Exp)
                (p49pz_term
                  (p49lc_F ctx tail (Prod.fst slot) (Exp.var 0))
                  (Exp.caseL
                    (p49lc_F ctx tail (Prod.fst slot)
                      (Exp.sleaf (Exp.var 0)))
                    (Exp.var 1)
                    (p49brs 100
                      (fn [l :- Nat]
                        (ih ((p49crepl ctx) (Prod.fst slot)
                              (P49C.leaf l)) tail)) 0)))
                (fn [j :- Nat, _ :- Exp]
                  (p49pd_term j
                    (p49lc_F ctx tail (Prod.fst slot) (Exp.var 0))
                    (Exp.caseL
                      (p49lc_F ctx tail (Prod.fst slot)
                        (Exp.sleaf (Exp.var 0)))
                      (Exp.var 1)
                      (p49brs 100
                        (fn [l :- Nat]
                          (ih ((p49crepl ctx) (Prod.fst slot)
                                (P49C.leaf l)) tail)) 0))
                    (Exp.caseL
                      (p49nm_M (p49nm_F ctx tail (Prod.fst slot)))
                      (Exp.var 7)
                      (p49brs 100
                        (fn [l :- Nat]
                          (Exp.app
                            (Exp.app
                              (Exp.app
                                (Exp.app
                                  (ih
                                    ((p49crepl ctx) (Prod.fst slot)
                                      (P49C.node l
                                        (P49C.hole (p49fresh ctx tail))
                                        (P49C.hole
                                          (Nat.succ (p49fresh ctx tail)))))
                                    (List.cons (Prod Nat Nat)
                                      (Prod.mk (p49fresh ctx tail) j)
                                      (List.cons (Prod Nat Nat)
                                        (Prod.mk
                                          (Nat.succ (p49fresh ctx tail)) j)
                                        tail)))
                                  (Exp.var 6))
                                (Exp.var 1))
                              (Exp.var 5))
                            (Exp.var 0))) 0))))
                (Prod.snd slot)))
             todo)))
       fuel)))

(kdef p49tau (=> Nat P49C (List (Prod Nat Nat)) Exp)
  (fn [fuel :- Nat, K :- P49C, slots :- (List (Prod Nat Nat))]
    (p49tau_core fuel K slots)))

(thm p49tau_terminal [fuel :- Nat, K :- P49C]
  (Eq Exp
    (p49tau (Nat.succ fuel) K (List.nil (Prod Nat Nat)))
    (p49tc_term (p49plug K p49tl_rho0)))
  (rfl))

(thm p49tau_terminal_zero [K :- P49C]
  (Eq Exp
    (p49tau 0 K (List.nil (Prod Nat Nat)))
    (p49tc_term (p49plug K p49tl_rho0)))
  (rfl))

(thm p49tau_nonempty_pos
  [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat))]
  (LT.lt 0
    (p49pot (List.cons (Prod Nat Nat) slot tail)))
  (have hp (LT.lt 0 (p49pow3 (Prod.snd slot)))
    (p49pow3_pos (Prod.snd slot)))
  (omega))

(thm p49tau_zero_nil
  [slots :- (List (Prod Nat Nat)),
   hpot :- (LE.le (p49pot slots) 0)]
  (Eq (List (Prod Nat Nat)) slots (List.nil (Prod Nat Nat)))
  (cases slots)
  (rfl)
  (have hp (LT.lt 0
      (p49pot (List.cons (Prod Nat Nat) head tail)))
    (p49tau_nonempty_pos head tail))
  (omega))
