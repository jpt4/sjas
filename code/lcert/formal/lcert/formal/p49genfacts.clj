(ns lcert.formal.p49genfacts
  "Fresh slot identifiers needed by the Proposition 4.9 generator."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.p49telerepl]
            [lcert.formal.p49fresh]))

(thm p49gf_beq_small_false
  [h :- Nat, x :- Nat, hlt :- (LT.lt h x)]
  (Eq Bool (Nat.beq h x) Bool.false)
  (exact (p49tr_false_of_not_true (Nat.beq h x)
    (fn [he :- (Eq Bool (Nat.beq h x) Bool.true)]
      (Nat.ne_of_lt hlt (Nat.eq_of_beq_eq_true he))))))

(thm p49gf_avoids_of_below [slots :- (List (Prod Nat Nat))]
  (forall [m Nat]
    (=> (p49slots_below slots m) (p49tr_avoids slots m)))
  (induction slots)
  (intro m h) (exact True.intro)
  (intro m h)
  (exact (And.intro
    (p49gf_beq_small_false (Prod.fst head) m (And.left h))
    (ih_tail m (And.right h)))))

(thm p49gf_fresh_avoids
  [K :- P49C, slots :- (List (Prod Nat Nat))]
  (p49tr_avoids slots (p49fresh K slots))
  (exact (p49gf_avoids_of_below slots (p49fresh K slots)
    (p49fresh_slots K slots))))

(thm p49gf_fresh_succ_avoids
  [K :- P49C, slots :- (List (Prod Nat Nat))]
  (p49tr_avoids slots (Nat.succ (p49fresh K slots)))
  (have h (p49slots_below slots (Nat.succ (p49fresh K slots)))
    (p49slots_below_mono slots (p49fresh K slots)
      (Nat.succ (p49fresh K slots))
      (Nat.le_succ (p49fresh K slots))
      (p49fresh_slots K slots)))
  (exact (p49gf_avoids_of_below slots (Nat.succ (p49fresh K slots)) h)))

;; After the left child has been pushed onto the telescope, its sibling is
;; still absent from all pending slots.
(thm p49gf_right_avoids_left_pending
  [K :- P49C, slots :- (List (Prod Nat Nat)), j :- Nat]
  (p49tr_avoids
    (List.cons (Prod Nat Nat) (Prod.mk (p49fresh K slots) j) slots)
    (Nat.succ (p49fresh K slots)))
  (exact (And.intro
    (p49gf_beq_small_false
      (p49fresh K slots) (Nat.succ (p49fresh K slots))
      (Nat.lt_succ_self (p49fresh K slots)))
    (p49gf_fresh_succ_avoids K slots))))
