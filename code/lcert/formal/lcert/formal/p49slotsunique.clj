(ns lcert.formal.p49slotsunique
  "Pairwise distinct pending holes for the all-depth fuel induction."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49genfacts]
            [lcert.formal.p49fuel]))

(kdef p49su_unique (=> (List (Prod Nat Nat)) Prop)
  (fn [slots :- (List (Prod Nat Nat))]
    (List.rec$1$0 (Prod Nat Nat)
      (fn [_ :- (List (Prod Nat Nat))] Prop)
      True
      (fn [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
           ih :- Prop]
        (And (p49tr_avoids tail (Prod.fst slot)) ih))
      slots)))

(thm p49su_head
  [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat) slot tail))]
  (p49tr_avoids tail (Prod.fst slot))
  (exact (And.left hu)))

(thm p49su_tail
  [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat) slot tail))]
  (p49su_unique tail)
  (exact (And.right hu)))

(thm p49su_leaf
  [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat) slot tail))]
  (p49su_unique tail)
  (exact (p49su_tail slot tail hu)))

(thm p49su_node
  [K :- P49C, old :- Nat, j :- Nat,
   tail :- (List (Prod Nat Nat)),
   hu :- (p49su_unique
     (List.cons (Prod Nat Nat)
       (Prod.mk old (Nat.succ j)) tail))]
  (p49su_unique
    (List.cons (Prod Nat Nat)
      (Prod.mk (p49fresh K tail) j)
      (List.cons (Prod Nat Nat)
        (Prod.mk (Nat.succ (p49fresh K tail)) j)
        tail)))
  (exact (And.intro
    (And.intro
      (p49tr_beq_false_of_lt
        (p49fresh K tail) (Nat.succ (p49fresh K tail))
        (Nat.lt_succ_self (p49fresh K tail)))
      (p49gf_fresh_avoids K tail))
    (And.intro
      (p49gf_fresh_succ_avoids K tail)
      (p49su_tail (Prod.mk old (Nat.succ j)) tail hu)))))

(thm p49su_one [slot :- (Prod Nat Nat)]
  (p49su_unique
    (List.cons (Prod Nat Nat) slot
      (List.nil (Prod Nat Nat))))
  (exact (And.intro True.intro True.intro)))

(thm p49su_leaf_fuel
  [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
   fuel :- Nat,
   hpot :- (LE.le
     (p49pot (List.cons (Prod Nat Nat) slot tail))
     (Nat.succ fuel))]
  (LE.le (p49pot tail) fuel)
  (have hd
    (LT.lt (p49pot tail)
      (p49pot (List.cons (Prod Nat Nat) slot tail)))
    (p49pot_leaf_decrease (Prod.fst slot) (Prod.snd slot) tail))
  (omega))

(thm p49su_node_fuel
  [K :- P49C, slot :- (Prod Nat Nat),
   tail :- (List (Prod Nat Nat)), fuel :- Nat,
   hpot :- (LE.le
     (p49pot
       (List.cons (Prod Nat Nat)
         (Prod.mk (Prod.fst slot) (Nat.succ (Prod.snd slot)))
         tail))
     (Nat.succ fuel))]
  (LE.le
    (p49pot
      (List.cons (Prod Nat Nat)
        (Prod.mk (p49fresh K tail) (Prod.snd slot))
        (List.cons (Prod Nat Nat)
          (Prod.mk (Nat.succ (p49fresh K tail)) (Prod.snd slot))
          tail)))
    fuel)
  (have hd
    (LT.lt
      (p49pot
        (List.cons (Prod Nat Nat)
          (Prod.mk (p49fresh K tail) (Prod.snd slot))
          (List.cons (Prod Nat Nat)
            (Prod.mk (Nat.succ (p49fresh K tail)) (Prod.snd slot))
            tail)))
      (p49pot
        (List.cons (Prod Nat Nat)
          (Prod.mk (Prod.fst slot) (Nat.succ (Prod.snd slot)))
          tail)))
    (p49pot_node_decrease
      (Prod.fst slot) (p49fresh K tail)
      (Nat.succ (p49fresh K tail))
      (Prod.snd slot) tail))
  (omega))
