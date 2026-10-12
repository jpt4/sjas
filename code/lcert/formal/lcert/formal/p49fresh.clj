(ns lcert.formal.p49fresh
  "Fresh child-hole identifiers for the Proposition 4.9 generator."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49context]))

;; Additive bounds avoid a maximum operation: each component bound is at
;; most the sum. The bound need not be minimal, only certified fresh.
(a/defn p49c_bound [K :- P49C] Nat
  (match K
    [(hole i) (Nat.succ i)]
    [(leaf l) 0]
    [(node l a b) (+ (p49c_bound a) (p49c_bound b))]))

(kdef p49c_below (=> P49C Nat Prop)
  (fn [K :- P49C]
    (P49C.rec$1 (fn [_ :- P49C] (=> Nat Prop))
      (fn [i :- Nat] (fn [m :- Nat] (LT.lt i m)))
      (fn [l :- Nat] (fn [m :- Nat] True))
      (fn [l :- Nat, a :- P49C, b :- P49C,
           ih_a :- (=> Nat Prop), ih_b :- (=> Nat Prop)]
        (fn [m :- Nat] (And (ih_a m) (ih_b m))))
      K)))

(thm p49c_below_mono [K :- P49C]
  (forall [m Nat] (forall [n Nat]
    (=> (LE.le m n) (=> (p49c_below K m) (p49c_below K n)))))
  (induction K)
  (intro m n hmn h)
  (change (LT.lt i n))
  (have hi (LT.lt i m) h)
  (omega)
  (intro m n hmn h) (exact True.intro)
  (intro m n hmn h)
  (exact (And.intro
    (ih_a m n hmn (And.left h))
    (ih_b m n hmn (And.right h)))))

(thm p49c_bound_sound [K :- P49C]
  (p49c_below K (p49c_bound K))
  (induction K)
  (change (LT.lt i (Nat.succ i)))
  (omega)
  (exact True.intro)
  (have ha (LE.le (p49c_bound a)
             (+ (p49c_bound a) (p49c_bound b)))
    (Nat.le_add_right (p49c_bound a) (p49c_bound b)))
  (have hb (LE.le (p49c_bound b)
             (+ (p49c_bound a) (p49c_bound b)))
    (Nat.le_add_left (p49c_bound b) (p49c_bound a)))
  (exact (And.intro
    (p49c_below_mono a (p49c_bound a)
      (+ (p49c_bound a) (p49c_bound b)) ha ih_a)
    (p49c_below_mono b (p49c_bound b)
      (+ (p49c_bound a) (p49c_bound b)) hb ih_b))))

(a/defn p49slots_bound [slots :- (List (Prod Nat Nat))] Nat
  (match slots
    [nil 0]
    [(cons slot rest)
     (+ (Nat.succ (Prod.fst slot)) (p49slots_bound rest))]))

(kdef p49slots_below (=> (List (Prod Nat Nat)) Nat Prop)
  (fn [slots :- (List (Prod Nat Nat))]
    (List.rec$1$0 (Prod Nat Nat)
       (fn [_ :- (List (Prod Nat Nat))] (=> Nat Prop))
       (fn [m :- Nat] True)
       (fn [slot :- (Prod Nat Nat), rest :- (List (Prod Nat Nat)),
            ih :- (=> Nat Prop)]
         (fn [m :- Nat]
           (And (LT.lt (Prod.fst slot) m) (ih m))))
       slots)))

(thm p49slots_below_mono [slots :- (List (Prod Nat Nat))]
  (forall [m Nat] (forall [n Nat]
    (=> (LE.le m n)
      (=> (p49slots_below slots m) (p49slots_below slots n)))))
  (induction slots)
  (intro m n hmn h) (exact True.intro)
  (intro m n hmn h)
  (have hx (LT.lt (Prod.fst head) m) (And.left h))
  (have hh (LT.lt (Prod.fst head) n) (Nat.lt_of_lt_of_le hx hmn))
  (exact (And.intro hh (ih_tail m n hmn (And.right h)))))

(thm p49slot_head_lt [i :- Nat, rest :- Nat]
  (LT.lt i (+ (Nat.succ i) rest))
  (omega))

(thm p49slots_bound_sound [slots :- (List (Prod Nat Nat))]
  (p49slots_below slots (p49slots_bound slots))
  (induction slots)
  (exact True.intro)
  (have hh (LT.lt (Prod.fst head)
             (+ (Nat.succ (Prod.fst head)) (p49slots_bound tail)))
    (p49slot_head_lt (Prod.fst head) (p49slots_bound tail)))
  (have ht (LE.le (p49slots_bound tail)
             (+ (Nat.succ (Prod.fst head)) (p49slots_bound tail)))
    (Nat.le_add_left (p49slots_bound tail) (Nat.succ (Prod.fst head))))
  (exact (And.intro hh
    (p49slots_below_mono tail (p49slots_bound tail)
      (+ (Nat.succ (Prod.fst head)) (p49slots_bound tail)) ht ih_tail))))

(kdef p49fresh (=> P49C (List (Prod Nat Nat)) Nat)
  (fn [K :- P49C, slots :- (List (Prod Nat Nat))]
    (+ (p49c_bound K) (p49slots_bound slots))))

(thm p49fresh_context [K :- P49C, slots :- (List (Prod Nat Nat))]
  (p49c_below K (p49fresh K slots))
  (have h (LE.le (p49c_bound K) (p49fresh K slots))
    (Nat.le_add_right (p49c_bound K) (p49slots_bound slots)))
  (exact (p49c_below_mono K (p49c_bound K)
    (p49fresh K slots) h (p49c_bound_sound K))))

(thm p49fresh_slots [K :- P49C, slots :- (List (Prod Nat Nat))]
  (p49slots_below slots (p49fresh K slots))
  (have h (LE.le (p49slots_bound slots) (p49fresh K slots))
    (Nat.le_add_left (p49slots_bound slots) (p49c_bound K)))
  (exact (p49slots_below_mono slots (p49slots_bound slots)
    (p49fresh K slots) h (p49slots_bound_sound slots))))
