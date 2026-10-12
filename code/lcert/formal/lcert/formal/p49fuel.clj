(ns lcert.formal.p49fuel
  "A kernel-checked decreasing potential for Proposition 4.9 holes."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]))

;; A node replaces one depth-(j+1) hole by two depth-j holes. Base 3
;; leaves a strictly positive margin at every j; base 2 would not.
(a/defn p49pow3 [n :- Nat] Nat
  (match n
    [zero 1]
    [(succ j) (+ (p49pow3 j) (+ (p49pow3 j) (p49pow3 j)))]))

(a/defn p49pot [slots :- (List (Prod Nat Nat))] Nat
  (match slots
    [nil 0]
    [(cons slot rest)
     (+ (p49pow3 (Prod.snd slot)) (p49pot rest))]))

(thm p49pow3_pos [j :- Nat] (LT.lt 0 (p49pow3 j))
  (induction j)
  (omega)
  (omega))

(thm p49pot_leaf_decrease
  [hole :- Nat, j :- Nat, tail :- (List (Prod Nat Nat))]
  (LT.lt (p49pot tail)
    (p49pot (List.cons (Prod Nat Nat) (Prod.mk hole j) tail)))
  (have hp (LT.lt 0 (p49pow3 j)) (p49pow3_pos j))
  (omega))

(thm p49pot_node_decrease
  [old :- Nat, left :- Nat, right :- Nat, j :- Nat,
   tail :- (List (Prod Nat Nat))]
  (LT.lt
    (p49pot
      (List.cons (Prod Nat Nat) (Prod.mk left j)
        (List.cons (Prod Nat Nat) (Prod.mk right j) tail)))
    (p49pot
      (List.cons (Prod Nat Nat) (Prod.mk old (Nat.succ j)) tail)))
  (have hp (LT.lt 0 (p49pow3 j)) (p49pow3_pos j))
  (change (LT.lt
    (+ (p49pow3 j) (+ (p49pow3 j) (p49pot tail)))
    (+ (+ (p49pow3 j) (+ (p49pow3 j) (p49pow3 j)))
       (p49pot tail))))
  (omega))
