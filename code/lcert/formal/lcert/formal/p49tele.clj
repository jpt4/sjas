(ns lcert.formal.p49tele
  "Telescope syntax for the bounded code-consistency family."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49context]))

;; Each slot gives one code binder and one affine depth-evidence binder.
;; The freshly bound code is index 1 below its evidence binder. Older code
;; variables move out by two indices, leaving all hole names stable.
(kdef p49tele (=> (=> Nat Exp Exp) P49C (List (Prod Nat Nat)) (=> Nat Exp) Exp)
  (fn [B :- (=> Nat Exp Exp), K :- P49C,
       slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp)]
    ((List.rec$1$0 (Prod Nat Nat)
       (fn [_ :- (List (Prod Nat Nat))] (=> (=> Nat Exp) Exp))
       (fn [env :- (=> Nat Exp)]
         (Exp.tPi U.u1
           (Exp.tT (Exp.chk (p49plug K env) (Exp.sleaf (Exp.lbl 15))))
           Exp.tEmpty))
       (fn [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
            ih :- (=> (=> Nat Exp) Exp)]
         (fn [env :- (=> Nat Exp)]
           (Exp.tPi U.uw Exp.tSyn
             (Exp.tPi U.u1 (Exp.tT (B (Prod.snd slot) (Exp.var 0)))
               (ih (fn [h :- Nat]
                 (if (Nat.beq h (Prod.fst slot))
                   (Exp.var 1)
                   (lift 2 0 (env h)))))))))
       slots) rho)))

;; A single slot has the exact statement of Proposition 4.9, independent
;; of how its depth predicate B is implemented.
(thm p49tele_one [B :- (=> Nat Exp Exp), k :- Nat, rho :- (=> Nat Exp)]
  (Eq Exp
    (p49tele B (P49C.hole 0)
      (List.cons (Prod Nat Nat) (Prod.mk 0 k)
        (List.nil (Prod Nat Nat))) rho)
    (Exp.tPi U.uw Exp.tSyn
      (Exp.tPi U.u1 (Exp.tT (B k (Exp.var 0)))
        (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 1)
                              (Exp.sleaf (Exp.lbl 15))))
          Exp.tEmpty))))
  (rfl))

;; The first code is lifted over the second code and its evidence. This
;; concrete equation catches the central off-by-two error in a node branch.
(thm p49tele_two [B :- (=> Nat Exp Exp), j :- Nat, k :- Nat,
                  rho :- (=> Nat Exp)]
  (Eq Exp
    (p49tele B (P49C.node 7 (P49C.hole 0) (P49C.hole 1))
      (List.cons (Prod Nat Nat) (Prod.mk 0 j)
        (List.cons (Prod Nat Nat) (Prod.mk 1 k)
          (List.nil (Prod Nat Nat)))) rho)
    (Exp.tPi U.uw Exp.tSyn
      (Exp.tPi U.u1 (Exp.tT (B j (Exp.var 0)))
        (Exp.tPi U.uw Exp.tSyn
          (Exp.tPi U.u1 (Exp.tT (B k (Exp.var 0)))
            (Exp.tPi U.u1
              (Exp.tT (Exp.chk
                (Exp.snode (Exp.lbl 7) (Exp.var 3) (Exp.var 1))
                (Exp.sleaf (Exp.lbl 15))))
              Exp.tEmpty))))))
  (rfl))
