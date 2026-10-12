(ns lcert.formal.p49applysubst
  "Cancellation of the code and evidence binders in a fresh telescope slot."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.p49telelaws]
            [lcert.formal.p49peeldepth]
            [lcert.formal.p49scoped]))

(thm p49as_linv [x :- Exp]
  (p49pd_LInv (fn [i :- Nat] (inst1 x i)) 0)
  (intro i)
  (cases i)
  (rfl)
  (rfl))

(thm p49as_cancel [x :- Exp, e :- Exp]
  (Eq Exp (subst1 x (lift 1 0 e)) e)
  (exact (p49pd_cancel e (fn [i :- Nat] (inst1 x i)) 0
    (p49as_linv x))))

;; Substituting the code under the still-bound evidence variable removes
;; one of the two lifts on every older environment entry.
(thm p49as_first_frame [x :- Exp, e :- Exp]
  (Eq Exp
    (subst (upn 1 (fn [i :- Nat] (inst1 x i))) (lift 2 0 e))
    (lift 1 0 e))
  (exact (Eq.trans
    (congrArg
      (fn [t :- Exp]
        (subst (upn 1 (fn [i :- Nat] (inst1 x i))) t))
      (Eq.symm (lift_comp e 1 1 0)))
    (Eq.trans
      (p49tl_subst_up1 (lift 1 0 e)
        (fn [i :- Nat] (inst1 x i)))
      (congrArg (fn [t :- Exp] (lift 1 0 t))
        (p49as_cancel x e))))))

(thm p49as_slot_env
  [x :- Exp, v :- Exp, rho :- (=> Nat Exp), h :- Nat, i :- Nat]
  (Eq Exp
    (subst1 v
      (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (if (Nat.beq i h) x (rho i)))
  (exact (Bool.rec$0
    (fn [flag :- Bool]
      (Eq Exp
        (subst1 v
          (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
            (if flag (Exp.var 1) (lift 2 0 (rho i)))))
        (if flag x (rho i))))
    (Eq.trans
      (congrArg (fn [t :- Exp] (subst1 v t))
        (p49as_first_frame x (rho i)))
      (p49as_cancel v (rho i)))
    (p49as_cancel v x)
    (Nat.beq i h))))

(thm p49as_slot_env_fun
  [x :- Exp, v :- Exp, rho :- (=> Nat Exp), h :- Nat]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (subst1 v
        (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
          (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))))
    (fn [i :- Nat]
      (if (Nat.beq i h) x (rho i))))
  (exact (funext (fn [i :- Nat]
    (p49as_slot_env x v rho h i)))))

;; This is the result-type equation for applying a telescope's leading
;; code/evidence pair. It holds for an arbitrary tail of remaining slots.
(thm p49as_slot_body
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
   h :- Nat, x :- Exp, v :- Exp,
   hB :- (p49tl_BSubst B)]
  (Eq Exp
    (subst1 v
      (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
        (p49tele B K slots
          (fn [i :- Nat]
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i)))))))
    (p49tele B K slots
      (fn [i :- Nat]
        (if (Nat.beq i h) x (rho i)))))
  (exact (Eq.trans
    (congrArg (fn [t :- Exp] (subst1 v t))
      (p49tl_subst_equiv slots B K
        (fn [i :- Nat]
          (if (Nat.beq i h) (Exp.var 1)
            (lift 2 0 (rho i))))
        (upn 1 (fn [k :- Nat] (inst1 x k)))
        hB p49tl_frame_subst))
    (Eq.trans
      (p49tl_subst_equiv slots B K
        (fn [i :- Nat]
          (subst (upn 1 (fn [k :- Nat] (inst1 x k)))
            (if (Nat.beq i h) (Exp.var 1)
              (lift 2 0 (rho i)))))
        (fn [k :- Nat] (inst1 v k))
        hB p49tl_frame_subst)
      (congrArg
        (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
        (p49as_slot_env_fun x v rho h))))))

(thm p49as_disjoint
  [m :- Nat, i :- Nat,
   hi :- (Eq Bool (Nat.beq i m) Bool.true)]
  (Eq Bool (Nat.beq i (Nat.succ m)) Bool.false)
  (exact (p49tr_false_of_not_true (Nat.beq i (Nat.succ m))
    (fn [hj :- (Eq Bool (Nat.beq i (Nat.succ m)) Bool.true)]
      (p49tr_beq_conflict i m (Nat.succ m) hj hi
        (p49gf_beq_small_false m (Nat.succ m)
          (Nat.lt_succ_self m)))))))

(thm p49as_swap
  [a :- Bool, b :- Bool, x :- Exp, y :- Exp, z :- Exp,
   h :- (=> (Eq Bool a Bool.true) (Eq Bool b Bool.false))]
  (Eq Exp
    (if b y (if a x z))
    (if a x (if b y z)))
  (by_cases a)
  (all_goals (rw [hc]))
  (all_goals (try (rfl)))
  (exact (Eq.mp
    (congrArg
      (fn [flag :- Bool]
        (Eq Exp (if flag y x) x))
      (Eq.symm (h hc)))
    (Eq.refl$1 x))))

(thm p49as_pair_env
  [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (if (Nat.beq i (Nat.succ m)) y
        (if (Nat.beq i m) x (rho i))))
    (p49sc_pair_env rho m x y))
  (exact (funext (fn [i :- Nat]
    (p49as_swap (Nat.beq i m) (Nat.beq i (Nat.succ m))
      x y (rho i)
      (fn [hi :- (Eq Bool (Nat.beq i m) Bool.true)]
        (p49as_disjoint m i hi)))))))

(thm p49as_second_slot_body
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), rho :- (=> Nat Exp),
   m :- Nat, x :- Exp, y :- Exp, v :- Exp,
   hB :- (p49tl_BSubst B)]
  (Eq Exp
    (subst1 v
      (subst (upn 1 (fn [k :- Nat] (inst1 y k)))
        (p49tele B K slots
          (fn [i :- Nat]
            (if (Nat.beq i (Nat.succ m)) (Exp.var 1)
              (lift 2 0
                (if (Nat.beq i m) x (rho i))))))))
    (p49tele B K slots (p49sc_pair_env rho m x y)))
  (exact (Eq.trans
    (p49as_slot_body B K slots
      (fn [i :- Nat] (if (Nat.beq i m) x (rho i)))
      (Nat.succ m) y v hB)
    (congrArg
      (fn [env :- (=> Nat Exp)] (p49tele B K slots env))
      (p49as_pair_env rho m x y)))))
