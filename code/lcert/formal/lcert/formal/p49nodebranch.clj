(ns lcert.formal.p49nodebranch
  "Node-branch result type after assigning the two fresh holes live children."
  (:require [lcert.formal.base :refer [thm]]
            [lcert.formal.p49scoped]))

(thm p49nb_pair_node_tail
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp,
   hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tba_F B K slots j
      (p49sc_pair_env rho (p49fresh K slots) x y)
      (Exp.snode (Exp.lbl l) x y))
    (p49tele B
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (Eq.trans
    (congrArg
      (fn [c :- Exp]
        (p49tba_F B K slots j
          (p49sc_pair_env rho (p49fresh K slots) x y) c))
      (Eq.trans
        (congrArg
          (fn [a :- Exp] (Exp.snode (Exp.lbl l) a y))
          (Eq.symm (p49sc_pair_left rho (p49fresh K slots) x y)))
        (congrArg
          (fn [b :- Exp]
            (Exp.snode (Exp.lbl l)
              (p49sc_pair_env rho (p49fresh K slots) x y
                (p49fresh K slots)) b))
          (Eq.symm (p49sc_pair_right rho (p49fresh K slots) x y)))))
    (p49tba_node_tail B K slots j l
      (p49sc_pair_env rho (p49fresh K slots) x y) hj))))

(thm p49nb_branch_type
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp,
   hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tba_F B K slots j rho
      (Exp.snode (Exp.lbl l) x y))
    (p49tele B
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (Eq.trans
    (congrArg
      (fn [F :- (=> Exp Exp)]
        (F (Exp.snode (Exp.lbl l) x y)))
      (p49sc_F_pair B K slots j rho x y))
    (p49nb_pair_node_tail B K slots j l rho x y hj))))

(thm p49nb_branch_concrete
  [K :- P49C, slots :- (List (Prod Nat Nat)),
   j :- Nat, l :- Nat, x :- Exp, y :- Exp,
   hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tba_F
      (fn [q :- Nat, c :- Exp] (p49dl q c))
      K slots j p49tl_rho0
      (Exp.snode (Exp.lbl l) x y))
    (p49tele
      (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots) x y)))
  (exact (p49nb_branch_type
    (fn [q :- Nat, c :- Exp] (p49dl q c))
    K slots j l p49tl_rho0 x y hj)))
