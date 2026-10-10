(ns lcert.formal.p49telebridgeaux
  "Unary telescope tail and instantiated successor-peel result types."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49telerepl]
            [lcert.formal.p49peel]))

;; The selected hole is the argument of the successor peel's unary family.
;; Other holes retain the ambient environment until their own slots bind them.
(kdef p49tba_F
  (=> (=> Nat Exp Exp) P49C (List (Prod Nat Nat)) Nat
      (=> Nat Exp) Exp Exp)
  (fn [B :- (=> Nat Exp Exp), K :- P49C,
       slots :- (List (Prod Nat Nat)), j :- Nat,
       rho :- (=> Nat Exp), c :- Exp]
    (p49tele B K slots
      (fn [i :- Nat]
        (if (Nat.beq i j) c (rho i))))))

;; These are the same syntactic naturality premises required of the unary
;; family by the successor peel. Keeping them local avoids loading that
;; proof's unrelated runtime-depth dependency just to check a type equation.
(kdef p49tba_Lift (=> (=> Exp Exp) Prop)
  (fn [F :- (=> Exp Exp)]
    (forall [n Nat] (forall [cut Nat] (forall [c Exp]
      (Eq Exp (lift n cut (F c)) (F (lift n cut c))))))))

(kdef p49tba_Subst (=> (=> Exp Exp) Prop)
  (fn [F :- (=> Exp Exp)]
    (forall [sg (=> Nat Exp)] (forall [c Exp]
      (Eq Exp (subst sg (F c)) (F (subst sg c)))))))

(thm p49tba_B_eq [F :- (=> Exp Exp), hL :- (p49tba_Lift F)]
  (Eq Exp
    (lift 1 2 (lift 1 0 (F (Exp.var 0))))
    (F (Exp.var 1)))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (lift 1 2 e))
      (hL 1 0 (Exp.var 0)))
    (hL 1 2 (Exp.var 1)))))

;; The label is at index 1 because the recursive evidence binder is at 0.
(thm p49tba_leafB_eq
  [F :- (=> Exp Exp), hL :- (p49tba_Lift F),
   hS :- (p49tba_Subst F)]
  (Eq Exp
    (p49peel_leafB (F (Exp.var 0)))
    (F (Exp.sleaf (Exp.var 1))))
  (exact (Eq.trans
    (congrArg
      (fn [e :- Exp] (subst (upn 1 sLeafI) e))
      (p49tba_B_eq F hL))
    (hS (upn 1 sLeafI) (Exp.var 1)))))

(thm p49tba_nodeB_eq
  [F :- (=> Exp Exp), hL :- (p49tba_Lift F),
   hS :- (p49tba_Subst F)]
  (Eq Exp
    (p49peel_nodeB (F (Exp.var 0)))
    (F (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))
  (exact (Eq.trans
    (congrArg
      (fn [e :- Exp] (subst (upn 1 sNodeI) e))
      (p49tba_B_eq F hL))
    (hS (upn 1 sNodeI) (Exp.var 1)))))

;; Once the bound label has been instantiated, the leaf result is exactly
;; the telescope whose selected context hole was replaced by that leaf.
(thm p49tba_leaf_tail
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tba_F B K slots j rho (Exp.sleaf (Exp.lbl l)))
    (p49tele B ((p49crepl K) j (P49C.leaf l)) slots rho))
  (exact (Eq.symm (p49tele_leaf_repl slots B K j l rho hj))))

;; For the node, both new hole names are generated above the old slots.
;; The corresponding tail family receives their current rendered codes.
(thm p49tba_node_tail
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tba_F B K slots j rho
      (Exp.snode (Exp.lbl l)
        (rho (p49fresh K slots))
        (rho (Nat.succ (p49fresh K slots)))))
    (p49tele B
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots rho))
  (exact (Eq.symm
    (p49tele_fresh_node_repl slots B K j l rho hj))))

;; Instantiating the peel's label variable yields the literal label syntax
;; used by P49C. These identities still require naturality of the chosen F.
(thm p49tba_leafB_inst
  [F :- (=> Exp Exp), hL :- (p49tba_Lift F),
   hS :- (p49tba_Subst F), sg :- (=> Nat Exp), l :- Nat,
   hl :- (Eq Exp (sg 1) (Exp.lbl l))]
  (Eq Exp
    (subst sg (p49peel_leafB (F (Exp.var 0))))
    (F (Exp.sleaf (Exp.lbl l))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sg e))
      (p49tba_leafB_eq F hL hS))
    (Eq.trans
      (hS sg (Exp.sleaf (Exp.var 1)))
      (congrArg F (congrArg Exp.sleaf hl))))))

(thm p49tba_nodeB_inst
  [F :- (=> Exp Exp), hL :- (p49tba_Lift F),
   hS :- (p49tba_Subst F), sg :- (=> Nat Exp), l :- Nat,
   c1 :- Exp, c2 :- Exp,
   hl :- (Eq Exp (sg 5) (Exp.lbl l)),
   h1 :- (Eq Exp (sg 4) c1),
   h2 :- (Eq Exp (sg 3) c2)]
  (Eq Exp
    (subst sg (p49peel_nodeB (F (Exp.var 0))))
    (F (Exp.snode (Exp.lbl l) c1 c2)))
  (have hc (Eq Exp
    (subst sg (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3)))
    (Exp.snode (Exp.lbl l) c1 c2))
    (Eq.trans
      (congrArg (fn [x :- Exp]
        (Exp.snode x (sg 4) (sg 3))) hl)
      (Eq.trans
        (congrArg (fn [x :- Exp]
          (Exp.snode (Exp.lbl l) x (sg 3))) h1)
        (congrArg (fn [x :- Exp]
          (Exp.snode (Exp.lbl l) c1 x)) h2))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sg e))
      (p49tba_nodeB_eq F hL hS))
    (Eq.trans
      (hS sg (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3)))
      (congrArg F hc)))))

(thm p49tba_leaf_result_tail
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), hj :- (p49tr_avoids slots j),
   hL :- (p49tba_Lift (p49tba_F B K slots j rho)),
   hS :- (p49tba_Subst (p49tba_F B K slots j rho)),
   sg :- (=> Nat Exp), hl :- (Eq Exp (sg 1) (Exp.lbl l))]
  (Eq Exp
    (subst sg
      (p49peel_leafB
        (p49tba_F B K slots j rho (Exp.var 0))))
    (p49tele B ((p49crepl K) j (P49C.leaf l)) slots rho))
  (exact (Eq.trans
    (p49tba_leafB_inst (p49tba_F B K slots j rho)
      hL hS sg l hl)
    (p49tba_leaf_tail B K slots j l rho hj))))

(thm p49tba_node_result_tail
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp), hj :- (p49tr_avoids slots j),
   hL :- (p49tba_Lift (p49tba_F B K slots j rho)),
   hS :- (p49tba_Subst (p49tba_F B K slots j rho)),
   sg :- (=> Nat Exp),
   hl :- (Eq Exp (sg 5) (Exp.lbl l)),
   h1 :- (Eq Exp (sg 4) (rho (p49fresh K slots))),
   h2 :- (Eq Exp (sg 3) (rho (Nat.succ (p49fresh K slots))))]
  (Eq Exp
    (subst sg
      (p49peel_nodeB
        (p49tba_F B K slots j rho (Exp.var 0))))
    (p49tele B
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots rho))
  (exact (Eq.trans
    (p49tba_nodeB_inst (p49tba_F B K slots j rho)
      hL hS sg l (rho (p49fresh K slots))
      (rho (Nat.succ (p49fresh K slots))) hl h1 h2)
    (p49tba_node_tail B K slots j l rho hj))))
