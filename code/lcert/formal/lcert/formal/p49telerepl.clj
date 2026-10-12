(ns lcert.formal.p49telerepl
  "Telescope equations for replacing holes in Proposition 4.9 contexts."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49tele]
            [lcert.formal.p49fresh]
            [lcert.formal.p49context]))

;; At the terminal telescope, replacing a context hole is exactly the
;; pointwise environment update from p49plug_repl. This is the base algebra
;; used when a peeled hole is instantiated by a leaf or node.
(thm p49tele_empty_repl
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat, R :- P49C,
   rho :- (=> Nat Exp)]
  (Eq Exp
    (p49tele B ((p49crepl K) j R)
      (List.nil (Prod Nat Nat)) rho)
    (p49tele B K (List.nil (Prod Nat Nat))
      (fn [i :- Nat]
        (if (Nat.beq i j) (p49plug R rho) (rho i)))))
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (p49plug_repl K j R rho))))

;; The leaf and node instances are named explicitly because these are the
;; two cases used by the all-depth branch recursion.
(thm p49tele_empty_leaf_repl
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat, l :- Nat,
   rho :- (=> Nat Exp)]
  (Eq Exp
    (p49tele B ((p49crepl K) j (P49C.leaf l))
      (List.nil (Prod Nat Nat)) rho)
    (p49tele B K (List.nil (Prod Nat Nat))
      (fn [i :- Nat]
        (if (Nat.beq i j) (Exp.sleaf (Exp.lbl l)) (rho i)))))
  (exact (p49tele_empty_repl B K j (P49C.leaf l) rho)))

(thm p49tele_empty_node_repl
  [B :- (=> Nat Exp Exp), K :- P49C, j :- Nat, l :- Nat,
   A :- P49C, C :- P49C, rho :- (=> Nat Exp)]
  (Eq Exp
    (p49tele B ((p49crepl K) j (P49C.node l A C))
      (List.nil (Prod Nat Nat)) rho)
    (p49tele B K (List.nil (Prod Nat Nat))
      (fn [i :- Nat]
        (if (Nat.beq i j)
          (Exp.snode (Exp.lbl l) (p49plug A rho) (p49plug C rho))
          (rho i)))))
  (exact (p49tele_empty_repl B K j (P49C.node l A C) rho)))

;; A peeled hole is absent from the remaining telescope slots. This is
;; expressed as a structural predicate so the induction retains the fact
;; when it descends through a slot.
(kdef p49tr_avoids (=> (List (Prod Nat Nat)) Nat Prop)
  (fn [slots :- (List (Prod Nat Nat))]
    (List.rec$1$0 (Prod Nat Nat)
      (fn [_ :- (List (Prod Nat Nat))] (=> Nat Prop))
      (fn [_ :- Nat] True)
      (fn [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
           ih :- (=> Nat Prop)]
        (fn [j :- Nat]
          (And (Eq Bool (Nat.beq (Prod.fst slot) j) Bool.false)
               (ih j))))
      slots)))

(thm p49tr_false_of_not_true [b :- Bool]
  (=> (=> (Eq Bool b Bool.true) False)
      (Eq Bool b Bool.false))
  (cases b)
  (all_goals (intro h))
  (exact (False.elim (h (Eq.refl$1 Bool.true))))
  (exact (Eq.refl$1 Bool.false)))

(thm p49tr_beq_conflict
  [i :- Nat, h :- Nat, j :- Nat,
   hij :- (Eq Bool (Nat.beq i j) Bool.true),
   hih :- (Eq Bool (Nat.beq i h) Bool.true),
   hneq :- (Eq Bool (Nat.beq h j) Bool.false)]
  False
  (have hhj (Eq Bool (Nat.beq h j) Bool.true)
    (Eq.mp
      (congrArg
        (fn [v :- Nat] (Eq Bool (Nat.beq v j) Bool.true))
        (Nat.eq_of_beq_eq_true hih))
      hij))
  (exact (Bool.noConfusion (Eq.trans (Eq.symm hhj) hneq))))

;; Updating a hole by a closed leaf commutes with crossing a different
;; code/evidence slot. The older entries are lifted by two; the leaf itself
;; is unchanged by that lift.
(thm p49tr_leaf_env
  [h :- Nat, j :- Nat, l :- Nat, rho :- (=> Nat Exp), i :- Nat,
   hneq :- (Eq Bool (Nat.beq h j) Bool.false)]
  (Eq Exp
    (if (Nat.beq i h)
      (Exp.var 1)
      (lift 2 0 (if (Nat.beq i j) (Exp.sleaf (Exp.lbl l)) (rho i))))
    (if (Nat.beq i j)
      (Exp.sleaf (Exp.lbl l))
      (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
  (by_cases (Nat.beq i j))
  (all_goals (rw [hc]))
  (all_goals (try (rfl)))
  (have hih (Eq Bool (Nat.beq i h) Bool.false)
    (p49tr_false_of_not_true (Nat.beq i h)
      (fn [he :- (Eq Bool (Nat.beq i h) Bool.true)]
        (p49tr_beq_conflict i h j hc he hneq))))
  (rw [hih]))

(thm p49tr_leaf_env_fun
  [h :- Nat, j :- Nat, l :- Nat, rho :- (=> Nat Exp),
   hneq :- (Eq Bool (Nat.beq h j) Bool.false)]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (if (Nat.beq i h)
        (Exp.var 1)
        (lift 2 0
          (if (Nat.beq i j) (Exp.sleaf (Exp.lbl l)) (rho i)))))
    (fn [i :- Nat]
      (if (Nat.beq i j)
        (Exp.sleaf (Exp.lbl l))
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))))
  (exact (funext (fn [i :- Nat]
    (p49tr_leaf_env h j l rho i hneq)))))

;; The leaf branch replacement equation holds through any number of
;; remaining code/evidence pairs, provided the selected hole was peeled.
(thm p49tele_leaf_repl [slots :- (List (Prod Nat Nat))]
  (forall [B (=> Nat Exp Exp)]
    (forall [K P49C]
      (forall [j Nat]
        (forall [l Nat]
          (forall [rho (=> Nat Exp)]
            (=> (p49tr_avoids slots j)
              (Eq Exp
                (p49tele B ((p49crepl K) j (P49C.leaf l)) slots rho)
                (p49tele B K slots
                  (fn [i :- Nat]
                    (if (Nat.beq i j)
                      (Exp.sleaf (Exp.lbl l)) (rho i)))))))))))
  (induction slots)
  (intro B K j l rho hav)
  (exact (p49tele_empty_leaf_repl B K j l rho))
  (intro B K j l rho hav)
  (have hneq (Eq Bool (Nat.beq (Prod.fst head) j) Bool.false)
    (And.left hav))
  (have htail (p49tr_avoids tail j) (And.right hav))
  (have hbody (Eq Exp
    (p49tele B ((p49crepl K) j (P49C.leaf l)) tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (rho i)))))
    (p49tele B K tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1)
          (lift 2 0
            (if (Nat.beq i j) (Exp.sleaf (Exp.lbl l)) (rho i)))))))
    (Eq.trans
      (ih_tail B K j l
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))
        htail)
      (congrArg (fn [env :- (=> Nat Exp)] (p49tele B K tail env))
        (Eq.symm
          (p49tr_leaf_env_fun (Prod.fst head) j l rho hneq)))))
  (exact (congrArg
    (fn [body :- Exp]
      (Exp.tPi U.uw Exp.tSyn
        (Exp.tPi U.u1
          (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
    hbody)))

;; A replacement context may itself contain holes. To move it across a
;; remaining binder, none of those holes may name the binder being crossed.
(kdef p49tr_context_avoids (=> P49C Nat Prop)
  (fn [R :- P49C]
    (P49C.rec$1 (fn [_ :- P49C] (=> Nat Prop))
      (fn [i :- Nat]
        (fn [h :- Nat] (Eq Bool (Nat.beq i h) Bool.false)))
      (fn [_ :- Nat] (fn [_ :- Nat] True))
      (fn [_ :- Nat, A :- P49C, C :- P49C,
           ih_A :- (=> Nat Prop), ih_C :- (=> Nat Prop)]
        (fn [h :- Nat] (And (ih_A h) (ih_C h))))
      R)))

(kdef p49tr_slots_avoid_context
  (=> (List (Prod Nat Nat)) P49C Prop)
  (fn [slots :- (List (Prod Nat Nat))]
    (List.rec$1$0 (Prod Nat Nat)
      (fn [_ :- (List (Prod Nat Nat))] (=> P49C Prop))
      (fn [_ :- P49C] True)
      (fn [slot :- (Prod Nat Nat), tail :- (List (Prod Nat Nat)),
           ih :- (=> P49C Prop)]
        (fn [R :- P49C]
          (And (p49tr_context_avoids R (Prod.fst slot))
               (ih R))))
      slots)))

(thm p49tr_plug_step [R :- P49C]
  (forall [h Nat] (forall [rho (=> Nat Exp)]
    (=> (p49tr_context_avoids R h)
      (Eq Exp
        (p49plug R (fn [i :- Nat]
          (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
        (lift 2 0 (p49plug R rho))))))
  (induction R)
  (intro h rho hav)
  (have he (Eq Bool (Nat.beq i h) Bool.false) hav)
  (change (Eq Exp
    (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))
    (lift 2 0 (rho i))))
  (rw [he])
  (intro h rho hav) (rfl)
  (intro h rho hav)
  (have ha (Eq Exp
    (p49plug a (fn [i :- Nat]
      (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (lift 2 0 (p49plug a rho)))
    (ih_a h rho (And.left hav)))
  (have hb (Eq Exp
    (p49plug b (fn [i :- Nat]
      (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
    (lift 2 0 (p49plug b rho)))
    (ih_b h rho (And.right hav)))
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x
        (p49plug b (fn [i :- Nat]
          (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))))) ha)
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l) (lift 2 0 (p49plug a rho)) y)) hb)))
  (all_goals (try (rfl))))

(thm p49tr_repl_env
  [R :- P49C, h :- Nat, j :- Nat, rho :- (=> Nat Exp), i :- Nat,
   hneq :- (Eq Bool (Nat.beq h j) Bool.false),
   hav :- (p49tr_context_avoids R h)]
  (Eq Exp
    (if (Nat.beq i h) (Exp.var 1)
      (lift 2 0 (if (Nat.beq i j) (p49plug R rho) (rho i))))
    (if (Nat.beq i j)
      (p49plug R (fn [z :- Nat]
        (if (Nat.beq z h) (Exp.var 1) (lift 2 0 (rho z)))))
      (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i)))))
  (by_cases (Nat.beq i j))
  (all_goals (rw [hc]))
  (all_goals (try (rfl)))
  (have hih (Eq Bool (Nat.beq i h) Bool.false)
    (p49tr_false_of_not_true (Nat.beq i h)
      (fn [he :- (Eq Bool (Nat.beq i h) Bool.true)]
        (p49tr_beq_conflict i h j hc he hneq))))
  (rw [hih])
  (exact (Eq.symm (p49tr_plug_step R h rho hav))))

(thm p49tr_repl_env_fun
  [R :- P49C, h :- Nat, j :- Nat, rho :- (=> Nat Exp),
   hneq :- (Eq Bool (Nat.beq h j) Bool.false),
   hav :- (p49tr_context_avoids R h)]
  (Eq (=> Nat Exp)
    (fn [i :- Nat]
      (if (Nat.beq i h) (Exp.var 1)
        (lift 2 0 (if (Nat.beq i j) (p49plug R rho) (rho i)))))
    (fn [i :- Nat]
      (if (Nat.beq i j)
        (p49plug R (fn [z :- Nat]
          (if (Nat.beq z h) (Exp.var 1) (lift 2 0 (rho z)))))
        (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))))
  (exact (funext (fn [i :- Nat]
    (p49tr_repl_env R h j rho i hneq hav)))))

(thm p49tele_repl [slots :- (List (Prod Nat Nat))]
  (forall [B (=> Nat Exp Exp)] (forall [K P49C]
    (forall [j Nat] (forall [R P49C]
      (forall [rho (=> Nat Exp)]
        (=> (p49tr_avoids slots j)
          (=> (p49tr_slots_avoid_context slots R)
            (Eq Exp
              (p49tele B ((p49crepl K) j R) slots rho)
              (p49tele B K slots
                (fn [i :- Nat]
                  (if (Nat.beq i j) (p49plug R rho) (rho i))))))))))))
  (induction slots)
  (intro B K j R rho hj hR)
  (exact (p49tele_empty_repl B K j R rho))
  (intro B K j R rho hj hR)
  (have hneq (Eq Bool (Nat.beq (Prod.fst head) j) Bool.false)
    (And.left hj))
  (have htail (p49tr_avoids tail j) (And.right hj))
  (have hRhead (p49tr_context_avoids R (Prod.fst head))
    (And.left hR))
  (have hRtail (p49tr_slots_avoid_context tail R) (And.right hR))
  (have hbody (Eq Exp
    (p49tele B ((p49crepl K) j R) tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (rho i)))))
    (p49tele B K tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1)
          (lift 2 0
            (if (Nat.beq i j) (p49plug R rho) (rho i)))))))
    (Eq.trans
      (ih_tail B K j R
        (fn [i :- Nat]
          (if (Nat.beq i (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho i))))
        htail hRtail)
      (congrArg (fn [env :- (=> Nat Exp)] (p49tele B K tail env))
        (Eq.symm
          (p49tr_repl_env_fun R (Prod.fst head) j rho
            hneq hRhead)))))
  (exact (congrArg
    (fn [body :- Exp]
      (Exp.tPi U.uw Exp.tSyn
        (Exp.tPi U.u1
          (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
    hbody)))

(thm p49tele_node_repl
  [slots :- (List (Prod Nat Nat)), B :- (=> Nat Exp Exp),
   K :- P49C, j :- Nat, l :- Nat, A :- P49C, C :- P49C,
   rho :- (=> Nat Exp),
   hj :- (p49tr_avoids slots j),
   hchildren :- (p49tr_slots_avoid_context slots (P49C.node l A C))]
  (Eq Exp
    (p49tele B ((p49crepl K) j (P49C.node l A C)) slots rho)
    (p49tele B K slots
      (fn [i :- Nat]
        (if (Nat.beq i j)
          (Exp.snode (Exp.lbl l) (p49plug A rho) (p49plug C rho))
          (rho i)))))
  (exact (p49tele_repl slots B K j (P49C.node l A C) rho
    hj hchildren)))

;; The two holes generated by a node are strictly above every existing
;; slot identifier, so the node replacement side condition is automatic.
(thm p49tr_beq_false_of_lt [h :- Nat, x :- Nat,
                           hlt :- (LT.lt h x)]
  (Eq Bool (Nat.beq x h) Bool.false)
  (exact (p49tr_false_of_not_true (Nat.beq x h)
    (fn [he :- (Eq Bool (Nat.beq x h) Bool.true)]
      (Nat.ne_of_lt hlt
        (Eq.symm (Nat.eq_of_beq_eq_true he)))))))

(thm p49tr_slots_avoid_fresh_node
  [slots :- (List (Prod Nat Nat))]
  (forall [l Nat] (forall [x Nat]
    (=> (p49slots_below slots x)
      (p49tr_slots_avoid_context slots
        (P49C.node l (P49C.hole x) (P49C.hole (Nat.succ x)))))))
  (induction slots)
  (intro l x hslots) (exact True.intro)
  (intro l x hslots)
  (have hlt (LT.lt (Prod.fst head) x) (And.left hslots))
  (have hlt2 (LT.lt (Prod.fst head) (Nat.succ x))
    (Nat.lt_of_lt_of_le hlt (Nat.le_succ x)))
  (exact (And.intro
    (And.intro
      (p49tr_beq_false_of_lt (Prod.fst head) x hlt)
      (p49tr_beq_false_of_lt (Prod.fst head) (Nat.succ x) hlt2))
    (ih_tail l x (And.right hslots)))))

(thm p49tele_fresh_node_repl
  [slots :- (List (Prod Nat Nat)), B :- (=> Nat Exp Exp),
   K :- P49C, j :- Nat, l :- Nat, rho :- (=> Nat Exp),
   hj :- (p49tr_avoids slots j)]
  (Eq Exp
    (p49tele B
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots rho)
    (p49tele B K slots
      (fn [i :- Nat]
        (if (Nat.beq i j)
          (Exp.snode (Exp.lbl l)
            (rho (p49fresh K slots))
            (rho (Nat.succ (p49fresh K slots))))
          (rho i)))))
  (exact (p49tele_node_repl slots B K j l
    (P49C.hole (p49fresh K slots))
    (P49C.hole (Nat.succ (p49fresh K slots))) rho hj
    (p49tr_slots_avoid_fresh_node slots l (p49fresh K slots)
      (p49fresh_slots K slots)))))
