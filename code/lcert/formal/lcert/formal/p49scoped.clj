(ns lcert.formal.p49scoped
  "Telescope congruence restricted to holes actually present in a context."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49tele]
            [lcert.formal.p49fresh]
            [lcert.formal.p49genfacts]
            [lcert.formal.p49telebridgeaux]
            [lcert.formal.p49telelaws]))

;; An environment can change at an identifier absent from K without
;; changing the rendered context or any telescope over K. In particular,
;; fresh child identifiers can be assigned live child variables while the
;; original selected-hole family keeps its closed default environment.
(kdef p49sc_agree (=> P49C (=> Nat Exp) (=> Nat Exp) Prop)
  (fn [K :- P49C]
    (P49C.rec$1
      (fn [_ :- P49C] (=> (=> Nat Exp) (=> Nat Exp) Prop))
      (fn [i :- Nat]
        (fn [rho :- (=> Nat Exp), sig :- (=> Nat Exp)]
          (Eq Exp (rho i) (sig i))))
      (fn [_ :- Nat]
        (fn [rho :- (=> Nat Exp), sig :- (=> Nat Exp)] True))
      (fn [_ :- Nat, a :- P49C, b :- P49C,
           ha :- (=> (=> Nat Exp) (=> Nat Exp) Prop),
           hb :- (=> (=> Nat Exp) (=> Nat Exp) Prop)]
        (fn [rho :- (=> Nat Exp), sig :- (=> Nat Exp)]
          (And (ha rho sig) (hb rho sig))))
      K)))

(thm p49sc_plug [K :- P49C]
  (forall [rho (=> Nat Exp)]
    (forall [sig (=> Nat Exp)]
      (=> (p49sc_agree K rho sig)
          (Eq Exp (p49plug K rho) (p49plug K sig)))))
  (induction K)
  (intro rho sig h) (exact h)
  (intro rho sig h) (rfl)
  (intro rho sig h)
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x (p49plug b rho)))
      (ih_a rho sig (And.left h)))
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l) (p49plug a sig) y))
      (ih_b rho sig (And.right h))))))

(thm p49sc_step [K :- P49C]
  (forall [rho (=> Nat Exp)]
    (forall [sig (=> Nat Exp)]
      (forall [h Nat]
        (=> (p49sc_agree K rho sig)
            (p49sc_agree K
              (fn [i :- Nat]
                (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (rho i))))
              (fn [i :- Nat]
                (if (Nat.beq i h) (Exp.var 1) (lift 2 0 (sig i)))))))))
  (induction K)
  (intro rho sig h hagree)
  (exact (Bool.rec$0
    (fn [flag :- Bool]
      (Eq Exp
        (if flag (Exp.var 1) (lift 2 0 (rho i)))
        (if flag (Exp.var 1) (lift 2 0 (sig i)))))
    (congrArg (fn [e :- Exp] (lift 2 0 e)) hagree)
    rfl (Nat.beq i h)))
  (intro rho sig h hagree) (exact True.intro)
  (intro rho sig h hagree)
  (exact (And.intro
    (ih_a rho sig h (And.left hagree))
    (ih_b rho sig h (And.right hagree)))))

(thm p49sc_tele [slots :- (List (Prod Nat Nat))]
  (forall [B (=> Nat Exp Exp)]
    (forall [K P49C]
      (forall [rho (=> Nat Exp)]
        (forall [sig (=> Nat Exp)]
          (=> (p49sc_agree K rho sig)
              (Eq Exp (p49tele B K slots rho)
                      (p49tele B K slots sig)))))))
  (induction slots)
  (intro B K rho sig hagree)
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (p49sc_plug K rho sig hagree)))
  (intro B K rho sig hagree)
  (exact (congrArg
    (fn [body :- Exp]
      (Exp.tPi U.uw Exp.tSyn
        (Exp.tPi U.u1
          (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
    (ih_tail B K
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (rho i))))
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (sig i))))
      (p49sc_step K rho sig (Prod.fst head) hagree)))))

(thm p49sc_below [K :- P49C]
  (forall [m Nat]
    (forall [rho (=> Nat Exp)]
      (forall [sig (=> Nat Exp)]
        (=> (p49c_below K m)
          (=> (forall [i Nat]
                (=> (LT.lt i m) (Eq Exp (rho i) (sig i))))
              (p49sc_agree K rho sig))))))
  (induction K)
  (intro m rho sig hbelow h) (exact (h i hbelow))
  (intro m rho sig hbelow h) (exact True.intro)
  (intro m rho sig hbelow h)
  (exact (And.intro
    (ih_a m rho sig (And.left hbelow) h)
    (ih_b m rho sig (And.right hbelow) h))))

(thm p49sc_tele_below
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), m :- Nat,
   rho :- (=> Nat Exp), sig :- (=> Nat Exp),
   hbelow :- (p49c_below K m),
   hagree :- (forall [i Nat]
               (=> (LT.lt i m) (Eq Exp (rho i) (sig i))))]
  (Eq Exp (p49tele B K slots rho) (p49tele B K slots sig))
  (exact (p49sc_tele slots B K rho sig
    (p49sc_below K m rho sig hbelow hagree))))

(kdef p49sc_pair_env
  (=> (=> Nat Exp) Nat Exp Exp (=> Nat Exp))
  (fn [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp]
    (fn [i :- Nat]
      (if (Nat.beq i m) x
        (if (Nat.beq i (Nat.succ m)) y (rho i))))))

(thm p49sc_pair_agree_lt
  [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp,
   i :- Nat, hi :- (LT.lt i m)]
  (Eq Exp (rho i) (p49sc_pair_env rho m x y i))
  (have h0 (Eq Bool (Nat.beq i m) Bool.false)
    (p49gf_beq_small_false i m hi))
  (have h1 (Eq Bool (Nat.beq i (Nat.succ m)) Bool.false)
    (p49gf_beq_small_false i (Nat.succ m)
      (Nat.lt_of_lt_of_le hi (Nat.le_succ m))))
  (unfold p49sc_pair_env)
  (rw [h0 h1]))

(thm p49sc_pair_tele
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), m :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp,
   hK :- (p49c_below K m)]
  (Eq Exp
    (p49tele B K slots rho)
    (p49tele B K slots (p49sc_pair_env rho m x y)))
  (exact (p49sc_tele_below B K slots m rho
    (p49sc_pair_env rho m x y) hK
    (fn [i :- Nat]
      (fn [hi :- (LT.lt i m)]
        (p49sc_pair_agree_lt rho m x y i hi))))))

(thm p49sc_fresh_pair_tele
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)),
   rho :- (=> Nat Exp), x :- Exp, y :- Exp]
  (Eq Exp
    (p49tele B K slots rho)
    (p49tele B K slots
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (p49sc_pair_tele B K slots (p49fresh K slots)
    rho x y (p49fresh_context K slots))))

(thm p49sc_selected_pair_agree_lt
  [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp,
   j :- Nat, c :- Exp, i :- Nat, hi :- (LT.lt i m)]
  (Eq Exp
    (if (Nat.beq i j) c (rho i))
    (if (Nat.beq i j) c (p49sc_pair_env rho m x y i)))
  (exact (Bool.rec$0
    (fn [flag :- Bool]
      (Eq Exp
        (if flag c (rho i))
        (if flag c (p49sc_pair_env rho m x y i))))
    (p49sc_pair_agree_lt rho m x y i hi)
    rfl (Nat.beq i j))))

(thm p49sc_F_pair
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp]
  (Eq (=> Exp Exp)
    (p49tba_F B K slots j rho)
    (p49tba_F B K slots j
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (funext (fn [c :- Exp]
    (p49sc_tele_below B K slots (p49fresh K slots)
      (fn [i :- Nat] (if (Nat.beq i j) c (rho i)))
      (fn [i :- Nat]
        (if (Nat.beq i j) c
          (p49sc_pair_env rho (p49fresh K slots) x y i)))
      (p49fresh_context K slots)
      (fn [i :- Nat]
        (fn [hi :- (LT.lt i (p49fresh K slots))]
          (p49sc_selected_pair_agree_lt rho
            (p49fresh K slots) x y j c i hi))))))))

(thm p49sc_F_pair_lift
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp,
   hL :- (p49tba_Lift (p49tba_F B K slots j rho))]
  (p49tba_Lift
    (p49tba_F B K slots j
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (Eq.mp
    (congrArg
      (fn [F :- (=> Exp Exp)] (p49tba_Lift F))
      (p49sc_F_pair B K slots j rho x y))
    hL)))

(thm p49sc_F_pair_subst
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat,
   rho :- (=> Nat Exp), x :- Exp, y :- Exp,
   hS :- (p49tba_Subst (p49tba_F B K slots j rho))]
  (p49tba_Subst
    (p49tba_F B K slots j
      (p49sc_pair_env rho (p49fresh K slots) x y)))
  (exact (Eq.mp
    (congrArg
      (fn [F :- (=> Exp Exp)] (p49tba_Subst F))
      (p49sc_F_pair B K slots j rho x y))
    hS)))

(thm p49sc_pair_left
  [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp]
  (Eq Exp (p49sc_pair_env rho m x y m) x)
  (unfold p49sc_pair_env)
  (rw [(Nat.beq_refl m)]))

(thm p49sc_pair_right
  [rho :- (=> Nat Exp), m :- Nat, x :- Exp, y :- Exp]
  (Eq Exp (p49sc_pair_env rho m x y (Nat.succ m)) y)
  (have h0 (Eq Bool (Nat.beq (Nat.succ m) m) Bool.false)
    (p49tr_beq_false_of_lt m (Nat.succ m)
      (Nat.lt_succ_self m)))
  (unfold p49sc_pair_env)
  (rw [h0 (Nat.beq_refl (Nat.succ m))]))

(thm p49sc_node_result_pair
  [B :- (=> Nat Exp Exp), K :- P49C,
   slots :- (List (Prod Nat Nat)), j :- Nat, l :- Nat,
   rho :- (=> Nat Exp),
   hj :- (p49tr_avoids slots j),
   hL :- (p49tba_Lift (p49tba_F B K slots j rho)),
   hS :- (p49tba_Subst (p49tba_F B K slots j rho)),
   sg :- (=> Nat Exp), x :- Exp, y :- Exp,
   hl :- (Eq Exp (sg 5) (Exp.lbl l)),
   hx :- (Eq Exp (sg 4) x),
   hy :- (Eq Exp (sg 3) y)]
  (Eq Exp
    (subst sg
      (p49peel_nodeB (p49tba_F B K slots j rho (Exp.var 0))))
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
        (subst sg (p49peel_nodeB (F (Exp.var 0)))))
      (p49sc_F_pair B K slots j rho x y))
    (p49tba_node_result_tail B K slots j l
      (p49sc_pair_env rho (p49fresh K slots) x y)
      hj
      (p49sc_F_pair_lift B K slots j rho x y hL)
      (p49sc_F_pair_subst B K slots j rho x y hS)
      sg hl
      (Eq.trans hx
        (Eq.symm (p49sc_pair_left rho (p49fresh K slots) x y)))
      (Eq.trans hy
        (Eq.symm (p49sc_pair_right rho (p49fresh K slots) x y)))))))

(thm p49sc_node_result_concrete
  [K :- P49C, slots :- (List (Prod Nat Nat)),
   j :- Nat, l :- Nat,
   hj :- (p49tr_avoids slots j),
   sg :- (=> Nat Exp), x :- Exp, y :- Exp,
   hl :- (Eq Exp (sg 5) (Exp.lbl l)),
   hx :- (Eq Exp (sg 4) x),
   hy :- (Eq Exp (sg 3) y)]
  (Eq Exp
    (subst sg
      (p49peel_nodeB
        (p49tba_F
          (fn [q :- Nat, c :- Exp] (p49dl q c))
          K slots j p49tl_rho0 (Exp.var 0))))
    (p49tele (fn [q :- Nat, c :- Exp] (p49dl q c))
      ((p49crepl K) j
        (P49C.node l
          (P49C.hole (p49fresh K slots))
          (P49C.hole (Nat.succ (p49fresh K slots)))))
      slots
      (p49sc_pair_env p49tl_rho0 (p49fresh K slots) x y)))
  (exact (p49sc_node_result_pair
    (fn [q :- Nat, c :- Exp] (p49dl q c))
    K slots j l p49tl_rho0 hj
    (p49tl_p49dl_Lift K slots j)
    (p49tl_p49dl_Subst K slots j)
    sg x y hl hx hy)))
