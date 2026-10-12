(ns lcert.formal.p49context
  "Code contexts with stable hole names for Proposition 4.9.
  A separate environment maps hole names to de Bruijn expressions."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.syntax]))

;; Hole identifiers stay fixed as the finite case tree is refined. The map
;; rho, rather than the tree, assigns de Bruijn expressions to those holes.
(a/inductive P49C []
  (hole [i Nat])
  (leaf [l Nat])
  (node [l Nat] [a P49C] [b P49C]))

(a/defn p49plug [K :- P49C] (=> (=> Nat Exp) Exp)
  (match K
    [(hole i) (fn [rho :- (=> Nat Exp)] (rho i))]
    [(leaf l) (fn [rho :- (=> Nat Exp)] (Exp.sleaf (Exp.lbl l)))]
    [(node l a b)
     (fn [rho :- (=> Nat Exp)]
       (Exp.snode (Exp.lbl l) ((p49plug a) rho) ((p49plug b) rho)))]))

;; Ansatz's generated a/defn equation for this two-argument motive leaves
;; a free variable in the kernel. State the structural recursor explicitly.
(kdef p49crepl (=> P49C Nat P49C P49C)
  (fn [K :- P49C]
    (P49C.rec$1 (fn [_ :- P49C] (=> Nat P49C P49C))
      (fn [h :- Nat]
        (fn [i :- Nat, R :- P49C]
          (if (Nat.beq h i) R (P49C.hole h))))
      (fn [l :- Nat]
        (fn [i :- Nat, R :- P49C] (P49C.leaf l)))
      (fn [l :- Nat, a :- P49C, b :- P49C,
           ih_a :- (=> Nat P49C P49C), ih_b :- (=> Nat P49C P49C)]
        (fn [i :- Nat, R :- P49C]
          (P49C.node l (ih_a i R) (ih_b i R))))
      K)))

;; Substitution and lifting commute with rendering because the only
;; constructors above a hole are the nonbinding code constructors.
(thm p49plug_subst [K :- P49C]
  (forall [sg (=> Nat Exp)]
    (forall [rho (=> Nat Exp)]
      (Eq Exp (subst sg (p49plug K rho))
        (p49plug K (fn [i :- Nat] (subst sg (rho i)))))))
  (induction K)
  (intro sg rho) (rfl)
  (intro sg rho) (rfl)
  (intro sg rho)
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x (subst sg (p49plug b rho))))
      (ih_a sg rho))
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l)
        (p49plug a (fn [i :- Nat] (subst sg (rho i)))) y))
      (ih_b sg rho)))))

(thm p49plug_lift [K :- P49C]
  (forall [n Nat]
    (forall [cut Nat]
      (forall [rho (=> Nat Exp)]
        (Eq Exp (lift n cut (p49plug K rho))
          (p49plug K (fn [i :- Nat] (lift n cut (rho i))))))))
  (induction K)
  (intro n cut rho) (rfl)
  (intro n cut rho) (rfl)
  (intro n cut rho)
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x (lift n cut (p49plug b rho))))
      (ih_a n cut rho))
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l)
        (p49plug a (fn [i :- Nat] (lift n cut (rho i)))) y))
      (ih_b n cut rho)))))

(thm p49plug_choose [flag :- Bool, X :- P49C, Y :- P49C,
                     rho :- (=> Nat Exp)]
  (Eq Exp (p49plug (if flag X Y) rho)
    (if flag (p49plug X rho) (p49plug Y rho)))
  (cases flag) (rfl) (rfl))

;; Replacing one named hole, then rendering, equals rendering under an
;; environment update. This is the algebraic bridge needed when a leaf or
;; node case replaces the selected code variable with a constructor.
(thm p49plug_repl [K :- P49C]
  (forall [j Nat]
    (forall [R P49C]
      (forall [rho (=> Nat Exp)]
        (Eq Exp (p49plug ((p49crepl K) j R) rho)
          (p49plug K (fn [i :- Nat]
            (if (Nat.beq i j) (p49plug R rho) (rho i))))))))
  (induction K)
  (intro j R rho)
  (exact (p49plug_choose (Nat.beq i j) R (P49C.hole i) rho))
  (intro j R rho) (rfl)
  (intro j R rho)
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x (p49plug ((p49crepl b) j R) rho)))
      (ih_a j R rho))
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l)
        (p49plug a (fn [i :- Nat]
          (if (Nat.beq i j) (p49plug R rho) (rho i)))) y))
      (ih_b j R rho)))))
