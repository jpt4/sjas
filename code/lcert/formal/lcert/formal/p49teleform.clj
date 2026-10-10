(ns lcert.formal.p49teleform
  "Formation of the p49dl telescope under a typed hole environment."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.p49tele]
            [lcert.formal.p49contextfacts]
            [lcert.formal.p49depth]
            [lcert.formal.derivations]))

;; The second binder is the depth witness. Existing holes cross both binders.
(thm p49tf_weaken2
  [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, e :- Exp,
   he :- (Tl chkf Bool.false D e Exp.tSyn)]
  (Tl chkf Bool.false
    (List.cons Exp A (List.cons Exp Exp.tSyn D))
    (lift 2 0 e) Exp.tSyn)
  (exact (tl_cast3 chkf Bool.false
    (List.cons Exp A (List.cons Exp Exp.tSyn D))
    (lift 1 0 (lift 1 0 e)) (lift 2 0 e)
    Exp.tSyn Exp.tSyn
    (tl_weaken chkf Bool.false
      (List.cons Exp Exp.tSyn D) (lift 1 0 e) Exp.tSyn
      (tl_weaken chkf Bool.false D e Exp.tSyn he 0 Exp.tSyn)
      0 A)
    (lift_comp e 1 1 0) rfl)))

(thm p49tf_bound_code
  [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp]
  (Tl chkf Bool.false
    (List.cons Exp A (List.cons Exp Exp.tSyn D))
    (Exp.var 1) Exp.tSyn)
  (exact (Tl.zVar chkf
    (List.cons Exp A (List.cons Exp Exp.tSyn D))
    1 Exp.tSyn
    (Eq.trans
      (nthE.eq_3 A (List.cons Exp Exp.tSyn D) 0)
      (nthE.eq_2 Exp.tSyn D)))))

(thm p49tf_if_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), flag :- Bool,
   x :- Exp, y :- Exp, A :- Exp,
   hx :- (Tl chkf Bool.false D x A),
   hy :- (Tl chkf Bool.false D y A)]
  (Tl chkf Bool.false D (if flag x y) A)
  (exact (Bool.rec$0
    (fn [b :- Bool] (Tl chkf Bool.false D (if b x y) A))
    hy hx flag)))

(thm p49tf_update_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), rho :- (=> Nat Exp),
   hrho :- (forall [h Nat] (Tl chkf Bool.false D (rho h) Exp.tSyn)),
   i :- Nat, A :- Exp]
  (forall [h Nat]
    (Tl chkf Bool.false
      (List.cons Exp A (List.cons Exp Exp.tSyn D))
      (if (Nat.beq h i) (Exp.var 1) (lift 2 0 (rho h))) Exp.tSyn))
  (intro h)
  (exact (p49tf_if_typed chkf
    (List.cons Exp A (List.cons Exp Exp.tSyn D))
    (Nat.beq h i) (Exp.var 1) (lift 2 0 (rho h)) Exp.tSyn
    (p49tf_bound_code chkf D A)
    (p49tf_weaken2 chkf D A (rho h) (hrho h)))))

;; This proves formation only. It assumes the ambient holes have type Syn
;; and that the finite labels in K are below 100. It supplies no inhabitant.
(thm p49tele_p49dl_formed
  [chkf :- (=> Code Code Bool), K :- P49C,
   hK :- (Eq Bool (p49c_lblok K) Bool.true),
   slots :- (List (Prod Nat Nat))]
  (forall [D (List Exp)]
    (forall [rho (=> Nat Exp)]
      (=> (forall [h Nat] (Tl chkf Bool.false D (rho h) Exp.tSyn))
          (Tl chkf Bool.true D
            (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c)) K slots rho)
            Exp.tUnit))))
  (induction slots)
  (intro D rho hrho)
  (exact (Tl.fPi chkf D U.u1
    (Exp.tT (Exp.chk (p49plug K rho) (Exp.sleaf (Exp.lbl 15))))
    Exp.tEmpty
    (Tl.fT chkf D (Exp.chk (p49plug K rho) (Exp.sleaf (Exp.lbl 15)))
      (Tl.zChk chkf D (p49plug K rho) (Exp.sleaf (Exp.lbl 15))
        (p49plug_typed chkf D rho hrho K hK)
        (Tl.zSleaf chkf D (Exp.lbl 15)
          (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl rfl))))
    (Tl.fBase chkf
      (List.cons Exp
        (Exp.tT (Exp.chk (p49plug K rho) (Exp.sleaf (Exp.lbl 15)))) D)
      Exp.tEmpty rfl)))
  (intro D rho hrho)
  (exact (Tl.fPi chkf D U.uw Exp.tSyn
    (Exp.tPi U.u1
      (Exp.tT (p49dl (Prod.snd head) (Exp.var 0)))
      (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c)) K tail
        (fn [h :- Nat]
          (if (Nat.beq h (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho h))))))
    (Tl.fBase chkf D Exp.tSyn rfl)
    (Tl.fPi chkf (List.cons Exp Exp.tSyn D) U.u1
      (Exp.tT (p49dl (Prod.snd head) (Exp.var 0)))
      (p49tele (fn [j :- Nat, c :- Exp] (p49dl j c)) K tail
        (fn [h :- Nat]
          (if (Nat.beq h (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho h)))))
      (Tl.fT chkf (List.cons Exp Exp.tSyn D)
        (p49dl (Prod.snd head) (Exp.var 0))
        (p49dl_typed chkf (Prod.snd head)
          (List.cons Exp Exp.tSyn D) (Exp.var 0)
          (Tl.zVar chkf (List.cons Exp Exp.tSyn D) 0 Exp.tSyn
            (nthE.eq_2 Exp.tSyn D))))
      (ih_tail
        (List.cons Exp (Exp.tT (p49dl (Prod.snd head) (Exp.var 0)))
          (List.cons Exp Exp.tSyn D))
        (fn [h :- Nat]
          (if (Nat.beq h (Prod.fst head))
            (Exp.var 1) (lift 2 0 (rho h))))
        (p49tf_update_typed chkf D rho hrho (Prod.fst head)
          (Exp.tT (p49dl (Prod.snd head) (Exp.var 0)))))))))
