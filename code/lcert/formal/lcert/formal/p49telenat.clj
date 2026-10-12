(ns lcert.formal.p49telenat
  "Extensionality of named-hole telescope environments."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.p49tele]))

;; The base checker expression sees only the values of the holes it uses.
(thm p49plug_rho_congr [K :- P49C]
  (forall [rho (=> Nat Exp)]
    (forall [sig (=> Nat Exp)]
      (=> (forall [i Nat] (Eq Exp (rho i) (sig i)))
          (Eq Exp (p49plug K rho) (p49plug K sig)))))
  (induction K)
  (intro rho sig h) (exact (h i))
  (intro rho sig h) (rfl)
  (intro rho sig h)
  (exact (Eq.trans
    (congrArg (fn [x :- Exp]
      (Exp.snode (Exp.lbl l) x (p49plug b rho)))
      (ih_a rho sig h))
    (congrArg (fn [y :- Exp]
      (Exp.snode (Exp.lbl l) (p49plug a sig) y))
      (ih_b rho sig h)))))

(thm p49tele_choose_lift_congr
  [flag :- Bool, x :- Exp, y :- Exp, h :- (Eq Exp x y)]
  (Eq Exp
    (if flag (Exp.var 1) (lift 2 0 x))
    (if flag (Exp.var 1) (lift 2 0 y)))
  (cases flag)
  (exact (congrArg (fn [v :- Exp] (lift 2 0 v)) h))
  (rfl))

;; This theorem lets later substitution and context-replacement proofs
;; compare environment updates pointwise without asserting function equality.
(thm p49tele_rho_congr
  [B :- (=> Nat Exp Exp), K :- P49C, slots :- (List (Prod Nat Nat))]
  (forall [rho (=> Nat Exp)]
    (forall [sig (=> Nat Exp)]
      (=> (forall [i Nat] (Eq Exp (rho i) (sig i)))
          (Eq Exp (p49tele B K slots rho)
                  (p49tele B K slots sig)))))
  (induction slots)
  (intro rho sig h)
  (exact (congrArg
    (fn [e :- Exp]
      (Exp.tPi U.u1
        (Exp.tT (Exp.chk e (Exp.sleaf (Exp.lbl 15))))
        Exp.tEmpty))
    (p49plug_rho_congr K rho sig h)))
  (intro rho sig h)
  (exact (congrArg
    (fn [body :- Exp]
      (Exp.tPi U.uw Exp.tSyn
        (Exp.tPi U.u1
          (Exp.tT (B (Prod.snd head) (Exp.var 0))) body)))
    (ih_tail
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (rho i))))
      (fn [i :- Nat]
        (if (Nat.beq i (Prod.fst head))
          (Exp.var 1) (lift 2 0 (sig i))))
      (fn [i :- Nat]
        (p49tele_choose_lift_congr
          (Nat.beq i (Prod.fst head)) (rho i) (sig i) (h i)))))))
