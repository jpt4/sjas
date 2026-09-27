(ns lcert.formal.den
  "F3c — denotation (R4-metatheory.md §3.2).

  ⟦t⟧ is a function of the term, the skeleton context G, the expected
  skeleton sk and an environment en : HEnv G, with values in Car sk.  It is
  parameterized by the checker chkf, a decoding dec of codes into (budget,
  term, type), and a type encoding encTy, which the model's theorems constrain
  through CheckSpec (ADR-0006, trust base).

  The clauses (one kernel definition per constructor, generated into
  den_gen.clj by formal/tools/gen_den.py) follow §3.2:
  - constructors, λ, application, pairs, let and the eliminators have their
    set-theoretic meaning; recN, recSyn and itR iterate over the value of
    their scrutinee; caseLbl's branch list denotes a function of the label;
  - abort and H₁ denote defaults; chk′ is chkf; print forgets tokens (the
    identity on Code); inspect branches on chkf;
  - reflect_D r e, at cap c: if the tree v = ⟦r⟧ has at most c nodes and
    chkf v ⌜D⌝ holds, the program dec v decodes to is run at its own budget m;
    otherwise the default.
  Running the decoded program needs the denotation at a smaller budget, so
  the denotation is built by recursion on a fuel level: denAt prev cap gives
  one level, with prev the level below, and denN iterates it.  den n := denN
  at fuel n and cap n."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]))

(kdef DenFn (Sort 1)
  (=> Nat Exp (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk))))))

(load "den_gen")

;; Fuel 0: running a decoded program is impossible (cap 0 admits no tree that
;; Check accepts, since an accepted tree has at least one node), so prev gives
;; defaults.
(kdef den0 DenFn (fn [m :- Nat, t :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G)] (dflt sk)))

(kdef denN
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat DenFn))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] DenFn)
      (fn [cap :- Nat, t :- Exp] (denAt chkf dec encTy den0 cap t))
      (fn [k :- Nat, prev :- DenFn] (fn [cap :- Nat, t :- Exp] (denAt chkf dec encTy prev cap t)))
      n)))

;; ⟦t⟧ⁿ: fuel n, cap n.
(kdef den
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat Exp (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk))))))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, t :- Exp]
    (denN chkf dec encTy n n t)))

;; Sanity: the kernel computes denotations.
(thm den_not_tt [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (= (den chkf dec encTy 0 (Exp.app (Exp.lam U.uw Exp.tBool (Exp.ite (Exp.var 0) Exp.ff Exp.tt)) Exp.tt)
          (List.nil Sk) Sk.bool Unit.unit)
     Bool.false)
  (rfl))

(thm den_recN_double [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (= (den chkf dec encTy 0 (Exp.recN Exp.tNat Exp.zero (Exp.succ (Exp.succ (Exp.var 0))) (Exp.succ (Exp.succ (Exp.succ Exp.zero))))
          (List.nil Sk) Sk.nat Unit.unit)
     6)
  (rfl))
