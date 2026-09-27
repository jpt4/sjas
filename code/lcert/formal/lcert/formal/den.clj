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
  the denotation is built as a table of levels (denT): denAt prev cap gives
  one level, with prev the table below.  den n := denT n n."
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

(kdef DenParams (Sort 1)
  (Prod (=> Code Code Bool) (Prod (=> Code (Option (Prod Nat (Prod Exp Exp)))) (=> Exp Code))))

;; The table of levels.  T n is a DenFn defined for every cap m ≤ n:
;;   T 0 m     = denAt den0 m                     (cap 0: no decoded program runs)
;;   T (n+1) m = T n m                  if m ≤ n
;;             = denAt (T n) m          otherwise
;; So T (n+1) (n+1) runs decoded programs through T n, and T n m = T m m for
;; m ≤ n (denT_stable): reflect at cap c runs its program at exactly its own
;; budget m, as §3.2 says, with no fuel-independence argument over terms.
(kdef DenBody (Sort 1) (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))))

;; pickD b x y: x if b, else y — a named selector, since Ansatz's elaborator
;; rejects a recursor applied to more arguments than its arity.
(kdef pickD (=> Bool DenBody DenBody DenBody)
  (fn [b :- Bool, x :- DenBody, y :- DenBody] (Bool.rec$1 (fn [_ :- Bool] DenBody) y x b)))

(kdef denT
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat DenFn))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] DenFn)
      (fn [m :- Nat, t :- Exp] (denAt chkf dec encTy den0 m t))
      (fn [k :- Nat, prev :- DenFn]
        (fn [m :- Nat, t :- Exp] (pickD (Nat.ble m k) (prev m t) (denAt chkf dec encTy prev m t))))
      n)))

;; One level: below the new level, the table is unchanged.
(thm denT_step [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                k :- Nat, m :- Nat, h :- (Nat.le m k), t :- Exp]
  (= (denT chkf dec encTy (Nat.succ k) m t) (denT chkf dec encTy k m t))
  (have hb (= (Nat.ble m k) Bool.true) (Nat.ble_eq_true_of_le h))
  (change (= (pickD (Nat.ble m k) (denT chkf dec encTy k m t) (denAt chkf dec encTy (denT chkf dec encTy k) m t))
             (denT chkf dec encTy k m t)))
  (rw [hb]))

(thm ble_succ_self [k :- Nat] (= (Nat.ble (Nat.succ k) k) Bool.false)
  (induction k)
  (rfl)
  (exact ih_n))

;; The top level of the table at n + 1 runs decoded programs through level n.
(thm denT_top [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
               k :- Nat, t :- Exp]
  (= (denT chkf dec encTy (Nat.succ k) (Nat.succ k) t) (denAt chkf dec encTy (denT chkf dec encTy k) (Nat.succ k) t))
  (have hb (= (Nat.ble (Nat.succ k) k) Bool.false) (ble_succ_self k))
  (change (= (pickD (Nat.ble (Nat.succ k) k) (denT chkf dec encTy k (Nat.succ k) t) (denAt chkf dec encTy (denT chkf dec encTy k) (Nat.succ k) t))
             (denAt chkf dec encTy (denT chkf dec encTy k) (Nat.succ k) t)))
  (rw [hb]))

;; ⟦t⟧ⁿ: the table at level n, cap n.
(kdef den
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat Exp (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk))))))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, t :- Exp]
    (denT chkf dec encTy n n t)))

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
