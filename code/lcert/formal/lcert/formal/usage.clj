(ns lcert.formal.usage
  "F1a — usages and their arithmetic (R4-metatheory.md §1.1).

  Usages are 0 (erased), 1 (at most once at runtime) and ω (unrestricted):
    0 + ρ = ρ     1 + 1 = ω     ω + ρ = ω
    0 · ρ = 0     1 · ρ = ρ     ω · ω = ω
  and both operations are commutative, so also ω · 0 = 0 and ω · 1 = ω
  (§1.1, as corrected in review round 3, P5-SEMI).

  Proved here: the table itself, commutativity, associativity, identities,
  annihilation, distributivity, and the two facts the restriction arguments
  of §5 use: a sum or product is 0 only when its parts allow it (usages add
  and scale without cancelling)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]))

(a/inductive U [] (u0) (u1) (uw))

(a/defn uadd [x :- U, y :- U] U
  (match x
    [u0 y]
    [u1 (match y [u0 U.u1] [u1 U.uw] [uw U.uw])]
    [uw U.uw]))

(a/defn umul [x :- U, y :- U] U
  (match x
    [u0 U.u0]
    [u1 y]
    [uw (match y [u0 U.u0] [u1 U.uw] [uw U.uw])]))

;; The table of §1.1.
(thm uadd_zero_left [r :- U] (= (uadd U.u0 r) r) (rfl))
(thm uadd_one_one [] (= (uadd U.u1 U.u1) U.uw) (rfl))
(thm uadd_omega_left [r :- U] (= (uadd U.uw r) U.uw) (rfl))
(thm umul_zero_left [r :- U] (= (umul U.u0 r) U.u0) (rfl))
(thm umul_one_left [r :- U] (= (umul U.u1 r) r) (rfl))
(thm umul_omega_omega [] (= (umul U.uw U.uw) U.uw) (rfl))
(thm umul_omega_one [] (= (umul U.uw U.u1) U.uw) (rfl))
(thm umul_omega_zero [] (= (umul U.uw U.u0) U.u0) (rfl))

;; Algebraic laws, by exhaustion.
(thm uadd_comm [x :- U, y :- U] (= (uadd x y) (uadd y x))
  (cases x) (all_goals (cases y)) (all_goals (rfl)))
(thm uadd_assoc [x :- U, y :- U, z :- U] (= (uadd (uadd x y) z) (uadd x (uadd y z)))
  (cases x) (all_goals (cases y)) (all_goals (cases z)) (all_goals (rfl)))
(thm uadd_zero_right [r :- U] (= (uadd r U.u0) r) (cases r) (all_goals (rfl)))
(thm umul_comm [x :- U, y :- U] (= (umul x y) (umul y x))
  (cases x) (all_goals (cases y)) (all_goals (rfl)))
(thm umul_assoc [x :- U, y :- U, z :- U] (= (umul (umul x y) z) (umul x (umul y z)))
  (cases x) (all_goals (cases y)) (all_goals (cases z)) (all_goals (rfl)))
(thm umul_one_right [r :- U] (= (umul r U.u1) r) (cases r) (all_goals (rfl)))
(thm umul_zero_right [r :- U] (= (umul r U.u0) U.u0) (cases r) (all_goals (rfl)))
(thm umul_distrib [x :- U, y :- U, z :- U] (= (umul x (uadd y z)) (uadd (umul x y) (umul x z)))
  (cases x) (all_goals (cases y)) (all_goals (cases z)) (all_goals (rfl)))

;; No cancelling: a sum is 0 only if both parts are, and a product of nonzero
;; usages is nonzero.  (The restriction fact of Theorem 4′ and Theorem 5.2.)
(thm uadd_eq_zero [x :- U, y :- U, h :- (= (uadd x y) U.u0)] (And (= x U.u0) (= y U.u0))
  (cases x) (all_goals (cases y)) (all_goals (first (exact (And.intro rfl rfl)) (exact (U.noConfusion h)))))
(thm umul_eq_zero [x :- U, y :- U, h :- (= (umul x y) U.u0)] (Or (= x U.u0) (= y U.u0))
  (cases x) (all_goals (cases y))
  (all_goals (first (exact (Or.inl rfl)) (exact (Or.inr rfl)) (exact (U.noConfusion h)))))
