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
;; ‹lemma uadd_zero_left: statement and proof elided, lines 32–32›
;; ‹lemma uadd_one_one: statement and proof elided, lines 33–33›
;; ‹lemma uadd_omega_left: statement and proof elided, lines 34–34›
;; ‹lemma umul_zero_left: statement and proof elided, lines 35–35›
;; ‹lemma umul_one_left: statement and proof elided, lines 36–36›
;; ‹lemma umul_omega_omega: statement and proof elided, lines 37–37›
;; ‹lemma umul_omega_one: statement and proof elided, lines 38–38›
;; ‹lemma umul_omega_zero: statement and proof elided, lines 39–39›

;; Algebraic laws, by exhaustion.
;; ‹lemma uadd_comm: statement and proof elided, lines 42–43›
;; ‹lemma uadd_assoc: statement and proof elided, lines 44–45›
;; ‹lemma uadd_zero_right: statement and proof elided, lines 46–46›
;; ‹lemma umul_comm: statement and proof elided, lines 47–48›
;; ‹lemma umul_assoc: statement and proof elided, lines 49–50›
;; ‹lemma umul_one_right: statement and proof elided, lines 51–51›
;; ‹lemma umul_zero_right: statement and proof elided, lines 52–52›
;; ‹lemma umul_distrib: statement and proof elided, lines 53–54›

;; No cancelling: a sum is 0 only if both parts are, and a product of nonzero
;; usages is nonzero.  (The restriction fact of Theorem 4′ and Theorem 5.2.)
;; ‹lemma uadd_eq_zero: statement and proof elided, lines 58–59›
;; ‹lemma umul_eq_zero: statement and proof elided, lines 60–62›
