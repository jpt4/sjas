(ns lcert.formal.check
  "F7 (part) — building blocks of the concrete checker.

  codeEq decides equality of codes (structurally; codeEq_sound, codeEq_refl);
  expEq decides equality of expressions by comparing their codes, sound
  because the encoding is injective (encE_inj, encode.clj)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.skeletons]
            [lcert.formal.encode :refer :all]))

;; codeEq: equality of codes, by structural recursion on both.
(kdef codeEq (=> Code Code Bool)
  (fn [a :- Code]
    (Code.rec$1 (fn [_ :- Code] (=> Code Bool))
      (fn [l :- Nat] (fn [b :- Code] (Code.rec$1 (fn [_ :- Code] Bool) (fn [l2 :- Nat] (Nat.beq l l2)) (fn [l2 :- Nat, x :- Code, y :- Code, _ :- Bool, _ :- Bool] Bool.false) b)))
      (fn [l :- Nat, x :- Code, y :- Code, ihx :- (=> Code Bool), ihy :- (=> Code Bool)]
        (fn [b :- Code] (Code.rec$1 (fn [_ :- Code] Bool) (fn [l2 :- Nat] Bool.false)
                          (fn [l2 :- Nat, x2 :- Code, y2 :- Code, _ :- Bool, _ :- Bool] (Bool.and (Nat.beq l l2) (Bool.and (ihx x2) (ihy y2)))) b)))
      a)))

;; soundness, node case by a step lemma with clean names
(thm codeEq_sn [l :- Nat, x :- Code, y :- Code, l2 :- Nat, x2 :- Code, y2 :- Code,
                  ihx :- (forall [b Code] (=> (Eq Bool (codeEq x b) Bool.true) (Eq Code x b))),
                  ihy :- (forall [b Code] (=> (Eq Bool (codeEq y b) Bool.true) (Eq Code y b))),
                  h :- (Eq Bool (Bool.and (Nat.beq l l2) (Bool.and (codeEq x x2) (codeEq y y2))) Bool.true)]
  (Eq Code (Code.sn l x y) (Code.sn l2 x2 y2))
  (have h1 (Eq Bool (Nat.beq l l2) Bool.true) (band_left (Nat.beq l l2) (Bool.and (codeEq x x2) (codeEq y y2)) h))
  (have h2 (Eq Bool (Bool.and (codeEq x x2) (codeEq y y2)) Bool.true) (band_right (Nat.beq l l2) (Bool.and (codeEq x x2) (codeEq y y2)) h))
  (rw [(Nat.eq_of_beq_eq_true h1) (ihx x2 (band_left (codeEq x x2) (codeEq y y2) h2)) (ihy y2 (band_right (codeEq x x2) (codeEq y y2) h2))]))

(thm nbeq_refl [i :- Nat] (Eq Bool (Nat.beq i i) Bool.true) (induction i) (rfl) (exact ih_n))
(thm codeEq_refl [c :- Code] (Eq Bool (codeEq c c) Bool.true)
  (induction c) (exact (nbeq_refl l))
  (change (Eq Bool (Bool.and (Nat.beq l l) (Bool.and (codeEq a a) (codeEq b b))) Bool.true))
  (rw [(nbeq_refl l) ih_a ih_b]))
(thm codeEq_sound [c :- Code] (forall [d Code] (=> (Eq Bool (codeEq c d) Bool.true) (Eq Code c d)))
  (induction c)
  (intro d) (cases d) (intro h) (exact (congrArg Code.sl (Nat.eq_of_beq_eq_true h))) (intro h) (exact (Bool.noConfusion h))
  (intro d) (cases d) (intro h) (exact (Bool.noConfusion h)) (intro h) (exact (codeEq_sn l a b _ _ _ ih_a ih_b h)))

;; Equality of expressions, decided through the injective encoding (E1).
(kdef expEq (=> Exp Exp Bool) (fn [a :- Exp, b :- Exp] (codeEq (encE a) (encE b))))
(thm expEq_sound [a :- Exp, b :- Exp, h :- (Eq Bool (expEq a b) Bool.true)] (Eq Exp a b)
  (exact (encE_inj a b (codeEq_sound (encE a) (encE b) h))))
(thm expEq_refl [a :- Exp] (Eq Bool (expEq a a) Bool.true) (exact (codeEq_refl (encE a))))
