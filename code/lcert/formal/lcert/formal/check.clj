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
;; ‹lemma codeEq_sn: statement and proof elided, lines 28–35›

;; ‹lemma nbeq_refl: statement and proof elided, lines 37–37›
;; ‹lemma codeEq_refl: statement and proof elided, lines 38–41›
;; ‹lemma codeEq_sound: statement and proof elided, lines 42–45›

;; Equality of expressions, decided through the injective encoding (E1).
(kdef expEq (=> Exp Exp Bool) (fn [a :- Exp, b :- Exp] (codeEq (encE a) (encE b))))
;; ‹lemma expEq_sound: statement and proof elided, lines 49–50›
;; ‹lemma expEq_refl: statement and proof elided, lines 51–51›
