(ns lcert.formal.skel
  "F2a — usage vectors, codes, skeletons and simple typing
  (R4-metatheory.md §1.1, §1.6, Lemma 2.5).

  Usage vectors.  A judgment's context is a telescope of types (a list, the
  innermost binder first) and a usage vector of the same length.  Context
  addition Γ₁ + Γ₂ and scaling ρΓ act on usage vectors only, so the context
  convention of §1.4 — every context in one rule shares one telescope — holds
  by construction.

  Codes.  A code is a finite tree over labels (natural numbers):
  `sl l` a leaf, `sn l a b` an internal node.  `codeOf` reads a closed
  canonical code term (built from sleaf, snode and label constants) as a
  code; it is `none` on anything else.  δ-steps apply only where it is
  defined (§1.5).

  Skeletons (Lemma 2.5).  skel(0) = skel(1) = skel(T b) = Unit; Π and Σ go to
  arrows and products; other base types and ◇ are their own skeletons."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]))

;; --- usage vectors -------------------------------------------------------

(a/defn vadd [x :- (List U), y :- (List U)] (List U)
  (match x
    [nil (List.nil)]
    [(cons a xs) (match y [nil (List.nil)] [(cons b ys) (List.cons (uadd a b) (vadd xs ys))])]))

(a/defn vscale [r :- U, x :- (List U)] (List U)
  (match x [nil (List.nil)] [(cons a xs) (List.cons (umul r a) (vscale r xs))]))

(a/defn vzero [n :- Nat] (List U)
  (match n [zero (List.nil)] [(succ m) (List.cons U.u0 (vzero m))]))

;; --- codes -----------------------------------------------------------------

(a/inductive Code [] (sl [l Nat]) (sn [l Nat] [a Code] [b Code]))

(a/defn codeOf [e :- Exp] (Option Code)
  (match e
    [(sleaf x) (match x [(lbl l) (Option.some (Code.sl l))] [_ Option.none])]
    [(snode x c1 c2)
     (match x
       [(lbl l) (match (codeOf c1)
                  [none Option.none]
                  [(some a) (match (codeOf c2) [none Option.none] [(some b) (Option.some (Code.sn l a b))])])]
       [_ Option.none])]
    [_ Option.none]))

(a/defn cnodes [c :- Code] Nat
  (match c [(sl l) 0] [(sn l x y) (+ 1 (+ (cnodes x) (cnodes y)))]))

;; --- skeletons ---------------------------------------------------------------

(a/inductive Sk [] (unit) (bool) (nat) (lbl) (syn) (dia) (cert)
  (arr [s Sk] [t Sk]) (prod [s Sk] [t Sk]))

(a/defn skel [A :- Exp] Sk
  (match A
    [tEmpty Sk.unit] [tUnit Sk.unit] [(tT b) Sk.unit]
    [tBool Sk.bool] [tNat Sk.nat] [tLbl Sk.lbl] [tSyn Sk.syn] [tDia Sk.dia] [tR Sk.cert]
    [(tPi r X Y) (Sk.arr (skel X) (skel Y))]
    [(tSig r X Y) (Sk.prod (skel X) (skel Y))]
    [_ Sk.unit]))

(thm skel_T [b :- Exp] (= (skel (Exp.tT b)) Sk.unit) (rfl))
(thm skel_pi [r :- U, X :- Exp, Y :- Exp] (= (skel (Exp.tPi r X Y)) (Sk.arr (skel X) (skel Y))) (rfl))
