(ns lcert.formal.encode
  "F7 (part) — the encoding of expressions into codes, and CheckSpec's
  encoding clauses (R4-metatheory.md §1.6).

  encE mirrors lcert.encode/enc-exp (the executable implementation of
  ADR-0005), with each label its position in lcert.syntax/encoding-labels
  (so ⌜0⌝ = sl 15 and the linear arrow is label 25, as CheckSpec states).
  Branch lists encode as lists (bnil = nil, bcons = cons); the pseudo-type
  tBrs, which no paper derivation contains, takes label 11 (:isType), which
  the expression encoding uses nowhere else; until 2026-10-05 it took 100,
  outside L (enclabels.clj proves every encoding label is below 97).

  - E5: ⌜A ⊸ 0⌝ = sn 25 ⌜A⌝ ⌜0⌝, by computation.
  - E1: encE is injective (encE_inj) — on all expressions, not only closed
    types — through a decoder with decT (encE e) = TUP e (roundtrip).
  - base_enc: the base-type codes of the rules are the encodings.
  - checkspec_sat: CheckSpec holds of the checker that accepts nothing, with
    this encoding.  The remaining obligation of F7 is a checker that accepts
    the certificates and satisfies CheckSpec's first clause (Lemmas 2.6–2.7)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.outer]))

;; --- the encoding ⌜·⌝ (lcert.encode's table, with the labels' positions in
;; lcert.syntax/encoding-labels as numbers) -----------------------------------------

(a/defn uLbl [r :- U, l0 :- Nat] Nat (match r [u0 l0] [u1 (+ l0 1)] [uw (+ l0 2)]))
(a/defn encNat [i :- Nat] Code (match i [zero (Code.sl 2)] [(succ j) (Code.sn 3 (encNat j) (Code.sl 0))]))
;; encE e: ⌜e⌝, by Exp.rec (one clause per constructor, as lcert.encode/enc-exp).
(kdef encE (=> Exp Code)
  (fn [e :- Exp]
    (Exp.rec$1 (fn [_ :- Exp] Code)
      (Code.sl 15)
      (Code.sl 16)
      (Code.sl 17)
      (Code.sl 18)
      (Code.sl 19)
      (Code.sl 20)
      (Code.sl 21)
      (Code.sl 22)
      (fn [b :- Exp, i_b :- Code] (Code.sn 23 i_b (Code.sl 0)))
      (fn [r :- U, A :- Exp, B :- Exp, i_A :- Code, i_B :- Code] (Code.sn (uLbl r 24) i_A i_B))
      (fn [r :- U, A :- Exp, B :- Exp, i_A :- Code, i_B :- Code] (Code.sn (uLbl r 27) i_A i_B))
      (fn [i :- Nat] (Code.sn 14 (encNat i) (Code.sl 0)))
      (Code.sl 30)
      (fn [A :- Exp, t :- Exp, i_A :- Code, i_t :- Code] (Code.sn 31 i_A i_t))
      (Code.sl 32)
      (Code.sl 33)
      (fn [b :- Exp, t :- Exp, e :- Exp, i_b :- Code, i_t :- Code, i_e :- Code] (Code.sn 34 i_b (Code.sn 13 i_t i_e)))
      (fn [P :- Exp, b :- Exp, t :- Exp, e :- Exp, i_P :- Code, i_b :- Code, i_t :- Code, i_e :- Code] (Code.sn 35 i_P (Code.sn 12 i_b (Code.sn 13 i_t i_e))))
      (Code.sl 36)
      (fn [n :- Exp, i_n :- Code] (Code.sn 37 i_n (Code.sl 0)))
      (fn [P :- Exp, z :- Exp, s :- Exp, n :- Exp, i_P :- Code, i_z :- Code, i_s :- Code, i_n :- Code] (Code.sn 38 i_P (Code.sn 12 i_z (Code.sn 12 i_s i_n))))
      (fn [l :- Nat] (Code.sn 39 (Code.sl l) (Code.sl 0)))
      (fn [P :- Exp, a :- Exp, bs :- Exp, i_P :- Code, i_a :- Code, i_bs :- Code] (Code.sn 40 i_P (Code.sn 12 i_a i_bs)))
      (Code.sl 0)
      (fn [h :- Exp, t :- Exp, i_h :- Code, i_t :- Code] (Code.sn 1 i_h i_t))
      (fn [a :- Exp, i_a :- Code] (Code.sn 41 i_a (Code.sl 0)))
      (fn [a :- Exp, c1 :- Exp, c2 :- Exp, i_a :- Code, i_c1 :- Code, i_c2 :- Code] (Code.sn 42 i_a (Code.sn 12 i_c1 i_c2)))
      (fn [P :- Exp, tl :- Exp, tn :- Exp, c :- Exp, i_P :- Code, i_tl :- Code, i_tn :- Code, i_c :- Code] (Code.sn 43 i_P (Code.sn 12 i_tl (Code.sn 12 i_tn i_c))))
      (fn [a :- Exp, i_a :- Code] (Code.sn 44 i_a (Code.sl 0)))
      (fn [d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp, i_d :- Code, i_a :- Code, i_r1 :- Code, i_r2 :- Code] (Code.sn 45 i_d (Code.sn 12 i_a (Code.sn 12 i_r1 i_r2))))
      (fn [X :- Exp, g :- Exp, h :- Exp, r :- Exp, i_X :- Code, i_g :- Code, i_h :- Code, i_r :- Code] (Code.sn 46 i_X (Code.sn 12 i_g (Code.sn 12 i_h i_r))))
      (fn [r :- Exp, i_r :- Code] (Code.sn 47 i_r (Code.sl 0)))
      (fn [r :- U, A :- Exp, t :- Exp, i_A :- Code, i_t :- Code] (Code.sn (uLbl r 48) i_A i_t))
      (fn [f :- Exp, u :- Exp, i_f :- Code, i_u :- Code] (Code.sn 51 i_f i_u))
      (fn [S :- Exp, a :- Exp, b :- Exp, i_S :- Code, i_a :- Code, i_b :- Code] (Code.sn 52 i_S (Code.sn 12 i_a i_b)))
      (fn [C :- Exp, p :- Exp, t :- Exp, i_C :- Code, i_p :- Code, i_t :- Code] (Code.sn 53 i_C (Code.sn 12 i_p i_t)))
      (fn [c :- Exp, d :- Exp, i_c :- Code, i_d :- Code] (Code.sn 54 i_c i_d))
      (fn [r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp, i_r :- Code, i_s :- Code, i_c :- Code, i_e1 :- Code, i_e2 :- Code] (Code.sn 55 i_r (Code.sn 12 i_s (Code.sn 12 i_c (Code.sn 12 i_e1 i_e2)))))
      (fn [D :- Exp, r :- Exp, e :- Exp, i_D :- Code, i_r :- Code, i_e :- Code] (Code.sn 56 i_D (Code.sn 12 i_r i_e)))
      (fn [X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp, i_X :- Code, i_r :- Code, i_c :- Code, i_t1 :- Code, i_t2 :- Code] (Code.sn 57 i_X (Code.sn 12 i_r (Code.sn 12 i_c (Code.sn 13 i_t1 i_t2)))))
      (fn [P :- Exp, k :- Nat, i_P :- Code] (Code.sn 11 i_P (encNat k)))
      e)))
(thm E5 [A :- Exp] (Eq Code (encE (Exp.tPi U.u1 A Exp.tEmpty)) (Code.sn 25 (encE A) (Code.sl 15)))
  ;; ‹proof elided: lines 80–80 of the full file; kernel-checked›
  )


;; --- the decoder, for injectivity (E1) ----------------------------------------------

;; decT c reads a code four ways at once — as an expression, as an argument
;; spine (labels 12, 13), as a unary number (2, 3), and as a leaf label — by one
;; structural recursion, so each reading can use the children's.

;; the decoder's four readings of a node: as an expression, as an argument spine, as a unary number, as a leaf label
(kdef Tup Type (Prod (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat)))))
(kdef ob (forall [α Type] (forall [β Type] (=> (Option α) (=> α (Option β)) (Option β))))
  (fn [α :- Type, β :- Type, o :- (Option α), f :- (=> α (Option β))] (Option.rec$1$0 α (fn [_ :- (Option α)] (Option β)) (Option.none β) f o)))
(kdef om (forall [α Type] (forall [β Type] (=> (=> α β) (Option α) (Option β))))
  (fn [α :- Type, β :- Type, f :- (=> α β), o :- (Option α)] (ob α β o (fn [x :- α] (Option.some β (f x))))))

(a/defn ap2 [f :- (=> Exp Exp Exp), r0 :- (List Exp)] (Option Exp) (match r0 [nil (Option.none Exp)] [(cons x1 r1) (match r1 [nil (Option.none Exp)] [(cons x2 r2) (match r2 [nil (Option.some Exp (f x1 x2))] [(cons _z _zs) (Option.none Exp)])])]))
(a/defn ap3 [f :- (=> Exp Exp Exp Exp), r0 :- (List Exp)] (Option Exp) (match r0 [nil (Option.none Exp)] [(cons x1 r1) (match r1 [nil (Option.none Exp)] [(cons x2 r2) (match r2 [nil (Option.none Exp)] [(cons x3 r3) (match r3 [nil (Option.some Exp (f x1 x2 x3))] [(cons _z _zs) (Option.none Exp)])])])]))
(a/defn ap4 [f :- (=> Exp Exp Exp Exp Exp), r0 :- (List Exp)] (Option Exp) (match r0 [nil (Option.none Exp)] [(cons x1 r1) (match r1 [nil (Option.none Exp)] [(cons x2 r2) (match r2 [nil (Option.none Exp)] [(cons x3 r3) (match r3 [nil (Option.none Exp)] [(cons x4 r4) (match r4 [nil (Option.some Exp (f x1 x2 x3 x4))] [(cons _z _zs) (Option.none Exp)])])])])]))
(kdef leafE (=> Nat (Option Exp)) (fn [l :- Nat] (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Option.none Exp) (Option.some Exp Exp.bnil) (Nat.beq l 0)) (Option.some Exp Exp.zero) (Nat.beq l 36)) (Option.some Exp Exp.ff) (Nat.beq l 33)) (Option.some Exp Exp.tt) (Nat.beq l 32)) (Option.some Exp Exp.star) (Nat.beq l 30)) (Option.some Exp Exp.tR) (Nat.beq l 22)) (Option.some Exp Exp.tDia) (Nat.beq l 21)) (Option.some Exp Exp.tSyn) (Nat.beq l 20)) (Option.some Exp Exp.tLbl) (Nat.beq l 19)) (Option.some Exp Exp.tNat) (Nat.beq l 18)) (Option.some Exp Exp.tBool) (Nat.beq l 17)) (Option.some Exp Exp.tUnit) (Nat.beq l 16)) (Option.some Exp Exp.tEmpty) (Nat.beq l 15))))

(kdef nodeE (=> Nat Tup Tup (Option Exp)) (fn [l :- Nat, ta :- Tup, tb :- Tup] (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Bool.rec$1 (fn [_ :- Bool] (Option Exp)) (Option.none Exp) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Nat Exp (fn [j :- Nat] (Exp.tBrs x j)) (Prod.fst (Prod.snd (Prod.snd tb)))))) (Nat.beq l 11)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap4 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp, x4 :- Exp] (Exp.insp x0 x1 x2 x3 x4)) ls))))) (Nat.beq l 57)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.refl x0 x1 x2)) ls))))) (Nat.beq l 56)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap4 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp, x4 :- Exp] (Exp.h1 x0 x1 x2 x3 x4)) ls))))) (Nat.beq l 55)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.chk x y)) (Prod.fst tb)))) (Nat.beq l 54)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.letp x0 x1 x2)) ls))))) (Nat.beq l 53)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.pair x0 x1 x2)) ls))))) (Nat.beq l 52)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.app x y)) (Prod.fst tb)))) (Nat.beq l 51)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.lam U.uw x y)) (Prod.fst tb)))) (Nat.beq l 50)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.lam U.u1 x y)) (Prod.fst tb)))) (Nat.beq l 49)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.lam U.u0 x y)) (Prod.fst tb)))) (Nat.beq l 48)) (om Exp Exp (fn [x :- Exp] (Exp.prn x)) (Prod.fst ta)) (Nat.beq l 47)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap3 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp] (Exp.itR x0 x1 x2 x3)) ls))))) (Nat.beq l 46)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap3 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp] (Exp.node x0 x1 x2 x3)) ls))))) (Nat.beq l 45)) (om Exp Exp (fn [x :- Exp] (Exp.leaf x)) (Prod.fst ta)) (Nat.beq l 44)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap3 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp] (Exp.recS x0 x1 x2 x3)) ls))))) (Nat.beq l 43)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.snode x0 x1 x2)) ls))))) (Nat.beq l 42)) (om Exp Exp (fn [x :- Exp] (Exp.sleaf x)) (Prod.fst ta)) (Nat.beq l 41)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.bcons x y)) (Prod.fst tb)))) (Nat.beq l 1)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.caseL x0 x1 x2)) ls))))) (Nat.beq l 40)) (om Nat Exp (fn [l2 :- Nat] (Exp.lbl l2)) (Prod.snd (Prod.snd (Prod.snd ta)))) (Nat.beq l 39)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap3 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp] (Exp.recN x0 x1 x2 x3)) ls))))) (Nat.beq l 38)) (om Exp Exp (fn [x :- Exp] (Exp.succ x)) (Prod.fst ta)) (Nat.beq l 37)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap3 (fn [x1 :- Exp, x2 :- Exp, x3 :- Exp] (Exp.elimB x0 x1 x2 x3)) ls))))) (Nat.beq l 35)) (ob Exp Exp (Prod.fst ta) (fn [x0 :- Exp] (ob (List Exp) Exp (Prod.fst (Prod.snd tb)) (fn [ls :- (List Exp)] (ap2 (fn [x1 :- Exp, x2 :- Exp] (Exp.ite x0 x1 x2)) ls))))) (Nat.beq l 34)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.abort x y)) (Prod.fst tb)))) (Nat.beq l 31)) (om Nat Exp (fn [i :- Nat] (Exp.var i)) (Prod.fst (Prod.snd (Prod.snd ta)))) (Nat.beq l 14)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tSig U.uw x y)) (Prod.fst tb)))) (Nat.beq l 29)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tSig U.u1 x y)) (Prod.fst tb)))) (Nat.beq l 28)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tSig U.u0 x y)) (Prod.fst tb)))) (Nat.beq l 27)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tPi U.uw x y)) (Prod.fst tb)))) (Nat.beq l 26)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tPi U.u1 x y)) (Prod.fst tb)))) (Nat.beq l 25)) (ob Exp Exp (Prod.fst ta) (fn [x :- Exp] (om Exp Exp (fn [y :- Exp] (Exp.tPi U.u0 x y)) (Prod.fst tb)))) (Nat.beq l 24)) (om Exp Exp (fn [x :- Exp] (Exp.tT x)) (Prod.fst ta)) (Nat.beq l 23))))
(kdef decT (=> Code Tup)
  (fn [c :- Code]
    (Code.rec$1 (fn [_ :- Code] Tup)
      (fn [l :- Nat] (Prod.mk (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat))) (leafE l)
                       (Prod.mk (Option (List Exp)) (Prod (Option Nat) (Option Nat)) (om Exp (List Exp) (fn [x :- Exp] (List.cons Exp x (List.nil Exp))) (leafE l))
                         (Prod.mk (Option Nat) (Option Nat) (Bool.rec$1 (fn [_ :- Bool] (Option Nat)) (Option.none Nat) (Option.some Nat 0) (Nat.beq l 2)) (Option.some Nat l)))))
      (fn [l :- Nat, a :- Code, b :- Code, ta :- Tup, tb :- Tup]
        (Prod.mk (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat))) (nodeE l ta tb)
          (Prod.mk (Option (List Exp)) (Prod (Option Nat) (Option Nat)) (Bool.rec$1 (fn [_ :- Bool] (Option (List Exp))) (om Exp (List Exp) (fn [x :- Exp] (List.cons Exp x (List.nil Exp))) (nodeE l ta tb)) (ob Exp (List Exp) (Prod.fst ta) (fn [x :- Exp] (om (List Exp) (List Exp) (fn [r :- (List Exp)] (List.cons Exp x r)) (Prod.fst (Prod.snd tb))))) (Bool.or (Nat.beq l 12) (Nat.beq l 13)))
            (Prod.mk (Option Nat) (Option Nat) (Bool.rec$1 (fn [_ :- Bool] (Option Nat)) (Option.none Nat) (om Nat Nat (fn [i :- Nat] (Nat.succ i)) (Prod.fst (Prod.snd (Prod.snd ta)))) (Nat.beq l 3)) (Option.none Nat)))))
      c)))


;; One step of decT at a node, and the readings of an encoded expression
;; (TUP) and of an encoded number (NATT).

(kdef NODE (=> Nat Tup Tup Tup) (fn [l :- Nat, ta :- Tup, tb :- Tup] (Prod.mk (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat))) (nodeE l ta tb)
          (Prod.mk (Option (List Exp)) (Prod (Option Nat) (Option Nat)) (Bool.rec$1 (fn [_ :- Bool] (Option (List Exp))) (om Exp (List Exp) (fn [x :- Exp] (List.cons Exp x (List.nil Exp))) (nodeE l ta tb)) (ob Exp (List Exp) (Prod.fst ta) (fn [x :- Exp] (om (List Exp) (List Exp) (fn [r :- (List Exp)] (List.cons Exp x r)) (Prod.fst (Prod.snd tb))))) (Bool.or (Nat.beq l 12) (Nat.beq l 13)))
            (Prod.mk (Option Nat) (Option Nat) (Bool.rec$1 (fn [_ :- Bool] (Option Nat)) (Option.none Nat) (om Nat Nat (fn [i :- Nat] (Nat.succ i)) (Prod.fst (Prod.snd (Prod.snd ta)))) (Nat.beq l 3)) (Option.none Nat))))))
;; ‹lemma decT_sn: statement and proof elided, lines 121–121›
(a/defn lfE [e :- Exp] (Option Nat)
  (match e [tEmpty (Option.some Nat 15)] [tUnit (Option.some Nat 16)] [tBool (Option.some Nat 17)] [tNat (Option.some Nat 18)]
           [tLbl (Option.some Nat 19)] [tSyn (Option.some Nat 20)] [tDia (Option.some Nat 21)] [tR (Option.some Nat 22)]
           [star (Option.some Nat 30)] [tt (Option.some Nat 32)] [ff (Option.some Nat 33)] [zero (Option.some Nat 36)] [bnil (Option.some Nat 0)]
           [_ (Option.none Nat)]))
(kdef TUP (=> Exp Tup)
  (fn [e :- Exp] (Prod.mk (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat))) (Option.some Exp e)
    (Prod.mk (Option (List Exp)) (Prod (Option Nat) (Option Nat)) (Option.some (List Exp) (List.cons Exp e (List.nil Exp)))
      (Prod.mk (Option Nat) (Option Nat) (Option.none Nat) (lfE e))))))

(kdef NATT (=> Nat Tup)
  (fn [i :- Nat] (Prod.mk (Option Exp) (Prod (Option (List Exp)) (Prod (Option Nat) (Option Nat))) (Option.none Exp)
    (Prod.mk (Option (List Exp)) (Prod (Option Nat) (Option Nat)) (Option.none (List Exp))
      (Prod.mk (Option Nat) (Option Nat) (Option.some Nat i) (Nat.rec$1 (fn [_ :- Nat] (Option Nat)) (Option.some Nat 2) (fn [j :- Nat, _ :- (Option Nat)] (Option.none Nat)) i))))))
;; ‹lemma decT_encNat: statement and proof elided, lines 136–139›
;; ‹lemma enc_tT: statement and proof elided, lines 140–140›
;; ‹lemma enc_tPi: statement and proof elided, lines 141–141›
;; ‹lemma enc_tSig: statement and proof elided, lines 142–142›
;; ‹lemma enc_var: statement and proof elided, lines 143–143›
;; ‹lemma enc_abort: statement and proof elided, lines 144–144›
;; ‹lemma enc_ite: statement and proof elided, lines 145–145›
;; ‹lemma enc_elimB: statement and proof elided, lines 146–146›
;; ‹lemma enc_succ: statement and proof elided, lines 147–147›
;; ‹lemma enc_recN: statement and proof elided, lines 148–148›
;; ‹lemma enc_lbl: statement and proof elided, lines 149–149›
;; ‹lemma enc_caseL: statement and proof elided, lines 150–150›
;; ‹lemma enc_bcons: statement and proof elided, lines 151–151›
;; ‹lemma enc_sleaf: statement and proof elided, lines 152–152›
;; ‹lemma enc_snode: statement and proof elided, lines 153–153›
;; ‹lemma enc_recS: statement and proof elided, lines 154–154›
;; ‹lemma enc_leaf: statement and proof elided, lines 155–155›
;; ‹lemma enc_node: statement and proof elided, lines 156–156›
;; ‹lemma enc_itR: statement and proof elided, lines 157–157›
;; ‹lemma enc_prn: statement and proof elided, lines 158–158›
;; ‹lemma enc_lam: statement and proof elided, lines 159–159›
;; ‹lemma enc_app: statement and proof elided, lines 160–160›
;; ‹lemma enc_pair: statement and proof elided, lines 161–161›
;; ‹lemma enc_letp: statement and proof elided, lines 162–162›
;; ‹lemma enc_chk: statement and proof elided, lines 163–163›
;; ‹lemma enc_h1: statement and proof elided, lines 164–164›
;; ‹lemma enc_refl: statement and proof elided, lines 165–165›
;; ‹lemma enc_insp: statement and proof elided, lines 166–166›
;; ‹lemma enc_tBrs: statement and proof elided, lines 167–167›


;; The round trip, one lemma per constructor (each: unfold the encoding, step
;; decT once per node, rewrite with the children's round trips).

;; ‹lemma rtc_tT: statement and proof elided, lines 173–174›
;; ‹lemma rtc_tPi: statement and proof elided, lines 175–176›
;; ‹lemma rtc_tSig: statement and proof elided, lines 177–178›
;; ‹lemma rtc_var: statement and proof elided, lines 179–180›
;; ‹lemma rtc_abort: statement and proof elided, lines 181–182›
;; ‹lemma rtc_ite: statement and proof elided, lines 183–184›
;; ‹lemma rtc_elimB: statement and proof elided, lines 185–186›
;; ‹lemma rtc_succ: statement and proof elided, lines 187–188›
;; ‹lemma rtc_recN: statement and proof elided, lines 189–190›
;; ‹lemma rtc_caseL: statement and proof elided, lines 191–192›
;; ‹lemma rtc_bcons: statement and proof elided, lines 193–194›
;; ‹lemma rtc_sleaf: statement and proof elided, lines 195–196›
;; ‹lemma rtc_snode: statement and proof elided, lines 197–198›
;; ‹lemma rtc_recS: statement and proof elided, lines 199–200›
;; ‹lemma rtc_leaf: statement and proof elided, lines 201–202›
;; ‹lemma rtc_node: statement and proof elided, lines 203–204›
;; ‹lemma rtc_itR: statement and proof elided, lines 205–206›
;; ‹lemma rtc_prn: statement and proof elided, lines 207–208›
;; ‹lemma rtc_lam: statement and proof elided, lines 209–210›
;; ‹lemma rtc_app: statement and proof elided, lines 211–212›
;; ‹lemma rtc_pair: statement and proof elided, lines 213–214›
;; ‹lemma rtc_letp: statement and proof elided, lines 215–216›
;; ‹lemma rtc_chk: statement and proof elided, lines 217–218›
;; ‹lemma rtc_h1: statement and proof elided, lines 219–220›
;; ‹lemma rtc_refl: statement and proof elided, lines 221–222›
;; ‹lemma rtc_insp: statement and proof elided, lines 223–224›
;; ‹lemma rtc_tBrs: statement and proof elided, lines 225–226›
;; ‹lemma roundtrip: statement and proof elided, lines 227–270›
(thm encE_inj [a :- Exp, b :- Exp, h :- (Eq Code (encE a) (encE b))] (Eq Exp a b)
  ;; ‹proof elided: lines 272–276 of the full file; kernel-checked›
  )


;; --- CheckSpec's encoding clauses, and its satisfiability ------------------------------

;; base_enc: the base-type codes of the rules are their encodings.  CheckSpec
;; holds of the checker that accepts nothing with this encoding: the trust
;; base is consistent, so no theorem assuming it is vacuous for that reason.

(thm base_enc [D :- Exp] (forall [cd Exp] (=> (Eq (Option Exp) (baseCode D) (Option.some Exp cd)) (Eq (Option Code) (codeOf cd) (Option.some Code (encE D)))))
  ;; ‹proof elided: lines 286–292 of the full file; kernel-checked›
  )
;; CheckSpec is satisfiable: the checker that accepts nothing, with the formal encoding
(thm checkspec_sat [] (CheckSpec (fn [c :- Code, d :- Code] Bool.false) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) encE)
  ;; ‹proof elided: lines 295–301 of the full file; kernel-checked›
  )
