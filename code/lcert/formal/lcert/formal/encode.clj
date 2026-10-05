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
(thm E5 [A :- Exp] (Eq Code (encE (Exp.tPi U.u1 A Exp.tEmpty)) (Code.sn 25 (encE A) (Code.sl 15))) (rfl))


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
(thm decT_sn [l :- Nat, a :- Code, b :- Code] (Eq Tup (decT (Code.sn l a b)) (NODE l (decT a) (decT b))) (rfl))
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
(thm decT_encNat [i :- Nat] (Eq Tup (decT (encNat i)) (NATT i))
  (induction i) (rfl)
  (change (Eq Tup (decT (Code.sn 3 (encNat n) (Code.sl 0))) (NATT (Nat.succ n))))
  (rw [decT_sn]) (rw [ih_n]))
(thm enc_tT [b :- Exp] (Eq Code (encE (Exp.tT b)) (Code.sn 23 (encE b) (Code.sl 0))) (rfl))
(thm enc_tPi [r :- U, A :- Exp, B :- Exp] (Eq Code (encE (Exp.tPi r A B)) (Code.sn (uLbl r 24) (encE A) (encE B))) (rfl))
(thm enc_tSig [r :- U, A :- Exp, B :- Exp] (Eq Code (encE (Exp.tSig r A B)) (Code.sn (uLbl r 27) (encE A) (encE B))) (rfl))
(thm enc_var [i :- Nat] (Eq Code (encE (Exp.var i)) (Code.sn 14 (encNat i) (Code.sl 0))) (rfl))
(thm enc_abort [A :- Exp, t :- Exp] (Eq Code (encE (Exp.abort A t)) (Code.sn 31 (encE A) (encE t))) (rfl))
(thm enc_ite [b :- Exp, t :- Exp, e :- Exp] (Eq Code (encE (Exp.ite b t e)) (Code.sn 34 (encE b) (Code.sn 13 (encE t) (encE e)))) (rfl))
(thm enc_elimB [P :- Exp, b :- Exp, t :- Exp, e :- Exp] (Eq Code (encE (Exp.elimB P b t e)) (Code.sn 35 (encE P) (Code.sn 12 (encE b) (Code.sn 13 (encE t) (encE e))))) (rfl))
(thm enc_succ [n :- Exp] (Eq Code (encE (Exp.succ n)) (Code.sn 37 (encE n) (Code.sl 0))) (rfl))
(thm enc_recN [P :- Exp, z :- Exp, s :- Exp, n :- Exp] (Eq Code (encE (Exp.recN P z s n)) (Code.sn 38 (encE P) (Code.sn 12 (encE z) (Code.sn 12 (encE s) (encE n))))) (rfl))
(thm enc_lbl [l :- Nat] (Eq Code (encE (Exp.lbl l)) (Code.sn 39 (Code.sl l) (Code.sl 0))) (rfl))
(thm enc_caseL [P :- Exp, a :- Exp, bs :- Exp] (Eq Code (encE (Exp.caseL P a bs)) (Code.sn 40 (encE P) (Code.sn 12 (encE a) (encE bs)))) (rfl))
(thm enc_bcons [h :- Exp, t :- Exp] (Eq Code (encE (Exp.bcons h t)) (Code.sn 1 (encE h) (encE t))) (rfl))
(thm enc_sleaf [a :- Exp] (Eq Code (encE (Exp.sleaf a)) (Code.sn 41 (encE a) (Code.sl 0))) (rfl))
(thm enc_snode [a :- Exp, c1 :- Exp, c2 :- Exp] (Eq Code (encE (Exp.snode a c1 c2)) (Code.sn 42 (encE a) (Code.sn 12 (encE c1) (encE c2)))) (rfl))
(thm enc_recS [P :- Exp, tl :- Exp, tn :- Exp, c :- Exp] (Eq Code (encE (Exp.recS P tl tn c)) (Code.sn 43 (encE P) (Code.sn 12 (encE tl) (Code.sn 12 (encE tn) (encE c))))) (rfl))
(thm enc_leaf [a :- Exp] (Eq Code (encE (Exp.leaf a)) (Code.sn 44 (encE a) (Code.sl 0))) (rfl))
(thm enc_node [d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp] (Eq Code (encE (Exp.node d a r1 r2)) (Code.sn 45 (encE d) (Code.sn 12 (encE a) (Code.sn 12 (encE r1) (encE r2))))) (rfl))
(thm enc_itR [X :- Exp, g :- Exp, h :- Exp, r :- Exp] (Eq Code (encE (Exp.itR X g h r)) (Code.sn 46 (encE X) (Code.sn 12 (encE g) (Code.sn 12 (encE h) (encE r))))) (rfl))
(thm enc_prn [r :- Exp] (Eq Code (encE (Exp.prn r)) (Code.sn 47 (encE r) (Code.sl 0))) (rfl))
(thm enc_lam [r :- U, A :- Exp, t :- Exp] (Eq Code (encE (Exp.lam r A t)) (Code.sn (uLbl r 48) (encE A) (encE t))) (rfl))
(thm enc_app [f :- Exp, u :- Exp] (Eq Code (encE (Exp.app f u)) (Code.sn 51 (encE f) (encE u))) (rfl))
(thm enc_pair [S :- Exp, a :- Exp, b :- Exp] (Eq Code (encE (Exp.pair S a b)) (Code.sn 52 (encE S) (Code.sn 12 (encE a) (encE b)))) (rfl))
(thm enc_letp [C :- Exp, p :- Exp, t :- Exp] (Eq Code (encE (Exp.letp C p t)) (Code.sn 53 (encE C) (Code.sn 12 (encE p) (encE t)))) (rfl))
(thm enc_chk [c :- Exp, d :- Exp] (Eq Code (encE (Exp.chk c d)) (Code.sn 54 (encE c) (encE d))) (rfl))
(thm enc_h1 [r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp] (Eq Code (encE (Exp.h1 r s c e1 e2)) (Code.sn 55 (encE r) (Code.sn 12 (encE s) (Code.sn 12 (encE c) (Code.sn 12 (encE e1) (encE e2)))))) (rfl))
(thm enc_refl [D :- Exp, r :- Exp, e :- Exp] (Eq Code (encE (Exp.refl D r e)) (Code.sn 56 (encE D) (Code.sn 12 (encE r) (encE e)))) (rfl))
(thm enc_insp [X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp] (Eq Code (encE (Exp.insp X r c t1 t2)) (Code.sn 57 (encE X) (Code.sn 12 (encE r) (Code.sn 12 (encE c) (Code.sn 13 (encE t1) (encE t2)))))) (rfl))
(thm enc_tBrs [P :- Exp, k :- Nat] (Eq Code (encE (Exp.tBrs P k)) (Code.sn 11 (encE P) (encNat k))) (rfl))


;; The round trip, one lemma per constructor (each: unfold the encoding, step
;; decT once per node, rewrite with the children's round trips).

(thm rtc_tT [b :- Exp, ih_b :- (Eq Tup (decT (encE b)) (TUP b))] (Eq Tup (decT (encE (Exp.tT b))) (TUP (Exp.tT b)))
  (rw [(enc_tT b)]) (rw [decT_sn]) (rw [ih_b]))
(thm rtc_tPi [r :- U, A :- Exp, B :- Exp, ih_A :- (Eq Tup (decT (encE A)) (TUP A)) , ih_B :- (Eq Tup (decT (encE B)) (TUP B))] (Eq Tup (decT (encE (Exp.tPi r A B))) (TUP (Exp.tPi r A B)))
  (rw [(enc_tPi r A B)]) (rw [decT_sn]) (rw [ih_A ih_B]) (cases r) (all_goals (rfl)))
(thm rtc_tSig [r :- U, A :- Exp, B :- Exp, ih_A :- (Eq Tup (decT (encE A)) (TUP A)) , ih_B :- (Eq Tup (decT (encE B)) (TUP B))] (Eq Tup (decT (encE (Exp.tSig r A B))) (TUP (Exp.tSig r A B)))
  (rw [(enc_tSig r A B)]) (rw [decT_sn]) (rw [ih_A ih_B]) (cases r) (all_goals (rfl)))
(thm rtc_var [i :- Nat] (Eq Tup (decT (encE (Exp.var i))) (TUP (Exp.var i)))
  (rw [(enc_var i)]) (rw [decT_sn]) (rw [(decT_encNat i)]))
(thm rtc_abort [A :- Exp, t :- Exp, ih_A :- (Eq Tup (decT (encE A)) (TUP A)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t))] (Eq Tup (decT (encE (Exp.abort A t))) (TUP (Exp.abort A t)))
  (rw [(enc_abort A t)]) (rw [decT_sn]) (rw [ih_A ih_t]))
(thm rtc_ite [b :- Exp, t :- Exp, e :- Exp, ih_b :- (Eq Tup (decT (encE b)) (TUP b)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t)) , ih_e :- (Eq Tup (decT (encE e)) (TUP e))] (Eq Tup (decT (encE (Exp.ite b t e))) (TUP (Exp.ite b t e)))
  (rw [(enc_ite b t e)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_b ih_t ih_e]))
(thm rtc_elimB [P :- Exp, b :- Exp, t :- Exp, e :- Exp, ih_P :- (Eq Tup (decT (encE P)) (TUP P)) , ih_b :- (Eq Tup (decT (encE b)) (TUP b)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t)) , ih_e :- (Eq Tup (decT (encE e)) (TUP e))] (Eq Tup (decT (encE (Exp.elimB P b t e))) (TUP (Exp.elimB P b t e)))
  (rw [(enc_elimB P b t e)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_P ih_b ih_t ih_e]))
(thm rtc_succ [n :- Exp, ih_n :- (Eq Tup (decT (encE n)) (TUP n))] (Eq Tup (decT (encE (Exp.succ n))) (TUP (Exp.succ n)))
  (rw [(enc_succ n)]) (rw [decT_sn]) (rw [ih_n]))
(thm rtc_recN [P :- Exp, z :- Exp, s :- Exp, n :- Exp, ih_P :- (Eq Tup (decT (encE P)) (TUP P)) , ih_z :- (Eq Tup (decT (encE z)) (TUP z)) , ih_s :- (Eq Tup (decT (encE s)) (TUP s)) , ih_n :- (Eq Tup (decT (encE n)) (TUP n))] (Eq Tup (decT (encE (Exp.recN P z s n))) (TUP (Exp.recN P z s n)))
  (rw [(enc_recN P z s n)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_P ih_z ih_s ih_n]))
(thm rtc_caseL [P :- Exp, a :- Exp, bs :- Exp, ih_P :- (Eq Tup (decT (encE P)) (TUP P)) , ih_a :- (Eq Tup (decT (encE a)) (TUP a)) , ih_bs :- (Eq Tup (decT (encE bs)) (TUP bs))] (Eq Tup (decT (encE (Exp.caseL P a bs))) (TUP (Exp.caseL P a bs)))
  (rw [(enc_caseL P a bs)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_P ih_a ih_bs]))
(thm rtc_bcons [h :- Exp, t :- Exp, ih_h :- (Eq Tup (decT (encE h)) (TUP h)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t))] (Eq Tup (decT (encE (Exp.bcons h t))) (TUP (Exp.bcons h t)))
  (rw [(enc_bcons h t)]) (rw [decT_sn]) (rw [ih_h ih_t]))
(thm rtc_sleaf [a :- Exp, ih_a :- (Eq Tup (decT (encE a)) (TUP a))] (Eq Tup (decT (encE (Exp.sleaf a))) (TUP (Exp.sleaf a)))
  (rw [(enc_sleaf a)]) (rw [decT_sn]) (rw [ih_a]))
(thm rtc_snode [a :- Exp, c1 :- Exp, c2 :- Exp, ih_a :- (Eq Tup (decT (encE a)) (TUP a)) , ih_c1 :- (Eq Tup (decT (encE c1)) (TUP c1)) , ih_c2 :- (Eq Tup (decT (encE c2)) (TUP c2))] (Eq Tup (decT (encE (Exp.snode a c1 c2))) (TUP (Exp.snode a c1 c2)))
  (rw [(enc_snode a c1 c2)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_a ih_c1 ih_c2]))
(thm rtc_recS [P :- Exp, tl :- Exp, tn :- Exp, c :- Exp, ih_P :- (Eq Tup (decT (encE P)) (TUP P)) , ih_tl :- (Eq Tup (decT (encE tl)) (TUP tl)) , ih_tn :- (Eq Tup (decT (encE tn)) (TUP tn)) , ih_c :- (Eq Tup (decT (encE c)) (TUP c))] (Eq Tup (decT (encE (Exp.recS P tl tn c))) (TUP (Exp.recS P tl tn c)))
  (rw [(enc_recS P tl tn c)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_P ih_tl ih_tn ih_c]))
(thm rtc_leaf [a :- Exp, ih_a :- (Eq Tup (decT (encE a)) (TUP a))] (Eq Tup (decT (encE (Exp.leaf a))) (TUP (Exp.leaf a)))
  (rw [(enc_leaf a)]) (rw [decT_sn]) (rw [ih_a]))
(thm rtc_node [d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp, ih_d :- (Eq Tup (decT (encE d)) (TUP d)) , ih_a :- (Eq Tup (decT (encE a)) (TUP a)) , ih_r1 :- (Eq Tup (decT (encE r1)) (TUP r1)) , ih_r2 :- (Eq Tup (decT (encE r2)) (TUP r2))] (Eq Tup (decT (encE (Exp.node d a r1 r2))) (TUP (Exp.node d a r1 r2)))
  (rw [(enc_node d a r1 r2)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_d ih_a ih_r1 ih_r2]))
(thm rtc_itR [X :- Exp, g :- Exp, h :- Exp, r :- Exp, ih_X :- (Eq Tup (decT (encE X)) (TUP X)) , ih_g :- (Eq Tup (decT (encE g)) (TUP g)) , ih_h :- (Eq Tup (decT (encE h)) (TUP h)) , ih_r :- (Eq Tup (decT (encE r)) (TUP r))] (Eq Tup (decT (encE (Exp.itR X g h r))) (TUP (Exp.itR X g h r)))
  (rw [(enc_itR X g h r)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_X ih_g ih_h ih_r]))
(thm rtc_prn [r :- Exp, ih_r :- (Eq Tup (decT (encE r)) (TUP r))] (Eq Tup (decT (encE (Exp.prn r))) (TUP (Exp.prn r)))
  (rw [(enc_prn r)]) (rw [decT_sn]) (rw [ih_r]))
(thm rtc_lam [r :- U, A :- Exp, t :- Exp, ih_A :- (Eq Tup (decT (encE A)) (TUP A)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t))] (Eq Tup (decT (encE (Exp.lam r A t))) (TUP (Exp.lam r A t)))
  (rw [(enc_lam r A t)]) (rw [decT_sn]) (rw [ih_A ih_t]) (cases r) (all_goals (rfl)))
(thm rtc_app [f :- Exp, u :- Exp, ih_f :- (Eq Tup (decT (encE f)) (TUP f)) , ih_u :- (Eq Tup (decT (encE u)) (TUP u))] (Eq Tup (decT (encE (Exp.app f u))) (TUP (Exp.app f u)))
  (rw [(enc_app f u)]) (rw [decT_sn]) (rw [ih_f ih_u]))
(thm rtc_pair [S :- Exp, a :- Exp, b :- Exp, ih_S :- (Eq Tup (decT (encE S)) (TUP S)) , ih_a :- (Eq Tup (decT (encE a)) (TUP a)) , ih_b :- (Eq Tup (decT (encE b)) (TUP b))] (Eq Tup (decT (encE (Exp.pair S a b))) (TUP (Exp.pair S a b)))
  (rw [(enc_pair S a b)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_S ih_a ih_b]))
(thm rtc_letp [C :- Exp, p :- Exp, t :- Exp, ih_C :- (Eq Tup (decT (encE C)) (TUP C)) , ih_p :- (Eq Tup (decT (encE p)) (TUP p)) , ih_t :- (Eq Tup (decT (encE t)) (TUP t))] (Eq Tup (decT (encE (Exp.letp C p t))) (TUP (Exp.letp C p t)))
  (rw [(enc_letp C p t)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_C ih_p ih_t]))
(thm rtc_chk [c :- Exp, d :- Exp, ih_c :- (Eq Tup (decT (encE c)) (TUP c)) , ih_d :- (Eq Tup (decT (encE d)) (TUP d))] (Eq Tup (decT (encE (Exp.chk c d))) (TUP (Exp.chk c d)))
  (rw [(enc_chk c d)]) (rw [decT_sn]) (rw [ih_c ih_d]))
(thm rtc_h1 [r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp, ih_r :- (Eq Tup (decT (encE r)) (TUP r)) , ih_s :- (Eq Tup (decT (encE s)) (TUP s)) , ih_c :- (Eq Tup (decT (encE c)) (TUP c)) , ih_e1 :- (Eq Tup (decT (encE e1)) (TUP e1)) , ih_e2 :- (Eq Tup (decT (encE e2)) (TUP e2))] (Eq Tup (decT (encE (Exp.h1 r s c e1 e2))) (TUP (Exp.h1 r s c e1 e2)))
  (rw [(enc_h1 r s c e1 e2)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_r ih_s ih_c ih_e1 ih_e2]))
(thm rtc_refl [D :- Exp, r :- Exp, e :- Exp, ih_D :- (Eq Tup (decT (encE D)) (TUP D)) , ih_r :- (Eq Tup (decT (encE r)) (TUP r)) , ih_e :- (Eq Tup (decT (encE e)) (TUP e))] (Eq Tup (decT (encE (Exp.refl D r e))) (TUP (Exp.refl D r e)))
  (rw [(enc_refl D r e)]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_D ih_r ih_e]))
(thm rtc_insp [X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp, ih_X :- (Eq Tup (decT (encE X)) (TUP X)) , ih_r :- (Eq Tup (decT (encE r)) (TUP r)) , ih_c :- (Eq Tup (decT (encE c)) (TUP c)) , ih_t1 :- (Eq Tup (decT (encE t1)) (TUP t1)) , ih_t2 :- (Eq Tup (decT (encE t2)) (TUP t2))] (Eq Tup (decT (encE (Exp.insp X r c t1 t2))) (TUP (Exp.insp X r c t1 t2)))
  (rw [(enc_insp X r c t1 t2)]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [decT_sn]) (rw [ih_X ih_r ih_c ih_t1 ih_t2]))
(thm rtc_tBrs [P :- Exp, k :- Nat, ih_P :- (Eq Tup (decT (encE P)) (TUP P))] (Eq Tup (decT (encE (Exp.tBrs P k))) (TUP (Exp.tBrs P k)))
  (rw [(enc_tBrs P k)]) (rw [decT_sn]) (rw [ih_P (decT_encNat k)]))
(thm roundtrip [] (forall [e Exp] (Eq Tup (decT (encE e)) (TUP e)))
  (intro e)
  (induction e)
  (rfl)
  (rfl)
  (rfl)
  (rfl)
  (rfl)
  (rfl)
  (rfl)
  (rfl)
  (refine' (rtc_tT _ ih_b))
  (refine' (rtc_tPi _ _ _ ih_A ih_B))
  (refine' (rtc_tSig _ _ _ ih_A ih_B))
  (refine' (rtc_var _ ))
  (rfl)
  (refine' (rtc_abort _ _ ih_A ih_t))
  (rfl)
  (rfl)
  (refine' (rtc_ite _ _ _ ih_b ih_t ih_e))
  (refine' (rtc_elimB _ _ _ _ ih_P ih_b ih_t ih_e))
  (rfl)
  (refine' (rtc_succ _ ih_n))
  (refine' (rtc_recN _ _ _ _ ih_P ih_z ih_s ih_n))
  (rfl)
  (refine' (rtc_caseL _ _ _ ih_P ih_a ih_bs))
  (rfl)
  (refine' (rtc_bcons _ _ ih_h ih_t))
  (refine' (rtc_sleaf _ ih_a))
  (refine' (rtc_snode _ _ _ ih_a ih_c1 ih_c2))
  (refine' (rtc_recS _ _ _ _ ih_P ih_tl ih_tn ih_c))
  (refine' (rtc_leaf _ ih_a))
  (refine' (rtc_node _ _ _ _ ih_d ih_a ih_r1 ih_r2))
  (refine' (rtc_itR _ _ _ _ ih_X ih_g ih_h ih_r))
  (refine' (rtc_prn _ ih_r))
  (refine' (rtc_lam _ _ _ ih_A ih_t))
  (refine' (rtc_app _ _ ih_f ih_u))
  (refine' (rtc_pair _ _ _ ih_S ih_a ih_b))
  (refine' (rtc_letp _ _ _ ih_C ih_p ih_t))
  (refine' (rtc_chk _ _ ih_c ih_d))
  (refine' (rtc_h1 _ _ _ _ _ ih_r ih_s ih_c ih_e1 ih_e2))
  (refine' (rtc_refl _ _ _ ih_D ih_r ih_e))
  (refine' (rtc_insp _ _ _ _ _ ih_X ih_r ih_c ih_t1 ih_t2))
  (refine' (rtc_tBrs _ _ ih_P)))
(thm encE_inj [a :- Exp, b :- Exp, h :- (Eq Code (encE a) (encE b))] (Eq Exp a b)
  (have ea (Eq (Option Exp) (Prod.fst (decT (encE a))) (Option.some Exp a)) (congrArg (fn [t :- Tup] (Prod.fst t)) (roundtrip a)))
  (have eb (Eq (Option Exp) (Prod.fst (decT (encE b))) (Option.some Exp b)) (congrArg (fn [t :- Tup] (Prod.fst t)) (roundtrip b)))
  (have e2 (Eq (Option Exp) (Option.some Exp a) (Option.some Exp b))
    (Eq.trans (Eq.symm ea) (Eq.trans (congrArg (fn [c :- Code] (Prod.fst (decT c))) h) eb)))
  (exact (some_inj a b e2)))


;; --- CheckSpec's encoding clauses, and its satisfiability ------------------------------

;; base_enc: the base-type codes of the rules are their encodings.  CheckSpec
;; holds of the checker that accepts nothing with this encoding: the trust
;; base is consistent, so no theorem assuming it is vacuous for that reason.

(thm base_enc [D :- Exp] (forall [cd Exp] (=> (Eq (Option Exp) (baseCode D) (Option.some Exp cd)) (Eq (Option Code) (codeOf cd) (Option.some Code (encE D)))))
  (intro cd hb)
  (have e (Eq Exp cd (cLeaf (leafOf D))) (base_leaf D cd hb))
  (rw [e])
  (revert hb)
  (cases D)
  (all_goals (intro hb))
  (all_goals (first (rfl) (exact (False.elim$0 (none_ne_someE cd hb))))))
;; CheckSpec is satisfiable: the checker that accepts nothing, with the formal encoding
(thm checkspec_sat [] (CheckSpec (fn [c :- Code, d :- Code] Bool.false) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) encE)
  (constructor)
  (intro c d h) (exact (Bool.noConfusion h))
  (constructor)
  (exact base_enc)
  (constructor)
  (intro A hA) (rfl)
  (intro A B hA hB h) (exact (encE_inj A B h)))
