(ns lcert.formal.enc46w
  "F4 — Theorem 4.6's hypotheses are jointly satisfiable (R4-metatheory.md
  §4.5; design note nachlass/docs-theorem46-design.md §5).

  Theorem 4.6 (thm46, theorem46.clj) and its corollaries (cor46.clj) assume
  CheckSpec, Enc46 and large certificates (LargeCert).  Each alone is
  satisfiable (checkspec_sat, enc46_sat: the checker that accepts nothing),
  but that checker has no certificates at all.  Here one computing checker
  meets all three at once, so the theorems are not vacuous through their
  hypotheses.

  chkW46 accepts c at d iff d = ⌜1⌝ (encE tUnit, F2's encoder) and c is a
  wrapper sn 0 u w whose children u, w are U-trees: label-1 nodes over sl 0
  leaves (isU46).  decW46 decodes sn l u w to the judgment
  Θ₀ ⊢ (λ(y :₀ Nat). ⋆) N̄ :¹ 1 with N = ‖u‖ — the paper's padded
  certificate term (pad46), derivable at budget 0 (rt_pad46).

  - chkW_spec46: CheckSpec chkW46 decW46 encE;
  - enc46_W: Enc46 chkW46 decW46 sz for every sz.  No accepted code has an
    accepted proper subtree (a subtree of a U-tree is a U-tree, and a U-tree
    is not a wrapper: isU_hole46, isU_notW46), so Enc46's premise never
    holds;
  - large46: LargeCert chkW46 decW46 Exp._sizeOf_1 ⌜1⌝ — the wrapper over the
    U-chain of k + 1 nodes decodes to a term of size above k (sizeOf, as
    Lean derives it for Exp: constructor applications);
  - thm46_hyps_sat: the three together.
  - thm46_W_noterm: what Theorem 4.6 says there: for every code cb ≠ ⌜1⌝,
    no term of □1 ⊸ □cb exists at any budget (thm46 would give a
    certificate of cb, and chkW46 accepts only at ⌜1⌝).

  The witness is a toy: its certificates are not encoded derivations.  It
  shows only that the hypotheses are consistent together.  F7's first,
  padded format did not satisfy Enc46 (enc46f7.clj); its canonical format,
  whose certificates are encoded derivations, does (thm46f7.clj)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.splitting :refer :all]
            [lcert.formal.outer]
            [lcert.formal.skof]
            [lcert.formal.encode]
            [lcert.formal.check]
            [lcert.formal.section4b]
            [lcert.formal.enc46]
            [lcert.formal.theorem46]
            [lcert.formal.cor46]))

;; --- the checker and the decoder ---------------------------------------------------------

;; isU46 c: c is a U-tree — every internal node labelled 1, every leaf sl 0.
(kdef isU46 (=> Code Bool)
  (fn [c :- Code]
    (Code.rec$1 (fn [_ :- Code] Bool)
      (fn [l :- Nat] (Nat.beq l 0))
      (fn [l :- Nat, a :- Code, b :- Code, pa :- Bool, pb :- Bool] (Bool.and (Nat.beq l 1) (Bool.and pa pb)))
      c)))
;; isW46 c: c is a wrapper sn 0 u w over two U-trees.
(kdef isW46 (=> Code Bool)
  (fn [c :- Code]
    (Code.rec$1 (fn [_ :- Code] Bool)
      (fn [l :- Nat] Bool.false)
      (fn [l :- Nat, a :- Code, b :- Code, pa :- Bool, pb :- Bool] (Bool.and (Nat.beq l 0) (Bool.and (isU46 a) (isU46 b))))
      c)))
(kdef chkW46 (=> Code Code Bool)
  (fn [c :- Code, d :- Code] (Bool.and (isW46 c) (codeEq d (encE Exp.tUnit)))))

;; The numeral N̄ and the padded term (λ(y :₀ Nat). ⋆) N̄.
(kdef num46 (=> Nat Exp)
  (fn [n :- Nat] (Nat.rec$1 (fn [_ :- Nat] Exp) Exp.zero (fn [k :- Nat, e :- Exp] (Exp.succ e)) n)))
(kdef pad46 (=> Nat Exp)
  (fn [n :- Nat] (Exp.app (Exp.lam U.u0 Exp.tNat Exp.star) (num46 n))))
(kdef decW46 (=> Code (Option (Prod Nat (Prod Exp Exp))))
  (fn [c :- Code]
    (Code.rec$1 (fn [_ :- Code] (Option (Prod Nat (Prod Exp Exp))))
      (fn [l :- Nat] (Option.none (Prod Nat (Prod Exp Exp))))
      (fn [l :- Nat, a :- Code, b :- Code, pa :- (Option (Prod Nat (Prod Exp Exp))), pb :- (Option (Prod Nat (Prod Exp Exp)))]
        (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk (pad46 (cnodes a)) Exp.tUnit))))
      c)))

;; --- the decoded judgment is derivable ----------------------------------------------------

(thm tl_num46 [chkf :- (=> Code Code Bool), D :- (List Exp)] (forall [n Nat] (Tl chkf Bool.false D (num46 n) Exp.tNat))
  (intro n) (induction n)
  (exact (Tl.zConst chkf D Exp.zero Exp.tNat (Eq.refl$1 Bool.true)))
  (exact (Tl.zSucc chkf D (num46 n) ih_n)))
;; Θ₀ ⊢ (λ(y :₀ Nat). ⋆) N̄ :¹ 1 — App at ρ = 0, the result type 1[N̄/y] = 1.
(thm rt_pad46 [chkf :- (=> Code Code Bool), n :- Nat] (Rt chkf (thetaD 0) (thetaU 0) (pad46 n) Exp.tUnit)
  (exact (Rt.rApp0 chkf (thetaD 0) (thetaU 0) (Exp.lam U.u0 Exp.tNat Exp.star) (num46 n) Exp.tNat Exp.tUnit
           (Rt.rLam chkf (thetaD 0) (thetaU 0) U.u0 Exp.tNat Exp.star Exp.tUnit (Tl.fBase chkf (thetaD 0) Exp.tNat (Eq.refl$1 Bool.true))
              (Rt.rConst chkf (List.cons Exp Exp.tNat (thetaD 0)) (List.cons U U.u0 (thetaU 0)) Exp.star Exp.tUnit rfl rfl))
           (tl_num46 chkf (thetaD 0) n)
           (Tl.fBase chkf (thetaD 0) Exp.tNat (Eq.refl$1 Bool.true))
           (Tl.fBase chkf (List.cons Exp Exp.tNat (thetaD 0)) Exp.tUnit (Eq.refl$1 Bool.true)))))

;; --- CheckSpec ------------------------------------------------------------------------------

;; CheckSpec's conjunction for an accepted code, at chkW46 / decW46 / encE.
(defn- Cb [ck cv dv mm tt AA]
  (list 'And (list 'Eq '(Option (Prod Nat (Prod Exp Exp))) (list 'decW46 cv) (list 'Option.some '(Prod Nat (Prod Exp Exp)) (list 'Prod.mk mm (list 'Prod.mk tt AA))))
   (list 'And (list 'Rt ck (list 'thetaD mm) (list 'thetaU mm) tt AA)
   (list 'And (list 'Tl ck 'Bool.true '(List.nil Exp) AA 'Exp.tUnit)
   (list 'And (list 'Eq 'Bool (list 'closedTy AA) 'Bool.true)
   (list 'And (list 'Eq 'Code (list 'encE AA) dv) (list 'Nat.lt mm (list 'cnodes cv))))))))
(defn- Cex [ck cv dv] (list 'Exists (list 'fn '[mm :- Nat] (list 'Exists (list 'fn '[tt :- Exp] (list 'Exists (list 'fn '[AA :- Exp] (Cb ck cv dv 'mm 'tt 'AA))))))))
(thm pos46 [x :- Nat] (LT.lt 0 (+ 1 x)) (omega))
;; An accepted wrapper decodes to Θ₀ ⊢ (λ(y :₀ Nat). ⋆) N̄ :¹ 1, at d = ⌜1⌝.
(eval (list 'lcert.formal.base/thm 'chkW_node46 '[l :- Nat, a :- Code, b :- Code, d :- Code, h :- (Eq Bool (chkW46 (Code.sn l a b) d) Bool.true)]
  (Cex 'chkW46 '(Code.sn l a b) 'd)
  '(have hd (Eq Bool (codeEq d (encE Exp.tUnit)) Bool.true) (band_right (isW46 (Code.sn l a b)) (codeEq d (encE Exp.tUnit)) h))
  '(have ed (Eq Code d (encE Exp.tUnit)) (codeEq_sound d (encE Exp.tUnit) hd))
  '(constructor) '(exact 0) '(constructor) '(exact (pad46 (cnodes a))) '(constructor) '(exact Exp.tUnit)
  '(constructor) '(rfl)
  '(constructor) '(exact (rt_pad46 chkW46 (cnodes a)))
  '(constructor) '(exact (Tl.fBase chkW46 (List.nil Exp) Exp.tUnit (Eq.refl$1 Bool.true)))
  '(constructor) '(rfl)
  '(constructor) '(exact (Eq.symm ed))
  '(exact (pos46 (+ (cnodes a) (cnodes b))))))
;; CheckSpec: a leaf is never accepted; the base-code clause, E5 and E1 are
;; encE's (as in checkspec_sat, encode.clj).
(thm chkW_spec46 [] (CheckSpec chkW46 decW46 encE)
  (constructor)
  (intro c) (cases c)
  (intro d h) (have h2 (Eq Bool Bool.false Bool.true) h) (exact (False.elim (Bool.noConfusion h2)))
  (intro d h) (exact (chkW_node46 l a b d h))
  (constructor) (exact base_enc)
  (constructor) (intro A hA) (rfl)
  (intro A B hA hB h) (exact (encE_inj A B h)))

;; --- Enc46: no accepted code has an accepted proper subtree ----------------------------------

;; A tree plugged into a hole of a U-tree is a U-tree.
(thm isU_hole46 [sg :- (=> Nat Code), ix :- Nat] (forall [K HTree] (=> (Eq Bool (isU46 (hfill sg K)) Bool.true) (Eq Bool (holeIn ix K) Bool.true) (Eq Bool (isU46 (sg ix)) Bool.true)))
  (intro K) (induction K)
  (intro hu hh) (have h2 (Eq Bool Bool.false Bool.true) hh) (exact (False.elim (Bool.noConfusion h2)))
  (intro hu hh)
  (have e (Eq Nat ix i) (Nat.eq_of_beq_eq_true hh))
  (exact (Eq.mpr (congrArg (fn [z :- Nat] (Eq Bool (isU46 (sg z)) Bool.true)) e) hu))
  (intro hu hh)
  (have hab (Eq Bool (Bool.and (isU46 (hfill sg a)) (isU46 (hfill sg b))) Bool.true) (band_right (Nat.beq l 1) (Bool.and (isU46 (hfill sg a)) (isU46 (hfill sg b))) hu))
  (have hor (Or (Eq Bool (holeIn ix a) Bool.true) (Eq Bool (holeIn ix b) Bool.true)) (bor_true_or (holeIn ix a) (holeIn ix b) hh))
  (cases hor)
  (exact (ih_a (band_left (isU46 (hfill sg a)) (isU46 (hfill sg b)) hab) h))
  (exact (ih_b (band_right (isU46 (hfill sg a)) (isU46 (hfill sg b)) hab) h)))
;; A U-tree is not a wrapper (labels 1 and 0).
(thm isU_notW46 [c :- Code] (=> (Eq Bool (isU46 c) Bool.true) (Eq Bool (isW46 c) Bool.true) False)
  (cases c)
  (intro hu hw) (have h2 (Eq Bool Bool.false Bool.true) hw) (exact (False.elim (Bool.noConfusion h2)))
  (intro hu hw)
  (have h1 (Eq Bool (Nat.beq l 1) Bool.true) (band_left (Nat.beq l 1) (Bool.and (isU46 a) (isU46 b)) hu))
  (have h0 (Eq Bool (Nat.beq l 0) Bool.true) (band_left (Nat.beq l 0) (Bool.and (isU46 a) (isU46 b)) hw))
  (have e1 (Eq Nat l 1) (Nat.eq_of_beq_eq_true h1))
  (have e0 (Eq Nat l 0) (Nat.eq_of_beq_eq_true h0))
  (omega))
;; A holed tree with a hole has a hole index.
(thm hasHole_ex46 [K :- HTree] (=> (Eq Bool (hasHole K) Bool.true) (Exists (fn [ix :- Nat] (Eq Bool (holeIn ix K) Bool.true))))
  (induction K)
  (intro hh) (have h2 (Eq Bool Bool.false Bool.true) hh) (exact (False.elim (Bool.noConfusion h2)))
  (intro hh) (constructor) (exact i) (exact (Nat.beq_refl i))
  (intro hh)
  (have hor (Or (Eq Bool (hasHole a) Bool.true) (Eq Bool (hasHole b) Bool.true)) (bor_true_or (hasHole a) (hasHole b) hh))
  (cases hor)
  (refine' (exN _ _ (ih_a h) _)) (intro ix hx) (constructor) (exact ix) (exact (bor_left_true (holeIn ix a) (holeIn ix b) hx))
  (refine' (exN _ _ (ih_b h) _)) (intro ix hx) (constructor) (exact ix) (exact (bor_right_true (holeIn ix a) (holeIn ix b) hx)))
;; Enc46's premise fails at a node: the plugged tree at a hole would be both
;; a U-tree (isU_hole46) and accepted, a wrapper (isU_notW46).
(thm enc46W_node [sg :- (=> Nat Code), l :- Nat, a :- HTree, b :- HTree, d :- Code,
                  hacc :- (Eq Bool (chkW46 (hfill sg (HTree.hnode l a b)) d) Bool.true),
                  hh :- (Eq Bool (hasHole (HTree.hnode l a b)) Bool.true),
                  hall :- (forall [ix Nat] (=> (Eq Bool (holeIn ix (HTree.hnode l a b)) Bool.true) (Exists (fn [D :- Code] (Eq Bool (chkW46 (sg ix) D) Bool.true)))))]
  False
  (have hw (Eq Bool (isW46 (Code.sn l (hfill sg a) (hfill sg b))) Bool.true)
    (band_left (isW46 (Code.sn l (hfill sg a) (hfill sg b))) (codeEq d (encE Exp.tUnit)) hacc))
  (have hab (Eq Bool (Bool.and (isU46 (hfill sg a)) (isU46 (hfill sg b))) Bool.true)
    (band_right (Nat.beq l 0) (Bool.and (isU46 (hfill sg a)) (isU46 (hfill sg b))) hw))
  (refine' (exN _ _ (hasHole_ex46 (HTree.hnode l a b) hh) _)) (intro ix hix)
  (have hU (Eq Bool (isU46 (sg ix)) Bool.true)
    (Or.elim (bor_true_or (holeIn ix a) (holeIn ix b) hix)
      (fn [h :- (Eq Bool (holeIn ix a) Bool.true)] (isU_hole46 sg ix a (band_left (isU46 (hfill sg a)) (isU46 (hfill sg b)) hab) h))
      (fn [h :- (Eq Bool (holeIn ix b) Bool.true)] (isU_hole46 sg ix b (band_right (isU46 (hfill sg a)) (isU46 (hfill sg b)) hab) h))))
  (refine' (exT Code _ _ (hall ix hix) _)) (intro D hD)
  (exact (isU_notW46 (sg ix) hU (band_left (isW46 (sg ix)) (codeEq D (encE Exp.tUnit)) hD))))
;; Enc46 for every term measure (its premise never holds).
(thm enc46_W [sz :- (=> Exp Nat)] (Enc46 chkW46 decW46 sz)
  (intro sg K d) (cases K)
  (intro h1 h2) (have h3 (Eq Bool Bool.false Bool.true) h2) (exact (False.elim (Bool.noConfusion h3)))
  (intro h1 h2) (have h3 (Eq Bool Bool.false Bool.true) h2) (exact (False.elim (Bool.noConfusion h3)))
  (intro h1 h2 h3 h4) (exact (False.elim (enc46W_node sg l a b d h1 h3 h4))))

;; --- large certificates of 1 ----------------------------------------------------------------

;; uN46 n: the U-chain of n label-1 nodes.
(kdef uN46 (=> Nat Code) (fn [n :- Nat] (Nat.rec$1 (fn [_ :- Nat] Code) (Code.sl 0) (fn [k :- Nat, c :- Code] (Code.sn 1 (Code.sl 0) c)) n)))
(thm uN_U46 [n :- Nat] (Eq Bool (isU46 (uN46 n)) Bool.true)
  (induction n) (rfl)
  (have e (Eq Bool (isU46 (uN46 (+ n 1))) (Bool.and Bool.true (Bool.and Bool.true (isU46 (uN46 n))))) (rfl))
  (rw [e ih_n]))
(thm uN_lbl46 [n :- Nat] (Eq Bool (lblOk (uN46 n)) Bool.true)
  (induction n) (rfl)
  (have e (Eq Bool (lblOk (uN46 (+ n 1))) (Bool.and Bool.true (Bool.and Bool.true (lblOk (uN46 n))))) (rfl))
  (rw [e ih_n]))
(thm uN_nodes46 [n :- Nat] (Eq Nat (cnodes (uN46 n)) n)
  (induction n) (rfl)
  (have e (Eq Nat (cnodes (uN46 (+ n 1))) (+ 1 (+ 0 (cnodes (uN46 n))))) (rfl))
  (rw [e ih_n]) (omega))
;; The numeral N̄ has size at least N, and so has the padded term.
(thm num_size46 [n :- Nat] (LE.le n (Exp._sizeOf_1 (num46 n)))
  (induction n) (exact (Nat.zero_le (Exp._sizeOf_1 (num46 0))))
  (have e (Eq Nat (Exp._sizeOf_1 (num46 (+ n 1))) (+ 1 (Exp._sizeOf_1 (num46 n)))) (Exp.succ.sizeOf_spec (num46 n)))
  (omega))
(thm pad_size46 [n :- Nat] (LE.le n (Exp._sizeOf_1 (pad46 n)))
  (have e (Eq Nat (Exp._sizeOf_1 (pad46 n)) (+ (+ 1 (Exp._sizeOf_1 (Exp.lam U.u0 Exp.tNat Exp.star))) (Exp._sizeOf_1 (num46 n))))
    (Exp.app.sizeOf_spec (Exp.lam U.u0 Exp.tNat Exp.star) (num46 n)))
  (have h (LE.le n (Exp._sizeOf_1 (num46 n))) (num_size46 n))
  (omega))
(thm chkW_wrap46 [u :- Code, w :- Code, hu :- (Eq Bool (isU46 u) Bool.true), hw :- (Eq Bool (isU46 w) Bool.true)]
  (Eq Bool (chkW46 (Code.sn 0 u w) (encE Exp.tUnit)) Bool.true)
  (have e (Eq Bool (chkW46 (Code.sn 0 u w) (encE Exp.tUnit)) (Bool.and (Bool.and Bool.true (Bool.and (isU46 u) (isU46 w))) (codeEq (encE Exp.tUnit) (encE Exp.tUnit)))) (rfl))
  (rw [e hu hw (codeEq_refl (encE Exp.tUnit))]))
(thm lbl_wrap46 [n :- Nat] (Eq Bool (lblOk (Code.sn 0 (uN46 n) (Code.sl 0))) Bool.true)
  (have e (Eq Bool (lblOk (Code.sn 0 (uN46 n) (Code.sl 0))) (Bool.and Bool.true (Bool.and (lblOk (uN46 n)) Bool.true))) (rfl))
  (rw [e (uN_lbl46 n)]))
(def ^:private DT '(Prod Nat (Prod Exp Exp)))
;; The wrapper over uN46 (k+1) decodes to the padded term at N = k + 1.
(eval (list 'lcert.formal.base/thm 'large_dec46 '[k :- Nat, m :- Nat, u :- Exp, A' :- Exp,
                  hd :- (Eq (Option (Prod Nat (Prod Exp Exp))) (decW46 (Code.sn 0 (uN46 (+ k 1)) (Code.sl 0))) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk u A'))))]
  '(LT.lt k (Exp._sizeOf_1 u))
  (list 'have 'hd2 (list 'Eq (list 'Option DT) (list 'Option.some DT '(Prod.mk 0 (Prod.mk (pad46 (cnodes (uN46 (+ k 1)))) Exp.tUnit))) (list 'Option.some DT '(Prod.mk m (Prod.mk u A')))) 'hd)
  (list 'have 'e (list 'Eq DT '(Prod.mk 0 (Prod.mk (pad46 (cnodes (uN46 (+ k 1)))) Exp.tUnit)) '(Prod.mk m (Prod.mk u A'))) '(Option.some.inj hd2))
  (list 'have 'eu '(Eq Exp (pad46 (cnodes (uN46 (+ k 1)))) u) (list 'congrArg (list 'fn ['p :- DT] '(Prod.fst (Prod.snd p))) 'e))
  '(have hs (LE.le (+ k 1) (Exp._sizeOf_1 (pad46 (+ k 1)))) (pad_size46 (+ k 1)))
  '(have hs2 (LE.le (+ k 1) (Exp._sizeOf_1 u))
     (Eq.mp (congrArg (fn [z :- Exp] (LE.le (+ k 1) (Exp._sizeOf_1 z))) (Eq.trans (congrArg pad46 (Eq.symm (uN_nodes46 (+ k 1)))) eu)) hs))
  '(omega)))
;; 1 has certificates of every term size.
(thm large46 [] (LargeCert chkW46 decW46 Exp._sizeOf_1 (encE Exp.tUnit))
  (intro k) (constructor) (exact (Code.sn 0 (uN46 (+ k 1)) (Code.sl 0)))
  (constructor) (exact (chkW_wrap46 (uN46 (+ k 1)) (Code.sl 0) (uN_U46 (+ k 1)) (Eq.refl$1 Bool.true)))
  (constructor) (exact (lbl_wrap46 (+ k 1)))
  (intro m u A' hd) (exact (large_dec46 k m u A' hd)))

;; --- the hypotheses together, and what Theorem 4.6 says there --------------------------------

(thm thm46_hyps_sat [] (And (CheckSpec chkW46 decW46 encE) (And (Enc46 chkW46 decW46 Exp._sizeOf_1) (LargeCert chkW46 decW46 Exp._sizeOf_1 (encE Exp.tUnit))))
  (exact (And.intro chkW_spec46 (And.intro (enc46_W Exp._sizeOf_1) large46))))
;; Under chkW46, for every code cb ≠ ⌜1⌝, no term of □1 ⊸ □cb exists at any
;; budget: Theorem 4.6 (thm46_one, at a large certificate of 1) would give a
;; certificate of cb, and chkW46 accepts only at ⌜1⌝.
(thm thm46_W_noterm [k :- Nat, t :- Exp, cb :- Code, hne :- (=> (Eq Code cb (encE Exp.tUnit)) False),
                     der :- (Rt chkW46 (thetaD k) (thetaU k) t (Exp.tPi U.u1 (boxTy (codeTerm (encE Exp.tUnit))) (boxTy (codeTerm cb))))]
  False
  (refine' (exT Code _ _ (large46 k) _)) (intro c hc)
  (have hx (Exists (fn [v :- Code] (And (Eq Bool (chkW46 v cb) Bool.true) (And (Eq Bool (lblOk v) Bool.true) (LE.le (cnodes v) k)))))
    (thm46_one chkW46 decW46 encE chkW_spec46 Exp._sizeOf_1 (enc46_W Exp._sizeOf_1) (encE Exp.tUnit) cb k t c hc hne der))
  (refine' (exT Code _ _ hx _)) (intro v hv)
  (have hq (Eq Bool (codeEq cb (encE Exp.tUnit)) Bool.true) (band_right (isW46 v) (codeEq cb (encE Exp.tUnit)) (And.left hv)))
  (exact (hne (codeEq_sound cb (encE Exp.tUnit) hq))))
