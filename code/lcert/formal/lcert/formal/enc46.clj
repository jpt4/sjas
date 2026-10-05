(ns lcert.formal.enc46
  "F4 (Theorem 4.6) — the one encoding fact Theorem 4.6 needs, as an explicit
  hypothesis on the checker and decoder (R4-metatheory.md §4.5, case 3 of the
  proof; design note nachlass/docs-theorem46-design.md §5).

  The proof of Theorem 4.6 evaluates the program on phantom certificates ★ᵢ,
  which print as real certificates cᵢ.  The output's print is then a tree
  built from the cᵢ and from nodes the program paid for.  Case 3 of the proof
  is the one where some cᵢ sits strictly inside an accepted tree.  The paper
  argues from E6, E2, E3, E4 and two facts about the rules that the accepted
  tree then pays, in its own nodes, at least the size of one embedded
  certificate's term.  That conclusion is all the proof uses, so it is the
  hypothesis, stated here for any term measure sz.

  Holed trees.  HTree is a certificate tree whose leaves may also be holes
  hhole i.  hfill σ K plugs the tree σ i into each hole i; hnodes K counts
  K's own internal nodes (a hole has none, whatever is plugged into it).
  holeIn i K: K has the hole i; hasHole K: K has some hole; isNodeH K: K's
  root is an internal node (K is not a bare hole or leaf).

  Enc46 chkf dec sz.  Whenever the checker accepts hfill σ K (at any code d),
  K is a node with at least one hole, and every tree plugged into a hole is
  itself accepted (at some code), then some hole i of K has: every decoding
  (m, t, A) of σ i has sz t ≤ hnodes K.  In words: an accepted tree that
  embeds accepted trees strictly inside pays, in its own nodes, at least the
  term size of one of them.

  It is satisfiable (enc46_sat, by the checker that accepts nothing) and not
  trivial (enc46_nontrivial: the checker that accepts everything fails it).
  F7's concrete checker does not satisfy it: Check decCert reads a
  certificate's padding only through size tests, so an accepted certificate
  can carry another accepted certificate in its padding at no cost in its
  own nodes (design note §5; ADR-0006).

  Phantom trees are holed trees.  In the phantom interpretation phRI J σ
  (rint.clj), the leaves sl i with i < J are the phantoms ★ᵢ, and any other
  leaf sl l stands for the standard leaf sl (l − J).  toH J reads a tree that
  way: hfill σ (toH J r) = phPr J σ r (print, toH_fill), hnodes (toH J r) =
  cnodes r (toH_nodes); a tree with a phantom has a hole (toH_hasHole), and
  every hole is a phantom index below J (toH_hole_lt)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            ;; exN: ∃-elimination over Nat
            [lcert.formal.splitting]
            [lcert.formal.rint]))

;; --- holed trees ---------------------------------------------------------------------

(a/inductive HTree [] (hleaf [l Nat]) (hhole [i Nat]) (hnode [l Nat] [a HTree] [b HTree]))

;; hfill σ K: K with σ i plugged into each hole i.
(kdef hfill (=> (=> Nat Code) HTree Code)
  (fn [sg :- (=> Nat Code), K :- HTree]
    (HTree.rec$1 (fn [_ :- HTree] Code)
      (fn [l :- Nat] (Code.sl l))
      (fn [i :- Nat] (sg i))
      (fn [l :- Nat, a :- HTree, b :- HTree, fa :- Code, fb :- Code] (Code.sn l fa fb))
      K)))
;; hnodes K: K's own internal nodes (as cnodes: 1 + left + right at a node).
(kdef hnodes (=> HTree Nat)
  (fn [K :- HTree]
    (HTree.rec$1 (fn [_ :- HTree] Nat)
      (fn [l :- Nat] 0)
      (fn [i :- Nat] 0)
      (fn [l :- Nat, a :- HTree, b :- HTree, na :- Nat, nb :- Nat] (+ 1 (+ na nb)))
      K)))
;; holeIn i K: K has the hole i.
(kdef holeIn (=> Nat HTree Bool)
  (fn [i :- Nat, K :- HTree]
    (HTree.rec$1 (fn [_ :- HTree] Bool)
      (fn [l :- Nat] Bool.false)
      (fn [j :- Nat] (Nat.beq i j))
      (fn [l :- Nat, a :- HTree, b :- HTree, pa :- Bool, pb :- Bool] (Bool.or pa pb))
      K)))
;; hasHole K: K has some hole.
(kdef hasHole (=> HTree Bool)
  (fn [K :- HTree]
    (HTree.rec$1 (fn [_ :- HTree] Bool)
      (fn [l :- Nat] Bool.false)
      (fn [j :- Nat] Bool.true)
      (fn [l :- Nat, a :- HTree, b :- HTree, pa :- Bool, pb :- Bool] (Bool.or pa pb))
      K)))
;; isNodeH K: K's root is an internal node.
(kdef isNodeH (=> HTree Bool)
  (fn [K :- HTree]
    (HTree.rec$1 (fn [_ :- HTree] Bool)
      (fn [l :- Nat] Bool.false)
      (fn [j :- Nat] Bool.false)
      (fn [l :- Nat, a :- HTree, b :- HTree, pa :- Bool, pb :- Bool] Bool.true)
      K)))

;; --- the hypothesis --------------------------------------------------------------------

(kdef Enc46 (=> (=> Code Code Bool) (=> Code (Option (Prod Nat (Prod Exp Exp)))) (=> Exp Nat) Prop)
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), sz :- (=> Exp Nat)]
    (forall [sg (=> Nat Code)] (forall [K HTree] (forall [d Code]
      (=> (Eq Bool (chkf (hfill sg K) d) Bool.true)
          (Eq Bool (isNodeH K) Bool.true)
          (Eq Bool (hasHole K) Bool.true)
          (forall [i Nat] (=> (Eq Bool (holeIn i K) Bool.true) (Exists (fn [D :- Code] (Eq Bool (chkf (sg i) D) Bool.true)))))
          (Exists (fn [i :- Nat]
            (And (Eq Bool (holeIn i K) Bool.true)
                 (forall [m Nat] (forall [t Exp] (forall [A Exp]
                   (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (dec (sg i)) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
                       (LE.le (sz t) (hnodes K))))))))))))))

;; Satisfiable: the checker that accepts nothing meets it vacuously.
(thm enc46_sat [dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), sz :- (=> Exp Nat)]
  (Enc46 (fn [a :- Code, b :- Code] Bool.false) dec sz)
  (intro sg K d h)
  (exact (False.elim (Bool.noConfusion h))))

;; Not trivial: the checker that accepts everything, with a decoder that
;; always returns a term of measure 5, fails it at the node over one hole
;; (no node of its own but the root: 5 ≰ 1).
(thm five_le_one [h :- (LE.le 5 1)] False (omega))
(thm enc46_nontrivial []
  (=> (Enc46 (fn [a :- Code, b :- Code] Bool.true)
             (fn [c :- Code] (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk Exp.star Exp.tUnit))))
             (fn [t :- Exp] 5))
      False)
  (intro h)
  (have hx (Exists (fn [i :- Nat]
             (And (Eq Bool (holeIn i (HTree.hnode 0 (HTree.hhole 0) (HTree.hleaf 0))) Bool.true)
                  (forall [m Nat] (forall [t Exp] (forall [A Exp]
                    (=> (Eq (Option (Prod Nat (Prod Exp Exp))) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk Exp.star Exp.tUnit)))
                                                              (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
                        (LE.le 5 1))))))))
    (h (fn [i :- Nat] (Code.sl 0)) (HTree.hnode 0 (HTree.hhole 0) (HTree.hleaf 0)) (Code.sl 0)
       (Eq.refl$1 Bool.true) (Eq.refl$1 Bool.true) (Eq.refl$1 Bool.true)
       (fn [i :- Nat, hi :- (Eq Bool (holeIn i (HTree.hnode 0 (HTree.hhole 0) (HTree.hleaf 0))) Bool.true)] (Exists.intro$1 (Code.sl 0) (Eq.refl$1 Bool.true)))))
  (refine' (exN _ _ hx _))
  (intro i hi)
  (exact (five_le_one ((And.right hi) 0 Exp.star Exp.tUnit (Eq.refl$1 (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk 0 (Prod.mk Exp.star Exp.tUnit))))))))

;; --- phantom trees as holed trees ----------------------------------------------------------

;; toH J r: the leaves sl i with i < J are the holes (phantoms ★ᵢ); any other
;; leaf sl l is the leaf l − J; nodes are kept.
(kdef toH (=> Nat Code HTree)
  (fn [J :- Nat, c :- Code]
    (Code.rec$1 (fn [_ :- Code] HTree)
      (fn [l :- Nat] (Bool.rec$1 (fn [_ :- Bool] HTree) (HTree.hleaf (- l J)) (HTree.hhole l) (Nat.blt l J)))
      (fn [l :- Nat, a :- Code, b :- Code, ha :- HTree, hb :- HTree] (HTree.hnode l ha hb))
      c)))

;; The leaf case of each fact below, for either value of the test l < J.
(thm hfill_sel [sg :- (=> Nat Code), l :- Nat, J :- Nat, bb :- Bool]
  (Eq Code (hfill sg (Bool.rec$1 (fn [_ :- Bool] HTree) (HTree.hleaf (- l J)) (HTree.hhole l) bb))
           (Bool.rec$1 (fn [_ :- Bool] Code) (Code.sl (- l J)) (sg l) bb))
  (cases bb) (rfl) (rfl))
(thm hnodes_sel [l :- Nat, J :- Nat, bb :- Bool]
  (Eq Nat (hnodes (Bool.rec$1 (fn [_ :- Bool] HTree) (HTree.hleaf (- l J)) (HTree.hhole l) bb)) 0)
  (cases bb) (rfl) (rfl))

;; print of a phantom tree = the holed tree, filled.
(thm toH_fill [J :- Nat, sg :- (=> Nat Code), c :- Code] (Eq Code (hfill sg (toH J c)) (phPr J sg c))
  (induction c)
  (exact (hfill_sel sg l J (Nat.blt l J)))
  (have h (Eq Code (Code.sn l (hfill sg (toH J a)) (hfill sg (toH J b))) (Code.sn l (phPr J sg a) (phPr J sg b)))
    (congr (congrArg (Code.sn l) ih_a) ih_b))
  (exact h))

;; A phantom tree's own nodes are the holed tree's.
(thm toH_nodes [J :- Nat, c :- Code] (Eq Nat (hnodes (toH J c)) (cnodes c))
  (induction c)
  (exact (hnodes_sel l J (Nat.blt l J)))
  (have h (Eq Nat (+ 1 (+ (hnodes (toH J a)) (hnodes (toH J b)))) (+ 1 (+ (cnodes a) (cnodes b))))
    (congrArg (fn [q :- Nat] (+ 1 q)) (congr (congrArg HAdd.hAdd ih_a) ih_b)))
  (exact h))

;; A tree that is not phantom-free has a hole.
(thm bool_and_ff [x :- Bool, y :- Bool] (=> (Eq Bool (Bool.and x y) Bool.false) (Or (Eq Bool x Bool.false) (Eq Bool y Bool.false)))
  (cases x)
  (intro h) (exact (Or.inl (Eq.refl$1 Bool.false)))
  (intro h) (exact (Or.inr h)))
(thm bor_left_true [x :- Bool, y :- Bool, h :- (Eq Bool x Bool.true)] (Eq Bool (Bool.or x y) Bool.true) (rw [h]) (rfl))
(thm bor_right_true [x :- Bool, y :- Bool, h :- (Eq Bool y Bool.true)] (Eq Bool (Bool.or x y) Bool.true) (cases x) (exact h) (rfl))
(thm hasHole_sel [l :- Nat, J :- Nat, bb :- Bool]
  (=> (Eq Bool (Nat.ble J l) Bool.false) (Eq Bool (Nat.blt l J) bb)
      (Eq Bool (hasHole (Bool.rec$1 (fn [_ :- Bool] HTree) (HTree.hleaf (- l J)) (HTree.hhole l) bb)) Bool.true))
  (cases bb)
  (intro h1 h2)
  (have hlt (LT.lt l J) (Nat.gt_of_not_le (fn [hle :- (LE.le J l)] (Bool.noConfusion (Eq.trans (Eq.symm (Nat.ble_eq_true_of_le hle)) h1)))))
  (exact (False.elim (Bool.noConfusion (Eq.trans (Eq.symm (Eq.mpr (Nat.blt_eq l J) hlt)) h2))))
  (intro h1 h2) (rfl))
(thm toH_hasHole [J :- Nat, c :- Code] (=> (Eq Bool (phPf J c) Bool.false) (Eq Bool (hasHole (toH J c)) Bool.true))
  (induction c)
  (intro hf)
  (exact (hasHole_sel l J (Nat.blt l J) hf (Eq.refl$1 (Nat.blt l J))))
  (intro hf)
  (have hab (Or (Eq Bool (phPf J a) Bool.false) (Eq Bool (phPf J b) Bool.false)) (bool_and_ff (phPf J a) (phPf J b) hf))
  (cases hab)
  (exact (bor_left_true (hasHole (toH J a)) (hasHole (toH J b)) (ih_a h)))
  (exact (bor_right_true (hasHole (toH J a)) (hasHole (toH J b)) (ih_b h))))

;; Every hole of a phantom tree is a phantom index below J.
(thm bor_true_or [x :- Bool, y :- Bool] (=> (Eq Bool (Bool.or x y) Bool.true) (Or (Eq Bool x Bool.true) (Eq Bool y Bool.true)))
  (cases x)
  (intro h) (exact (Or.inr h))
  (intro h) (exact (Or.inl (Eq.refl$1 Bool.true))))
(thm holeIn_sel [i :- Nat, l :- Nat, J :- Nat, bb :- Bool]
  (=> (Eq Bool (Nat.blt l J) bb)
      (Eq Bool (holeIn i (Bool.rec$1 (fn [_ :- Bool] HTree) (HTree.hleaf (- l J)) (HTree.hhole l) bb)) Bool.true)
      (LT.lt i J))
  (cases bb)
  (intro h1 h2) (exact (False.elim (Bool.noConfusion h2)))
  (intro h1 h2)
  (have he (Eq Nat i l) (Nat.eq_of_beq_eq_true h2))
  (have hl (LT.lt l J) (Eq.mp (Nat.blt_eq l J) h1))
  (rw [he]) (exact hl))
(thm toH_hole_lt [J :- Nat, i :- Nat, c :- Code] (=> (Eq Bool (holeIn i (toH J c)) Bool.true) (LT.lt i J))
  (induction c)
  (intro hi)
  (exact (holeIn_sel i l J (Nat.blt l J) (Eq.refl$1 (Nat.blt l J)) hi))
  (intro hi)
  (have hab (Or (Eq Bool (holeIn i (toH J a)) Bool.true) (Eq Bool (holeIn i (toH J b)) Bool.true))
    (bor_true_or (holeIn i (toH J a)) (holeIn i (toH J b)) hi))
  (cases hab)
  (exact (ih_a h))
  (exact (ih_b h)))

;; A node's holed tree is a node.
(thm toH_node [J :- Nat, l :- Nat, a :- Code, b :- Code] (Eq Bool (isNodeH (toH J (Code.sn l a b))) Bool.true) (rfl))
