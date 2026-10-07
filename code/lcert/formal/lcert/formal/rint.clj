(ns lcert.formal.rint
  "F4 (Theorem 4.6) — R-interpretations: what the model does with
  certificates, as a parameter (R4-metatheory.md §4.5; design note
  nachlass/docs-theorem46-design.md §2).

  The standard model (carrier.clj, den_gen.clj) represents a certificate
  value by its tree, a Code: a token carries no information.  Then
  ⟦leaf a⟧ = sl ⟦a⟧, print is the identity, and itR reads a leaf's label.
  The phantom model of §4.5 needs certificate values ★ᵢ that have no internal
  node, that itR sees as a leaf, and that print sends to a real certificate
  cᵢ.  It keeps the carrier Code and changes how certificates are read:

  RInt — an R-interpretation: a triple (riLf, riPr, riPf) together with a
  proof of its laws (RLaws, below), as a Subtype of the triples RIntD:
    riLf ri : Nat → Nat    the label of the tree leaf a builds;
    riPr ri : Code → Code  print;
    riPf ri : Code → Bool  phantom-free: reflect may decode the tree.
  Carrying the laws in ri (ri_laws) means a lemma of the generic chain that
  needs them — the ι-steps of print and itR in conversion — reads them off
  its ri parameter, with no hypothesis threaded through the chain.
  riIt ri l, the label itR passes for a leaf sl l, is derived: the label of
  riPr ri (sl l) if that is a leaf, ℓ₀ = 0 otherwise.

  stdRI is the standard model: riLf the identity, riPr the identity, riPf
  always true.  Every clause of the generic model reduces to the standard
  one there.

  phRI J σ is the phantom model with J phantoms ★₀ … ★_{J−1}, the leaves
  sl 0 … sl (J−1).  leaf a builds sl (⟦a⟧ + J), so no term builds a phantom;
  print sends ★ᵢ to σ i and sl (l + J) to sl l, structurally; a tree is
  phantom-free when every leaf label is at least J.

  RLaws ri — the four facts the generic proof of the fundamental lemma uses:
    L1 riPr (sl (riLf a)) = sl a              (ι: print (leaf a) ⇝ sleaf a,
                                               and itR on a leaf, via riIt)
    L2 riPr (sn l v w) = sn l (riPr v) (riPr w)  (ι: print on a node)
    L3 riPf v → cnodes (riPr v) = cnodes v    (Lemma 2.8 on phantom-free
                                               trees: the budget descent)
    L4 lblOk (riPr (sl 0))                     (the default certificate is
                                               in V(R))
  Both instances satisfy them (stdRI_laws; phRI_laws, given that the
  phantom codes' labels lie in L, PhOk), so both are RInts: stdRI, and
  phRI J σ hσ with hσ : PhOk J σ.

  PhCons chkf encTy ri — Corollary 3.7, for trees that are not phantom-free:
  no such tree prints to an accepted refutation, and no pair one of which is
  not phantom-free prints to a contradictory pair.  The generic H₁ and
  Reflect cases use it where a phantom blocks the budget descent.  It holds
  trivially at stdRI (stdRI_phcons), so the standard Lemma 3.6 is an
  instance of the generic proof; at phRI it follows from the standard
  Corollary 3.7 (lemma46a.clj)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            ;; band_left / band_right
            [lcert.formal.skof]))

;; --- the interpretation and its fields -------------------------------------------------

;; RIntD: the data of an interpretation, (leaf label, print, phantom-free).
(kdef RIntD (Sort 1) (Prod (=> Nat Nat) (Prod (=> Code Code) (=> Code Bool))))
(kdef mkRID (=> (=> Nat Nat) (=> Code Code) (=> Code Bool) RIntD)
  (fn [lf :- (=> Nat Nat), pr :- (=> Code Code), pf :- (=> Code Bool)] (Prod.mk lf (Prod.mk pr pf))))
(kdef dLf (=> RIntD Nat Nat) (fn [d :- RIntD, l :- Nat] (Prod.fst d l)))
(kdef dPr (=> RIntD Code Code) (fn [d :- RIntD, c :- Code] (Prod.fst (Prod.snd d) c)))
(kdef dPf (=> RIntD Code Bool) (fn [d :- RIntD, c :- Code] (Prod.snd (Prod.snd d) c)))

;; --- the laws --------------------------------------------------------------------------

;; RLawsD d: L1–L4 of the module documentation, of the data d.
(kdef RLawsD (=> RIntD Prop)
  (fn [d :- RIntD]
    (And (forall [l Nat] (Eq Code (dPr d (Code.sl (dLf d l))) (Code.sl l)))
    (And (forall [l Nat] (forall [v Code] (forall [w Code] (Eq Code (dPr d (Code.sn l v w)) (Code.sn l (dPr d v) (dPr d w))))))
    (And (forall [v Code] (=> (Eq Bool (dPf d v) Bool.true) (Eq Nat (cnodes (dPr d v)) (cnodes v))))
         (Eq Bool (lblOk (dPr d (Code.sl 0))) Bool.true))))))

;; RInt: the lawful interpretations.  riLf, riPr, riPf read the data.
(kdef RInt (Sort 1) (Subtype RLawsD))
(kdef riLf (=> RInt Nat Nat) (fn [ri :- RInt, l :- Nat] (dLf (Subtype.val ri) l)))
(kdef riPr (=> RInt Code Code) (fn [ri :- RInt, c :- Code] (dPr (Subtype.val ri) c)))
(kdef riPf (=> RInt Code Bool) (fn [ri :- RInt, c :- Code] (dPf (Subtype.val ri) c)))

;; leafLbl c: c's label if c is a leaf, ℓ₀ = 0 if it is a node.
(kdef leafLbl (=> Code Nat)
  (fn [c :- Code] (Code.rec$1 (fn [_ :- Code] Nat) (fn [l :- Nat] l) (fn [l :- Nat, a :- Code, b :- Code, ia :- Nat, ib :- Nat] 0) c)))
;; The label itR passes to g for the leaf sl l.
(kdef riIt (=> RInt Nat Nat) (fn [ri :- RInt, l :- Nat] (leafLbl (riPr ri (Code.sl l)))))

;; RLaws ri: the laws of ri's data.  Every RInt has them (ri_laws); the cases
;; of the generic fundamental lemma that read them still take them as a
;; hypothesis hrl, which ri_laws discharges.
(kdef RLaws (=> RInt Prop) (fn [ri :- RInt] (RLawsD (Subtype.val ri))))
(thm ri_laws [ri :- RInt] (RLaws ri) (exact (Subtype.property ri)))

;; The laws, read off a proof of RLaws (the generic proof cites them by these
;; names rather than by projections).
(thm rl_leaf [ri :- RInt, h :- (RLaws ri), l :- Nat] (Eq Code (riPr ri (Code.sl (riLf ri l))) (Code.sl l))
  (exact ((And.left h) l)))
(thm rl_node [ri :- RInt, h :- (RLaws ri), l :- Nat, v :- Code, w :- Code]
  (Eq Code (riPr ri (Code.sn l v w)) (Code.sn l (riPr ri v) (riPr ri w)))
  (exact ((And.left (And.right h)) l v w)))
(thm rl_nodes [ri :- RInt, h :- (RLaws ri), v :- Code, hf :- (Eq Bool (riPf ri v) Bool.true)]
  (Eq Nat (cnodes (riPr ri v)) (cnodes v))
  (exact ((And.left (And.right (And.right h))) v hf)))
(thm rl_dflt [ri :- RInt, h :- (RLaws ri)] (Eq Bool (lblOk (riPr ri (Code.sl 0))) Bool.true)
  (exact (And.right (And.right (And.right h)))))

;; The laws of ri itself (ri_laws), for lemmas with no hypothesis hrl.
(thm ri_leaf [ri :- RInt, l :- Nat] (Eq Code (riPr ri (Code.sl (riLf ri l))) (Code.sl l))
  (exact (rl_leaf ri (ri_laws ri) l)))
(thm ri_node [ri :- RInt, l :- Nat, v :- Code, w :- Code]
  (Eq Code (riPr ri (Code.sn l v w)) (Code.sn l (riPr ri v) (riPr ri w)))
  (exact (rl_node ri (ri_laws ri) l v w)))

;; itR on the tree leaf a builds passes a: riIt (riLf a) = a (L1).
(thm riIt_leaf [ri :- RInt, h :- (RLaws ri), l :- Nat] (Eq Nat (riIt ri (riLf ri l)) l)
  (exact (congrArg leafLbl (rl_leaf ri h l))))

(thm ri_itleaf [ri :- RInt, l :- Nat] (Eq Nat (riIt ri (riLf ri l)) l) (exact (riIt_leaf ri (ri_laws ri) l)))

;; A leaf whose print has its labels in L passes a label in L to itR.
(thm leafLbl_lt [c :- Code] (=> (Eq Bool (lblOk c) Bool.true) (LT.lt (leafLbl c) 100))
  (cases c)
  (intro hc) (exact (Eq.mp (Nat.blt_eq l 100) hc))
  (intro hc) (exact (Nat.zero_lt_succ 99)))
(thm riIt_lt [ri :- RInt, l :- Nat, h :- (Eq Bool (lblOk (riPr ri (Code.sl l))) Bool.true)] (LT.lt (riIt ri l) 100)
  (exact (leafLbl_lt (riPr ri (Code.sl l)) h)))

;; --- the consistency side condition -------------------------------------------------------

(kdef PhCons (=> (=> Code Code Bool) (=> Exp Code) RInt Prop)
  (fn [chkf :- (=> Code Code Bool), encTy :- (=> Exp Code), ri :- RInt]
    (And (forall [v Code] (=> (Eq Bool (riPf ri v) Bool.false) (Eq Bool (chkf (riPr ri v) (encTy Exp.tEmpty)) Bool.true) False))
         (forall [v Code] (forall [w Code] (forall [d Code]
           (=> (Or (Eq Bool (riPf ri v) Bool.false) (Eq Bool (riPf ri w) Bool.false))
               (Eq Bool (chkf (riPr ri v) d) Bool.true)
               (Eq Bool (chkf (riPr ri w) (Code.sn 25 d (Code.sl 15))) Bool.true)
               False)))))))

;; --- the standard interpretation ----------------------------------------------------------

(kdef stdRID RIntD (mkRID (fn [l :- Nat] l) (fn [c :- Code] c) (fn [c :- Code] Bool.true)))
(thm stdRI_laws [] (RLawsD stdRID)
  (constructor) (intro l) (exact (Eq.refl$1 (Code.sl l)))
  (constructor) (intro l v w) (exact (Eq.refl$1 (Code.sn l v w)))
  (constructor) (intro v hv) (exact (Eq.refl$1 (cnodes v)))
  (exact (Eq.refl$1 Bool.true)))
(kdef stdRI RInt (Subtype.mk stdRID stdRI_laws))

;; At stdRI every field is the standard model's, definitionally.
(thm stdRI_pr [c :- Code] (Eq Code (riPr stdRI c) c) (rfl))
(thm stdRI_lf [l :- Nat] (Eq Nat (riLf stdRI l) l) (rfl))
(thm stdRI_pf [c :- Code] (Eq Bool (riPf stdRI c) Bool.true) (rfl))
(thm stdRI_it [l :- Nat] (Eq Nat (riIt stdRI l) l) (rfl))

;; Every tree is phantom-free at stdRI, so PhCons holds vacuously.
(thm stdRI_phcons [chkf :- (=> Code Code Bool), encTy :- (=> Exp Code)] (PhCons chkf encTy stdRI)
  (constructor)
  (intro v hv) (exact (False.elim (Bool.noConfusion hv)))
  (intro v w d hor)
  (cases hor)
  (exact (False.elim (Bool.noConfusion h)))
  (exact (False.elim (Bool.noConfusion h))))

;; --- the phantom interpretation ------------------------------------------------------------

;; phPr J σ: print with the phantoms ★ᵢ = sl i (i < J) sent to σ i, and every
;; other leaf sl l to the standard leaf sl (l − J).
(kdef phPr (=> Nat (=> Nat Code) Code Code)
  (fn [J :- Nat, sg :- (=> Nat Code), c :- Code]
    (Code.rec$1 (fn [_ :- Code] Code)
      (fn [l :- Nat] (Bool.rec$1 (fn [_ :- Bool] Code) (Code.sl (- l J)) (sg l) (Nat.blt l J)))
      (fn [l :- Nat, a :- Code, b :- Code, pa :- Code, pb :- Code] (Code.sn l pa pb))
      c)))
;; phPf J: every leaf label is at least J.
(kdef phPf (=> Nat Code Bool)
  (fn [J :- Nat, c :- Code]
    (Code.rec$1 (fn [_ :- Code] Bool)
      (fn [l :- Nat] (Nat.ble J l))
      (fn [l :- Nat, a :- Code, b :- Code, pa :- Bool, pb :- Bool] (Bool.and pa pb))
      c)))
(kdef phRID (=> Nat (=> Nat Code) RIntD)
  (fn [J :- Nat, sg :- (=> Nat Code)] (mkRID (fn [l :- Nat] (+ l J)) (phPr J sg) (phPf J))))
;; PhOk J σ: the phantom codes σ 0 … σ (J−1) have their labels in L.
(kdef PhOk (=> Nat (=> Nat Code) Prop)
  (fn [J :- Nat, sg :- (=> Nat Code)] (forall [i Nat] (=> (LT.lt i J) (Eq Bool (lblOk (sg i)) Bool.true)))))

(thm phPr_sl [J :- Nat, sg :- (=> Nat Code), l :- Nat]
  (Eq Code (phPr J sg (Code.sl l)) (Bool.rec$1 (fn [_ :- Bool] Code) (Code.sl (- l J)) (sg l) (Nat.blt l J)))
  (rfl))
(thm phPr_sn [J :- Nat, sg :- (=> Nat Code), l :- Nat, a :- Code, b :- Code]
  (Eq Code (phPr J sg (Code.sn l a b)) (Code.sn l (phPr J sg a) (phPr J sg b)))
  (rfl))
(thm phPf_sl [J :- Nat, l :- Nat] (Eq Bool (phPf J (Code.sl l)) (Nat.ble J l)) (rfl))
(thm phPf_sn [J :- Nat, l :- Nat, a :- Code, b :- Code] (Eq Bool (phPf J (Code.sn l a b)) (Bool.and (phPf J a) (phPf J b))) (rfl))

;; A Boolean that is not true is false.
(thm bool_ff_of [b :- Bool] (=> (=> (Eq Bool b Bool.true) False) (Eq Bool b Bool.false))
  (cases b) (all_goals (intro h)) (exact (False.elim (h (Eq.refl$1 Bool.true)))) (exact (Eq.refl$1 Bool.false)))

;; A leaf at or above J is printed as the standard leaf below it.
(thm blt_ff_ge [l :- Nat, J :- Nat, h :- (LE.le J l)] (Eq Bool (Nat.blt l J) Bool.false)
  (refine' (bool_ff_of (Nat.blt l J) _))
  (intro hb)
  (have hl (LT.lt l J) (Eq.mp (Nat.blt_eq l J) hb))
  (omega))
(thm phPr_std [J :- Nat, sg :- (=> Nat Code), l :- Nat, h :- (LE.le J l)] (Eq Code (phPr J sg (Code.sl l)) (Code.sl (- l J)))
  (rw [(phPr_sl J sg l) (blt_ff_ge l J h)]))
;; The phantom ★ᵢ prints as σ i.
(thm phPr_ph [J :- Nat, sg :- (=> Nat Code), i :- Nat, h :- (LT.lt i J)] (Eq Code (phPr J sg (Code.sl i)) (sg i))
  (have hb (Eq Bool (Nat.blt i J) Bool.true) (Eq.mpr (Nat.blt_eq i J) h))
  (rw [(phPr_sl J sg i) hb]))
;; ★ᵢ is not phantom-free.
(thm phPf_ph [J :- Nat, i :- Nat, h :- (LT.lt i J)] (Eq Bool (phPf J (Code.sl i)) Bool.false)
  (rw [(phPf_sl J i)])
  (refine' (bool_ff_of (Nat.ble J i) _))
  (intro hb)
  (have hl (LE.le J i) (Nat.le_of_ble_eq_true hb))
  (omega))

;; L1: the tree of leaf a, sl (a + J), prints as sl a.
(thm phRI_leaf [J :- Nat, sg :- (=> Nat Code), l :- Nat] (Eq Code (phPr J sg (Code.sl (+ l J))) (Code.sl l))
  (rw [(phPr_std J sg (+ l J) (Nat.le_add_left J l)) (Nat.add_sub_cancel l J)]))

;; L3: on a phantom-free tree print keeps the node count.
(thm phRI_nodes [J :- Nat, sg :- (=> Nat Code), v :- Code]
  (=> (Eq Bool (phPf J v) Bool.true) (Eq Nat (cnodes (phPr J sg v)) (cnodes v)))
  (induction v)
  (intro hf)
  (have hJ (LE.le J l) (Nat.le_of_ble_eq_true (Eq.trans (Eq.symm (phPf_sl J l)) hf)))
  (exact (congrArg cnodes (phPr_std J sg l hJ)))
  (intro hf)
  (have hab (Eq Bool (Bool.and (phPf J a) (phPf J b)) Bool.true) (Eq.trans (Eq.symm (phPf_sn J l a b)) hf))
  (have ha (Eq Bool (phPf J a) Bool.true) (band_left (phPf J a) (phPf J b) hab))
  (have hb (Eq Bool (phPf J b) Bool.true) (band_right (phPf J a) (phPf J b) hab))
  (rw [(phPr_sn J sg l a b)])
  (change (Eq Nat (+ 1 (+ (cnodes (phPr J sg a)) (cnodes (phPr J sg b)))) (+ 1 (+ (cnodes a) (cnodes b)))))
  (rw [(ih_a ha) (ih_b hb)]))

;; L4: sl 0 prints as σ 0 if J > 0 (★₀), and as sl 0 otherwise.  (The
;; hypothesis on σ is part of the goal, so that `cases J` rewrites it.)
(thm phRI_dflt [J :- Nat, sg :- (=> Nat Code)]
  (=> (PhOk J sg) (Eq Bool (lblOk (phPr J sg (Code.sl 0))) Bool.true))
  (cases J)
  (intro hsg) (rfl)
  (intro hsg)
  (rw [(phPr_ph (+ n 1) sg 0 (Nat.zero_lt_succ n))])
  (exact (hsg 0 (Nat.zero_lt_succ n))))

;; phRI J σ satisfies the laws, when the phantom codes' labels lie in L.
(thm phRI_laws [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg)]
  (RLawsD (phRID J sg))
  (constructor) (intro l) (exact (phRI_leaf J sg l))
  (constructor) (intro l v w) (exact (phPr_sn J sg l v w))
  (constructor) (intro v hv) (exact (phRI_nodes J sg v hv))
  (exact (phRI_dflt J sg hsg)))

;; The phantom interpretation, with its laws.
(kdef phRI (forall [J Nat] (forall [sg (=> Nat Code)] (=> (PhOk J sg) RInt)))
  (fn [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg)] (Subtype.mk (phRID J sg) (phRI_laws J sg hsg))))

;; Its fields, definitionally.
(thm phRI_pr [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), c :- Code] (Eq Code (riPr (phRI J sg hsg) c) (phPr J sg c)) (rfl))
(thm phRI_lf [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), l :- Nat] (Eq Nat (riLf (phRI J sg hsg) l) (+ l J)) (rfl))
(thm phRI_pf [J :- Nat, sg :- (=> Nat Code), hsg :- (PhOk J sg), c :- Code] (Eq Bool (riPf (phRI J sg hsg) c) (phPf J c)) (rfl))
