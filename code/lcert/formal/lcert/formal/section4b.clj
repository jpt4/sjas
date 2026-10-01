(ns lcert.formal.section4b
  "F4 — the derivation constructions of R4-metatheory.md §4 that use conversion.

  Proposition 4.2 (per instance of D1).  A certificate is a code v — the
  tree print will return — accepted by the checker at a closed type A:
  chkf v (encTy A) = tt.  lit v rebuilds that tree from the tokens of Θ_{‖v‖},
  taken in preorder: a leaf is leaf(ℓ), and a node spends the next token and
  recurses on the left subtree, then the right.  Then
  Θ_{‖v‖} ⊢ (lit v, ⋆) :¹ □A, where
  □A = Σ(r :₁ R). T(chk′ (print r) ⌜A⌝) and ⌜A⌝ is the canonical code term
  codeTerm (encTy A), the unique sleaf/snode term with codeOf ⌜A⌝ = encTy A.

  The second component is ⋆ : 1, transported along an explicit Cv chain:
  print (lit v) ι-reduces to ⌜v⌝ (Hd.prnL / Hd.prnN), chk′ of the two
  canonical codes δ-reduces to tt (Hd.delta, using the hypothesis on chkf),
  and T(tt) reduces to 1 (Hd.tTT).  Each element is skeleton-typed at Unit
  and satisfies nbr.  Pair at usage 1 adds the two contexts.

  Labels are the calculus's Lbl constants, which constTyped accepts only
  below NL = 100.  That is lblOk (skel.clj): a code outside L is not a
  term of the axiom rule, so the statements take lblOk v and lblOk (encTy A).
  The paper's certificates are trees over L, so every certificate the paper
  talks about is covered.

  Proposition 4.10.  From a certificate of 0 ⊸ 0, H₁ yields H° at budget
  ‖v‖: the refutation certificate is H₁'s first argument, and ⋆ is the
  second because neg c⊥ = ⌜0 ⊸ 0⌝ (CheckSpec's E5 and the base-type code
  of 0) and print (lit v) converts as in Proposition 4.2.

  Proposition 4.5, per instance.  Boxed contraction □A ⊸ □A ⊗ □A at budget
  2‖v‖, by two copies of the Proposition 4.2 pair on disjoint token blocks
  (Lemma 2.1, via rt_theta_prepend / rt_theta_append), discarding the input
  — an axiom may carry its usage (the affine reading).  D3, □A ⊸ □□A, at
  budget ‖w‖ for a certificate w of □A, by the same discarding lambda
  around one Proposition 4.2 pair."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.skeletons :refer :all]
            [lcert.formal.substitution :refer :all]
            [lcert.formal.derivations :refer :all]
            [lcert.formal.skof :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.syntactic :refer :all]))

;; --- the canonical code term ⌜c⌝ -------------------------------------------------------

;; codeTerm c is the closed canonical code: sleaf / snode over label
;; constants.  codeOf reads it back (codeTerm_of).  It is the ⌜A⌝ of □A
;; when c = encTy A, and the δ-redex's arguments.
(a/defn codeTerm [c :- Code] Exp
  (match c
    [(sl l) (Exp.sleaf (Exp.lbl l))]
    [(sn l a b) (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b))]))

(thm codeTerm_sl [l :- Nat]
  (Eq Exp (codeTerm (Code.sl l)) (Exp.sleaf (Exp.lbl l)))
  (rfl))

(thm codeTerm_sn [l :- Nat, a :- Code, b :- Code]
  (Eq Exp (codeTerm (Code.sn l a b)) (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
  (rfl))

;; One step of codeOf at a labelled node.  The recursive readings stay in the
;; matches, so an induction can rewrite them one at a time.
(thm codeOf_step [l :- Nat, x :- Exp, y :- Exp]
  (Eq (Option Code) (codeOf (Exp.snode (Exp.lbl l) x y))
      (match (codeOf x)
        [none (Option.none Code)]
        [(some a) (match (codeOf y)
                    [none (Option.none Code)]
                    [(some b) (Option.some Code (Code.sn l a b))])]))
  (rfl))

;; codeOf (codeTerm c) = some c.  Unfold the node, then each child's reading.
(thm codeTerm_of [c :- Code]
  (Eq (Option Code) (codeOf (codeTerm c)) (Option.some Code c))
  (induction c)
  (rfl)
  (rw [codeTerm_sn])
  (rw [codeOf_step])
  (rw [ih_a])
  (rw [ih_b]))

;; --- lit v, preorder, with an offset ---------------------------------------------------

;; litAt v is a function of the first token's de Bruijn index, so the
;; recursion is structural on v alone (a/defn would not reduce if both
;; arguments shrank).  A leaf spends nothing.  A node spends `off`, then
;; the left subtree, then the right: ‖sn ℓ a b‖ = 1 + ‖a‖ + ‖b‖ tokens.
(a/defn litAt [v :- Code] (=> Nat Exp)
  (match v
    [(sl l) (fn [off :- Nat] (Exp.leaf (Exp.lbl l)))]
    [(sn l a b) (fn [off :- Nat]
                  (Exp.node (Exp.var off) (Exp.lbl l)
                    ((litAt a) (+ off 1))
                    ((litAt b) (+ (+ off 1) (cnodes a)))))]))

(thm litAt_sl [l :- Nat, off :- Nat]
  (Eq Exp ((litAt (Code.sl l)) off) (Exp.leaf (Exp.lbl l)))
  (rfl))

(thm litAt_sn [l :- Nat, a :- Code, b :- Code, off :- Nat]
  (Eq Exp ((litAt (Code.sn l a b)) off)
      (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
  (rfl))

;; lit v at the innermost token.  Proposition 4.2's term uses this.
(a/defn lit0 [v :- Code] Exp ((litAt v) 0))

;; --- □A and the evidence type -----------------------------------------------------------

;; □A, with ⌜A⌝ = ca a closed code term.  var 0 is the bound certificate.
(a/defn boxTy [ca :- Exp] Exp
  (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) ca))))

;; T(chk′ (print t) ca), the type ⋆ converts to.
(a/defn evTy [t :- Exp, ca :- Exp] Exp
  (Exp.tT (Exp.chk (Exp.prn t) ca)))

;; (lit v, ⋆) at □A.
(a/defn certTerm [v :- Code, ca :- Exp] Exp
  (Exp.pair (boxTy ca) (lit0 v) Exp.star))

;; --- labels below L ----------------------------------------------------------------------

;; lblOk_sl / lblOk_sn are fundamental.clj.  constTyped (ℓ) Lbl is the same
;; test, since NL = 100.
(thm nl_100 [] (Eq Nat (NL) 100) (rfl))

(thm const_lbl_eq [l :- Nat]
  (Eq Bool (constTyped (Exp.lbl l) Exp.tLbl) (Nat.blt l 100))
  (rfl))

(thm const_lbl [l :- Nat, h :- (Eq Bool (Nat.blt l 100) Bool.true)]
  (Eq Bool (constTyped (Exp.lbl l) Exp.tLbl) Bool.true)
  (exact (Eq.trans (const_lbl_eq l) h)))

;; --- ⌜c⌝ is a closed canonical code -------------------------------------------------------

;; nbr, skeleton typing, and closedness of ⌜c⌝.  print's ι-contractum and
;; δ's two arguments are this term, so the Cv chain can type it at Syn.
(thm nbr_code [c :- Code]
  (Eq Bool (nbr (codeTerm c)) Bool.true)
  (induction c)
  (rfl)
  (rw [codeTerm_sn])
  (exact (andb_intro (nbr (Exp.lbl l)) (Bool.and (nbr (codeTerm a)) (nbr (codeTerm b))) rfl
           (andb_intro (nbr (codeTerm a)) (nbr (codeTerm b)) ih_a ih_b))))

(thm skj_code [G :- (List Sk), c :- Code]
  (SkJ Bool.false G (codeTerm c) Sk.syn)
  (induction c)
  (exact (SkJ.sSleaf G (Exp.lbl l) (SkJ.sLbl G l)))
  (exact (SkJ.sSnode G (Exp.lbl l) (codeTerm a) (codeTerm b) (SkJ.sLbl G l) ih_a ih_b)))

;; ⌜c⌝ has no variables, so lift and subst1 fix it.  rw does not see
;; codeTerm under lift, so the node is unfolded by congruence.
(thm lift_code [c :- Code]
  (forall [k Nat] (forall [cut Nat] (Eq Exp (lift k cut (codeTerm c)) (codeTerm c))))
  (induction c)
  (intro k cut) (rfl)
  (intro k cut)
  (have hL (Eq Exp (lift k cut (codeTerm (Code.sn l a b))) (lift k cut (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b))))
    (congrArg (fn [v :- Exp] (lift k cut v)) (codeTerm_sn l a b)))
  (have hR (Eq Exp (codeTerm (Code.sn l a b)) (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
    (codeTerm_sn l a b))
  (have hI (Eq Exp (lift k cut (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
                  (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
    (Eq.trans (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) v (lift k cut (codeTerm b)))) (ih_a k cut))
              (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) (codeTerm a) v)) (ih_b k cut))))
  (exact (Eq.trans hL (Eq.trans hI (Eq.symm hR)))))

(thm subst_code [u :- Exp, c :- Code]
  (Eq Exp (subst1 u (codeTerm c)) (codeTerm c))
  (induction c)
  (rfl)
  (have hL (Eq Exp (subst1 u (codeTerm (Code.sn l a b))) (subst1 u (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b))))
    (congrArg (fn [v :- Exp] (subst1 u v)) (codeTerm_sn l a b)))
  (have hR (Eq Exp (codeTerm (Code.sn l a b)) (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
    (codeTerm_sn l a b))
  (have hI (Eq Exp (subst1 u (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
                  (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
    (Eq.trans (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) v (subst1 u (codeTerm b)))) ih_a)
              (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) (codeTerm a) v)) ih_b)))
  (exact (Eq.trans hL (Eq.trans hI (Eq.symm hR)))))

(thm closed_code [c :- Code]
  (Eq Bool (closedTy (codeTerm c)) Bool.true)
  (induction c)
  (rfl)
  (rw [codeTerm_sn])
  (exact (andb_intro (closedTy (Exp.lbl l)) (Bool.and (closedTy (codeTerm a)) (closedTy (codeTerm b))) rfl
           (andb_intro (closedTy (codeTerm a)) (closedTy (codeTerm b)) ih_a ih_b))))

;; --- shifting a literal's tokens ----------------------------------------------------------

;; (+ off 1) + k = (+ off k) + 1, and the same with a middle addend.
;; lit_shift's left and right subtrees land on these indices.
(thm add_rot [x :- Nat, y :- Nat] (Eq Nat (+ (+ x 1) y) (+ (+ x y) 1)) (omega))

(thm add_rot2 [x :- Nat, y :- Nat, n :- Nat]
  (Eq Nat (+ (+ (+ x 1) n) y) (+ (+ (+ x y) 1) n))
  (omega))

;; lift k 0 moves every token of litAt by k.  The node token is a variable
;; at or above the cutoff; the subtrees are the induction hypotheses.
(thm lit_shift [v :- Code]
  (forall [off Nat] (forall [k Nat] (Eq Exp (lift k 0 ((litAt v) off)) ((litAt v) (+ off k)))))
  (induction v)
  (intro off k) (rfl)
  (intro off k)
  (have hv (Eq Exp (lift k 0 (Exp.var off)) (Exp.var (+ off k))) (lift_var_above k 0 off (Nat.zero_le off)))
  (have ha (Eq Exp (lift k 0 ((litAt a) (+ off 1))) ((litAt a) (+ (+ off k) 1)))
    (Eq.trans (ih_a (+ off 1) k) (congrArg (fn [i :- Nat] ((litAt a) i)) (add_rot off k))))
  (have hb (Eq Exp (lift k 0 ((litAt b) (+ (+ off 1) (cnodes a)))) ((litAt b) (+ (+ (+ off k) 1) (cnodes a))))
    (Eq.trans (ih_b (+ (+ off 1) (cnodes a)) k) (congrArg (fn [i :- Nat] ((litAt b) i)) (add_rot2 off k (cnodes a)))))
  (have hL (Eq Exp (lift k 0 ((litAt (Code.sn l a b)) off))
             (lift k 0 (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a))))))
    (congrArg (fn [t :- Exp] (lift k 0 t)) (litAt_sn l a b off)))
  (have hN (Eq Exp (lift k 0 (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
             (Exp.node (Exp.var (+ off k)) (Exp.lbl l) ((litAt a) (+ (+ off k) 1)) ((litAt b) (+ (+ (+ off k) 1) (cnodes a)))))
    (Eq.trans (congrArg (fn [d :- Exp] (Exp.node d (Exp.lbl l) (lift k 0 ((litAt a) (+ off 1))) (lift k 0 ((litAt b) (+ (+ off 1) (cnodes a)))))) hv)
      (Eq.trans (congrArg (fn [t :- Exp] (Exp.node (Exp.var (+ off k)) (Exp.lbl l) t (lift k 0 ((litAt b) (+ (+ off 1) (cnodes a)))))) ha)
                (congrArg (fn [t :- Exp] (Exp.node (Exp.var (+ off k)) (Exp.lbl l) ((litAt a) (+ (+ off k) 1)) t)) hb))))
  (exact (Eq.trans hL (Eq.trans hN (Eq.symm (litAt_sn l a b (+ off k)))))))

;; --- a literal's variables lie in its token block ----------------------------------------

;; Nat.blt i (i + succ n) = true, so a variable at the start of a nonempty
;; block is in scope.  closedF does not reduce under rw, hence the equations.
(thm lt_shift [i :- Nat, n :- Nat] (LT.lt i (+ i (Nat.succ n))) (omega))

(thm blt_shift [i :- Nat, n :- Nat]
  (Eq Bool (Nat.blt i (+ i (Nat.succ n))) Bool.true)
  (exact (Eq.mpr (Nat.blt_eq i (+ i (Nat.succ n))) (lt_shift i n))))

(thm scope_add_l [off :- Nat, na :- Nat, nb :- Nat, extra :- Nat]
  (Eq Nat (+ (+ off 1) (+ na (+ nb extra))) (+ off (+ (+ 1 (+ na nb)) extra)))
  (omega))

(thm scope_add_r [off :- Nat, na :- Nat, nb :- Nat, extra :- Nat]
  (Eq Nat (+ (+ (+ off 1) na) (+ nb extra)) (+ off (+ (+ 1 (+ na nb)) extra)))
  (omega))

(thm closed_node [d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp, c :- Nat]
  (Eq Bool ((closedF (Exp.node d x r1 r2)) c)
      (Bool.and ((closedF d) c) (Bool.and ((closedF x) c) (Bool.and ((closedF r1) c) ((closedF r2) c)))))
  (rfl))

(thm closedF_var [i :- Nat, c :- Nat]
  (Eq Bool ((closedF (Exp.var i)) c) (Nat.blt i c))
  (rfl))

(thm closed_lbl [l :- Nat, c :- Nat]
  (Eq Bool ((closedF (Exp.lbl l)) c) Bool.true)
  (rfl))

(thm bound_succ [off :- Nat, na :- Nat, nb :- Nat, extra :- Nat]
  (Eq Nat (+ off (+ (+ 1 (+ na nb)) extra)) (+ off (Nat.succ (+ (+ na nb) extra))))
  (omega))

;; closedF (litAt v off) (off + ‖v‖ + extra) = true.  The slack `extra`
;; lets a subtree's induction hypothesis be read at the parent's bound.
(thm lit_scope [v :- Code]
  (forall [off Nat] (forall [extra Nat]
    (Eq Bool ((closedF ((litAt v) off)) (+ off (+ (cnodes v) extra))) Bool.true)))
  (induction v)
  (intro off extra) (rfl)
  (intro off extra)
  (rw [litAt_sn])
  (rw [cnodes_sn])
  (rw [closed_node])
  (have parent (Eq Nat (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)) (+ off (Nat.succ (+ (+ (cnodes a) (cnodes b)) extra))))
    (bound_succ off (cnodes a) (cnodes b) extra))
  (have hvar (Eq Bool ((closedF (Exp.var off)) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))) Bool.true)
    (Eq.trans (closedF_var off (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
      (Eq.trans (congrArg (fn [q :- Nat] (Nat.blt off q)) parent)
                (blt_shift off (+ (+ (cnodes a) (cnodes b)) extra)))))
  (have ha (Eq Bool ((closedF ((litAt a) (+ off 1))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))) Bool.true)
    (Eq.mp (congrArg (fn [q :- Nat] (Eq Bool ((closedF ((litAt a) (+ off 1))) q) Bool.true))
                     (scope_add_l off (cnodes a) (cnodes b) extra))
           (ih_a (+ off 1) (+ (cnodes b) extra))))
  (have hb (Eq Bool ((closedF ((litAt b) (+ (+ off 1) (cnodes a)))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))) Bool.true)
    (Eq.mp (congrArg (fn [q :- Nat] (Eq Bool ((closedF ((litAt b) (+ (+ off 1) (cnodes a)))) q) Bool.true))
                     (scope_add_r off (cnodes a) (cnodes b) extra))
           (ih_b (+ (+ off 1) (cnodes a)) extra)))
  (exact (andb_intro ((closedF (Exp.var off)) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
           (Bool.and ((closedF (Exp.lbl l)) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
             (Bool.and ((closedF ((litAt a) (+ off 1))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
                       ((closedF ((litAt b) (+ (+ off 1) (cnodes a)))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))))
           hvar
           (andb_intro ((closedF (Exp.lbl l)) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
             (Bool.and ((closedF ((litAt a) (+ off 1))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
                       ((closedF ((litAt b) (+ (+ off 1) (cnodes a)))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))))
             (closed_lbl l (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
             (andb_intro ((closedF ((litAt a) (+ off 1))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
                         ((closedF ((litAt b) (+ (+ off 1) (cnodes a)))) (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
                         ha hb)))))
