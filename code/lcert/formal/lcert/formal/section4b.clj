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
                         ha hb))))

;; --- nbr and skeleton typing of a literal ------------------------------------------------

(thm nbr_node [d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp]
  (Eq Bool (nbr (Exp.node d x r1 r2))
      (Bool.and (nbr d) (Bool.and (nbr x) (Bool.and (nbr r1) (nbr r2)))))
  (rfl))

(thm nbr_leaf [x :- Exp] (Eq Bool (nbr (Exp.leaf x)) (nbr x)) (rfl))
(thm nbr_var_tt [i :- Nat] (Eq Bool (nbr (Exp.var i)) Bool.true) (rfl))
(thm nbr_lbl_tt [l :- Nat] (Eq Bool (nbr (Exp.lbl l)) Bool.true) (rfl))

;; Every token of litAt is a variable, every label a constant, both of which
;; nbr accepts.  rw does not match litAt at a leaf, so that case is a
;; congruence; the node unfolds.
(thm nbr_lit [v :- Code]
  (forall [off Nat] (Eq Bool (nbr ((litAt v) off)) Bool.true))
  (induction v)
  (intro off)
  (exact (Eq.trans (congrArg nbr (litAt_sl l off)) (Eq.trans (nbr_leaf (Exp.lbl l)) (nbr_lbl_tt l))))
  (intro off)
  (rw [litAt_sn])
  (rw [nbr_node])
  (exact (andb_intro (nbr (Exp.var off))
           (Bool.and (nbr (Exp.lbl l)) (Bool.and (nbr ((litAt a) (+ off 1))) (nbr ((litAt b) (+ (+ off 1) (cnodes a))))))
           (nbr_var_tt off)
           (andb_intro (nbr (Exp.lbl l))
             (Bool.and (nbr ((litAt a) (+ off 1))) (nbr ((litAt b) (+ (+ off 1) (cnodes a)))))
             (nbr_lbl_tt l)
             (andb_intro (nbr ((litAt a) (+ off 1))) (nbr ((litAt b) (+ (+ off 1) (cnodes a))))
               (ih_a (+ off 1)) (ih_b (+ (+ off 1) (cnodes a))))))))

(thm skel_dia [] (Eq Sk (skel Exp.tDia) Sk.dia) (rfl))
(thm thetaD_succ [k :- Nat]
  (Eq (List Exp) (thetaD (Nat.succ k)) (List.cons Exp Exp.tDia (thetaD k))) (rfl))
(thm skels_cons [x :- Exp, xs :- (List Exp)]
  (Eq (List Sk) (skels (List.cons Exp x xs)) (List.cons Sk (skel x) (skels xs))) (rfl))
(thm nthS_zero [s :- Sk, G :- (List Sk)]
  (Eq (Option Sk) (nthS (List.cons Sk s G) 0) (Option.some Sk s))
  (exact (nthS.eq_2 s G)))
(thm nthS_succ [s :- Sk, G :- (List Sk), j :- Nat]
  (Eq (Option Sk) (nthS (List.cons Sk s G) (Nat.succ j)) (nthS G j))
  (exact (nthS.eq_3 s G j)))

;; Θ's skeleton context is a block of ◇.  Index i is ◇ whenever at least
;; one entry sits at or past i.
(thm nthS_skels_theta [i :- Nat]
  (forall [extra Nat]
    (Eq (Option Sk) (nthS (skels (thetaD (+ i (Nat.succ extra)))) i) (Option.some Sk Sk.dia)))
  (induction i)
  (intro extra)
  (have e0 (Eq Nat (+ 0 (Nat.succ extra)) (Nat.succ extra)) (Nat.zero_add (Nat.succ extra)))
  (have hctx (Eq (List Sk) (skels (thetaD (Nat.succ extra))) (List.cons Sk Sk.dia (skels (thetaD extra))))
    (Eq.trans (congrArg skels (thetaD_succ extra))
      (Eq.trans (skels_cons Exp.tDia (thetaD extra))
                (congrArg (fn [s :- Sk] (List.cons Sk s (skels (thetaD extra)))) skel_dia))))
  (exact (Eq.trans (congrArg (fn [q :- Nat] (nthS (skels (thetaD q)) 0)) e0)
                   (Eq.trans (congrArg (fn [G :- (List Sk)] (nthS G 0)) hctx)
                             (nthS_zero Sk.dia (skels (thetaD extra))))))
  (intro extra)
  (have eadd (Eq Nat (+ (Nat.succ n) (Nat.succ extra)) (Nat.succ (+ n (Nat.succ extra))))
    (Nat.succ_add n (Nat.succ extra)))
  (have hctx (Eq (List Sk) (skels (thetaD (Nat.succ (+ n (Nat.succ extra)))))
                 (List.cons Sk Sk.dia (skels (thetaD (+ n (Nat.succ extra))))))
    (Eq.trans (congrArg skels (thetaD_succ (+ n (Nat.succ extra))))
      (Eq.trans (skels_cons Exp.tDia (thetaD (+ n (Nat.succ extra))))
                (congrArg (fn [s :- Sk] (List.cons Sk s (skels (thetaD (+ n (Nat.succ extra)))))) skel_dia))))
  (have hnth (Eq (Option Sk)
      (nthS (skels (thetaD (Nat.succ (+ n (Nat.succ extra))))) (Nat.succ n))
      (nthS (skels (thetaD (+ n (Nat.succ extra)))) n))
    (Eq.trans (congrArg (fn [G :- (List Sk)] (nthS G (Nat.succ n))) hctx)
              (nthS_succ Sk.dia (skels (thetaD (+ n (Nat.succ extra)))) n)))
  (exact (Eq.trans (congrArg (fn [q :- Nat] (nthS (skels (thetaD q)) (Nat.succ n))) eadd)
                   (Eq.trans hnth (ih_n extra)))))

;; litAt v off is a certificate in any token context long enough to hold
;; its block and `extra` unused tokens after it.  The node's token is ◇
;; at `off`; the subtrees are the same context, whose length the scope
;; equations identify with each child's own bound.
(thm skj_lit [v :- Code]
  (forall [off Nat] (forall [extra Nat]
    (SkJ Bool.false (skels (thetaD (+ off (+ (cnodes v) extra)))) ((litAt v) off) Sk.cert)))
  (induction v)
  (intro off extra)
  (exact (Eq.mpr (congrArg (fn [t :- Exp] (SkJ Bool.false (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) t Sk.cert)) (litAt_sl l off))
           (SkJ.sLeaf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) (Exp.lbl l)
             (SkJ.sLbl (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) l))))
  (intro off extra)
  (rw [litAt_sn])
  (rw [cnodes_sn])
  (have hlenL (Eq Nat (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra)))
                        (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
    (scope_add_l off (cnodes a) (cnodes b) extra))
  (have hlenR (Eq Nat (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra))
                        (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))
    (scope_add_r off (cnodes a) (cnodes b) extra))
  (have ha (SkJ Bool.false (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))))
                ((litAt a) (+ off 1)) Sk.cert)
    (Eq.mp (congrArg (fn [q :- Nat] (SkJ Bool.false (skels (thetaD q)) ((litAt a) (+ off 1)) Sk.cert)) hlenL)
           (ih_a (+ off 1) (+ (cnodes b) extra))))
  (have hb (SkJ Bool.false (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))))
                ((litAt b) (+ (+ off 1) (cnodes a))) Sk.cert)
    (Eq.mp (congrArg (fn [q :- Nat] (SkJ Bool.false (skels (thetaD q)) ((litAt b) (+ (+ off 1) (cnodes a))) Sk.cert)) hlenR)
           (ih_b (+ (+ off 1) (cnodes a)) extra)))
  (have hlook (Eq (Option Sk)
      (nthS (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))) off)
      (Option.some Sk Sk.dia))
    (Eq.trans (congrArg (fn [q :- Nat] (nthS (skels (thetaD q)) off))
                        (bound_succ off (cnodes a) (cnodes b) extra))
              (nthS_skels_theta off (+ (+ (cnodes a) (cnodes b)) extra))))
  (exact (SkJ.sNode (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra))))
           (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))
           (SkJ.sVar (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))) off Sk.dia hlook)
           (SkJ.sLbl (skels (thetaD (+ off (+ (+ 1 (+ (cnodes a) (cnodes b))) extra)))) l)
           ha hb)))

;; --- lengths, and the usage vector of a node ---------------------------------------------

(thm lit0_at [v :- Code] (Eq Exp (lit0 v) ((litAt v) 0)) (rfl))
(thm closed_tR [] (Eq Bool (closedTy Exp.tR) Bool.true) (rfl))
(thm closed_tDia [] (Eq Bool (closedTy Exp.tDia) Bool.true) (rfl))
(thm nonzero_u1 [] (Eq Bool (nonzero U.u1) Bool.true) (rfl))
(thm lenE_cons [x :- Exp, xs :- (List Exp)]
  (Eq Nat (lenE (List.cons Exp x xs)) (Nat.succ (lenE xs))) (rfl))
(thm lenU_cons [r :- U, us :- (List U)]
  (Eq Nat (lenU (List.cons U r us)) (Nat.succ (lenU us))) (rfl))
(thm lenE_theta [m :- Nat] (Eq Nat (lenE (thetaD m)) m)
  (induction m) (rfl) (exact (congrArg Nat.succ ih_n)))
(thm lenU_theta [m :- Nat] (Eq Nat (lenU (thetaU m)) m)
  (induction m) (rfl) (exact (congrArg Nat.succ ih_n)))
(thm lenU_vzero [m :- Nat] (Eq Nat (lenU (vzero m)) m)
  (induction m) (rfl) (exact (congrArg Nat.succ ih_n)))
(thm cnodes_sl [l :- Nat] (Eq Nat (cnodes (Code.sl l)) 0) (rfl))
(thm thetaD_zero [] (Eq (List Exp) (thetaD 0) (List.nil Exp)) (rfl))
(thm thetaU_zero [] (Eq (List U) (thetaU 0) (List.nil U)) (rfl))
(thm len_nilE [] (Eq Nat (lenE (List.nil Exp)) 0) (rfl))
(thm len_nilU [] (Eq Nat (lenU (List.nil U)) 0) (rfl))
(thm prefixU_zero [r :- U, us :- (List U)] (Eq (List U) (prefixU 0 r us) us) (rfl))
(thm prefixU_succ [k :- Nat, r :- U, us :- (List U)]
  (Eq (List U) (prefixU (Nat.succ k) r us) (List.cons U r (prefixU k r us))) (rfl))
(thm vzero_succ [m :- Nat] (Eq (List U) (vzero (Nat.succ m)) (List.cons U U.u0 (vzero m))) (rfl))
(thm thetaU_succ [m :- Nat] (Eq (List U) (thetaU (Nat.succ m)) (List.cons U U.u1 (thetaU m))) (rfl))
(thm insU_zero [r :- U, us :- (List U)] (Eq (List U) (insU 0 r us) (List.cons U r us)) (rfl))
(thm nthE_theta0 [m :- Nat]
  (Eq (Option Exp) (nthE (thetaD (Nat.succ m)) 0) (Option.some Exp Exp.tDia))
  (exact (Eq.trans (congrArg (fn [D :- (List Exp)] (nthE D 0)) (thetaD_succ m)) (nthE.eq_2 Exp.tDia (thetaD m)))))
(thm nthU_cons0 [r :- U, us :- (List U)]
  (Eq (Option U) (nthU (List.cons U r us) 0) (Option.some U r))
  (exact (nthU.eq_2 r us)))

;; uadd u0 u1 reduces, so a zero vector under a token block is that block.
(thm vadd_zero_theta [m :- Nat]
  (Eq (List U) (vadd (vzero m) (thetaU m)) (thetaU m))
  (induction m) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n)))

(thm vadd_zzz_theta [m :- Nat]
  (Eq (List U) (vadd (vzero m) (vadd (vzero m) (vadd (vzero m) (thetaU m)))) (thetaU m))
  (induction m) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n)))

(thm vadd_cons4 [a :- U, b :- U, c :- U, d :- U, xa :- (List U), xb :- (List U), xc :- (List U), xd :- (List U)]
  (Eq (List U)
    (vadd (List.cons U a xa) (vadd (List.cons U b xb) (vadd (List.cons U c xc) (List.cons U d xd))))
    (List.cons U (uadd a (uadd b (uadd c d))) (vadd xa (vadd xb (vadd xc xd)))))
  (rfl))

(thm uadd_token_head [] (Eq U (uadd U.u0 (uadd U.u0 (uadd U.u1 U.u0))) U.u1) (rfl))
(thm uadd_lit_head [] (Eq U (uadd U.u1 (uadd U.u0 (uadd U.u0 U.u0))) U.u1) (rfl))

;; The two subtree blocks, with the token and the label still to be added:
;; zeros everywhere, 1 on the left block, 0 then the right block's tokens.
(thm vadd_blocks [na :- Nat, nb :- Nat]
  (Eq (List U)
    (vadd (vzero (+ na nb))
      (vadd (vzero (+ na nb))
        (vadd (prefixU na U.u1 (vzero nb)) (prefixU na U.u0 (thetaU nb)))))
    (thetaU (+ na nb)))
  (induction na)
  (have ez (Eq Nat (+ 0 nb) nb) (Nat.zero_add nb))
  (have h1 (Eq (List U)
      (vadd (vzero (+ 0 nb)) (vadd (vzero (+ 0 nb)) (vadd (prefixU 0 U.u1 (vzero nb)) (prefixU 0 U.u0 (thetaU nb)))))
      (vadd (vzero nb) (vadd (vzero nb) (vadd (prefixU 0 U.u1 (vzero nb)) (prefixU 0 U.u0 (thetaU nb))))))
    (congrArg (fn [k :- Nat] (vadd (vzero k) (vadd (vzero k) (vadd (prefixU 0 U.u1 (vzero nb)) (prefixU 0 U.u0 (thetaU nb)))))) ez))
  (have h2 (Eq (List U)
      (vadd (vzero nb) (vadd (vzero nb) (vadd (prefixU 0 U.u1 (vzero nb)) (prefixU 0 U.u0 (thetaU nb)))))
      (vadd (vzero nb) (vadd (vzero nb) (vadd (vzero nb) (prefixU 0 U.u0 (thetaU nb))))))
    (congrArg (fn [us :- (List U)] (vadd (vzero nb) (vadd (vzero nb) (vadd us (prefixU 0 U.u0 (thetaU nb))))))
              (prefixU_zero U.u1 (vzero nb))))
  (have h3 (Eq (List U)
      (vadd (vzero nb) (vadd (vzero nb) (vadd (vzero nb) (prefixU 0 U.u0 (thetaU nb)))))
      (vadd (vzero nb) (vadd (vzero nb) (vadd (vzero nb) (thetaU nb)))))
    (congrArg (fn [us :- (List U)] (vadd (vzero nb) (vadd (vzero nb) (vadd (vzero nb) us))))
              (prefixU_zero U.u0 (thetaU nb))))
  (exact (Eq.trans h1 (Eq.trans h2 (Eq.trans h3 (Eq.trans (vadd_zzz_theta nb) (congrArg thetaU (Eq.symm ez)))))))
  (have es (Eq Nat (+ (Nat.succ n) nb) (Nat.succ (+ n nb))) (Nat.succ_add n nb))
  (have hI (Eq (List U)
      (vadd (vzero (+ (Nat.succ n) nb)) (vadd (vzero (+ (Nat.succ n) nb)) (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb)))))
      (vadd (vzero (Nat.succ (+ n nb))) (vadd (vzero (Nat.succ (+ n nb))) (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb))))))
    (congrArg (fn [k :- Nat] (vadd (vzero k) (vadd (vzero k) (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb)))))) es))
  (have hU (Eq (List U)
      (vadd (vzero (Nat.succ (+ n nb))) (vadd (vzero (Nat.succ (+ n nb))) (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb)))))
      (vadd (List.cons U U.u0 (vzero (+ n nb)))
        (vadd (List.cons U U.u0 (vzero (+ n nb)))
          (vadd (List.cons U U.u1 (prefixU n U.u1 (vzero nb))) (List.cons U U.u0 (prefixU n U.u0 (thetaU nb)))))))
    (Eq.trans
      (congrArg (fn [us :- (List U)] (vadd us (vadd (vzero (Nat.succ (+ n nb))) (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb))))))
                (vzero_succ (+ n nb)))
      (Eq.trans
        (congrArg (fn [us :- (List U)] (vadd (List.cons U U.u0 (vzero (+ n nb))) (vadd us (vadd (prefixU (Nat.succ n) U.u1 (vzero nb)) (prefixU (Nat.succ n) U.u0 (thetaU nb))))))
                  (vzero_succ (+ n nb)))
        (Eq.trans
          (congrArg (fn [us :- (List U)] (vadd (List.cons U U.u0 (vzero (+ n nb))) (vadd (List.cons U U.u0 (vzero (+ n nb))) (vadd us (prefixU (Nat.succ n) U.u0 (thetaU nb))))))
                    (prefixU_succ n U.u1 (vzero nb)))
          (congrArg (fn [us :- (List U)] (vadd (List.cons U U.u0 (vzero (+ n nb))) (vadd (List.cons U U.u0 (vzero (+ n nb))) (vadd (List.cons U U.u1 (prefixU n U.u1 (vzero nb))) us))))
                    (prefixU_succ n U.u0 (thetaU nb)))))))
  (have hC (Eq (List U)
      (vadd (List.cons U U.u0 (vzero (+ n nb)))
        (vadd (List.cons U U.u0 (vzero (+ n nb)))
          (vadd (List.cons U U.u1 (prefixU n U.u1 (vzero nb))) (List.cons U U.u0 (prefixU n U.u0 (thetaU nb))))))
      (List.cons U U.u1 (vadd (vzero (+ n nb)) (vadd (vzero (+ n nb)) (vadd (prefixU n U.u1 (vzero nb)) (prefixU n U.u0 (thetaU nb)))))))
    (Eq.trans (vadd_cons4 U.u0 U.u0 U.u1 U.u0 (vzero (+ n nb)) (vzero (+ n nb)) (prefixU n U.u1 (vzero nb)) (prefixU n U.u0 (thetaU nb)))
              (congrArg (fn [r :- U] (List.cons U r (vadd (vzero (+ n nb)) (vadd (vzero (+ n nb)) (vadd (prefixU n U.u1 (vzero nb)) (prefixU n U.u0 (thetaU nb)))))))
                        uadd_token_head)))
  (have hT (Eq (List U)
      (List.cons U U.u1 (vadd (vzero (+ n nb)) (vadd (vzero (+ n nb)) (vadd (prefixU n U.u1 (vzero nb)) (prefixU n U.u0 (thetaU nb))))))
      (List.cons U U.u1 (thetaU (+ n nb))))
    (congrArg (fn [us :- (List U)] (List.cons U U.u1 us)) ih_n))
  (exact (Eq.trans hI (Eq.trans hU (Eq.trans hC (Eq.trans hT
           (Eq.trans (Eq.symm (thetaU_succ (+ n nb))) (congrArg thetaU (Eq.symm es)))))))))

;; Token at usage 1, label at 0, left subtree on the next ‖a‖ tokens, right
;; subtree on the rest.  Their sum is Θ_{1+‖a‖+‖b‖}.
(thm vadd_lit_us [na :- Nat, nb :- Nat]
  (Eq (List U)
    (vadd (List.cons U U.u1 (vzero (+ na nb)))
      (vadd (vzero (Nat.succ (+ na nb)))
        (vadd (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb)))
              (prefixU (Nat.succ na) U.u0 (thetaU nb)))))
    (thetaU (Nat.succ (+ na nb))))
  (have hU (Eq (List U)
      (vadd (List.cons U U.u1 (vzero (+ na nb)))
        (vadd (vzero (Nat.succ (+ na nb)))
          (vadd (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb)))
                (prefixU (Nat.succ na) U.u0 (thetaU nb)))))
      (vadd (List.cons U U.u1 (vzero (+ na nb)))
        (vadd (List.cons U U.u0 (vzero (+ na nb)))
          (vadd (List.cons U U.u0 (prefixU na U.u1 (vzero nb)))
                (List.cons U U.u0 (prefixU na U.u0 (thetaU nb)))))))
    (Eq.trans
      (congrArg (fn [us :- (List U)]
                  (vadd (List.cons U U.u1 (vzero (+ na nb)))
                    (vadd us (vadd (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb)))
                                   (prefixU (Nat.succ na) U.u0 (thetaU nb))))))
                (vzero_succ (+ na nb)))
      (Eq.trans
        (congrArg (fn [us :- (List U)]
                    (vadd (List.cons U U.u1 (vzero (+ na nb)))
                      (vadd (List.cons U U.u0 (vzero (+ na nb)))
                        (vadd us (prefixU (Nat.succ na) U.u0 (thetaU nb))))))
                  (Eq.trans (prefixU_succ 0 U.u0 (prefixU na U.u1 (vzero nb)))
                            (congrArg (fn [t :- (List U)] (List.cons U U.u0 t)) (prefixU_zero U.u0 (prefixU na U.u1 (vzero nb))))))
        (congrArg (fn [us :- (List U)]
                    (vadd (List.cons U U.u1 (vzero (+ na nb)))
                      (vadd (List.cons U U.u0 (vzero (+ na nb)))
                        (vadd (List.cons U U.u0 (prefixU na U.u1 (vzero nb))) us))))
                  (prefixU_succ na U.u0 (thetaU nb))))))
  (have hC (Eq (List U)
      (vadd (List.cons U U.u1 (vzero (+ na nb)))
        (vadd (List.cons U U.u0 (vzero (+ na nb)))
          (vadd (List.cons U U.u0 (prefixU na U.u1 (vzero nb)))
                (List.cons U U.u0 (prefixU na U.u0 (thetaU nb))))))
      (List.cons U U.u1
        (vadd (vzero (+ na nb))
          (vadd (vzero (+ na nb))
            (vadd (prefixU na U.u1 (vzero nb)) (prefixU na U.u0 (thetaU nb)))))))
    (Eq.trans (vadd_cons4 U.u1 U.u0 U.u0 U.u0 (vzero (+ na nb)) (vzero (+ na nb)) (prefixU na U.u1 (vzero nb)) (prefixU na U.u0 (thetaU nb)))
              (congrArg (fn [r :- U] (List.cons U r (vadd (vzero (+ na nb)) (vadd (vzero (+ na nb)) (vadd (prefixU na U.u1 (vzero nb)) (prefixU na U.u0 (thetaU nb)))))))
                        uadd_lit_head)))
  (exact (Eq.trans hU (Eq.trans hC (Eq.trans (congrArg (fn [us :- (List U)] (List.cons U U.u1 us)) (vadd_blocks na nb))
                                              (Eq.symm (thetaU_succ (+ na nb))))))))

;; --- placing the two subtrees in Θ --------------------------------------------------------

(thm le_refl [n :- Nat] (LE.le n n) (omega))
(thm add0_bound [n :- Nat] (Eq Nat (+ 0 (+ n 0)) n) (omega))
(thm add_1na_nb [na :- Nat, nb :- Nat] (Eq Nat (+ (+ 1 na) nb) (+ 1 (+ na nb))) (omega))
(thm succ_1na [na :- Nat] (Eq Nat (Nat.succ na) (+ 1 na)) (omega))
(thm add1_succ [n :- Nat] (Eq Nat (+ 1 n) (Nat.succ n)) (exact (Eq.symm (succ_1na n))))
(thm idx_right [na :- Nat] (Eq Nat (+ 0 (+ 1 na)) (+ (+ 0 1) na)) (omega))

(thm closedF_cast [t1 :- Exp, t2 :- Exp, q :- Nat, e :- (Eq Exp t1 t2), h :- (Eq Bool ((closedF t2) q) Bool.true)]
  (Eq Bool ((closedF t1) q) Bool.true)
  (subst e) (exact h))

(thm closedF_bound [t :- Exp, q1 :- Nat, q2 :- Nat, e :- (Eq Nat q2 q1), h :- (Eq Bool ((closedF t) q1) Bool.true)]
  (Eq Bool ((closedF t) q2) Bool.true)
  (subst e) (exact h))

;; lit0 v mentions only variables below ‖v‖, so a lift at cutoff ‖v‖ fixes it.
(thm lit_closed_block [v :- Code]
  (Eq Bool ((closedF (lit0 v)) (cnodes v)) Bool.true)
  (exact (closedF_cast (lit0 v) ((litAt v) 0) (cnodes v) (lit0_at v)
           (closedF_bound ((litAt v) 0) (+ 0 (+ (cnodes v) 0)) (cnodes v) (Eq.symm (add0_bound (cnodes v)))
             (lit_scope v 0 0)))))

;; The right subtree keeps its own tokens and gains ‖left-of-it‖ unused
;; tokens in front.  Lemma 2.1, via rt_theta_prepend.
(thm rt_place_right [chkf :- (=> Code Code Bool), na :- Nat, nb :- Nat, t :- Exp, A :- Exp,
                     hc :- (Eq Bool (closedTy A) Bool.true),
                     h :- (Rt chkf (thetaD nb) (thetaU nb) t A)]
  (Rt chkf (thetaD (+ 1 (+ na nb))) (prefixU (+ 1 na) U.u0 (thetaU nb)) (lift (+ 1 na) 0 t) A)
  (exact (rt_reindex chkf (thetaD (+ (+ 1 na) nb)) (prefixU (+ 1 na) U.u0 (thetaU nb)) (lift (+ 1 na) 0 t) A
           (thetaD (+ 1 (+ na nb))) (prefixU (+ 1 na) U.u0 (thetaU nb)) (lift (+ 1 na) 0 t) A
           (rt_theta_prepend chkf nb t A hc h (+ 1 na))
           (congrArg thetaD (add_1na_nb na nb)) rfl rfl rfl)))

;; The left subtree is appended with ‖b‖ unused outer tokens, which do not
;; move its variables (they lie below ‖a‖), then one unused token is
;; inserted in front for the node.  The result uses 0, then ‖a‖ ones, then
;; ‖b‖ zeros.
(thm rt_place_left [chkf :- (=> Code Code Bool), na :- Nat, nb :- Nat, t :- Exp,
                    hsc :- (Eq Bool ((closedF t) na) Bool.true),
                    h :- (Rt chkf (thetaD na) (thetaU na) t Exp.tR)]
  (Rt chkf (thetaD (+ 1 (+ na nb))) (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb))) (lift 1 0 t) Exp.tR)
  (have hAp (Rt chkf (thetaD (+ na nb)) (prefixU na U.u1 (vzero nb)) (lift nb na t) Exp.tR)
    (rt_theta_append chkf na t Exp.tR closed_tR h nb))
  (have hfix (Eq Exp (lift nb na t) t) (closed_lift t na hsc nb na (le_refl na)))
  (have hAp2 (Rt chkf (thetaD (+ na nb)) (prefixU na U.u1 (vzero nb)) t Exp.tR)
    (rt_cast_t chkf (thetaD (+ na nb)) (prefixU na U.u1 (vzero nb)) (lift nb na t) t Exp.tR Exp.tR hAp hfix rfl))
  (have hW (Rt chkf (insD 0 Exp.tDia (thetaD (+ na nb))) (insU 0 U.u0 (prefixU na U.u1 (vzero nb))) (lift 1 0 t) (lift 1 0 Exp.tR))
    (rt_weaken chkf (thetaD (+ na nb)) (prefixU na U.u1 (vzero nb)) t Exp.tR hAp2 0 Exp.tDia))
  (have eD (Eq (List Exp) (insD 0 Exp.tDia (thetaD (+ na nb))) (thetaD (+ 1 (+ na nb))))
    (Eq.trans (insD_zero Exp.tDia (thetaD (+ na nb)))
      (Eq.trans (Eq.symm (thetaD_succ (+ na nb)))
                (congrArg thetaD (Eq.symm (add1_succ (+ na nb)))))))
  (have eU (Eq (List U) (insU 0 U.u0 (prefixU na U.u1 (vzero nb))) (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb))))
    (Eq.trans (insU_zero U.u0 (prefixU na U.u1 (vzero nb)))
      (Eq.symm (Eq.trans (prefixU_succ 0 U.u0 (prefixU na U.u1 (vzero nb)))
                         (congrArg (fn [us :- (List U)] (List.cons U U.u0 us)) (prefixU_zero U.u0 (prefixU na U.u1 (vzero nb))))))))
  (exact (rt_reindex chkf (insD 0 Exp.tDia (thetaD (+ na nb))) (insU 0 U.u0 (prefixU na U.u1 (vzero nb)))
           (lift 1 0 t) (lift 1 0 Exp.tR)
           (thetaD (+ 1 (+ na nb))) (prefixU (Nat.succ 0) U.u0 (prefixU na U.u1 (vzero nb)))
           (lift 1 0 t) Exp.tR hW eD eU rfl (closedTy_lift Exp.tR closed_tR 1 0))))

;; A node, before the usage sum and the lit0 abbreviation are restored.
;; The token is var 0 : ◇ at usage 1; the label is an axiom at usage 0;
;; the subtrees sit where rt_place_left / rt_place_right put them.
(thm rt_node_raw [chkf :- (=> Code Code Bool), l :- Nat, a :- Code, b :- Code,
                  ha :- (Rt chkf (thetaD (cnodes a)) (thetaU (cnodes a)) (lit0 a) Exp.tR),
                  hb :- (Rt chkf (thetaD (cnodes b)) (thetaU (cnodes b)) (lit0 b) Exp.tR),
                  hlbl :- (Eq Bool (Nat.blt l 100) Bool.true)]
  (Rt chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (vadd (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
        (vadd (vzero (Nat.succ (+ (cnodes a) (cnodes b))))
          (vadd (prefixU (Nat.succ 0) U.u0 (prefixU (cnodes a) U.u1 (vzero (cnodes b))))
                (prefixU (Nat.succ (cnodes a)) U.u0 (thetaU (cnodes b))))))
      (Exp.node (Exp.var 0) (Exp.lbl l) ((litAt a) (+ 0 1)) ((litAt b) (+ (+ 0 1) (cnodes a))))
      Exp.tR)
  (have hlv (Eq Nat (lenU (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b)))))
                   (lenE (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))))
    (Eq.trans (lenU_cons U.u1 (vzero (+ (cnodes a) (cnodes b))))
      (Eq.trans (congrArg Nat.succ (lenU_vzero (+ (cnodes a) (cnodes b))))
                (Eq.symm (lenE_theta (Nat.succ (+ (cnodes a) (cnodes b))))))))
  (have hVar0 (Rt chkf (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))
                 (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
                 (Exp.var 0) (lift (+ 0 1) 0 Exp.tDia))
    (Rt.rVar chkf (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))
      (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
      0 Exp.tDia U.u1 hlv
      (nthE_theta0 (+ (cnodes a) (cnodes b)))
      (nthU_cons0 U.u1 (vzero (+ (cnodes a) (cnodes b))))
      nonzero_u1))
  (have hVar (Rt chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
                 (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
                 (Exp.var 0) Exp.tDia)
    (rt_reindex chkf (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))
      (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
      (Exp.var 0) (lift (+ 0 1) 0 Exp.tDia)
      (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
      (Exp.var 0) Exp.tDia
      hVar0
      (congrArg thetaD (Eq.symm (add1_succ (+ (cnodes a) (cnodes b)))))
      rfl rfl (closedTy_lift Exp.tDia closed_tDia (+ 0 1) 0)))
  (have hLbl (Rt chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
                 (vzero (Nat.succ (+ (cnodes a) (cnodes b)))) (Exp.lbl l) Exp.tLbl)
    (rt_reindex chkf (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))
      (vzero (Nat.succ (+ (cnodes a) (cnodes b)))) (Exp.lbl l) Exp.tLbl
      (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (vzero (Nat.succ (+ (cnodes a) (cnodes b)))) (Exp.lbl l) Exp.tLbl
      (Rt.rConst chkf (thetaD (Nat.succ (+ (cnodes a) (cnodes b))))
        (vzero (Nat.succ (+ (cnodes a) (cnodes b)))) (Exp.lbl l) Exp.tLbl
        (Eq.trans (lenU_vzero (Nat.succ (+ (cnodes a) (cnodes b))))
                  (Eq.symm (lenE_theta (Nat.succ (+ (cnodes a) (cnodes b))))))
        (const_lbl l hlbl))
      (congrArg thetaD (Eq.symm (add1_succ (+ (cnodes a) (cnodes b)))))
      rfl rfl rfl))
  (have hL (Rt chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
               (prefixU (Nat.succ 0) U.u0 (prefixU (cnodes a) U.u1 (vzero (cnodes b))))
               ((litAt a) (+ 0 1)) Exp.tR)
    (rt_cast_t chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (prefixU (Nat.succ 0) U.u0 (prefixU (cnodes a) U.u1 (vzero (cnodes b))))
      (lift 1 0 (lit0 a)) ((litAt a) (+ 0 1)) Exp.tR Exp.tR
      (rt_place_left chkf (cnodes a) (cnodes b) (lit0 a) (lit_closed_block a) ha)
      (Eq.trans (congrArg (fn [t :- Exp] (lift 1 0 t)) (lit0_at a)) (lit_shift a 0 1))
      rfl))
  (have hR (Rt chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
               (prefixU (Nat.succ (cnodes a)) U.u0 (thetaU (cnodes b)))
               ((litAt b) (+ (+ 0 1) (cnodes a))) Exp.tR)
    (rt_reindex chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (prefixU (+ 1 (cnodes a)) U.u0 (thetaU (cnodes b)))
      (lift (+ 1 (cnodes a)) 0 (lit0 b)) Exp.tR
      (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
      (prefixU (Nat.succ (cnodes a)) U.u0 (thetaU (cnodes b)))
      ((litAt b) (+ (+ 0 1) (cnodes a))) Exp.tR
      (rt_place_right chkf (cnodes a) (cnodes b) (lit0 b) Exp.tR closed_tR hb)
      rfl
      (congrArg (fn [k :- Nat] (prefixU k U.u0 (thetaU (cnodes b)))) (Eq.symm (succ_1na (cnodes a))))
      (Eq.trans (congrArg (fn [t :- Exp] (lift (+ 1 (cnodes a)) 0 t)) (lit0_at b))
        (Eq.trans (lit_shift b 0 (+ 1 (cnodes a)))
                  (congrArg (fn [i :- Nat] ((litAt b) i)) (idx_right (cnodes a)))))
      rfl))
  (exact (Rt.rNode chkf (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
           (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
           (vzero (Nat.succ (+ (cnodes a) (cnodes b))))
           (prefixU (Nat.succ 0) U.u0 (prefixU (cnodes a) U.u1 (vzero (cnodes b))))
           (prefixU (Nat.succ (cnodes a)) U.u0 (thetaU (cnodes b)))
           (Exp.var 0) (Exp.lbl l) ((litAt a) (+ 0 1)) ((litAt b) (+ (+ 0 1) (cnodes a)))
           hVar hLbl hL hR)))

;; Proposition 4.2, the certificate half: if every label of v is in L,
;; Θ_{‖v‖} ⊢ lit v :¹ R.  The evidence ⋆ : T(chk′ (print (lit v)) ⌜A⌝)
;; is the conversion half, built afterwards.
(thm rt_lit [chkf :- (=> Code Code Bool), v :- Code]
  (=> (Eq Bool (lblOk v) Bool.true)
      (Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v)) (lit0 v) Exp.tR))
  (induction v)
  (intro h)
  (have hleaf (Rt chkf (List.nil Exp) (List.nil U) (Exp.leaf (Exp.lbl l)) Exp.tR)
    (Rt.rLeaf chkf (List.nil Exp) (List.nil U) (Exp.lbl l)
      (Rt.rConst chkf (List.nil Exp) (List.nil U) (Exp.lbl l) Exp.tLbl
        (Eq.trans len_nilU (Eq.symm len_nilE))
        (const_lbl l (Eq.trans (Eq.symm (lblOk_sl l)) h)))))
  (exact (rt_reindex chkf (List.nil Exp) (List.nil U) (Exp.leaf (Exp.lbl l)) Exp.tR
           (thetaD (cnodes (Code.sl l))) (thetaU (cnodes (Code.sl l))) (lit0 (Code.sl l)) Exp.tR
           hleaf
           (Eq.trans (Eq.symm thetaD_zero) (congrArg thetaD (Eq.symm (cnodes_sl l))))
           (Eq.trans (Eq.symm thetaU_zero) (congrArg thetaU (Eq.symm (cnodes_sl l))))
           (Eq.trans (Eq.symm (litAt_sl l 0)) (Eq.symm (lit0_at (Code.sl l))))
           rfl))
  (intro h)
  (have hok (Eq Bool (Bool.and (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b))) Bool.true)
    (Eq.trans (Eq.symm (lblOk_sn l a b)) h))
  (have hab (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
    (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (exact (rt_reindex chkf
           (thetaD (+ 1 (+ (cnodes a) (cnodes b))))
           (vadd (List.cons U U.u1 (vzero (+ (cnodes a) (cnodes b))))
             (vadd (vzero (Nat.succ (+ (cnodes a) (cnodes b))))
               (vadd (prefixU (Nat.succ 0) U.u0 (prefixU (cnodes a) U.u1 (vzero (cnodes b))))
                     (prefixU (Nat.succ (cnodes a)) U.u0 (thetaU (cnodes b))))))
           (Exp.node (Exp.var 0) (Exp.lbl l) ((litAt a) (+ 0 1)) ((litAt b) (+ (+ 0 1) (cnodes a))))
           Exp.tR
           (thetaD (cnodes (Code.sn l a b))) (thetaU (cnodes (Code.sn l a b))) (lit0 (Code.sn l a b)) Exp.tR
           (rt_node_raw chkf l a b (ih_a (band_left (lblOk a) (lblOk b) hab)) (ih_b (band_right (lblOk a) (lblOk b) hab))
             (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
           (congrArg thetaD (Eq.symm (cnodes_sn l a b)))
           (Eq.trans (vadd_lit_us (cnodes a) (cnodes b))
             (congrArg thetaU (Eq.trans (Eq.symm (add1_succ (+ (cnodes a) (cnodes b)))) (Eq.symm (cnodes_sn l a b)))))
           (Eq.trans (Eq.symm (litAt_sn l a b 0)) (Eq.symm (lit0_at (Code.sn l a b))))
           rfl)))

;; --- the conversion under T(chk′) -------------------------------------------------------
;; A Step is a path, a head redex and its contractum (conv.clj).  At the
;; empty path, getP and setP are the identity.  Those two equations live in
;; conversion.clj, which this namespace does not require, so they are
;; restated.  One-child steps are packed from the child and setKid equations.
(thm getP_nil [e :- Exp] (Eq (Option Exp) (getP (List.nil Nat) e) (Option.some Exp e)) (rfl))

(thm setP_nil [e :- Exp, x :- Exp] (Eq Exp (setP (List.nil Nat) e x) x) (rfl))

(thm child_tT [b :- Exp] (Eq (Option Exp) (child (Exp.tT b) 0) (Option.some Exp b)) (rfl))

(thm set_tT [b :- Exp, x :- Exp] (Eq Exp (setKid (Exp.tT b) 0 x) (Exp.tT x)) (rfl))

(thm child_chk [c :- Exp, d :- Exp] (Eq (Option Exp) (child (Exp.chk c d) 0) (Option.some Exp c)) (rfl))

(thm set_chk [c :- Exp, d :- Exp, x :- Exp] (Eq Exp (setKid (Exp.chk c d) 0 x) (Exp.chk x d)) (rfl))

(thm child_sn1 [x :- Exp, a :- Exp, b :- Exp]
  (Eq (Option Exp) (child (Exp.snode x a b) 1) (Option.some Exp a)) (rfl))

(thm child_sn2 [x :- Exp, a :- Exp, b :- Exp]
  (Eq (Option Exp) (child (Exp.snode x a b) 2) (Option.some Exp b)) (rfl))

(thm set_sn1 [x :- Exp, a :- Exp, b :- Exp, y :- Exp]
  (Eq Exp (setKid (Exp.snode x a b) 1 y) (Exp.snode x y b)) (rfl))

(thm set_sn2 [x :- Exp, a :- Exp, b :- Exp, y :- Exp]
  (Eq Exp (setKid (Exp.snode x a b) 2 y) (Exp.snode x a y)) (rfl))

(thm boolExp_tt [] (Eq Exp (boolExp Bool.true) Exp.tt) (rfl))

(thm nbr_tUnit [] (Eq Bool (nbr Exp.tUnit) Bool.true) (rfl))

(thm nbr_tt [] (Eq Bool (nbr Exp.tt) Bool.true) (rfl))

(thm nbr_tT [b :- Exp] (Eq Bool (nbr (Exp.tT b)) (nbr b)) (rfl))

(thm nbr_chk [c :- Exp, d :- Exp] (Eq Bool (nbr (Exp.chk c d)) (Bool.and (nbr c) (nbr d))) (rfl))

(thm nbr_prn [r :- Exp] (Eq Bool (nbr (Exp.prn r)) (nbr r)) (rfl))

(thm nbr_snode [x :- Exp, a :- Exp, b :- Exp]
  (Eq Bool (nbr (Exp.snode x a b)) (Bool.and (nbr x) (Bool.and (nbr a) (nbr b)))) (rfl))

(thm nbr_sleaf [x :- Exp] (Eq Bool (nbr (Exp.sleaf x)) (nbr x)) (rfl))

(thm step_hd [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp, h :- (Hd chkf e e2)]
  (Step chkf e e2)
  (unfold Step)
  (apply (Exists.intro (List.nil Nat)))
  (apply (Exists.intro e))
  (apply (Exists.intro e2))
  (exact (And.intro (getP_nil e) (And.intro h (Eq.symm (setP_nil e e2))))))

(thm step_cast [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp, e3 :- Exp, hs :- (Step chkf e e2), eq :- (Eq Exp e2 e3)]
  (Step chkf e e3)
  (subst eq) (exact hs))

(thm step_pack [chkf :- (=> Code Code Bool), i :- Nat, E :- Exp, C :- Exp, C2 :- Exp,
                     hch :- (Eq (Option Exp) (child E i) (Option.some Exp C)),
                     p :- (List Nat), r :- Exp, r2 :- Exp,
                     h3 :- (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))]
  (Step chkf E (setKid E i C2))
  (unfold Step)
  (apply (Exists.intro (List.cons Nat i p)))
  (apply (Exists.intro r))
  (apply (Exists.intro r2))
  (exact (And.intro (Eq.trans (getP_some i p E C hch) (And.left h3))
           (And.intro (And.left (And.right h3))
             (Eq.trans (congrArg (fn [t :- Exp] (setKid E i t)) (And.right (And.right h3)))
                       (Eq.symm (setP_some i p E C r2 hch)))))))

(thm step_pack0 [chkf :- (=> Code Code Bool), E :- Exp, C :- Exp, C2 :- Exp,
                      hch :- (Eq (Option Exp) (child E 0) (Option.some Exp C)),
                      p :- (List Nat), r :- Exp, r2 :- Exp,
                      h3 :- (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))]
  (Step chkf E (setKid E 0 C2))
  (unfold Step)
  (apply (Exists.intro (List.cons Nat 0 p)))
  (apply (Exists.intro r))
  (apply (Exists.intro r2))
  (exact (And.intro (Eq.trans (getP_some 0 p E C hch) (And.left h3))
           (And.intro (And.left (And.right h3))
             (Eq.trans (congrArg (fn [t :- Exp] (setKid E 0 t)) (And.right (And.right h3)))
                       (Eq.symm (setP_some 0 p E C r2 hch)))))))

(thm step_cons0 [chkf :- (=> Code Code Bool), E :- Exp, C :- Exp, C2 :- Exp,
                      hch :- (Eq (Option Exp) (child E 0) (Option.some Exp C)),
                      hs :- (Step chkf C C2)]
  (Step chkf E (setKid E 0 C2))
  (exact (exists_elimL
    (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
      (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))))))
    (Step chkf E (setKid E 0 C2))
    hs
    (fn [p :- (List Nat),
         hp :- (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
           (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))))))]
      (exists_elimE
        (fn [r :- Exp] (Exists (fn [r2 :- Exp]
          (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))))
        (Step chkf E (setKid E 0 C2))
        hp
        (fn [r :- Exp,
             hr :- (Exists (fn [r2 :- Exp]
               (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))))]
          (exists_elimE
            (fn [r2 :- Exp] (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))
            (Step chkf E (setKid E 0 C2))
            hr
            (fn [r2 :- Exp,
                 h3 :- (And (Eq (Option Exp) (getP p C) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))]
              (step_pack0 chkf E C C2 hch p r r2 h3)))))))))

(thm step_at [chkf :- (=> Code Code Bool), i :- Nat, E :- Exp, C :- Exp, C2 :- Exp,
                   hch :- (Eq (Option Exp) (child E i) (Option.some Exp C)),
                   hs :- (Step chkf C C2)]
  (Step chkf E (setKid E i C2))
  (exact (exists_elimL
    (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
      (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
           (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))))))
    (Step chkf E (setKid E i C2)) hs
    (fn [p :- (List Nat), hp :- (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
           (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
                (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))))))]
      (exists_elimE
        (fn [r :- Exp] (Exists (fn [r2 :- Exp]
          (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
               (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))))
        (Step chkf E (setKid E i C2)) hp
        (fn [r :- Exp, hr :- (Exists (fn [r2 :- Exp]
               (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
                    (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))))]
          (exists_elimE
            (fn [r2 :- Exp]
              (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
                   (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2)))))
            (Step chkf E (setKid E i C2)) hr
            (fn [r2 :- Exp, h3 :- (And (Eq (Option Exp) (getP p C) (Option.some Exp r))
                                       (And (Hd chkf r r2) (Eq Exp C2 (setP p C r2))))]
              (step_pack chkf i E C C2 hch p r r2 h3)))))))))

(thm step_delta_tt [chkf :- (=> Code Code Bool), c :- Exp, d :- Exp, cc :- Code, dc :- Code,
                         hc :- (Eq (Option Code) (codeOf c) (Option.some Code cc)),
                         hd :- (Eq (Option Code) (codeOf d) (Option.some Code dc)),
                         hck :- (Eq Bool (chkf cc dc) Bool.true)]
  (Step chkf (Exp.chk c d) Exp.tt)
  (exact (step_cast chkf (Exp.chk c d) (boolExp (chkf cc dc)) Exp.tt
           (step_hd chkf (Exp.chk c d) (boolExp (chkf cc dc)) (Hd.delta chkf c d cc dc hc hd))
           (Eq.trans (congrArg boolExp hck) boolExp_tt))))

(thm step_tchk [chkf :- (=> Code Code Bool), u :- Exp, u2 :- Exp, ca :- Exp, hs :- (Step chkf u u2)]
  (Step chkf (Exp.tT (Exp.chk u ca)) (Exp.tT (Exp.chk u2 ca)))
  (exact (step_at chkf 0 (Exp.tT (Exp.chk u ca)) (Exp.chk u ca) (Exp.chk u2 ca)
           (child_tT (Exp.chk u ca))
           (step_at chkf 0 (Exp.chk u ca) u u2 (child_chk u ca) hs))))

(thm cv_trans [chkf :- (=> Code Code Bool), G :- (List Sk), B0 :- Exp, C0 :- Exp, hbc :- (Cv chkf G B0 C0)]
  (forall [A0 Exp] (=> (Cv chkf G A0 B0) (Cv chkf G A0 C0)))
  (induction hbc)
  (intro A0 h0) (exact h0)
  (intro A0 h0) (exact (Cv.cvFwd chkf G A0 B C (ih_hab A0 h0) hs hc hn))
  (intro A0 h0) (exact (Cv.cvBwd chkf G A0 B C (ih_hab A0 h0) hs hc hn)))

(thm cv_unit_tt [chkf :- (=> Code Code Bool), G :- (List Sk)]
  (Cv chkf G Exp.tUnit (Exp.tT Exp.tt))
  (exact (Cv.cvBwd chkf G Exp.tUnit Exp.tUnit (Exp.tT Exp.tt)
           (Cv.cvRefl chkf G Exp.tUnit (SkJ.wUnit G) nbr_tUnit)
           (step_hd chkf (Exp.tT Exp.tt) Exp.tUnit (Hd.tTT chkf))
           (SkJ.wT G Exp.tt (SkJ.sTT G))
           (Eq.trans (nbr_tT Exp.tt) nbr_tt))))

(thm cv_code_tt [chkf :- (=> Code Code Bool), G :- (List Sk), v :- Code, cA :- Code,
                      hck :- (Eq Bool (chkf v cA) Bool.true)]
  (Cv chkf G Exp.tUnit (Exp.tT (Exp.chk (codeTerm v) (codeTerm cA))))
  (exact (Cv.cvBwd chkf G Exp.tUnit (Exp.tT Exp.tt) (Exp.tT (Exp.chk (codeTerm v) (codeTerm cA)))
           (cv_unit_tt chkf G)
           (step_cons0 chkf (Exp.tT (Exp.chk (codeTerm v) (codeTerm cA))) (Exp.chk (codeTerm v) (codeTerm cA)) Exp.tt
             (child_tT (Exp.chk (codeTerm v) (codeTerm cA)))
             (step_delta_tt chkf (codeTerm v) (codeTerm cA) v cA (codeTerm_of v) (codeTerm_of cA) hck))
           (SkJ.wT G (Exp.chk (codeTerm v) (codeTerm cA))
             (SkJ.sChk G (codeTerm v) (codeTerm cA) (skj_code G v) (skj_code G cA)))
           (Eq.trans (nbr_tT (Exp.chk (codeTerm v) (codeTerm cA)))
             (Eq.trans (nbr_chk (codeTerm v) (codeTerm cA))
               (andb_intro (nbr (codeTerm v)) (nbr (codeTerm cA)) (nbr_code v) (nbr_code cA)))))))


;; A syn reduction whose every right-hand term is skeleton-typed at Syn and
;; satisfies nbr.  A head step preserves both (the constructors below); there
;; is no subject reduction at an arbitrary path, so print's ι chain carries
;; the two proofs and wr_to_cv turns the chain into a Cv under T(chk).
(a/inductive WRed [chkf (=> Code Code Bool), G (List Sk)] :in Prop :indices [a Exp, b Exp]
    (wrRefl [a Exp] [hj (SkJ Bool.false G a Sk.syn)] [hn (Eq Bool (nbr a) Bool.true)] :where [a a])
    (wrStep [a Exp] [b Exp] [c Exp] [h (WRed chkf G a b)] [s (Step chkf b c)]
            [hj (SkJ Bool.false G c Sk.syn)] [hn (Eq Bool (nbr c) Bool.true)] :where [a c]))

(thm wr_right [chkf :- (=> Code Code Bool), G :- (List Sk), a0 :- Exp, b0 :- Exp, h :- (WRed chkf G a0 b0)]
  (And (SkJ Bool.false G b0 Sk.syn) (Eq Bool (nbr b0) Bool.true))
  (induction h)
  (exact (And.intro hj hn))
  (exact (And.intro hj hn)))

(thm wr_trans [chkf :- (=> Code Code Bool), G :- (List Sk), b0 :- Exp, c0 :- Exp, hbc :- (WRed chkf G b0 c0)]
  (forall [a0 Exp] (=> (WRed chkf G a0 b0) (WRed chkf G a0 c0)))
  (induction hbc)
  (intro a0 h1) (exact h1)
  (intro a0 h1) (exact (WRed.wrStep chkf G a0 b c (ih_h a0 h1) s hj hn)))

(thm wr_cast_G [chkf :- (=> Code Code Bool), G :- (List Sk), G2 :- (List Sk), a :- Exp, b :- Exp,
                     h :- (WRed chkf G a b), eq :- (Eq (List Sk) G G2)]
  (WRed chkf G2 a b)
  (subst eq) (exact h))

(thm skj_cast_G [w :- Bool, G :- (List Sk), G2 :- (List Sk), t :- Exp, s :- Sk,
                      h :- (SkJ w G t s), eq :- (Eq (List Sk) G G2)]
  (SkJ w G2 t s)
  (subst eq) (exact h))

(thm wr_cast_end [chkf :- (=> Code Code Bool), G :- (List Sk), a :- Exp, b :- Exp, b2 :- Exp,
                       h :- (WRed chkf G a b), eq :- (Eq Exp b b2)]
  (WRed chkf G a b2) (subst eq) (exact h))

(thm wr_cast_start [chkf :- (=> Code Code Bool), G :- (List Sk), a :- Exp, a2 :- Exp, b :- Exp,
                         h :- (WRed chkf G a b), eq :- (Eq Exp a a2)]
  (WRed chkf G a2 b) (subst eq) (exact h))

(thm skj_cast_tm [w :- Bool, G :- (List Sk), t :- Exp, t2 :- Exp, s :- Sk,
                       h :- (SkJ w G t s), eq :- (Eq Exp t t2)]
  (SkJ w G t2 s) (subst eq) (exact h))

(thm wr_sn1 [chkf :- (=> Code Code Bool), G :- (List Sk), x :- Exp, r :- Exp, a0 :- Exp, b0 :- Exp,
                  h0 :- (WRed chkf G a0 b0),
                  hx :- (SkJ Bool.false G x Sk.lbl), hnx :- (Eq Bool (nbr x) Bool.true),
                  hr :- (SkJ Bool.false G r Sk.syn), hnr :- (Eq Bool (nbr r) Bool.true)]
  (WRed chkf G (Exp.snode x a0 r) (Exp.snode x b0 r))
  (induction h0)
  (exact (WRed.wrRefl chkf G (Exp.snode x a r)
           (SkJ.sSnode G x a r hx hj hr)
           (Eq.trans (nbr_snode x a r)
             (andb_intro (nbr x) (Bool.and (nbr a) (nbr r)) hnx (andb_intro (nbr a) (nbr r) hn hnr)))))
  (exact (WRed.wrStep chkf G (Exp.snode x a r) (Exp.snode x b r) (Exp.snode x c r) ih_h
           (step_at chkf 1 (Exp.snode x b r) b c (child_sn1 x b r) s)
           (SkJ.sSnode G x c r hx hj hr)
           (Eq.trans (nbr_snode x c r)
             (andb_intro (nbr x) (Bool.and (nbr c) (nbr r)) hnx (andb_intro (nbr c) (nbr r) hn hnr))))))

(thm wr_sn2 [chkf :- (=> Code Code Bool), G :- (List Sk), x :- Exp, u :- Exp, a0 :- Exp, b0 :- Exp,
                  h0 :- (WRed chkf G a0 b0),
                  hx :- (SkJ Bool.false G x Sk.lbl), hnx :- (Eq Bool (nbr x) Bool.true),
                  hu :- (SkJ Bool.false G u Sk.syn), hnu :- (Eq Bool (nbr u) Bool.true)]
  (WRed chkf G (Exp.snode x u a0) (Exp.snode x u b0))
  (induction h0)
  (exact (WRed.wrRefl chkf G (Exp.snode x u a)
           (SkJ.sSnode G x u a hx hu hj)
           (Eq.trans (nbr_snode x u a)
             (andb_intro (nbr x) (Bool.and (nbr u) (nbr a)) hnx (andb_intro (nbr u) (nbr a) hnu hn)))))
  (exact (WRed.wrStep chkf G (Exp.snode x u a) (Exp.snode x u b) (Exp.snode x u c) ih_h
           (step_at chkf 2 (Exp.snode x u b) b c (child_sn2 x u b) s)
           (SkJ.sSnode G x u c hx hu hj)
           (Eq.trans (nbr_snode x u c)
             (andb_intro (nbr x) (Bool.and (nbr u) (nbr c)) hnx (andb_intro (nbr u) (nbr c) hnu hn))))))

(thm wr_to_cv [chkf :- (=> Code Code Bool), G :- (List Sk), ca :- Exp, a0 :- Exp, b0 :- Exp,
                    h0 :- (WRed chkf G a0 b0),
                    hca :- (SkJ Bool.false G ca Sk.syn),
                    hnca :- (Eq Bool (nbr ca) Bool.true)]
  (Cv chkf G (Exp.tT (Exp.chk b0 ca)) (Exp.tT (Exp.chk a0 ca)))
  (induction h0)
  (exact (Cv.cvRefl chkf G (Exp.tT (Exp.chk a ca))
           (SkJ.wT G (Exp.chk a ca) (SkJ.sChk G a ca hj hca))
           (Eq.trans (nbr_tT (Exp.chk a ca))
             (Eq.trans (nbr_chk a ca) (andb_intro (nbr a) (nbr ca) hn hnca)))))
  (have one (Cv chkf G (Exp.tT (Exp.chk c ca)) (Exp.tT (Exp.chk b ca)))
    (Cv.cvBwd chkf G (Exp.tT (Exp.chk c ca)) (Exp.tT (Exp.chk c ca)) (Exp.tT (Exp.chk b ca))
      (Cv.cvRefl chkf G (Exp.tT (Exp.chk c ca))
        (SkJ.wT G (Exp.chk c ca) (SkJ.sChk G c ca hj hca))
        (Eq.trans (nbr_tT (Exp.chk c ca))
          (Eq.trans (nbr_chk c ca) (andb_intro (nbr c) (nbr ca) hn hnca))))
      (step_tchk chkf b c ca s)
      (SkJ.wT G (Exp.chk b ca) (SkJ.sChk G b ca (And.left (wr_right chkf G a b h)) hca))
      (Eq.trans (nbr_tT (Exp.chk b ca))
        (Eq.trans (nbr_chk b ca)
          (andb_intro (nbr b) (nbr ca) (And.right (wr_right chkf G a b h)) hnca)))))
  (exact ((cv_trans chkf G (Exp.tT (Exp.chk b ca)) (Exp.tT (Exp.chk a ca)) ih_h)
          (Exp.tT (Exp.chk c ca)) one)))

(thm wr_prn_node [chkf :- (=> Code Code Bool), G :- (List Sk), l :- Nat, a :- Code, b :- Code, off :- Nat,
                       hL :- (WRed chkf G (Exp.prn ((litAt a) (+ off 1))) (codeTerm a)),
                       hR :- (WRed chkf G (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))) (codeTerm b)),
                       hnode :- (SkJ Bool.false G (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))) Sk.cert),
                       hLa :- (SkJ Bool.false G ((litAt a) (+ off 1)) Sk.cert),
                       hLb :- (SkJ Bool.false G ((litAt b) (+ (+ off 1) (cnodes a))) Sk.cert)]
  (WRed chkf G (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
              (codeTerm (Code.sn l a b)))
  (have hnL (Eq Bool (nbr (Exp.prn ((litAt a) (+ off 1)))) Bool.true)
    (Eq.trans (nbr_prn ((litAt a) (+ off 1))) (nbr_lit a (+ off 1))))
  (have hnR (Eq Bool (nbr (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))) Bool.true)
    (Eq.trans (nbr_prn ((litAt b) (+ (+ off 1) (cnodes a)))) (nbr_lit b (+ (+ off 1) (cnodes a)))))
  (have hS (WRed chkf G
            (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
            (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)))
    ((wr_trans chkf G
        (Exp.snode (Exp.lbl l) (codeTerm a) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
        (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b))
        (wr_sn2 chkf G (Exp.lbl l) (codeTerm a)
           (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))) (codeTerm b) hR
           (SkJ.sLbl G l) (nbr_lbl_tt l) (skj_code G a) (nbr_code a)))
      (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
      (wr_sn1 chkf G (Exp.lbl l) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))
         (Exp.prn ((litAt a) (+ off 1))) (codeTerm a) hL
         (SkJ.sLbl G l) (nbr_lbl_tt l)
         (SkJ.sPrn G ((litAt b) (+ (+ off 1) (cnodes a))) hLb) hnR)))
  (have hP (WRed chkf G
            (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
            (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))))
    (WRed.wrStep chkf G
      (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
      (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
      (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
      (WRed.wrRefl chkf G
        (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
        (SkJ.sPrn G (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))) hnode)
        (Eq.trans (nbr_prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
                  (nbr_lit (Code.sn l a b) off)))
      (step_hd chkf
        (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
        (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
        (Hd.prnN chkf (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
      (SkJ.sSnode G (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))
        (SkJ.sLbl G l) (SkJ.sPrn G ((litAt a) (+ off 1)) hLa) (SkJ.sPrn G ((litAt b) (+ (+ off 1) (cnodes a))) hLb))
      (Eq.trans (nbr_snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
        (andb_intro (nbr (Exp.lbl l))
          (Bool.and (nbr (Exp.prn ((litAt a) (+ off 1)))) (nbr (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))))
          (nbr_lbl_tt l) (andb_intro (nbr (Exp.prn ((litAt a) (+ off 1)))) (nbr (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a))))) hnL hnR)))))
  (exact (wr_cast_end chkf G
           (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
           (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b))
           (codeTerm (Code.sn l a b))
           ((wr_trans chkf G
              (Exp.snode (Exp.lbl l) (Exp.prn ((litAt a) (+ off 1))) (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))))
              (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)) hS)
            (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
            hP)
           (Eq.symm (codeTerm_sn l a b)))))


;; print (litAt v off) ι-reduces to ⌜v⌝ by Hd.prnL and Hd.prnN, in any token
;; context long enough for v's block and `extra` tokens after it.  This is
;; the ι stage of Proposition 4.2.
(thm wr_print [chkf :- (=> Code Code Bool), v :- Code]
  (forall [off Nat] (forall [extra Nat]
    (WRed chkf (skels (thetaD (+ off (+ (cnodes v) extra)))) (Exp.prn ((litAt v) off)) (codeTerm v))))
  (induction v)

  (intro off extra)
  (exact (wr_cast_end chkf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra))))
           (Exp.prn (Exp.leaf (Exp.lbl l))) (Exp.sleaf (Exp.lbl l)) (codeTerm (Code.sl l))
           (WRed.wrStep chkf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra))))
             (Exp.prn (Exp.leaf (Exp.lbl l))) (Exp.prn (Exp.leaf (Exp.lbl l))) (Exp.sleaf (Exp.lbl l))
             (WRed.wrRefl chkf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra))))
               (Exp.prn (Exp.leaf (Exp.lbl l)))
               (SkJ.sPrn (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) (Exp.leaf (Exp.lbl l))
                 (SkJ.sLeaf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) (Exp.lbl l)
                   (SkJ.sLbl (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) l)))
               (Eq.trans (nbr_prn (Exp.leaf (Exp.lbl l))) (Eq.trans (nbr_leaf (Exp.lbl l)) (nbr_lbl_tt l))))
             (step_hd chkf (Exp.prn (Exp.leaf (Exp.lbl l))) (Exp.sleaf (Exp.lbl l)) (Hd.prnL chkf (Exp.lbl l)))
             (SkJ.sSleaf (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) (Exp.lbl l)
               (SkJ.sLbl (skels (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))) l))
             (Eq.trans (nbr_sleaf (Exp.lbl l)) (nbr_lbl_tt l)))
           (Eq.symm (codeTerm_sl l))))

  (intro off extra)
  (have eGL (Eq (List Sk)
      (skels (thetaD (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra)))))
      (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))))
    (Eq.trans (congrArg (fn [n :- Nat] (skels (thetaD n))) (scope_add_l off (cnodes a) (cnodes b) extra))
      (congrArg (fn [n :- Nat] (skels (thetaD n)))
        (congrArg (fn [k :- Nat] (+ off (+ k extra))) (Eq.symm (cnodes_sn l a b))))))
  (have eGR (Eq (List Sk)
      (skels (thetaD (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra))))
      (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))))
    (Eq.trans (congrArg (fn [n :- Nat] (skels (thetaD n))) (scope_add_r off (cnodes a) (cnodes b) extra))
      (congrArg (fn [n :- Nat] (skels (thetaD n)))
        (congrArg (fn [k :- Nat] (+ off (+ k extra))) (Eq.symm (cnodes_sn l a b))))))
  (exact (wr_cast_start chkf (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
           (Exp.prn (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))))
           (Exp.prn ((litAt (Code.sn l a b)) off))
           (codeTerm (Code.sn l a b))
           (wr_prn_node chkf (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))) l a b off
             (wr_cast_G chkf
               (skels (thetaD (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra)))))
               (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
               (Exp.prn ((litAt a) (+ off 1))) (codeTerm a)
               (ih_a (+ off 1) (+ (cnodes b) extra)) eGL)
             (wr_cast_G chkf
               (skels (thetaD (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra))))
               (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
               (Exp.prn ((litAt b) (+ (+ off 1) (cnodes a)))) (codeTerm b)
               (ih_b (+ (+ off 1) (cnodes a)) extra) eGR)
             (skj_cast_tm Bool.false (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
               ((litAt (Code.sn l a b)) off)
               (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a))))
               Sk.cert (skj_lit (Code.sn l a b) off extra) (litAt_sn l a b off))
             (skj_cast_G Bool.false
               (skels (thetaD (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra)))))
               (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
               ((litAt a) (+ off 1)) Sk.cert
               (skj_lit a (+ off 1) (+ (cnodes b) extra)) eGL)
             (skj_cast_G Bool.false
               (skels (thetaD (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra))))
               (skels (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))))
               ((litAt b) (+ (+ off 1) (cnodes a))) Sk.cert
               (skj_lit b (+ (+ off 1) (cnodes a)) extra) eGR))
           (Eq.symm (congrArg Exp.prn (litAt_sn l a b off)))))

  )

(thm cv_cast_G [chkf :- (=> Code Code Bool), G :- (List Sk), G2 :- (List Sk), A :- Exp, B :- Exp,
                     h :- (Cv chkf G A B), eq :- (Eq (List Sk) G G2)]
  (Cv chkf G2 A B)
  (subst eq) (exact h))


;; Proposition 4.2's Cv chain at the innermost token: 1 ▷ T(tt) by Hd.tTT,
;; ▷ T(chk ⌜v⌝ ⌜A⌝) by Hd.delta using chkf v (encTy A) = tt, then back along
;; the print reduction to T(chk (print (lit v)) ⌜A⌝).
(thm cv_lit [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                  hck :- (Eq Bool (chkf v cA) Bool.true)]
  (Cv chkf (skels (thetaD (cnodes v))) Exp.tUnit (Exp.tT (Exp.chk (Exp.prn (lit0 v)) (codeTerm cA))))
  (exact (cv_cast_G chkf
           (skels (thetaD (+ 0 (+ (cnodes v) 0))))
           (skels (thetaD (cnodes v)))
           Exp.tUnit
           (Exp.tT (Exp.chk (Exp.prn (lit0 v)) (codeTerm cA)))
           ((cv_trans chkf (skels (thetaD (+ 0 (+ (cnodes v) 0))))
              (Exp.tT (Exp.chk (codeTerm v) (codeTerm cA)))
              (Exp.tT (Exp.chk (Exp.prn (lit0 v)) (codeTerm cA)))
              (wr_to_cv chkf (skels (thetaD (+ 0 (+ (cnodes v) 0))))
                (codeTerm cA) (Exp.prn (lit0 v)) (codeTerm v)
                (wr_print chkf v 0 0)
                (skj_code (skels (thetaD (+ 0 (+ (cnodes v) 0)))) cA)
                (nbr_code cA)))
            Exp.tUnit
            (cv_code_tt chkf (skels (thetaD (+ 0 (+ (cnodes v) 0)))) v cA hck))
           (congrArg (fn [n :- Nat] (skels (thetaD n))) (add0_bound (cnodes v))))))

(thm base_tR [] (Eq Bool (isBaseOrDia Exp.tR) Bool.true) (rfl))

(thm star_unit [] (Eq Bool (constTyped Exp.star Exp.tUnit) Bool.true) (rfl))


;; --- □A is a type, and (lit v, ⋆) inhabits it -------------------------------------------
;; ⌜c⌝ :⁰ Syn wherever every label is below NL: constTyped's test for ℓ : Lbl.
(thm tl_code [chkf :- (=> Code Code Bool), D :- (List Exp), c :- Code]
  (=> (Eq Bool (lblOk c) Bool.true) (Tl chkf Bool.false D (codeTerm c) Exp.tSyn))
  (induction c)
  (intro h)
  (exact (Tl.zSleaf chkf D (Exp.lbl l)
           (Tl.zConst chkf D (Exp.lbl l) Exp.tLbl (const_lbl l (Eq.trans (Eq.symm (lblOk_sl l)) h)))))
  (intro h)
  (have hok (Eq Bool (Bool.and (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b))) Bool.true)
    (Eq.trans (Eq.symm (lblOk_sn l a b)) h))
  (have hl (Eq Bool (Nat.blt l 100) Bool.true)
    (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (have hab (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
    (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (exact (tl_cast3 chkf Bool.false D
           (Exp.snode (Exp.lbl l) (codeTerm a) (codeTerm b)) (codeTerm (Code.sn l a b)) Exp.tSyn Exp.tSyn
           (Tl.zSnode chkf D (Exp.lbl l) (codeTerm a) (codeTerm b)
             (Tl.zConst chkf D (Exp.lbl l) Exp.tLbl (const_lbl l hl))
             (ih_a (band_left (lblOk a) (lblOk b) hab))
             (ih_b (band_right (lblOk a) (lblOk b) hab)))
           (Eq.symm (codeTerm_sn l a b)) rfl)))

(thm nthE_theta [i :- Nat]
  (forall [extra Nat]
    (Eq (Option Exp) (nthE (thetaD (+ i (Nat.succ extra))) i) (Option.some Exp Exp.tDia)))
  (induction i)
  (intro extra)
  (have e0 (Eq Nat (+ 0 (Nat.succ extra)) (Nat.succ extra)) (Nat.zero_add (Nat.succ extra)))
  (exact (Eq.trans (congrArg (fn [q :- Nat] (nthE (thetaD q) 0)) e0)
           (Eq.trans (congrArg (fn [D :- (List Exp)] (nthE D 0)) (thetaD_succ extra))
                     (nthE.eq_2 Exp.tDia (thetaD extra)))))
  (intro extra)
  (have eadd (Eq Nat (+ (Nat.succ n) (Nat.succ extra)) (Nat.succ (+ n (Nat.succ extra))))
    (Nat.succ_add n (Nat.succ extra)))
  (have hnth (Eq (Option Exp)
      (nthE (thetaD (Nat.succ (+ n (Nat.succ extra)))) (Nat.succ n))
      (nthE (thetaD (+ n (Nat.succ extra))) n))
    (Eq.trans (congrArg (fn [D :- (List Exp)] (nthE D (Nat.succ n))) (thetaD_succ (+ n (Nat.succ extra))))
              (nthE.eq_3 Exp.tDia (thetaD (+ n (Nat.succ extra))) n)))
  (exact (Eq.trans (congrArg (fn [q :- Nat] (nthE (thetaD q) (Nat.succ n))) eadd)
                   (Eq.trans hnth (ih_n extra)))))

(thm tl_cast_D [chkf :- (=> Code Code Bool), w :- Bool, D :- (List Exp), D2 :- (List Exp), t :- Exp, A :- Exp,
                     h :- (Tl chkf w D t A), eq :- (Eq (List Exp) D D2)]
  (Tl chkf w D2 t A)
  (subst eq) (exact h))

(thm tl_lit [chkf :- (=> Code Code Bool), v :- Code]
  (=> (Eq Bool (lblOk v) Bool.true)
      (forall [off Nat] (forall [extra Nat]
        (Tl chkf Bool.false (thetaD (+ off (+ (cnodes v) extra))) ((litAt v) off) Exp.tR))))
  (induction v)
  (intro h) (intro off) (intro extra)
  (exact (tl_cast3 chkf Bool.false (thetaD (+ off (+ (cnodes (Code.sl l)) extra)))
           (Exp.leaf (Exp.lbl l)) ((litAt (Code.sl l)) off) Exp.tR Exp.tR
           (Tl.zLeaf chkf (thetaD (+ off (+ (cnodes (Code.sl l)) extra))) (Exp.lbl l)
             (Tl.zConst chkf (thetaD (+ off (+ (cnodes (Code.sl l)) extra))) (Exp.lbl l) Exp.tLbl
               (const_lbl l (Eq.trans (Eq.symm (lblOk_sl l)) h))))
           (Eq.symm (litAt_sl l off)) rfl))
  (intro h) (intro off) (intro extra)
  (have hok (Eq Bool (Bool.and (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b))) Bool.true)
    (Eq.trans (Eq.symm (lblOk_sn l a b)) h))
  (have hl (Eq Bool (Nat.blt l 100) Bool.true)
    (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (have hab (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
    (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (have eL (Eq Nat (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra)))
                     (+ off (+ (cnodes (Code.sn l a b)) extra)))
    (Eq.trans (scope_add_l off (cnodes a) (cnodes b) extra)
      (congrArg (fn [k :- Nat] (+ off (+ k extra))) (Eq.symm (cnodes_sn l a b)))))
  (have eR (Eq Nat (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra))
                     (+ off (+ (cnodes (Code.sn l a b)) extra)))
    (Eq.trans (scope_add_r off (cnodes a) (cnodes b) extra)
      (congrArg (fn [k :- Nat] (+ off (+ k extra))) (Eq.symm (cnodes_sn l a b)))))
  (have eTok (Eq Nat (+ off (+ (cnodes (Code.sn l a b)) extra))
                      (+ off (Nat.succ (+ (+ (cnodes a) (cnodes b)) extra))))
    (congrArg (fn [k :- Nat] (+ off k))
      (Eq.trans (congrArg (fn [k :- Nat] (+ k extra)) (cnodes_sn l a b))
        (Eq.trans (congrArg (fn [k :- Nat] (+ k extra)) (add1_succ (+ (cnodes a) (cnodes b))))
                  (Nat.succ_add (+ (cnodes a) (cnodes b)) extra)))))
  (have hlook (Eq (Option Exp) (nthE (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))) off)
                                  (Option.some Exp Exp.tDia))
    (Eq.trans (congrArg (fn [q :- Nat] (nthE (thetaD q) off)) eTok)
              (nthE_theta off (+ (+ (cnodes a) (cnodes b)) extra))))
  (have hvar (Tl chkf Bool.false (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))) (Exp.var off) Exp.tDia)
    (tl_cast chkf Bool.false (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))) (Exp.var off)
      (lift (+ off 1) 0 Exp.tDia) Exp.tDia
      (Tl.zVar chkf (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))) off Exp.tDia hlook)
      (closedTy_lift Exp.tDia closed_tDia (+ off 1) 0)))
  (exact (tl_cast3 chkf Bool.false (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))
           (Exp.node (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a))))
           ((litAt (Code.sn l a b)) off) Exp.tR Exp.tR
           (Tl.zNode chkf (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))
             (Exp.var off) (Exp.lbl l) ((litAt a) (+ off 1)) ((litAt b) (+ (+ off 1) (cnodes a)))
             hvar
             (Tl.zConst chkf (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra))) (Exp.lbl l) Exp.tLbl (const_lbl l hl))
             (tl_cast_D chkf Bool.false
               (thetaD (+ (+ off 1) (+ (cnodes a) (+ (cnodes b) extra))))
               (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))
               ((litAt a) (+ off 1)) Exp.tR
               ((ih_a (band_left (lblOk a) (lblOk b) hab)) (+ off 1) (+ (cnodes b) extra))
               (congrArg thetaD eL))
             (tl_cast_D chkf Bool.false
               (thetaD (+ (+ (+ off 1) (cnodes a)) (+ (cnodes b) extra)))
               (thetaD (+ off (+ (cnodes (Code.sn l a b)) extra)))
               ((litAt b) (+ (+ off 1) (cnodes a))) Exp.tR
               ((ih_b (band_right (lblOk a) (lblOk b) hab)) (+ (+ off 1) (cnodes a)) extra)
               (congrArg thetaD eR)))
           (litAt_sn l a b off) rfl)))

(thm subst_ev [u :- Exp, ca :- Exp]
  (Eq Exp (subst1 u (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) ca)))
          (Exp.tT (Exp.chk (Exp.prn u) (subst1 u ca))))
  (rfl))

(thm subst_ev_code [u :- Exp, cA :- Code]
  (Eq Exp (subst1 u (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
          (evTy u (codeTerm cA)))
  (exact (Eq.trans (subst_ev u (codeTerm cA))
           (congrArg (fn [c :- Exp] (Exp.tT (Exp.chk (Exp.prn u) c))) (subst_code u cA)))))

(thm tl_box_body [chkf :- (=> Code Code Bool), D :- (List Exp), cA :- Code,
                       hA :- (Eq Bool (lblOk cA) Bool.true)]
  (Tl chkf Bool.true (List.cons Exp Exp.tR D)
      (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))) Exp.tUnit)
  (exact (Tl.fT chkf (List.cons Exp Exp.tR D) (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))
           (Tl.zChk chkf (List.cons Exp Exp.tR D) (Exp.prn (Exp.var 0)) (codeTerm cA)
             (Tl.zPrn chkf (List.cons Exp Exp.tR D) (Exp.var 0)
               (tl_cast chkf Bool.false (List.cons Exp Exp.tR D) (Exp.var 0)
                 (lift 1 0 Exp.tR) Exp.tR
                 (Tl.zVar chkf (List.cons Exp Exp.tR D) 0 Exp.tR (nthE.eq_2 Exp.tR D))
                 (closedTy_lift Exp.tR closed_tR 1 0)))
             ((tl_code chkf (List.cons Exp Exp.tR D) cA) hA)))))

(thm tl_box_body [chkf :- (=> Code Code Bool), D :- (List Exp), cA :- Code,
                       hA :- (Eq Bool (lblOk cA) Bool.true)]
  (Tl chkf Bool.true (List.cons Exp Exp.tR D)
      (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))) Exp.tUnit)
  (exact (Tl.fT chkf (List.cons Exp Exp.tR D) (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))
           (Tl.zChk chkf (List.cons Exp Exp.tR D) (Exp.prn (Exp.var 0)) (codeTerm cA)
             (Tl.zPrn chkf (List.cons Exp Exp.tR D) (Exp.var 0)
               (tl_cast chkf Bool.false (List.cons Exp Exp.tR D) (Exp.var 0)
                 (lift 1 0 Exp.tR) Exp.tR
                 (Tl.zVar chkf (List.cons Exp Exp.tR D) 0 Exp.tR (nthE.eq_2 Exp.tR D))
                 (closedTy_lift Exp.tR closed_tR 1 0)))
             ((tl_code chkf (List.cons Exp Exp.tR D) cA) hA)))))

(thm tl_ev [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                 hok :- (Eq Bool (lblOk v) Bool.true), hA :- (Eq Bool (lblOk cA) Bool.true)]
  (Tl chkf Bool.true (thetaD (cnodes v)) (evTy (lit0 v) (codeTerm cA)) Exp.tUnit)
  (exact (Tl.fT chkf (thetaD (cnodes v)) (Exp.chk (Exp.prn (lit0 v)) (codeTerm cA))
           (Tl.zChk chkf (thetaD (cnodes v)) (Exp.prn (lit0 v)) (codeTerm cA)
             (Tl.zPrn chkf (thetaD (cnodes v)) (lit0 v)
               (tl_cast_D chkf Bool.false
                 (thetaD (+ 0 (+ (cnodes v) 0))) (thetaD (cnodes v)) (lit0 v) Exp.tR
                 (((tl_lit chkf v) hok) 0 0)
                 (congrArg thetaD (add0_bound (cnodes v)))))
             ((tl_code chkf (thetaD (cnodes v)) cA) hA)))))

(thm rt_star [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                  hok :- (Eq Bool (lblOk v) Bool.true),
                  hA :- (Eq Bool (lblOk cA) Bool.true),
                  hck :- (Eq Bool (chkf v cA) Bool.true)]
  (Rt chkf (thetaD (cnodes v)) (vzero (cnodes v)) Exp.star
      (subst1 (lit0 v) (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))))
  (exact (rt_cast chkf (thetaD (cnodes v)) (vzero (cnodes v)) (vzero (cnodes v)) Exp.star
           (evTy (lit0 v) (codeTerm cA))
           (subst1 (lit0 v) (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
           (Rt.rConv chkf (thetaD (cnodes v)) (vzero (cnodes v)) Exp.star Exp.tUnit
             (evTy (lit0 v) (codeTerm cA))
             (Rt.rConst chkf (thetaD (cnodes v)) (vzero (cnodes v)) Exp.star Exp.tUnit
               (Eq.trans (lenU_vzero (cnodes v)) (Eq.symm (lenE_theta (cnodes v))))
               star_unit)
             (tl_ev chkf v cA hok hA)
             (cv_lit chkf v cA hck))
           rfl
           (Eq.symm (subst_ev_code (lit0 v) cA)))))

(thm us_pair [n :- Nat]
  (Eq (List U) (vadd (vscale U.u1 (thetaU n)) (vzero n)) (thetaU n))
  (exact (Eq.trans (congrArg (fn [us :- (List U)] (vadd us (vzero n))) (vscale_one (thetaU n)))
                   (vadd_theta_zero n))))

(thm certTerm_unfold [v :- Code, cA :- Code]
  (Eq Exp (certTerm v (codeTerm cA))
      (Exp.pair (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
                (lit0 v) Exp.star))
  (rfl))

(thm boxTy_unfold [cA :- Code]
  (Eq Exp (boxTy (codeTerm cA))
      (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))))
  (rfl))

(thm prop42_raw [chkf :- (=> Code Code Bool), v :- Code, cA :- Code,
                     hok :- (Eq Bool (lblOk v) Bool.true),
                     hA :- (Eq Bool (lblOk cA) Bool.true),
                     hck :- (Eq Bool (chkf v cA) Bool.true)]
  (Rt chkf (thetaD (cnodes v))
      (vadd (vscale U.u1 (thetaU (cnodes v))) (vzero (cnodes v)))
      (Exp.pair (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA))))
                (lit0 v) Exp.star)
      (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))))
  (exact (Rt.rPair chkf (thetaD (cnodes v)) (thetaU (cnodes v)) (vzero (cnodes v)) U.u1
           Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm cA)))
           (lit0 v) Exp.star
           nonzero_u1
           (Tl.fBase chkf (thetaD (cnodes v)) Exp.tR base_tR)
           (tl_box_body chkf (thetaD (cnodes v)) cA hA)
           ((rt_lit chkf v) hok)
           (rt_star chkf v cA hok hA hck))))


;; Proposition 4.2 (R4-metatheory.md §4.2), for an arbitrary checker, decoder
;; and encoder.  If v is a certificate of A — its labels lie in L, so do the
;; labels of encTy A, and chkf accepts v at that code — then
;; Θ_{‖v‖} ⊢ (lit v, ⋆) :¹ □A.  The decoder is not used.  Pair is at usage 1:
;; lit v takes every token once, and ⋆, typed at 1 by the axiom, converts
;; along cv_lit and may carry the zero usage.
(thm prop42 [chkf :- (=> Code Code Bool),
                  dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                  encTy :- (=> Exp Code),
                  A :- Exp, v :- Code,
                  hok :- (Eq Bool (lblOk v) Bool.true),
                  hA :- (Eq Bool (lblOk (encTy A)) Bool.true),
                  hck :- (Eq Bool (chkf v (encTy A)) Bool.true)]
  (Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v))
      (certTerm v (codeTerm (encTy A))) (boxTy (codeTerm (encTy A))))
  (exact (rt_reindex chkf
           (thetaD (cnodes v))
           (vadd (vscale U.u1 (thetaU (cnodes v))) (vzero (cnodes v)))
           (Exp.pair (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm (encTy A)))))
                     (lit0 v) Exp.star)
           (Exp.tSig U.u1 Exp.tR (Exp.tT (Exp.chk (Exp.prn (Exp.var 0)) (codeTerm (encTy A)))))
           (thetaD (cnodes v)) (thetaU (cnodes v))
           (certTerm v (codeTerm (encTy A))) (boxTy (codeTerm (encTy A)))
           (prop42_raw chkf v (encTy A) hok hA hck)
           rfl (us_pair (cnodes v))
           (Eq.symm (certTerm_unfold v (encTy A)))
           (Eq.symm (boxTy_unfold (encTy A))))))


;; --- Proposition 4.10 (R4-metatheory.md §4.9) ---------------------------------------------
;; neg c⊥ is ⌜0 ⊸ 0⌝.  CheckSpec's base-code clause reads ⌜0⌝ off c⊥, and E5
;; builds the arrow code; codeTerm of that code is neg c⊥.
(thm some_inj_code [x :- Code, y :- Code, h :- (Eq (Option Code) (Option.some Code x) (Option.some Code y))]
  (Eq Code x y) (cases h) (rfl))

(thm code_cbot [] (Eq (Option Code) (codeOf (cbot)) (Option.some Code (Code.sl 15))) (rfl))

(thm base_empty [] (Eq (Option Exp) (baseCode Exp.tEmpty) (Option.some Exp (cbot))) (rfl))

(thm closed_empty [] (Eq Bool (closedTy Exp.tEmpty) Bool.true) (rfl))

(thm enc_empty [chkf :- (=> Code Code Bool),
                     dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                     encTy :- (=> Exp Code),
                     hspec :- (CheckSpec chkf dec encTy)]
  (Eq Code (encTy Exp.tEmpty) (Code.sl 15))
  (have h (Eq (Option Code) (codeOf (cbot)) (Option.some Code (encTy Exp.tEmpty)))
    (((And.left (And.right hspec)) Exp.tEmpty (cbot)) base_empty))
  (exact (Eq.symm (some_inj_code (Code.sl 15) (encTy Exp.tEmpty) (Eq.trans (Eq.symm code_cbot) h)))))

(thm neg_unfold []
  (Eq Exp (negT (cbot)) (Exp.snode (Exp.lbl 25) (Exp.sleaf (Exp.lbl 15)) (Exp.sleaf (Exp.lbl 15))))
  (rfl))

(thm enc_neg [chkf :- (=> Code Code Bool),
                   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                   encTy :- (=> Exp Code),
                   hspec :- (CheckSpec chkf dec encTy)]
  (Eq Code (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)) (Code.sn 25 (Code.sl 15) (Code.sl 15)))
  (have h (Eq Code (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)) (Code.sn 25 (encTy Exp.tEmpty) (Code.sl 15)))
    (((And.left (And.right (And.right hspec))) Exp.tEmpty) closed_empty))
  (exact (Eq.trans h (congrArg (fn [c :- Code] (Code.sn 25 c (Code.sl 15))) (enc_empty chkf dec encTy hspec)))))

(thm neg_cbot [chkf :- (=> Code Code Bool),
                    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code),
                    hspec :- (CheckSpec chkf dec encTy)]
  (Eq Exp (negT (cbot)) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))
  (have hcode (Eq Exp (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))
                      (codeTerm (Code.sn 25 (Code.sl 15) (Code.sl 15))))
    (congrArg codeTerm (enc_neg chkf dec encTy hspec)))
  (have hunf (Eq Exp (codeTerm (Code.sn 25 (Code.sl 15) (Code.sl 15)))
                   (Exp.snode (Exp.lbl 25) (Exp.sleaf (Exp.lbl 15)) (Exp.sleaf (Exp.lbl 15))))
    (Eq.trans (codeTerm_sn 25 (Code.sl 15) (Code.sl 15))
      (Eq.trans (congrArg (fn [a :- Exp] (Exp.snode (Exp.lbl 25) a (codeTerm (Code.sl 15)))) (codeTerm_sl 15))
                (congrArg (fn [b :- Exp] (Exp.snode (Exp.lbl 25) (Exp.sleaf (Exp.lbl 15)) b)) (codeTerm_sl 15)))))
  (exact (Eq.trans neg_unfold (Eq.symm (Eq.trans hcode hunf)))))

(thm lift_unit [k :- Nat] (Eq Exp (lift k 0 Exp.tUnit) Exp.tUnit) (rfl))

(thm lift_ev [k :- Nat, t :- Exp, ca :- Exp]
  (Eq Exp (lift k 0 (evTy t ca)) (evTy (lift k 0 t) (lift k 0 ca))) (rfl))

(thm lift_chkT [k :- Nat, r :- Exp, c :- Exp]
  (Eq Exp (lift k 0 (chkT r c)) (chkT (lift k 0 r) (lift k 0 c))) (rfl))

(thm lit_lift2 [v :- Code]
  (Eq Exp (lift 2 0 (lit0 v)) ((litAt v) 2))
  (exact (Eq.trans (congrArg (fn [t :- Exp] (lift 2 0 t)) (lit0_at v))
           (Eq.trans (lit_shift v 0 2) (congrArg (fn [i :- Nat] ((litAt v) i)) (Nat.zero_add 2))))))

(thm vscale_vzero [r :- U, n :- Nat] (Eq (List U) (vscale r (vzero n)) (vzero n))
  (induction n) (rfl)
  (exact (Eq.trans (congrArg (fn [us :- (List U)] (List.cons U (umul r U.u0) us)) ih_n)
                   (congrArg (fn [a :- U] (List.cons U a (vzero n))) (umul_zero_right r)))))

(thm two [] (Eq Nat (+ 1 1) 2) (omega))

(thm lift_cbot [k :- Nat] (Eq Exp (lift k 0 (cbot)) (cbot)) (rfl))

(thm lift_var0 [k :- Nat]
  (Eq Exp (lift k 0 (Exp.var 0)) (Exp.var k))
  (exact (Eq.trans (lift_var_above k 0 0 (le_refl 0)) (congrArg Exp.var (Nat.zero_add k)))))

(thm chkT_shift []
  (Eq Exp (lift 1 0 (chkT (Exp.var 0) (cbot))) (chkT (Exp.var 1) (cbot)))
  (exact (Eq.trans (lift_chkT 1 (Exp.var 0) (cbot))
           (Eq.trans (congrArg (fn [r :- Exp] (chkT r (lift 1 0 (cbot)))) (lift_var0 1))
                     (congrArg (fn [c :- Exp] (chkT (Exp.var 1) c)) (lift_cbot 1))))))

(thm rt_lit2 [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true)]
  (Rt chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
      ((litAt v) 2) Exp.tR)
  (have h1 (Rt chkf (List.cons Exp Exp.tR (thetaD (cnodes v)))
               (List.cons U U.u0 (thetaU (cnodes v)))
               (lift 1 0 (lit0 v)) Exp.tR)
    (rt_cast chkf (insD 0 Exp.tR (thetaD (cnodes v)))
      (insU 0 U.u0 (thetaU (cnodes v)))
      (List.cons U U.u0 (thetaU (cnodes v)))
      (lift 1 0 (lit0 v))
      (lift 1 0 Exp.tR) Exp.tR
      (((rt_weaken chkf (thetaD (cnodes v)) (thetaU (cnodes v)) (lit0 v) Exp.tR ((rt_lit chkf v) hok)) 0) Exp.tR)
      (insU_zero U.u0 (thetaU (cnodes v)))
      (closedTy_lift Exp.tR closed_tR 1 0)))
  (have h2 (Rt chkf
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
             (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
             (lift 1 0 (lift 1 0 (lit0 v))) Exp.tR)
    (rt_cast chkf
      (insD 0 (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (insU 0 U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
      (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
      (lift 1 0 (lift 1 0 (lit0 v)))
      (lift 1 0 Exp.tR) Exp.tR
      (((rt_weaken chkf (List.cons Exp Exp.tR (thetaD (cnodes v)))
           (List.cons U U.u0 (thetaU (cnodes v)))
           (lift 1 0 (lit0 v)) Exp.tR h1) 0) (chkT (Exp.var 0) (cbot)))
      (insU_zero U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
      (closedTy_lift Exp.tR closed_tR 1 0)))
  (exact (rt_cast_t chkf
           (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
           (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
           (lift 1 0 (lift 1 0 (lit0 v))) ((litAt v) 2) Exp.tR Exp.tR h2
           (Eq.trans (lift_comp (lit0 v) 1 1 0)
             (Eq.trans (congrArg (fn [k :- Nat] (lift k 0 (lit0 v))) two) (lit_lift2 v)))
           rfl)))

(thm lbl_arrow []
  (Eq Bool (lblOk (Code.sn 25 (Code.sl 15) (Code.sl 15))) Bool.true) (rfl))

(thm lbl15 [] (Eq Bool (constTyped (Exp.lbl 15) Exp.tLbl) Bool.true) (rfl))

(thm nth_r [n :- Nat, A :- Exp]
  (Eq (Option Exp) (nthE (List.cons Exp A (List.cons Exp Exp.tR (thetaD n))) 1) (Option.some Exp Exp.tR))
  (exact (Eq.trans (nthE.eq_3 A (List.cons Exp Exp.tR (thetaD n)) 0) (nthE.eq_2 Exp.tR (thetaD n)))))

(thm nth_e [n :- Nat, A :- Exp]
  (Eq (Option Exp) (nthE (List.cons Exp A (List.cons Exp Exp.tR (thetaD n))) 0) (Option.some Exp A))
  (exact (nthE.eq_2 A (List.cons Exp Exp.tR (thetaD n)))))

(thm len_pad [n :- Nat, A :- Exp]
  (Eq Nat (lenU (List.cons U U.u0 (List.cons U U.u1 (vzero n))))
          (lenE (List.cons Exp A (List.cons Exp Exp.tR (thetaD n)))))
  (exact (Eq.trans
           (Eq.trans (lenU_cons U.u0 (List.cons U U.u1 (vzero n)))
             (Eq.trans (congrArg Nat.succ (lenU_cons U.u1 (vzero n)))
                       (congrArg (fn [k :- Nat] (Nat.succ (Nat.succ k))) (lenU_vzero n))))
           (Eq.symm
             (Eq.trans (lenE_cons A (List.cons Exp Exp.tR (thetaD n)))
               (Eq.trans (congrArg Nat.succ (lenE_cons Exp.tR (thetaD n)))
                         (congrArg (fn [k :- Nat] (Nat.succ (Nat.succ k))) (lenE_theta n))))))))

(thm rt_binder_r [chkf :- (=> Code Code Bool), n :- Nat, A :- Exp]
  (Rt chkf (List.cons Exp A (List.cons Exp Exp.tR (thetaD n)))
      (List.cons U U.u0 (List.cons U U.u1 (vzero n))) (Exp.var 1) Exp.tR)
  (exact (rt_cast chkf (List.cons Exp A (List.cons Exp Exp.tR (thetaD n)))
           (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
           (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
           (Exp.var 1) (lift (+ 1 1) 0 Exp.tR) Exp.tR
           (Rt.rVar chkf (List.cons Exp A (List.cons Exp Exp.tR (thetaD n)))
             (List.cons U U.u0 (List.cons U U.u1 (vzero n))) 1 Exp.tR U.u1
             (len_pad n A) (nth_r n A)
             (Eq.trans (nthU.eq_3 U.u0 (List.cons U U.u1 (vzero n)) 0) (nthU.eq_2 U.u1 (vzero n)))
             nonzero_u1)
           rfl (closedTy_lift Exp.tR closed_tR (+ 1 1) 0))))

(thm rt_binder_e [chkf :- (=> Code Code Bool), n :- Nat]
  (Rt chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
      (List.cons U U.u1 (List.cons U U.u0 (vzero n))) (Exp.var 0) (chkT (Exp.var 1) (cbot)))
  (exact (rt_cast chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
           (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
           (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
           (Exp.var 0) (lift 1 0 (chkT (Exp.var 0) (cbot))) (chkT (Exp.var 1) (cbot))
           (Rt.rVar chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
             (List.cons U U.u1 (List.cons U U.u0 (vzero n))) 0 (chkT (Exp.var 0) (cbot)) U.u1
             (len_pad n (chkT (Exp.var 0) (cbot)))
             (nth_e n (chkT (Exp.var 0) (cbot)))
             (nthU.eq_2 U.u1 (List.cons U U.u0 (vzero n)))
             nonzero_u1)
           rfl chkT_shift)))

(thm vadd_zero_zero [n :- Nat] (Eq (List U) (vadd (vzero n) (vzero n)) (vzero n))
  (induction n) (rfl)
  (exact (congrArg (fn [us :- (List U)] (List.cons U U.u0 us)) ih_n)))

(thm uadd_e_head [] (Eq U (uadd U.u0 (uadd U.u0 (uadd U.u0 (uadd U.u1 U.u0)))) U.u1) (rfl))

(thm uadd_r_head [] (Eq U (uadd U.u1 (uadd U.u0 (uadd U.u0 (uadd U.u0 U.u0)))) U.u1) (rfl))

(thm vadd_cons5 [a :- U, b :- U, c :- U, d :- U, e :- U,
                      xa :- (List U), xb :- (List U), xc :- (List U), xd :- (List U), xe :- (List U)]
  (Eq (List U)
    (vadd (List.cons U a xa)
      (vadd (List.cons U b xb)
        (vadd (List.cons U c xc)
          (vadd (List.cons U d xd) (List.cons U e xe)))))
    (List.cons U (uadd a (uadd b (uadd c (uadd d e))))
      (vadd xa (vadd xb (vadd xc (vadd xd xe))))))
  (rfl))

(thm uadd_tail_head [] (Eq U (uadd U.u0 (uadd U.u1 (uadd U.u0 (uadd U.u0 U.u0)))) U.u1) (rfl))

(thm h1_tail [n :- Nat]
  (Eq (List U)
    (vadd (vzero n) (vadd (thetaU n) (vadd (vzero n) (vadd (vzero n) (vzero n)))))
    (thetaU n))
  (induction n) (rfl)
  (have hC (Eq (List U)
      (vadd (List.cons U U.u0 (vzero n))
        (vadd (List.cons U U.u1 (thetaU n))
          (vadd (List.cons U U.u0 (vzero n))
            (vadd (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n))))))
      (List.cons U U.u1 (vadd (vzero n) (vadd (thetaU n) (vadd (vzero n) (vadd (vzero n) (vzero n)))))))
    (Eq.trans (vadd_cons5 U.u0 U.u1 U.u0 U.u0 U.u0 (vzero n) (thetaU n) (vzero n) (vzero n) (vzero n))
              (congrArg (fn [r :- U] (List.cons U r (vadd (vzero n) (vadd (thetaU n) (vadd (vzero n) (vadd (vzero n) (vzero n)))))))
                        uadd_tail_head)))
  (exact (Eq.trans
           (congrArg (fn [us :- (List U)]
                       (vadd us (vadd (thetaU (Nat.succ n)) (vadd (vzero (Nat.succ n)) (vadd (vzero (Nat.succ n)) (vzero (Nat.succ n)))))))
                     (vzero_succ n))
           (Eq.trans
             (congrArg (fn [us :- (List U)]
                         (vadd (List.cons U U.u0 (vzero n))
                           (vadd us (vadd (vzero (Nat.succ n)) (vadd (vzero (Nat.succ n)) (vzero (Nat.succ n)))))))
                       (thetaU_succ n))
             (Eq.trans
               (congrArg (fn [us :- (List U)]
                           (vadd (List.cons U U.u0 (vzero n))
                             (vadd (List.cons U U.u1 (thetaU n))
                               (vadd us (vadd (vzero (Nat.succ n)) (vzero (Nat.succ n)))))))
                         (vzero_succ n))
               (Eq.trans
                 (congrArg (fn [us :- (List U)]
                             (vadd (List.cons U U.u0 (vzero n))
                               (vadd (List.cons U U.u1 (thetaU n))
                                 (vadd (List.cons U U.u0 (vzero n)) (vadd us (vzero (Nat.succ n)))))))
                           (vzero_succ n))
                 (Eq.trans
                   (congrArg (fn [us :- (List U)]
                               (vadd (List.cons U U.u0 (vzero n))
                                 (vadd (List.cons U U.u1 (thetaU n))
                                   (vadd (List.cons U U.u0 (vzero n))
                                     (vadd (List.cons U U.u0 (vzero n)) us)))))
                             (vzero_succ n))
                   (Eq.trans hC
                     (Eq.trans (congrArg (fn [us :- (List U)] (List.cons U U.u1 us)) ih_n)
                               (Eq.symm (thetaU_succ n)))))))))))

(thm h1_us [n :- Nat]
  (Eq (List U)
    (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
      (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
        (vadd (vscale U.uw (vzero (Nat.succ (Nat.succ n))))
          (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
                (vzero (Nat.succ (Nat.succ n)))))))
    (List.cons U U.u1 (List.cons U U.u1 (thetaU n))))
  (have z (Eq (List U) (vzero (Nat.succ (Nat.succ n))) (List.cons U U.u0 (List.cons U U.u0 (vzero n))))
    (Eq.trans (vzero_succ (Nat.succ n)) (congrArg (fn [us :- (List U)] (List.cons U U.u0 us)) (vzero_succ n))))
  (have hS (Eq (List U)
      (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
        (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
          (vadd (vscale U.uw (vzero (Nat.succ (Nat.succ n))))
            (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
                  (vzero (Nat.succ (Nat.succ n)))))))
      (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
        (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
          (vadd (List.cons U U.u0 (List.cons U U.u0 (vzero n)))
            (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
                  (List.cons U U.u0 (List.cons U U.u0 (vzero n))))))))
    (Eq.trans
      (congrArg (fn [us :- (List U)]
                  (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
                    (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
                      (vadd us (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
                                     (vzero (Nat.succ (Nat.succ n))))))))
                (Eq.trans (vscale_vzero U.uw (Nat.succ (Nat.succ n))) z))
      (congrArg (fn [us :- (List U)]
                  (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
                    (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
                      (vadd (List.cons U U.u0 (List.cons U U.u0 (vzero n)))
                        (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n))) us)))))
                z)))
  (have h1 (Eq (List U)
      (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero n)))
        (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU n)))
          (vadd (List.cons U U.u0 (List.cons U U.u0 (vzero n)))
            (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero n)))
                  (List.cons U U.u0 (List.cons U U.u0 (vzero n)))))))
      (List.cons U U.u1
        (vadd (List.cons U U.u1 (vzero n))
          (vadd (List.cons U U.u0 (thetaU n))
            (vadd (List.cons U U.u0 (vzero n))
              (vadd (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n))))))))
    (Eq.trans (vadd_cons5 U.u0 U.u0 U.u0 U.u1 U.u0
                (List.cons U U.u1 (vzero n)) (List.cons U U.u0 (thetaU n))
                (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n)))
              (congrArg (fn [r :- U]
                          (List.cons U r
                            (vadd (List.cons U U.u1 (vzero n))
                              (vadd (List.cons U U.u0 (thetaU n))
                                (vadd (List.cons U U.u0 (vzero n))
                                  (vadd (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n))))))))
                        uadd_e_head)))
  (have h2 (Eq (List U)
      (List.cons U U.u1
        (vadd (List.cons U U.u1 (vzero n))
          (vadd (List.cons U U.u0 (thetaU n))
            (vadd (List.cons U U.u0 (vzero n))
              (vadd (List.cons U U.u0 (vzero n)) (List.cons U U.u0 (vzero n)))))))
      (List.cons U U.u1 (List.cons U U.u1
        (vadd (vzero n) (vadd (thetaU n) (vadd (vzero n) (vadd (vzero n) (vzero n))))))))
    (congrArg (fn [us :- (List U)] (List.cons U U.u1 us))
      (Eq.trans (vadd_cons5 U.u1 U.u0 U.u0 U.u0 U.u0 (vzero n) (thetaU n) (vzero n) (vzero n) (vzero n))
                (congrArg (fn [r :- U] (List.cons U r (vadd (vzero n) (vadd (thetaU n) (vadd (vzero n) (vadd (vzero n) (vzero n)))))))
                          uadd_r_head))))
  (exact (Eq.trans hS (Eq.trans h1 (Eq.trans h2
           (congrArg (fn [us :- (List U)] (List.cons U U.u1 (List.cons U U.u1 us))) (h1_tail n)))))))

(thm cv_cast_end [chkf :- (=> Code Code Bool), G :- (List Sk), A :- Exp, B :- Exp, B2 :- Exp,
                       h :- (Cv chkf G A B), eq :- (Eq Exp B B2)]
  (Cv chkf G A B2) (subst eq) (exact h))

(thm cv_cast_start [chkf :- (=> Code Code Bool), G :- (List Sk), A :- Exp, A2 :- Exp, B :- Exp,
                         h :- (Cv chkf G A B), eq :- (Eq Exp A A2)]
  (Cv chkf G A2 B) (subst eq) (exact h))

(thm lbl_neg [chkf :- (=> Code Code Bool),
                   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                   encTy :- (=> Exp Code),
                   hspec :- (CheckSpec chkf dec encTy)]
  (Eq Bool (lblOk (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) Bool.true)
  (exact (Eq.trans (congrArg lblOk (enc_neg chkf dec encTy hspec)) lbl_arrow)))

(thm ev_arrow [chkf :- (=> Code Code Bool),
                    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code),
                    hspec :- (CheckSpec chkf dec encTy),
                    v :- Code]
  (Eq Exp (lift 1 0 (lift 1 0 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))))
          (evTy ((litAt v) 2) (negT (cbot))))
  (exact (Eq.trans (lift_comp (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))) 1 1 0)
           (Eq.trans (congrArg (fn [k :- Nat] (lift k 0 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))))) two)
             (Eq.trans (lift_ev 2 (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))
               (Eq.trans (congrArg (fn [t :- Exp] (evTy t (lift 2 0 (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))))) (lit_lift2 v))
                 (Eq.trans (congrArg (fn [c :- Exp] (evTy ((litAt v) 2) c)) ((lift_code (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) 2 0))
                           (congrArg (fn [c :- Exp] (evTy ((litAt v) 2) c)) (Eq.symm (neg_cbot chkf dec encTy hspec))))))))))

(thm unit_arrow []
  (Eq Exp (lift 1 0 (lift 1 0 Exp.tUnit)) Exp.tUnit)
  (exact (Eq.trans (congrArg (fn [t :- Exp] (lift 1 0 t)) (lift_unit 1)) (lift_unit 1))))

(thm cv_arrow [chkf :- (=> Code Code Bool),
                    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                    encTy :- (=> Exp Code),
                    hspec :- (CheckSpec chkf dec encTy),
                    v :- Code,
                    hck :- (Eq Bool (chkf v (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) Bool.true)]
  (Cv chkf (skels (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v)))))
      Exp.tUnit (evTy ((litAt v) 2) (negT (cbot))))
  (exact (cv_cast_end chkf
           (skels (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v)))))
           Exp.tUnit
           (lift 1 0 (lift 1 0 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))))
           (evTy ((litAt v) 2) (negT (cbot)))
           (cv_cast_start chkf
             (skels (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v)))))
             (lift 1 0 (lift 1 0 Exp.tUnit)) Exp.tUnit
             (lift 1 0 (lift 1 0 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))))
             (cv_weaken chkf (List.cons Exp Exp.tR (thetaD (cnodes v)))
               (lift 1 0 Exp.tUnit)
               (lift 1 0 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)))))
               (cv_weaken chkf (thetaD (cnodes v)) Exp.tUnit
                 (evTy (lit0 v) (codeTerm (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))))
                 (cv_lit chkf v (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty)) hck)
                 0 Exp.tR)
               0 (chkT (Exp.var 0) (cbot)))
             unit_arrow)
           (ev_arrow chkf dec encTy hspec v))))

(thm lbl25 [] (Eq Bool (constTyped (Exp.lbl 25) Exp.tLbl) Bool.true) (rfl))

(thm tl_lit2 [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true)]
  (Tl chkf Bool.false
      (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      ((litAt v) 2) Exp.tR)
  (have h0 (Tl chkf Bool.false (thetaD (cnodes v)) (lit0 v) Exp.tR)
    (tl_cast_D chkf Bool.false (thetaD (+ 0 (+ (cnodes v) 0))) (thetaD (cnodes v)) (lit0 v) Exp.tR
      (((tl_lit chkf v) hok) 0 0) (congrArg thetaD (add0_bound (cnodes v)))))
  (have h1 (Tl chkf Bool.false (List.cons Exp Exp.tR (thetaD (cnodes v))) (lift 1 0 (lit0 v)) Exp.tR)
    (tl_cast3 chkf Bool.false (List.cons Exp Exp.tR (thetaD (cnodes v)))
      (lift 1 0 (lit0 v)) (lift 1 0 (lit0 v)) (lift 1 0 Exp.tR) Exp.tR
      (((tl_weaken chkf Bool.false (thetaD (cnodes v)) (lit0 v) Exp.tR h0) 0) Exp.tR)
      rfl (closedTy_lift Exp.tR closed_tR 1 0)))
  (have h2 (Tl chkf Bool.false
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
             (lift 1 0 (lift 1 0 (lit0 v))) Exp.tR)
    (tl_cast3 chkf Bool.false
      (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (lift 1 0 (lift 1 0 (lit0 v))) (lift 1 0 (lift 1 0 (lit0 v))) (lift 1 0 Exp.tR) Exp.tR
      (((tl_weaken chkf Bool.false (List.cons Exp Exp.tR (thetaD (cnodes v))) (lift 1 0 (lit0 v)) Exp.tR h1) 0)
        (chkT (Exp.var 0) (cbot)))
      rfl (closedTy_lift Exp.tR closed_tR 1 0)))
  (exact (tl_cast3 chkf Bool.false
           (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
           (lift 1 0 (lift 1 0 (lit0 v))) ((litAt v) 2) Exp.tR Exp.tR h2
           (Eq.trans (lift_comp (lit0 v) 1 1 0)
             (Eq.trans (congrArg (fn [k :- Nat] (lift k 0 (lit0 v))) two) (lit_lift2 v)))
           rfl)))

(thm tl_cbot [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Tl chkf Bool.false D (cbot) Exp.tSyn)
  (exact (Tl.zSleaf chkf D (Exp.lbl 15) (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl lbl15))))

(thm tl_neg [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Tl chkf Bool.false D (negT (cbot)) Exp.tSyn)
  (exact (tl_cast3 chkf Bool.false D
           (Exp.snode (Exp.lbl 25) (Exp.sleaf (Exp.lbl 15)) (Exp.sleaf (Exp.lbl 15)))
           (negT (cbot)) Exp.tSyn Exp.tSyn
           (Tl.zSnode chkf D (Exp.lbl 25) (Exp.sleaf (Exp.lbl 15)) (Exp.sleaf (Exp.lbl 15))
             (Tl.zConst chkf D (Exp.lbl 25) Exp.tLbl lbl25)
             (Tl.zSleaf chkf D (Exp.lbl 15) (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl lbl15))
             (Tl.zSleaf chkf D (Exp.lbl 15) (Tl.zConst chkf D (Exp.lbl 15) Exp.tLbl lbl15)))
           (Eq.symm neg_unfold) rfl)))

(thm tl_binder [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Tl chkf Bool.true (List.cons Exp Exp.tR D) (chkT (Exp.var 0) (cbot)) Exp.tUnit)
  (exact (Tl.fT chkf (List.cons Exp Exp.tR D) (Exp.chk (Exp.prn (Exp.var 0)) (cbot))
           (Tl.zChk chkf (List.cons Exp Exp.tR D) (Exp.prn (Exp.var 0)) (cbot)
             (Tl.zPrn chkf (List.cons Exp Exp.tR D) (Exp.var 0)
               (tl_cast chkf Bool.false (List.cons Exp Exp.tR D) (Exp.var 0) (lift 1 0 Exp.tR) Exp.tR
                 (Tl.zVar chkf (List.cons Exp Exp.tR D) 0 Exp.tR (nthE.eq_2 Exp.tR D))
                 (closedTy_lift Exp.tR closed_tR 1 0)))
             (tl_cbot chkf (List.cons Exp Exp.tR D))))))

(thm len_zz [n :- Nat]
  (Eq Nat (lenU (vzero (Nat.succ (Nat.succ n))))
          (lenE (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))))
  (exact (Eq.trans (lenU_vzero (Nat.succ (Nat.succ n)))
           (Eq.symm
             (Eq.trans (lenE_cons (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
               (Eq.trans (congrArg Nat.succ (lenE_cons Exp.tR (thetaD n)))
                         (congrArg (fn [k :- Nat] (Nat.succ (Nat.succ k))) (lenE_theta n))))))))

(thm rt_cbot [chkf :- (=> Code Code Bool), n :- Nat]
  (Rt chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
      (vscale U.uw (vzero (Nat.succ (Nat.succ n)))) (cbot) Exp.tSyn)
  (exact (rt_cast chkf
           (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
           (vzero (Nat.succ (Nat.succ n)))
           (vscale U.uw (vzero (Nat.succ (Nat.succ n))))
           (cbot) Exp.tSyn Exp.tSyn
           (Rt.rSleaf chkf
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
             (vzero (Nat.succ (Nat.succ n))) (Exp.lbl 15)
             (Rt.rConst chkf
               (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD n)))
               (vzero (Nat.succ (Nat.succ n))) (Exp.lbl 15) Exp.tLbl
               (len_zz n) lbl15))
           (Eq.symm (vscale_vzero U.uw (Nat.succ (Nat.succ n)))) rfl)))

(thm tl_ev_h [chkf :- (=> Code Code Bool), v :- Code, hok :- (Eq Bool (lblOk v) Bool.true)]
  (Tl chkf Bool.true
      (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (evTy ((litAt v) 2) (negT (cbot))) Exp.tUnit)
  (exact (Tl.fT chkf
           (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
           (Exp.chk (Exp.prn ((litAt v) 2)) (negT (cbot)))
           (Tl.zChk chkf
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
             (Exp.prn ((litAt v) 2)) (negT (cbot))
             (Tl.zPrn chkf
               (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
               ((litAt v) 2) (tl_lit2 chkf v hok))
             (tl_neg chkf
               (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v)))))))))

(thm rt_star_h [chkf :- (=> Code Code Bool),
                     dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                     encTy :- (=> Exp Code),
                     hspec :- (CheckSpec chkf dec encTy),
                     v :- Code,
                     hok :- (Eq Bool (lblOk v) Bool.true),
                     hck :- (Eq Bool (chkf v (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) Bool.true)]
  (Rt chkf (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (vzero (Nat.succ (Nat.succ (cnodes v)))) Exp.star
      (evTy ((litAt v) 2) (negT (cbot))))
  (exact (Rt.rConv chkf
           (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
           (vzero (Nat.succ (Nat.succ (cnodes v)))) Exp.star Exp.tUnit
           (evTy ((litAt v) 2) (negT (cbot)))
           (Rt.rConst chkf
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
             (vzero (Nat.succ (Nat.succ (cnodes v)))) Exp.star Exp.tUnit
             (len_zz (cnodes v)) star_unit)
           (tl_ev_h chkf v hok)
           (cv_arrow chkf dec encTy hspec v hck))))


;; Proposition 4.10.  A certificate v of 0 ⊸ 0 gives, at budget ‖v‖,
;; Θ_{‖v‖} ⊢ λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). H₁ r (lit v) c⊥ e ⋆ :¹ H°.
;; r and e are H₁'s refutation and its evidence; lit v, shifted under the two
;; binders, is the certificate of 0 ⊸ 0, and ⋆ converts because neg c⊥ is that
;; code (cv_arrow).  c⊥ is carried at usage ω · 0.
(thm prop410 [chkf :- (=> Code Code Bool),
                   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                   encTy :- (=> Exp Code),
                   hspec :- (CheckSpec chkf dec encTy),
                   v :- Code,
                   hok :- (Eq Bool (lblOk v) Bool.true),
                   hck :- (Eq Bool (chkf v (encTy (Exp.tPi U.u1 Exp.tEmpty Exp.tEmpty))) Bool.true)]
  (Rt chkf (thetaD (cnodes v)) (thetaU (cnodes v))
      (Exp.lam U.u1 Exp.tR
        (Exp.lam U.u1 (chkT (Exp.var 0) (cbot))
          (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star)))
      (Hcirc))
  (have hH (Rt chkf
             (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
             (List.cons U U.u1 (List.cons U U.u1 (thetaU (cnodes v))))
             (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star) Exp.tEmpty)
    (rt_reindex chkf
      (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (vadd (List.cons U U.u0 (List.cons U U.u1 (vzero (cnodes v))))
        (vadd (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
          (vadd (vscale U.uw (vzero (Nat.succ (Nat.succ (cnodes v)))))
            (vadd (List.cons U U.u1 (List.cons U U.u0 (vzero (cnodes v))))
                  (vzero (Nat.succ (Nat.succ (cnodes v))))))))
      (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star) Exp.tEmpty
      (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
      (List.cons U U.u1 (List.cons U U.u1 (thetaU (cnodes v))))
      (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star) Exp.tEmpty
      (Rt.rH1 chkf
        (List.cons Exp (chkT (Exp.var 0) (cbot)) (List.cons Exp Exp.tR (thetaD (cnodes v))))
        (List.cons U U.u0 (List.cons U U.u1 (vzero (cnodes v))))
        (List.cons U U.u0 (List.cons U U.u0 (thetaU (cnodes v))))
        (vzero (Nat.succ (Nat.succ (cnodes v))))
        (List.cons U U.u1 (List.cons U U.u0 (vzero (cnodes v))))
        (vzero (Nat.succ (Nat.succ (cnodes v))))
        (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star
        (rt_binder_r chkf (cnodes v) (chkT (Exp.var 0) (cbot)))
        (rt_lit2 chkf v hok)
        (rt_cbot chkf (cnodes v))
        (rt_binder_e chkf (cnodes v))
        (rt_star_h chkf dec encTy hspec v hok hck))
      rfl (h1_us (cnodes v)) rfl rfl))
  (exact (Rt.rLam chkf (thetaD (cnodes v)) (thetaU (cnodes v)) U.u1 Exp.tR
           (Exp.lam U.u1 (chkT (Exp.var 0) (cbot))
             (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star))
           (Exp.tPi U.u1 (chkT (Exp.var 0) (cbot)) Exp.tEmpty)
           (Tl.fBase chkf (thetaD (cnodes v)) Exp.tR base_tR)
           (Rt.rLam chkf (List.cons Exp Exp.tR (thetaD (cnodes v)))
             (List.cons U U.u1 (thetaU (cnodes v))) U.u1 (chkT (Exp.var 0) (cbot))
             (Exp.h1 (Exp.var 1) ((litAt v) 2) (cbot) (Exp.var 0) Exp.star) Exp.tEmpty
             (tl_binder chkf (thetaD (cnodes v)))
             hH))))

