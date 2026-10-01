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
