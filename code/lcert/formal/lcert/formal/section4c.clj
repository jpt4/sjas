(ns lcert.formal.section4c
  "F4 — the remaining constructions of R4-metatheory.md §4.

  Proposition 4.11 (certificates are not normal).  Check accepts a derivation
  whose term contains β-redexes.  For every n the closed term sh4_cn n is

      let x₀ = sleaf ℓ in
      let x₁ = snode ℓ x₀ x₀ in
      ⋯
      let xₙ = snode ℓ xₙ₋₁ xₙ₋₁ in
      xₙ

  with ℓ = 0 (a label below NL) and each let an application of a λ at usage ω,
  the de Bruijn form of a cut.  sh4_open n is the chain from x₁ on, in a
  context whose variable 0 is the tree bound so far; sh4_cn n wraps it around
  sleaf ℓ.  prop411_typed: ⊢ sh4_cn n :¹ Syn at budget 0 (the empty context).

  Its denotation, at every cap, is the complete binary code sh4_bush n of n
  doublings (prop411_den).  That code has 2ⁿ − 1 internal nodes: sh4_pow n
  is 2ⁿ and prop411_nodes is cnodes(sh4_bush n) = 2ⁿ − 1, stated as
  cnodes + 1 = 2ⁿ because Nat subtraction truncates.  The same count is the
  literal size of the normal form (codeTerm of the bush).  The certificate of
  the redex term is not part of the statement: the paper measures it in lcert,
  and the formal content asked for is the typing and the denotation."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.unfold :refer :all]
            [lcert.formal.subst :refer :all]
            [lcert.formal.fundamental :refer :all]
            [lcert.formal.conversion :refer :all]
            [lcert.formal.derivations :refer :all]))

;; --- the terms and the value -------------------------------------------------------

;; 2ⁿ.  The successor clause is the doubling the node count uses.
(a/defn sh4_pow [n :- Nat] Nat
  (match n [zero 1] [(succ k) (+ (sh4_pow k) (sh4_pow k))]))

;; The complete binary code of n doublings, every label 0.  n = 0 is the leaf.
(a/defn sh4_bush [n :- Nat] Code
  (match n [zero (Code.sl 0)] [(succ k) (Code.sn 0 (sh4_bush k) (sh4_bush k))]))

;; n doublings of an arbitrary code: grow (n+1) v = grow n (snode 0 v v).
;; This is the value of the open chain, whose outermost redex builds one node
;; and whose body doubles that node n times.
(a/defn sh4_grow [n :- Nat, v :- Code] Code
  (match n [zero v] [(succ k) (sh4_grow k (Code.sn 0 v v))]))

;; The open chain.  Variable 0 is the tree bound by the surrounding let.
;;   open 0       = x
;;   open (n+1)   = (λ(y :ω Syn). open n) (snode ℓ x x)
;; so open n mentions only variable 0, and placing it under a fresh λ captures
;; that variable as the new binding.  No lift: the free variable is the one
;; the new binder is meant to catch.
(a/defn sh4_open [n :- Nat] Exp
  (match n
    [zero (Exp.var 0)]
    [(succ k) (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open k))
                       (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)))]))

;; cₙ, closed.  The outer let binds x₀ to sleaf ℓ; sh4_open n is the rest,
;; ending in the variable xₙ.
(a/defn sh4_cn [n :- Nat] Exp
  (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n)) (Exp.sleaf (Exp.lbl 0))))

(thm sh4_pow_succ [k :- Nat]
  (Eq Nat (sh4_pow (Nat.succ k)) (+ (sh4_pow k) (sh4_pow k))) (rfl))
(thm sh4_bush_succ [k :- Nat]
  (Eq Code (sh4_bush (Nat.succ k)) (Code.sn 0 (sh4_bush k) (sh4_bush k))) (rfl))
;; sh4_grow changes its code argument at the recursive call, so the kernel
;; accepts it as well-founded recursion (Nat.fix) rather than as a
;; definitional match.  The equations are sh4_grow.eq_1 and .eq_2, and the
;; fix puts the non-decreasing argument first: eq_1 takes the code, eq_2
;; takes the code and then the predecessor.
(thm sh4_grow_zero [v :- Code]
  (Eq Code (sh4_grow 0 v) v)
  (exact (sh4_grow.eq_1 v)))
(thm sh4_grow_succ [k :- Nat, v :- Code]
  (Eq Code (sh4_grow (Nat.succ k) v) (sh4_grow k (Code.sn 0 v v)))
  (exact (sh4_grow.eq_2 v k)))
(thm sh4_open_zero [] (Eq Exp (sh4_open 0) (Exp.var 0)) (rfl))
(thm sh4_open_succ [k :- Nat]
  (Eq Exp (sh4_open (Nat.succ k))
          (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open k))
                   (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))))
  (rfl))
(thm sh4_cn_unfold [n :- Nat]
  (Eq Exp (sh4_cn n) (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n)) (Exp.sleaf (Exp.lbl 0))))
  (rfl))

;; --- usage arithmetic for the ω-redex --------------------------------------------------
;; The chain is typed in (Syn :: D) at usage (ω :: 0ⁿ).  A constant may be
;; given any usage vector (rConst), so the label pays nothing and the two
;; uses of variable 0, each at ω, sum to ω.  The λ that binds the next tree
;; does not use the outer variables, so its vector is (0 :: 0ⁿ); scaling the
;; argument's (ω :: 0ⁿ) by ω and adding restores (ω :: 0ⁿ).

(thm sh4_lenU_cons [r :- U, us :- (List U)]
  (Eq Nat (lenU (List.cons U r us)) (Nat.succ (lenU us))) (rfl))
(thm sh4_lenE_cons [x :- Exp, xs :- (List Exp)]
  (Eq Nat (lenE (List.cons Exp x xs)) (Nat.succ (lenE xs))) (rfl))
(thm sh4_lenU_vzero [m :- Nat] (Eq Nat (lenU (vzero m)) m)
  (induction m) (rfl) (exact (congrArg Nat.succ ih_n)))
(thm sh4_len_head [r :- U, D :- (List Exp)]
  (Eq Nat (lenU (List.cons U r (vzero (lenE D)))) (lenE (List.cons Exp Exp.tSyn D)))
  (exact (Eq.trans (sh4_lenU_cons r (vzero (lenE D)))
           (Eq.trans (congrArg Nat.succ (sh4_lenU_vzero (lenE D)))
                     (Eq.symm (sh4_lenE_cons Exp.tSyn D))))))

(thm sh4_vadd_zz [m :- Nat]
  (Eq (List U) (vadd (vzero m) (vzero m)) (vzero m))
  (induction m) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u0 v)) ih_n)))
(thm sh4_vscale_z [m :- Nat]
  (Eq (List U) (vscale U.uw (vzero m)) (vzero m))
  (induction m) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u0 v)) ih_n)))

;; vadd (ω :: 0ⁿ) (ω :: 0ⁿ) = ω :: 0ⁿ.  The cons reduces; the tail is sh4_vadd_zz.
(thm sh4_vadd_uw [m :- Nat]
  (Eq (List U) (vadd (List.cons U U.uw (vzero m)) (List.cons U U.uw (vzero m)))
               (List.cons U U.uw (vzero m)))
  (change (Eq (List U) (List.cons U U.uw (vadd (vzero m) (vzero m)))
                         (List.cons U U.uw (vzero m))))
  (rw [(sh4_vadd_zz m)]))
;; vadd (0 :: 0ⁿ) (ω :: 0ⁿ) = ω :: 0ⁿ.  0 + ω = ω.
(thm sh4_vadd_u0uw [m :- Nat]
  (Eq (List U) (vadd (List.cons U U.u0 (vzero m)) (List.cons U U.uw (vzero m)))
               (List.cons U U.uw (vzero m)))
  (change (Eq (List U) (List.cons U U.uw (vadd (vzero m) (vzero m)))
                         (List.cons U U.uw (vzero m))))
  (rw [(sh4_vadd_zz m)]))
(thm sh4_vscale_uw [m :- Nat]
  (Eq (List U) (vscale U.uw (List.cons U U.uw (vzero m))) (List.cons U U.uw (vzero m)))
  (change (Eq (List U) (List.cons U U.uw (vscale U.uw (vzero m)))
                         (List.cons U U.uw (vzero m))))
  (rw [(sh4_vscale_z m)]))
;; The snode's three vectors: label at 0, each copy of x at ω.
(thm sh4_vadd_node [m :- Nat]
  (Eq (List U)
      (vadd (List.cons U U.u0 (vzero m))
            (vadd (List.cons U U.uw (vzero m)) (List.cons U U.uw (vzero m))))
      (List.cons U U.uw (vzero m)))
  (rw [(sh4_vadd_uw m)])
  (exact (sh4_vadd_u0uw m)))
;; The redex's vectors: the λ at 0, its argument scaled by ω.
(thm sh4_vadd_app [m :- Nat]
  (Eq (List U)
      (vadd (List.cons U U.u0 (vzero m))
            (vscale U.uw (List.cons U U.uw (vzero m))))
      (List.cons U U.uw (vzero m)))
  (rw [(sh4_vscale_uw m)])
  (exact (sh4_vadd_u0uw m)))

(thm sh4_nthE0 [A :- Exp, D :- (List Exp)]
  (Eq (Option Exp) (nthE (List.cons Exp A D) 0) (Option.some Exp A))
  (exact (nthE.eq_2 A D)))
(thm sh4_nthU0 [r :- U, us :- (List U)]
  (Eq (Option U) (nthU (List.cons U r us) 0) (Option.some U r))
  (exact (nthU.eq_2 r us)))
(thm sh4_nonzero_w [] (Eq Bool (nonzero U.uw) Bool.true) (rfl))

;; --- typing ----------------------------------------------------------------------------

;; Variable 0 at ω, in Syn :: D.  lift 1 0 Syn is Syn (Syn is closed).
(thm sh4_var [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Rt chkf (List.cons Exp Exp.tSyn D) (List.cons U U.uw (vzero (lenE D))) (Exp.var 0) Exp.tSyn)
  (exact (Rt.rVar chkf (List.cons Exp Exp.tSyn D) (List.cons U U.uw (vzero (lenE D)))
           0 Exp.tSyn U.uw
           (sh4_len_head U.uw D) (sh4_nthE0 Exp.tSyn D)
           (sh4_nthU0 U.uw (vzero (lenE D))) sh4_nonzero_w)))

;; The label, charged nothing.  constTyped (lbl 0) Lbl computes: 0 < NL.
(thm sh4_lbl [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Rt chkf (List.cons Exp Exp.tSyn D) (List.cons U U.u0 (vzero (lenE D))) (Exp.lbl 0) Exp.tLbl)
  (exact (Rt.rConst chkf (List.cons Exp Exp.tSyn D) (List.cons U U.u0 (vzero (lenE D)))
           (Exp.lbl 0) Exp.tLbl (sh4_len_head U.u0 D) rfl)))

;; snode ℓ x x, using x twice at ω.  rSnode's summed vector is cast back to ω :: 0ⁿ.
(thm sh4_snode [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (Rt chkf (List.cons Exp Exp.tSyn D) (List.cons U U.uw (vzero (lenE D)))
      (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)) Exp.tSyn)
  (exact (rt_cast chkf (List.cons Exp Exp.tSyn D)
           (vadd (List.cons U U.u0 (vzero (lenE D)))
                 (vadd (List.cons U U.uw (vzero (lenE D))) (List.cons U U.uw (vzero (lenE D)))))
           (List.cons U U.uw (vzero (lenE D)))
           (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)) Exp.tSyn Exp.tSyn
           (Rt.rSnode chkf (List.cons Exp Exp.tSyn D)
             (List.cons U U.u0 (vzero (lenE D)))
             (List.cons U U.uw (vzero (lenE D)))
             (List.cons U U.uw (vzero (lenE D)))
             (Exp.lbl 0) (Exp.var 0) (Exp.var 0)
             (sh4_lbl chkf D) (sh4_var chkf D) (sh4_var chkf D))
           (sh4_vadd_node (lenE D)) rfl)))

;; One more let, given the body typed under the new Syn binder at
;; (ω :: 0 :: 0ⁿ) — the usage rLam demands, and the one sh4_open's induction
;; hypothesis has at context Syn :: Syn :: D.
(thm sh4_open_step [chkf :- (=> Code Code Bool), n :- Nat, D :- (List Exp),
                    hbody :- (Rt chkf (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn D))
                                 (List.cons U U.uw (List.cons U U.u0 (vzero (lenE D))))
                                 (sh4_open n) Exp.tSyn)]
  (Rt chkf (List.cons Exp Exp.tSyn D) (List.cons U U.uw (vzero (lenE D)))
      (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n))
               (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)))
      Exp.tSyn)
  (exact (rt_cast chkf (List.cons Exp Exp.tSyn D)
           (vadd (List.cons U U.u0 (vzero (lenE D)))
                 (vscale U.uw (List.cons U U.uw (vzero (lenE D)))))
           (List.cons U U.uw (vzero (lenE D)))
           (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n))
                    (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)))
           Exp.tSyn Exp.tSyn
           (Rt.rApp chkf (List.cons Exp Exp.tSyn D)
             (List.cons U U.u0 (vzero (lenE D)))
             (List.cons U U.uw (vzero (lenE D)))
             U.uw
             (Exp.lam U.uw Exp.tSyn (sh4_open n))
             (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
             Exp.tSyn Exp.tSyn
             rfl
             (Rt.rLam chkf (List.cons Exp Exp.tSyn D)
               (List.cons U U.u0 (vzero (lenE D)))
               U.uw Exp.tSyn (sh4_open n) Exp.tSyn
               (Tl.fBase chkf (List.cons Exp Exp.tSyn D) Exp.tSyn rfl)
               hbody)
             (sh4_snode chkf D)
             (Tl.fBase chkf (List.cons Exp Exp.tSyn D) Exp.tSyn rfl)
             (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn D)) Exp.tSyn rfl))
           (sh4_vadd_app (lenE D)) rfl)))

;; The open chain, in every context Syn :: D, at usage ω on that Syn and 0 after.
(thm sh4_open_typed [chkf :- (=> Code Code Bool), n :- Nat]
  (forall [D (List Exp)]
    (Rt chkf (List.cons Exp Exp.tSyn D) (List.cons U U.uw (vzero (lenE D))) (sh4_open n) Exp.tSyn))
  (induction n)
  (intro D)
  (exact (sh4_var chkf D))
  (intro D)
  (exact (sh4_open_step chkf n D (ih_n (List.cons Exp Exp.tSyn D)))))

;; Proposition 4.11, the typing: ⊢ cₙ :¹ Syn at budget 0.
(thm prop411_typed [chkf :- (=> Code Code Bool), n :- Nat]
  (Rt chkf (List.nil Exp) (List.nil U) (sh4_cn n) Exp.tSyn)
  (exact (Rt.rApp chkf (List.nil Exp) (List.nil U) (List.nil U) U.uw
           (Exp.lam U.uw Exp.tSyn (sh4_open n))
           (Exp.sleaf (Exp.lbl 0))
           Exp.tSyn Exp.tSyn
           rfl
           (Rt.rLam chkf (List.nil Exp) (List.nil U) U.uw Exp.tSyn (sh4_open n) Exp.tSyn
             (Tl.fBase chkf (List.nil Exp) Exp.tSyn rfl)
             (sh4_open_typed chkf n (List.nil Exp)))
           (Rt.rSleaf chkf (List.nil Exp) (List.nil U) (Exp.lbl 0)
             (Rt.rConst chkf (List.nil Exp) (List.nil U) (Exp.lbl 0) Exp.tLbl rfl rfl))
           (Tl.fBase chkf (List.nil Exp) Exp.tSyn rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.nil Exp)) Exp.tSyn rfl))))

;; --- denotation ------------------------------------------------------------------------
;; ⟦λ(y). t⟧ at an arrow, applied to v, is ⟦t⟧ at (v, η) (den_lam_arr).
;; ⟦f u⟧ is that application when skOf finds u's skeleton (den_app_some);
;; sleaf and snode both have skeleton Syn, read off the constructor.

(thm sh4_sk_sleaf [G :- (List Sk), x :- Exp]
  (Eq (Option Sk) (skOf G (Exp.sleaf x)) (Option.some Sk Sk.syn)) (rfl))
(thm sh4_sk_snode [G :- (List Sk), x :- Exp, c1 :- Exp, c2 :- Exp]
  (Eq (Option Sk) (skOf G (Exp.snode x c1 c2)) (Option.some Sk Sk.syn)) (rfl))

;; ⟦x⟧ at (v, η) is v.  lookup of index 0 is coe, and coe Syn Syn is the identity.
(thm sh4_den_var0 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, G :- (List Sk), en :- (HEnv G), v :- Code]
  (Eq Code (den chkf dec encTy cap (Exp.var 0) (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en)) v)
  (rw [(den_var_at chkf dec encTy cap 0 (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))]))

;; ⟦snode ℓ x x⟧ at (v, η) is the node with two copies of v.
(thm sh4_den_snode [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                    cap :- Nat, G :- (List Sk), en :- (HEnv G), v :- Code]
  (Eq Code (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
               (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
           (Code.sn 0 v v))
  (rw [(den_snode_code chkf dec encTy cap 0 (Exp.var 0) (Exp.var 0) (List.cons Sk Sk.syn G) (Prod.mk v en))])
  (rw [(sh4_den_var0 chkf dec encTy cap G en v)]))

;; One redex doubles, then the body (the induction hypothesis) doubles n times.
(thm sh4_open_den_step [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                        cap :- Nat, n :- Nat,
                        ih :- (forall [G (List Sk)] (forall [en (HEnv G)] (forall [v Code]
                               (Eq Code (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
                                       (sh4_grow n v)))))]
  (forall [G (List Sk)] (forall [en (HEnv G)] (forall [v Code]
    (Eq Code (den chkf dec encTy cap
                 (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n))
                          (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)))
                 (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
            (sh4_grow (Nat.succ n) v)))))
  (intro G en v)
  (have hd (Eq Code
            (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                 (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
            (Code.sn 0 v v))
    (sh4_den_snode chkf dec encTy cap G en v))
  (have ha (Eq Code
            (den chkf dec encTy cap
                 (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n))
                          (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0)))
                 (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
            ((den chkf dec encTy cap (Exp.lam U.uw Exp.tSyn (sh4_open n))
                  (List.cons Sk Sk.syn G) (Sk.arr Sk.syn Sk.syn) (Prod.mk v en))
             (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                  (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))))
    (den_app_some chkf dec encTy cap
       (Exp.lam U.uw Exp.tSyn (sh4_open n))
       (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
       (List.cons Sk Sk.syn G) Sk.syn Sk.syn (Prod.mk v en)
       (sh4_sk_snode (List.cons Sk Sk.syn G) (Exp.lbl 0) (Exp.var 0) (Exp.var 0))))
  (have hb (Eq Code
            ((den chkf dec encTy cap (Exp.lam U.uw Exp.tSyn (sh4_open n))
                  (List.cons Sk Sk.syn G) (Sk.arr Sk.syn Sk.syn) (Prod.mk v en))
             (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                  (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en)))
            (den chkf dec encTy cap (sh4_open n)
                 (List.cons Sk Sk.syn (List.cons Sk Sk.syn G)) Sk.syn
                 (Prod.mk (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                               (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
                          (Prod.mk v en))))
    (den_lam_arr chkf dec encTy cap U.uw Exp.tSyn (sh4_open n)
       (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en)
       (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
            (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))))
  (have hc (Eq Code
            (den chkf dec encTy cap (sh4_open n)
                 (List.cons Sk Sk.syn (List.cons Sk Sk.syn G)) Sk.syn
                 (Prod.mk (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                               (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
                          (Prod.mk v en)))
            (den chkf dec encTy cap (sh4_open n)
                 (List.cons Sk Sk.syn (List.cons Sk Sk.syn G)) Sk.syn
                 (Prod.mk (Code.sn 0 v v) (Prod.mk v en))))
    (congrArg (fn [c :- Code]
                (den chkf dec encTy cap (sh4_open n)
                     (List.cons Sk Sk.syn (List.cons Sk Sk.syn G)) Sk.syn
                     (Prod.mk c (Prod.mk v en))))
              hd))
  (have he (Eq Code
            (den chkf dec encTy cap (sh4_open n)
                 (List.cons Sk Sk.syn (List.cons Sk Sk.syn G)) Sk.syn
                 (Prod.mk (Code.sn 0 v v) (Prod.mk v en)))
            (sh4_grow n (Code.sn 0 v v)))
    (ih (List.cons Sk Sk.syn G) (Prod.mk v en) (Code.sn 0 v v)))
  (exact (Eq.trans ha (Eq.trans hb (Eq.trans hc (Eq.trans he (Eq.symm (sh4_grow_succ n v))))))))

;; ⟦open n⟧ at (v, η) is v doubled n times.
(thm sh4_open_den [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, n :- Nat]
  (forall [G (List Sk)] (forall [en (HEnv G)] (forall [v Code]
    (Eq Code (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn G) Sk.syn (Prod.mk v en))
            (sh4_grow n v)))))
  (induction n)
  (intro G en v)
  (exact (Eq.trans (sh4_den_var0 chkf dec encTy cap G en v) (Eq.symm (sh4_grow_zero v))))
  (exact (sh4_open_den_step chkf dec encTy cap n ih_n)))

;; Doubling commutes with one already-built node, so iterating from a leaf
;; is the complete bush.
;; The binder is named c, not v: a goal-local named in a rw lemma is elaborated
;; outside the goal and rejected.  The transports below are exact, which does
;; see the local.  One node already built commutes with n further doublings.
(thm sh4_grow_double [n :- Nat]
  (forall [c Code]
    (Eq Code (sh4_grow n (Code.sn 0 c c)) (Code.sn 0 (sh4_grow n c) (sh4_grow n c))))
  (induction n)
  (intro c)
  (exact (Eq.trans (sh4_grow_zero (Code.sn 0 c c))
           (Eq.symm (congrArg (fn [a :- Code] (Code.sn 0 a a)) (sh4_grow_zero c)))))
  (intro c)
  (exact (Eq.trans (sh4_grow_succ n (Code.sn 0 c c))
           (Eq.trans (ih_n (Code.sn 0 c c))
             (Eq.symm (congrArg (fn [a :- Code] (Code.sn 0 a a)) (sh4_grow_succ n c)))))))
(thm sh4_grow_bush [n :- Nat]
  (Eq Code (sh4_grow n (Code.sl 0)) (sh4_bush n))
  (induction n)
  (exact (sh4_grow_zero (Code.sl 0)))
  (exact (Eq.trans (sh4_grow_succ n (Code.sl 0))
           (Eq.trans (sh4_grow_double n (Code.sl 0))
             (congrArg (fn [a :- Code] (Code.sn 0 a a)) ih_n)))))

;; Proposition 4.11, the denotation, at every cap: ⟦cₙ⟧ = the bush of n doublings.
(thm prop411_den [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  cap :- Nat, n :- Nat]
  (Eq Code (den chkf dec encTy cap (sh4_cn n) (List.nil Sk) Sk.syn Unit.unit) (sh4_bush n))
  (have ha (Eq Code
            (den chkf dec encTy cap
                 (Exp.app (Exp.lam U.uw Exp.tSyn (sh4_open n)) (Exp.sleaf (Exp.lbl 0)))
                 (List.nil Sk) Sk.syn Unit.unit)
            ((den chkf dec encTy cap (Exp.lam U.uw Exp.tSyn (sh4_open n))
                  (List.nil Sk) (Sk.arr Sk.syn Sk.syn) Unit.unit)
             (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit)))
    (den_app_some chkf dec encTy cap
       (Exp.lam U.uw Exp.tSyn (sh4_open n)) (Exp.sleaf (Exp.lbl 0))
       (List.nil Sk) Sk.syn Sk.syn Unit.unit
       (sh4_sk_sleaf (List.nil Sk) (Exp.lbl 0))))
  (have hl (Eq Code (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit) (Code.sl 0))
    (den_sleaf_code chkf dec encTy cap 0 (List.nil Sk) Unit.unit))
  (have hb (Eq Code
            ((den chkf dec encTy cap (Exp.lam U.uw Exp.tSyn (sh4_open n))
                  (List.nil Sk) (Sk.arr Sk.syn Sk.syn) Unit.unit)
             (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit))
            (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn (List.nil Sk)) Sk.syn
                 (Prod.mk (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit) Unit.unit)))
    (den_lam_arr chkf dec encTy cap U.uw Exp.tSyn (sh4_open n)
       (List.nil Sk) Sk.syn Unit.unit
       (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit)))
  (have hc (Eq Code
            (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn (List.nil Sk)) Sk.syn
                 (Prod.mk (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit) Unit.unit))
            (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn (List.nil Sk)) Sk.syn
                 (Prod.mk (Code.sl 0) Unit.unit)))
    (congrArg (fn [c :- Code]
                (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn (List.nil Sk)) Sk.syn
                     (Prod.mk c Unit.unit)))
              hl))
  (have he (Eq Code
            (den chkf dec encTy cap (sh4_open n) (List.cons Sk Sk.syn (List.nil Sk)) Sk.syn
                 (Prod.mk (Code.sl 0) Unit.unit))
            (sh4_grow n (Code.sl 0)))
    (sh4_open_den chkf dec encTy cap n (List.nil Sk) Unit.unit (Code.sl 0)))
  (exact (Eq.trans ha (Eq.trans hb (Eq.trans hc (Eq.trans he (sh4_grow_bush n)))))))

;; --- the node count --------------------------------------------------------------------
;; 1 + (1 + (a + a)) = (1 + a) + (1 + a), so the successor of the bush
;; (one node plus two copies) has one more than twice the predecessor's nodes.

(thm sh4_twice [a :- Nat]
  (Eq Nat (+ 1 (+ 1 (+ a a))) (+ (+ 1 a) (+ 1 a)))
  (omega))
(thm sh4_bush_size [n :- Nat]
  (Eq Nat (+ 1 (cnodes (sh4_bush n))) (sh4_pow n))
  (induction n)
  (rfl)
  (rw [(cnodes_sn 0 (sh4_bush n) (sh4_bush n))])
  (rw [(sh4_twice (cnodes (sh4_bush n)))])
  (rw [ih_n]))

;; Nat addition recurses on its second argument, so (+ 1 a) is not the
;; constructor succ a.  Subtracting 1 from either is a, and the node count
;; is stated with the same (+ 1 ·) the size lemma uses.
(thm sh4_sub_succ [a :- Nat] (Eq Nat (Nat.sub (Nat.succ a) 1) a) (omega))
(thm sh4_sub_add1 [a :- Nat] (Eq Nat (Nat.sub (+ 1 a) 1) a) (omega))
(thm prop411_nodes [n :- Nat]
  (Eq Nat (cnodes (sh4_bush n)) (Nat.sub (sh4_pow n) 1))
  (exact (Eq.trans (Eq.symm (sh4_sub_add1 (cnodes (sh4_bush n))))
                   (congrArg (fn [k :- Nat] (Nat.sub k 1)) (sh4_bush_size n)))))

;; Proposition 4.11: typed at budget 0, and the denotation has 2ⁿ − 1 internal nodes.
(thm prop411 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
              cap :- Nat, n :- Nat]
  (And (Rt chkf (List.nil Exp) (List.nil U) (sh4_cn n) Exp.tSyn)
       (Eq Nat (cnodes (den chkf dec encTy cap (sh4_cn n) (List.nil Sk) Sk.syn Unit.unit))
               (Nat.sub (sh4_pow n) 1)))
  (exact (And.intro (prop411_typed chkf n)
           (Eq.trans (congrArg cnodes (prop411_den chkf dec encTy cap n)) (prop411_nodes n)))))

;; --- Proposition 4.12 (1) --------------------------------------------------------------
;; recN doubling a tree, through the packaged motive Σ(p :ω Syn). 1.  The
;; motive does not mention the Nat it is recursing on, so stepTy and every
;; subst1 of it are the motive itself.  The base is the leaf, packaged with
;; ⋆.  The step opens the accumulator (variable 0, usage 1) and rebuilds a
;; node with two copies of its code; the predecessor (variable 1, usage ω) is
;; not mentioned, and its usage is carried by the ⋆, which rConst types at
;; any vector.  Part (2) of the proposition, that the certificate stays
;; polynomial, is the paper's citation of T3 and Proposition 4.1; it is not
;; restated here.  The formal content is the budget-0 typing and the
;; denotation: at numeral n the package's code has 2ⁿ − 1 internal nodes.

;; The motive, the step's two contexts, and the usage vectors the rules sum to.
;; Each vector below is what the corresponding vadd/vscale computes to, so the
;; rule's where-clause usage and the vector are the same list.
(a/defn sh4_M [] Exp (Exp.tSig U.uw Exp.tSyn Exp.tUnit))
(a/defn sh4_CB [] (List Exp)
  (List.cons Exp Exp.tUnit
    (List.cons Exp Exp.tSyn
      (List.cons Exp (sh4_M) (List.cons Exp Exp.tNat (List.nil Exp))))))
(a/defn sh4_CS [] (List Exp)
  (List.cons Exp (sh4_M) (List.cons Exp Exp.tNat (List.nil Exp))))
(a/defn sh4_usL [] (List U)
  (List.cons U U.u0 (List.cons U U.u0 (List.cons U U.u0 (List.cons U U.u0 (List.nil U))))))
(a/defn sh4_usC [] (List U)
  (List.cons U U.u0 (List.cons U U.uw (List.cons U U.u0 (List.cons U U.u0 (List.nil U))))))
(a/defn sh4_usS [] (List U)
  (List.cons U U.u1 (List.cons U U.u0 (List.cons U U.u0 (List.cons U U.uw (List.nil U))))))
(a/defn sh4_usB [] (List U)
  (List.cons U U.u1 (List.cons U U.uw (List.cons U U.u0 (List.cons U U.uw (List.nil U))))))
(a/defn sh4_usP [] (List U) (List.cons U U.u1 (List.cons U U.u0 (List.nil U))))
(a/defn sh4_us2 [] (List U) (List.cons U U.u0 (List.cons U U.uw (List.nil U))))
(a/defn sh4_usR [] (List U) (List.cons U U.u1 (List.cons U U.uw (List.nil U))))

;; Body, in [1, Syn, Σ, Nat]: (snode ℓ y y, ⋆), with y the code at index 1.
(a/defn sh4_dbl_body [] Exp
  (Exp.pair (sh4_M) (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1)) Exp.star))
;; Step, in [Σ, Nat]: open the accumulator and return the doubled package.
(a/defn sh4_dbl_step [] Exp (Exp.letp (sh4_M) (Exp.var 0) (sh4_dbl_body)))
(a/defn sh4_dbl_base [] Exp (Exp.pair (sh4_M) (Exp.sleaf (Exp.lbl 0)) Exp.star))
(a/defn sh4_num [n :- Nat] Exp
  (match n [zero Exp.zero] [(succ k) (Exp.succ (sh4_num k))]))
(a/defn sh4_doubler [e :- Exp] Exp
  (Exp.recN (sh4_M) (sh4_dbl_base) (sh4_dbl_step) e))

;; The motive is closed, so the recursor's type computations are identities,
;; and the concrete vectors are the sums the rules form.
(thm sh4_motive_step [] (Eq Exp (stepTy (sh4_M)) (sh4_M)) (rfl))
(thm sh4_motive_sub [e :- Exp] (Eq Exp (subst1 e (sh4_M)) (sh4_M)) (rfl))
(thm sh4_star_ty [] (Eq Bool (constTyped Exp.star Exp.tUnit) Bool.true) (rfl))
(thm sh4_zero_ty [] (Eq Bool (constTyped Exp.zero Exp.tNat) Bool.true) (rfl))
(thm sh4_nz1 [] (Eq Bool (nonzero U.u1) Bool.true) (rfl))
(thm sh4_len_nil [] (Eq Nat (lenU (List.nil U)) (lenE (List.nil Exp))) (rfl))
(thm sh4_lenL [] (Eq Nat (lenU (sh4_usL)) (lenE (sh4_CB))) (rfl))
(thm sh4_lenC [] (Eq Nat (lenU (sh4_usC)) (lenE (sh4_CB))) (rfl))
(thm sh4_lenS [] (Eq Nat (lenU (sh4_usS)) (lenE (sh4_CB))) (rfl))
(thm sh4_lenP [] (Eq Nat (lenU (sh4_usP)) (lenE (sh4_CS))) (rfl))
;; Index 1 is the Syn under the Unit binder, so peel both.
(thm sh4_nth_syn []
  (Eq (Option Exp) (nthE (sh4_CB) 1) (Option.some Exp Exp.tSyn))
  (exact (Eq.trans (nthE.eq_3 Exp.tUnit (List.cons Exp Exp.tSyn (sh4_CS)) 0)
                   (nthE.eq_2 Exp.tSyn (sh4_CS)))))
(thm sh4_nthu_w []
  (Eq (Option U) (nthU (sh4_usC) 1) (Option.some U U.uw))
  (exact (Eq.trans (nthU.eq_3 U.u0 (List.cons U U.uw (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))) 0)
                   (nthU.eq_2 U.uw (List.cons U U.u0 (List.cons U U.u0 (List.nil U)))))))

(thm sh4_lbl_body [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CB) (sh4_usL) (Exp.lbl 0) Exp.tLbl)
  (exact (Rt.rConst chkf (sh4_CB) (sh4_usL) (Exp.lbl 0) Exp.tLbl sh4_lenL rfl)))
(thm sh4_code_var [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CB) (sh4_usC) (Exp.var 1) Exp.tSyn)
  (exact (Rt.rVar chkf (sh4_CB) (sh4_usC) 1 Exp.tSyn U.uw sh4_lenC sh4_nth_syn sh4_nthu_w sh4_nonzero_w)))
(thm sh4_star_body [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CB) (sh4_usS) Exp.star Exp.tUnit)
  (exact (Rt.rConst chkf (sh4_CB) (sh4_usS) Exp.star Exp.tUnit sh4_lenS sh4_star_ty)))
(thm sh4_snode_body [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CB) (sh4_usC) (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1)) Exp.tSyn)
  (exact (Rt.rSnode chkf (sh4_CB) (sh4_usL) (sh4_usC) (sh4_usC)
           (Exp.lbl 0) (Exp.var 1) (Exp.var 1)
           (sh4_lbl_body chkf) (sh4_code_var chkf) (sh4_code_var chkf))))
(thm sh4_pair_body [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CB) (sh4_usB) (sh4_dbl_body) (sh4_M))
  (exact (Rt.rPair chkf (sh4_CB) (sh4_usC) (sh4_usS) U.uw Exp.tSyn Exp.tUnit
           (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1)) Exp.star
           sh4_nonzero_w
           (Tl.fBase chkf (sh4_CB) Exp.tSyn rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (sh4_CB)) Exp.tUnit rfl)
           (sh4_snode_body chkf) (sh4_star_body chkf))))
(thm sh4_acc_var [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CS) (sh4_usP) (Exp.var 0) (sh4_M))
  (exact (Rt.rVar chkf (sh4_CS) (sh4_usP) 0 (sh4_M) U.u1
           sh4_lenP (nthE.eq_2 (sh4_M) (List.cons Exp Exp.tNat (List.nil Exp)))
           (nthU.eq_2 U.u1 (List.cons U U.u0 (List.nil U))) sh4_nz1)))
(thm sh4_let_step [chkf :- (=> Code Code Bool)]
  (Rt chkf (sh4_CS) (sh4_usR) (sh4_dbl_step) (sh4_M))
  (exact (Rt.rLet chkf (sh4_CS) (sh4_usP) (sh4_us2) U.uw Exp.tSyn Exp.tUnit (sh4_M)
           (Exp.var 0) (sh4_dbl_body)
           (sh4_acc_var chkf)
           (Tl.fSig chkf (sh4_CS) U.uw Exp.tSyn Exp.tUnit
             (Tl.fBase chkf (sh4_CS) Exp.tSyn rfl)
             (Tl.fBase chkf (List.cons Exp Exp.tSyn (sh4_CS)) Exp.tUnit rfl))
           (Tl.fBase chkf (sh4_CS) Exp.tSyn rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (sh4_CS)) Exp.tUnit rfl)
           (sh4_pair_body chkf))))
(thm sh4_base [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.nil Exp) (List.nil U) (sh4_dbl_base) (sh4_M))
  (exact (Rt.rPair chkf (List.nil Exp) (List.nil U) (List.nil U) U.uw Exp.tSyn Exp.tUnit
           (Exp.sleaf (Exp.lbl 0)) Exp.star
           sh4_nonzero_w
           (Tl.fBase chkf (List.nil Exp) Exp.tSyn rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.nil Exp)) Exp.tUnit rfl)
           (Rt.rSleaf chkf (List.nil Exp) (List.nil U) (Exp.lbl 0)
             (Rt.rConst chkf (List.nil Exp) (List.nil U) (Exp.lbl 0) Exp.tLbl sh4_len_nil rfl))
           (Rt.rConst chkf (List.nil Exp) (List.nil U) Exp.star Exp.tUnit sh4_len_nil sh4_star_ty))))
(thm sh4_num_typed [chkf :- (=> Code Code Bool), n :- Nat]
  (Rt chkf (List.nil Exp) (List.nil U) (sh4_num n) Exp.tNat)
  (induction n)
  (exact (Rt.rConst chkf (List.nil Exp) (List.nil U) Exp.zero Exp.tNat sh4_len_nil sh4_zero_ty))
  (exact (Rt.rSucc chkf (List.nil Exp) (List.nil U) (sh4_num n) ih_n)))
(thm sh4_formP [chkf :- (=> Code Code Bool)]
  (Tl chkf Bool.true (List.cons Exp Exp.tNat (List.nil Exp)) (sh4_M) Exp.tUnit)
  (exact (Tl.fSig chkf (List.cons Exp Exp.tNat (List.nil Exp)) U.uw Exp.tSyn Exp.tUnit
           (Tl.fBase chkf (List.cons Exp Exp.tNat (List.nil Exp)) Exp.tSyn rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.cons Exp Exp.tNat (List.nil Exp))) Exp.tUnit rfl))))

;; Proposition 4.12 (1), the typing: the doubler at numeral n, budget 0.
(thm prop412_typed [chkf :- (=> Code Code Bool), n :- Nat]
  (Rt chkf (List.nil Exp) (List.nil U) (sh4_doubler (sh4_num n)) (sh4_M))
  (exact (Rt.rRecN chkf (List.nil Exp) (List.nil U) (List.nil U) (List.nil U)
           (sh4_M) (sh4_dbl_base) (sh4_dbl_step) (sh4_num n)
           (sh4_num_typed chkf n) (sh4_formP chkf) (sh4_base chkf) (sh4_let_step chkf))))

;; --- denotation of the doubler -------------------------------------------------------------
;; ⟦recN⟧ at a successor is the step at (⟦recN⟧ of the predecessor, the
;; predecessor, η) (den_recNS_eval).  The step's let splits the package
;; (den_letp_some; skOf of variable 0 is the Σ skeleton) into (⋆, the code),
;; and the node is two copies of that code.  Iterating from the leaf is the
;; same bush as Proposition 4.11.

(thm sh4_den_var1 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code, u :- Unit]
  (Eq Code (den chkf dec encTy cap (Exp.var 1)
               (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.syn
               (Prod.mk u (Prod.mk c en)))
           c)
  (rw [(den_var_at chkf dec encTy cap 1 (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.syn (Prod.mk u (Prod.mk c en)))]))
(thm sh4_den_star [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, G :- (List Sk), en :- (HEnv G)]
  (Eq Unit (den chkf dec encTy cap Exp.star G Sk.unit en) Unit.unit)
  (rw [(den_star_at chkf dec encTy cap G Sk.unit en)]))
;; nthS does not compute, so the equation lemma supplies the lookup.
(thm sh4_sk_var0 [G :- (List Sk)]
  (Eq (Option Sk) (skOf (List.cons Sk (Sk.prod Sk.syn Sk.unit) G) (Exp.var 0))
                  (Option.some Sk (Sk.prod Sk.syn Sk.unit)))
  (exact (nthS.eq_2 (Sk.prod Sk.syn Sk.unit) G)))
(thm sh4_den_acc [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  cap :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code, j :- Nat]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (Exp.var 0)
           (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
           (Sk.prod Sk.syn Sk.unit)
           (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))
      (Prod.mk c Unit.unit))
  (rw [(den_var_at chkf dec encTy cap 0
         (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
         (Sk.prod Sk.syn Sk.unit)
         (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))]))
(thm sh4_fst [c :- Code] (Eq Code (Prod.fst (Prod.mk c Unit.unit)) c) (rfl))
(thm sh4_snd [c :- Code] (Eq Unit (Prod.snd (Prod.mk c Unit.unit)) Unit.unit) (rfl))

(thm sh4_den_snode1 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                     cap :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code, u :- Unit]
  (Eq Code (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1))
               (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.syn
               (Prod.mk u (Prod.mk c en)))
           (Code.sn 0 c c))
  (rw [(den_snode_code chkf dec encTy cap 0 (Exp.var 1) (Exp.var 1)
         (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) (Prod.mk u (Prod.mk c en)))])
  (exact (congrArg (fn [a :- Code] (Code.sn 0 a a)) (sh4_den_var1 chkf dec encTy cap G en c u))))

(thm sh4_den_body [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code, u :- Unit]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (sh4_dbl_body)
           (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) (Sk.prod Sk.syn Sk.unit)
           (Prod.mk u (Prod.mk c en)))
      (Prod.mk (Code.sn 0 c c) Unit.unit))
  (have hs (Eq Code
            (den chkf dec encTy cap (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1))
                 (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.syn (Prod.mk u (Prod.mk c en)))
            (Code.sn 0 c c))
    (sh4_den_snode1 chkf dec encTy cap G en c u))
  (have hu (Eq Unit
            (den chkf dec encTy cap Exp.star
                 (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.unit (Prod.mk u (Prod.mk c en)))
            Unit.unit)
    (sh4_den_star chkf dec encTy cap (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) (Prod.mk u (Prod.mk c en))))
  (exact (Eq.trans
    (den_pair_prod chkf dec encTy cap (sh4_M)
       (Exp.snode (Exp.lbl 0) (Exp.var 1) (Exp.var 1)) Exp.star
       (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.syn Sk.unit (Prod.mk u (Prod.mk c en)))
    (Eq.trans
      (congrArg (fn [a :- Code]
                  (Prod.mk a (den chkf dec encTy cap Exp.star
                       (List.cons Sk Sk.unit (List.cons Sk Sk.syn G)) Sk.unit (Prod.mk u (Prod.mk c en)))))
                hs)
      (congrArg (fn [b :- Unit] (Prod.mk (Code.sn 0 c c) b)) hu)))))

;; Opening a package whose code is c yields the package of the doubled node.
(thm sh4_den_step [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   cap :- Nat, G :- (List Sk), en :- (HEnv G), c :- Code, j :- Nat]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (sh4_dbl_step)
           (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
           (Sk.prod Sk.syn Sk.unit)
           (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))
      (Prod.mk (Code.sn 0 c c) Unit.unit))
  (have hlet (Eq (Prod Code Unit)
              (den chkf dec encTy cap (sh4_dbl_step)
                   (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
                   (Sk.prod Sk.syn Sk.unit)
                   (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))
              (den chkf dec encTy cap (sh4_dbl_body)
                   (List.cons Sk Sk.unit (List.cons Sk Sk.syn
                     (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))))
                   (Sk.prod Sk.syn Sk.unit)
                   (Prod.mk (Prod.snd (den chkf dec encTy cap (Exp.var 0)
                        (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
                        (Sk.prod Sk.syn Sk.unit)
                        (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))
                     (Prod.mk (Prod.fst (den chkf dec encTy cap (Exp.var 0)
                        (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
                        (Sk.prod Sk.syn Sk.unit)
                        (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))
                       (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))))
    (den_letp_some chkf dec encTy cap (sh4_M) (Exp.var 0) (sh4_dbl_body)
       (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
       Sk.syn Sk.unit (Sk.prod Sk.syn Sk.unit)
       (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))
       (sh4_sk_var0 (List.cons Sk Sk.nat G))))
  (have henv (Eq (Prod Unit (Prod Code (HEnv (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G)))))
              (Prod.mk (Prod.snd (den chkf dec encTy cap (Exp.var 0)
                   (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
                   (Sk.prod Sk.syn Sk.unit)
                   (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))
                (Prod.mk (Prod.fst (den chkf dec encTy cap (Exp.var 0)
                   (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
                   (Sk.prod Sk.syn Sk.unit)
                   (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))
                  (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en))))
              (Prod.mk Unit.unit (Prod.mk c (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))))
    (congrArg (fn [p :- (Prod Code Unit)]
                (Prod.mk (Prod.snd p)
                  (Prod.mk (Prod.fst p) (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))))
              (sh4_den_acc chkf dec encTy cap G en c j)))
  (have hb (Eq (Prod Code Unit)
            (den chkf dec encTy cap (sh4_dbl_body)
                 (List.cons Sk Sk.unit (List.cons Sk Sk.syn
                   (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))))
                 (Sk.prod Sk.syn Sk.unit)
                 (Prod.mk Unit.unit (Prod.mk c (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)))))
            (Prod.mk (Code.sn 0 c c) Unit.unit))
    (sh4_den_body chkf dec encTy cap
       (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))
       (Prod.mk (Prod.mk c Unit.unit) (Prod.mk j en)) c Unit.unit))
  (exact (Eq.trans hlet
           (Eq.trans (congrArg (fn [e :- (Prod Unit (Prod Code (HEnv (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G)))))]
                        (den chkf dec encTy cap (sh4_dbl_body)
                             (List.cons Sk Sk.unit (List.cons Sk Sk.syn
                               (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat G))))
                             (Sk.prod Sk.syn Sk.unit) e))
                      henv)
             hb))))

(thm sh4_den_base [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (sh4_dbl_base) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
      (Prod.mk (Code.sl 0) Unit.unit))
  (have hl (Eq Code (den chkf dec encTy cap (Exp.sleaf (Exp.lbl 0)) (List.nil Sk) Sk.syn Unit.unit) (Code.sl 0))
    (den_sleaf_code chkf dec encTy cap 0 (List.nil Sk) Unit.unit))
  (have hu (Eq Unit (den chkf dec encTy cap Exp.star (List.nil Sk) Sk.unit Unit.unit) Unit.unit)
    (sh4_den_star chkf dec encTy cap (List.nil Sk) Unit.unit))
  (exact (Eq.trans
    (den_pair_prod chkf dec encTy cap (sh4_M) (Exp.sleaf (Exp.lbl 0)) Exp.star
       (List.nil Sk) Sk.syn Sk.unit Unit.unit)
    (Eq.trans
      (congrArg (fn [a :- Code] (Prod.mk a (den chkf dec encTy cap Exp.star (List.nil Sk) Sk.unit Unit.unit))) hl)
      (congrArg (fn [b :- Unit] (Prod.mk (Code.sl 0) b)) hu)))))

(thm sh4_den_num [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, n :- Nat]
  (Eq Nat (den chkf dec encTy cap (sh4_num n) (List.nil Sk) Sk.nat Unit.unit) n)
  (induction n)
  (exact (den_zero_nat chkf dec encTy cap (List.nil Sk) Unit.unit))
  (rw [(den_succ_at chkf dec encTy cap (sh4_num n) (List.nil Sk) Sk.nat Unit.unit)])
  (exact (congrArg Nat.succ ih_n)))

(thm sh4_den_doubler [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, n :- Nat]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
      (Prod.mk (sh4_bush n) Unit.unit))
  (induction n)
  (exact (Eq.trans
    (Eq.symm (den_recNZ chkf dec encTy cap (sh4_M) (sh4_dbl_base) (sh4_dbl_step)
               (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit))
    (sh4_den_base chkf dec encTy cap)))
  (have he (Eq (Prod Code Unit)
            (den chkf dec encTy cap
                 (Exp.recN (sh4_M) (sh4_dbl_base) (sh4_dbl_step) (Exp.succ (sh4_num n)))
                 (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
            (den chkf dec encTy cap (sh4_dbl_step)
                 (sk2 (Sk.prod Sk.syn Sk.unit) Sk.nat (List.nil Sk))
                 (Sk.prod Sk.syn Sk.unit)
                 (Prod.mk (den chkf dec encTy cap (sh4_doubler (sh4_num n))
                              (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
                          (Prod.mk (den chkf dec encTy cap (sh4_num n) (List.nil Sk) Sk.nat Unit.unit) Unit.unit))))
    (den_recNS_eval chkf dec encTy cap (sh4_M) (sh4_dbl_base) (sh4_dbl_step) (sh4_num n)
       (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit))
  (have henv (Eq (Prod (Prod Code Unit) (Prod Nat Unit))
              (Prod.mk (den chkf dec encTy cap (sh4_doubler (sh4_num n))
                           (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
                       (Prod.mk (den chkf dec encTy cap (sh4_num n) (List.nil Sk) Sk.nat Unit.unit) Unit.unit))
              (Prod.mk (Prod.mk (sh4_bush n) Unit.unit) (Prod.mk n Unit.unit)))
    (Eq.trans
      (congrArg (fn [p :- (Prod Code Unit)]
                  (Prod.mk p (Prod.mk (den chkf dec encTy cap (sh4_num n) (List.nil Sk) Sk.nat Unit.unit) Unit.unit)))
                ih_n)
      (congrArg (fn [k :- Nat] (Prod.mk (Prod.mk (sh4_bush n) Unit.unit) (Prod.mk k Unit.unit)))
                (sh4_den_num chkf dec encTy cap n))))
  (have hs (Eq (Prod Code Unit)
            (den chkf dec encTy cap (sh4_dbl_step)
                 (List.cons Sk (Sk.prod Sk.syn Sk.unit) (List.cons Sk Sk.nat (List.nil Sk)))
                 (Sk.prod Sk.syn Sk.unit)
                 (Prod.mk (Prod.mk (sh4_bush n) Unit.unit) (Prod.mk n Unit.unit)))
            (Prod.mk (Code.sn 0 (sh4_bush n) (sh4_bush n)) Unit.unit))
    (sh4_den_step chkf dec encTy cap (List.nil Sk) Unit.unit (sh4_bush n) n))
  (exact (Eq.trans he
           (Eq.trans (congrArg (fn [e :- (Prod (Prod Code Unit) (Prod Nat Unit))]
                        (den chkf dec encTy cap (sh4_dbl_step)
                             (sk2 (Sk.prod Sk.syn Sk.unit) Sk.nat (List.nil Sk))
                             (Sk.prod Sk.syn Sk.unit) e))
                      henv)
             hs))))

;; Proposition 4.12 (1), the denotation: at numeral n the package is the bush.
(thm prop412_den [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, n :- Nat]
  (Eq (Prod Code Unit)
      (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)
      (Prod.mk (sh4_bush n) Unit.unit))
  (exact (sh4_den_doubler chkf dec encTy cap n)))
(thm prop412_nodes [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, n :- Nat]
  (Eq Nat (cnodes (Prod.fst (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)))
          (Nat.sub (sh4_pow n) 1))
  (exact (Eq.trans
    (congrArg cnodes (Eq.trans (congrArg Prod.fst (prop412_den chkf dec encTy cap n)) (sh4_fst (sh4_bush n))))
    (prop411_nodes n))))
;; Proposition 4.12 (1): budget 0, and the denoted code has 2ⁿ − 1 internal nodes.
(thm prop412 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), cap :- Nat, n :- Nat]
  (And (Rt chkf (List.nil Exp) (List.nil U) (sh4_doubler (sh4_num n)) (sh4_M))
       (Eq Nat (cnodes (Prod.fst (den chkf dec encTy cap (sh4_doubler (sh4_num n)) (List.nil Sk) (Sk.prod Sk.syn Sk.unit) Unit.unit)))
               (Nat.sub (sh4_pow n) 1)))
  (exact (And.intro (prop412_typed chkf n) (prop412_nodes chkf dec encTy cap n))))

;; --- Proposition 4.9, the part that is a single derivation ----------------------------
;; The paper's inhabitant, for each k, is a closed term of
;; Π(c :ω Syn). T(depthLeq c k) ⊸ T(chk′ c c⊥) ⊸ 0, built by case analysis
;; on c down to depth k.  Two facts make the full term a different size of
;; job from the arms:
;;   - δ (Hd.delta, then Corollary 3.7) fires only when both codes are
;;     canonical.  While c is a variable, chk c c⊥ does not compute, so the
;;     evidence T(chk c c⊥) does not become T(ff) until a recSyn/caseLbl
;;     branch has substituted a constructor.  Labels are 0..NL-1, so each
;;     caseLbl is 100 branches, and a node has two subcodes: the derivation
;;     for depth k is doubly exponential in k.
;;   - The arm that is uniform, and is proved here, is the one the paper
;;     uses when the depth bound has already computed to ff: T(ff) ▹ 0 by
;;     Hd.tTF, so the evidence itself inhabits 0 (sh4_from_ff).
;; sh4_depth0 is the k = 0 bound: recSyn with motive Bool, tt on a leaf and
;; ff on a node.  It is the predicate the k = 0 inhabitant would case on.
;; It is not that inhabitant.

(thm sh4_nbr_ff [] (Eq Bool (nbr Exp.ff) Bool.true) (rfl))
(thm sh4_nbr_empty [] (Eq Bool (nbr Exp.tEmpty) Bool.true) (rfl))
(thm sh4_nbr_tff [] (Eq Bool (nbr (Exp.tT Exp.ff)) Bool.true) (rfl))
(thm sh4_tt_ty [] (Eq Bool (constTyped Exp.tt Exp.tBool) Bool.true) (rfl))
(thm sh4_ff_ty [] (Eq Bool (constTyped Exp.ff Exp.tBool) Bool.true) (rfl))

;; A head step is a Step at the empty position.
(thm sh4_step_hd [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp, h :- (Hd chkf e e2)]
  (Step chkf e e2)
  (unfold Step)
  (apply (Exists.intro (List.nil Nat)))
  (apply (Exists.intro e))
  (apply (Exists.intro e2))
  (exact (And.intro (getP_nil e) (And.intro h (Eq.symm (setP_nil e e2))))))

;; Hd.tTF: T(ff) ▹ 0.  The skeleton of T(ff) is Unit, witnessed by wT/sFF.
(thm sh4_cv_ff [chkf :- (=> Code Code Bool), G :- (List Sk)]
  (Cv chkf G (Exp.tT Exp.ff) Exp.tEmpty)
  (exact (Cv.cvFwd chkf G (Exp.tT Exp.ff) (Exp.tT Exp.ff) Exp.tEmpty
           (Cv.cvRefl chkf G (Exp.tT Exp.ff) (SkJ.wT G Exp.ff (SkJ.sFF G)) sh4_nbr_tff)
           (sh4_step_hd chkf (Exp.tT Exp.ff) Exp.tEmpty (Hd.tTF chkf))
           (SkJ.wEmpty G) sh4_nbr_empty)))

;; Where the depth bound has computed to ff, the evidence inhabits 0.
(thm sh4_from_ff [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.cons Exp (Exp.tT Exp.ff) (List.nil Exp))
      (List.cons U U.u1 (List.nil U)) (Exp.var 0) Exp.tEmpty)
  (exact (Rt.rConv chkf (List.cons Exp (Exp.tT Exp.ff) (List.nil Exp))
           (List.cons U U.u1 (List.nil U)) (Exp.var 0) (Exp.tT Exp.ff) Exp.tEmpty
           (Rt.rVar chkf (List.cons Exp (Exp.tT Exp.ff) (List.nil Exp))
             (List.cons U U.u1 (List.nil U)) 0 (Exp.tT Exp.ff) U.u1
             rfl (nthE.eq_2 (Exp.tT Exp.ff) (List.nil Exp))
             (nthU.eq_2 U.u1 (List.nil U)) sh4_nz1)
           (Tl.fBase chkf (List.cons Exp (Exp.tT Exp.ff) (List.nil Exp)) Exp.tEmpty rfl)
           (sh4_cv_ff chkf (List.cons Sk Sk.unit (List.nil Sk))))))

;; depthLeq at 0.  The leaf does not use the label or the outer code, and the
;; node does not use its recursive results; those forced usages are carried
;; by the constant (rConst accepts any vector of the right length).
(a/defn sh4_depth0 [] Exp
  (Exp.lam U.uw Exp.tSyn (Exp.recS Exp.tBool Exp.tt Exp.ff (Exp.var 0))))
(thm sh4_depth_leaf [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp)))
      (consU U.uw (vscale U.uw (List.cons U U.u0 (List.nil U))))
      Exp.tt Exp.tBool)
  (exact (Rt.rConst chkf (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp)))
           (consU U.uw (vscale U.uw (List.cons U U.u0 (List.nil U))))
           Exp.tt Exp.tBool rfl sh4_tt_ty)))
(thm sh4_depth_node [chkf :- (=> Code Code Bool)]
  (Rt chkf
      (List.cons Exp Exp.tBool (List.cons Exp Exp.tBool
        (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn
          (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp)))))))
      (consU U.u1 (consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw (List.cons U U.u0 (List.nil U))))))))
      Exp.ff Exp.tBool)
  (exact (Rt.rConst chkf
           (List.cons Exp Exp.tBool (List.cons Exp Exp.tBool
             (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn
               (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp)))))))
           (consU U.u1 (consU U.u1 (consU U.uw (consU U.uw (consU U.uw (vscale U.uw (List.cons U U.u0 (List.nil U))))))))
           Exp.ff Exp.tBool rfl sh4_ff_ty)))
(thm sh4_depth_rec [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.cons Exp Exp.tSyn (List.nil Exp)) (List.cons U U.uw (List.nil U))
      (Exp.recS Exp.tBool Exp.tt Exp.ff (Exp.var 0)) Exp.tBool)
  (exact (Rt.rRecS chkf (List.cons Exp Exp.tSyn (List.nil Exp))
           (List.cons U U.uw (List.nil U)) (List.cons U U.u0 (List.nil U)) (List.cons U U.u0 (List.nil U))
           Exp.tBool Exp.tt Exp.ff (Exp.var 0)
           (Rt.rVar chkf (List.cons Exp Exp.tSyn (List.nil Exp))
             (List.cons U U.uw (List.nil U)) 0 Exp.tSyn U.uw rfl
             (nthE.eq_2 Exp.tSyn (List.nil Exp)) (nthU.eq_2 U.uw (List.nil U)) sh4_nonzero_w)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.nil Exp))) Exp.tBool rfl)
           (sh4_depth_leaf chkf) (sh4_depth_node chkf)
           (Tl.fBase chkf (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn
                            (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp))))) Exp.tBool rfl)
           (Tl.fBase chkf (List.cons Exp Exp.tBool (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn
                            (List.cons Exp Exp.tLbl (List.cons Exp Exp.tSyn (List.nil Exp)))))) Exp.tBool rfl))))
(thm sh4_depth0_typed [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.nil Exp) (List.nil U) (sh4_depth0) (Exp.tPi U.uw Exp.tSyn Exp.tBool))
  (exact (Rt.rLam chkf (List.nil Exp) (List.nil U) U.uw Exp.tSyn
           (Exp.recS Exp.tBool Exp.tt Exp.ff (Exp.var 0)) Exp.tBool
           (Tl.fBase chkf (List.nil Exp) Exp.tSyn rfl)
           (sh4_depth_rec chkf))))
