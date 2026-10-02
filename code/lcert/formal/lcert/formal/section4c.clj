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
