(ns lcert.formal.recsyn
  "The RecSyn case of the fundamental lemma (R4-metatheory.md §3.5, Lemma 3.6).

  recSyn_{x.P}(a. tl, a c₁ c₂ y₁ y₂. tn, c) denotes, at an environment η, the
  code recursor Code.rec on ⟦c⟧ (den_gen.clj, den_recS):
    leaf l        ↦ ⟦tl⟧ at (l, η)
    node l a b ya yb ↦ ⟦tn⟧ at (yb, (ya, (b, (a, (l, η)))))
  The paper's argument: both methods run once per constructor, in ω-scaled
  contexts, so by induction on the code every result has footprint 0.
  V(Syn) is the codes whose labels are all below NL (lblOk, skel.clj);
  V(Lbl) is the labels below NL.  c's induction hypothesis gives lblOk ⟦c⟧,
  and the goal V(P[c/x])η is V(P) at (⟦c⟧, η) by V_subst1, raised from
  footprint 0 to k by V_mono.  The shape follows F_recN (fundamental.clj).

  The motive instances are substitutions of P (judgment.clj):
    leafTy P = P[sLeafI]   under the label a          (lbl :: Γ → syn :: Γ)
    nodeTy P = P[sNodeI]   under the five node binders
    y1Ty P   = P[sAt 1 3]  under c₂ c₁ a              (the recursive result at c₁)
    y2Ty P   = P[sAt 1 4]  under y₁ c₂ c₁ a           (the recursive result at c₂)
  This file first shows each substitution is SubOK and computes envOf, the
  same two facts subOK_sSucc and envOf_sSucc give RecN's step (vweaken.clj).
  V at the substituted motive is then V(P) at that environment (V_subst),
  which is what the code induction reads."
  (:require [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.fundamental]
            [lcert.formal.vweaken]))

;; ---------------------------------------------------------------------------
;; sLeafI.  P lives under x : Syn.  sLeafI sends that variable to sleaf a
;; (variable 0 of the leaf method's context, a : Lbl) and shifts the rest of
;; Γ up by one.  It is consSub of (sleaf (var 0)) and the shift, exactly as
;; sSucc is consSub of (succ (var 0)) and the shift.
;;   source context  syn :: G     (where P is formed)
;;   target context  lbl :: G     (where tl is typed, at leafTy P)
;;   environment     (l, η)  ↦  (sl l, η)
;; ---------------------------------------------------------------------------

(thm sLeafI_consSub [i :- Nat]
  (Eq Exp (sLeafI i) (consSub (Exp.sleaf (Exp.var 0)) (fn [j :- Nat] (Exp.var (+ j 1))) i))
  (cases i) (rfl) (rfl))

;; SubOK (syn :: G) sLeafI (lbl :: G).  The head sleaf (var 0) is a code
;; (sSleaf, its label the variable 0 of the label context); the tail is the
;; shift, SubOK by subOK_shift.  skOf of sleaf is syn regardless of its
;; argument, so the skOf-faithfulness at index 0 is the constant some syn.
(thm subOK_sLeafI [G :- (List Sk)]
  (SubOK (List.cons Sk Sk.syn G) (fn [i :- Nat] (sLeafI i)) (List.cons Sk Sk.lbl G))
  (have e (Eq (=> Nat Exp) (fn [i :- Nat] (sLeafI i))
              (consSub (Exp.sleaf (Exp.var 0)) (fn [j :- Nat] (Exp.var (+ j 1)))))
    (funext (fn [i :- Nat] (sLeafI_consSub i))))
  (rw [e])
  (exact (subOK_cons (Exp.sleaf (Exp.var 0)) Sk.syn G (fn [j :- Nat] (Exp.var (+ j 1))) (List.cons Sk Sk.lbl G)
           (SkJ.sSleaf (List.cons Sk Sk.lbl G) (Exp.var 0)
             (SkJ.sVar (List.cons Sk Sk.lbl G) 0 Sk.lbl (nthS.eq_2 Sk.lbl G)))
           (Eq.refl$1 (Option.some Sk Sk.syn))
           (subOK_shift Sk.lbl G))))

;; ⟦sleaf (var 0)⟧ at (l, η) in the label context is the code sl l.
;; den_sleaf_at exposes the clause; den_var_eq then reduces the label
;; (lookup of variable 0, coerced from Lbl to Lbl) to l, and coe syn syn
;; of the resulting leaf is the leaf.
(thm den_sleaf_var0 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                     n :- Nat, G :- (List Sk), l :- Nat, en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (Exp.sleaf (Exp.var 0)) (List.cons Sk Sk.lbl G) Sk.syn (Prod.mk l en)) (Code.sl l))
  (rw [(den_sleaf_at chkf dec encTy n (Exp.var 0) (List.cons Sk Sk.lbl G) Sk.syn (Prod.mk l en))])
  (rw [(den_var_eq chkf dec encTy n 0)]))

;; envOf of sLeafI at (l, η) is (sl l, η): the consSub environment is the
;; denotation of the head in front of the shift's environment, and the shift
;; drops the label binder (envOf_shift).
(thm envOf_sLeafI [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   n :- Nat, G :- (List Sk), l :- Nat, en :- (HEnv G)]
  (Eq (HEnv (List.cons Sk Sk.syn G))
      (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sLeafI j)) (List.cons Sk Sk.lbl G) (Prod.mk l en))
      (Prod.mk (Code.sl l) en))
  (have e (Eq (=> Nat Exp) (fn [j :- Nat] (sLeafI j)) (consSub (Exp.sleaf (Exp.var 0)) (fn [j :- Nat] (Exp.var (+ j 1)))))
    (funext (fn [j :- Nat] (sLeafI_consSub j))))
  (rw [e])
  (change (Eq (HEnv (List.cons Sk Sk.syn G))
              (Prod.mk (den chkf dec encTy n (Exp.sleaf (Exp.var 0)) (List.cons Sk Sk.lbl G) Sk.syn (Prod.mk l en))
                       (envOf chkf dec encTy n G (fn [j :- Nat] (Exp.var (+ j 1))) (List.cons Sk Sk.lbl G) (Prod.mk l en)))
              (Prod.mk (Code.sl l) en)))
  (rw [(den_sleaf_var0 chkf dec encTy n G l en) (envOf_shift chkf dec encTy n Sk.lbl G l en)]))

;; ---------------------------------------------------------------------------
;; A variable shift past a prefix.  sNodeI and sAt do not stop at one binder:
;; sAt k sh replaces variable 0 by variable k and sends j+1 to variable j+sh,
;; and sNodeI's tail is the shift by 5.  For a prefix `pre` of skeleton
;; binders standing in front of G,
;;   variable (j + |pre|) of (pre ++ G) is variable j of G,
;; so the substitution j ↦ var (j + |pre|) is SubOK from G to pre ++ G, and
;; its environment at any extension of η is η itself (the prefix values are
;; not read).  appS is prefix concatenation (substitution.clj); it reduces
;; on cons, as does List.length.
;; ---------------------------------------------------------------------------

;; skel of a variable is Unit (the catch-all of skel): a shifted variable is
;; a term, which is what SubOK's third clause asks.
(thm skel_var [i :- Nat] (Eq Sk (skel (Exp.var i)) Sk.unit) (rfl))

;; skOf of a variable is the context's nth skeleton.  skOfF's variable clause
;; is nthS, so this is the computation, not an equation lemma.
(thm skOf_var [G :- (List Sk), i :- Nat]
  (Eq (Option Sk) (skOf G (Exp.var i)) (nthS G i))
  (rfl))

;; nthS (pre ++ G) (i + |pre|) = nthS G i.  One binder at a time is
;; nthS.eq_3; the prefix is peeled by induction.
(thm nthS_past [G :- (List Sk), i :- Nat, pre :- (List Sk)]
  (Eq (Option Sk) (nthS (appS pre G) (+ i (List.length Sk pre))) (nthS G i))
  (induction pre)
  (rfl)
  (exact (Eq.trans (nthS.eq_3 head (appS tail G) (+ i (List.length Sk tail))) ih_tail)))

(thm subOK_past [G :- (List Sk), pre :- (List Sk)]
  (SubOK G (fn [j :- Nat] (Exp.var (+ j (List.length Sk pre)))) (appS pre G))
  (exact (subOK_mk G (fn [j :- Nat] (Exp.var (+ j (List.length Sk pre)))) (appS pre G)
           (fn [i :- Nat, s :- Sk, h :- (Eq (Option Sk) (nthS G i) (Option.some Sk s))]
             (SkJ.sVar (appS pre G) (+ i (List.length Sk pre)) s (Eq.trans (nthS_past G i pre) h)))
           (fn [i :- Nat]
             (Eq.trans (skOf_var (appS pre G) (+ i (List.length Sk pre))) (nthS_past G i pre)))
           (fn [i :- Nat] (skel_var (+ i (List.length Sk pre)))))))

;; var (j + m + 1) is lift 1 0 of var (j + m): the shift by one more binder
;; is the lifted shift, which is what envAt_lift / envOf_lift1 consumes.
(thm var_shift_succ [j :- Nat, m :- Nat]
  (Eq Exp (Exp.var (+ j (Nat.succ m))) (lift 1 0 (Exp.var (+ j m))))
  (exact (Eq.symm (lift_var_above 1 0 (+ j m) (Nat.zero_le (+ j m))))))

;; Dropping one binder from a substitution's environment.  envAt_lift says a
;; substitution lifted under one ignored binder denotes the same environment;
;; envOf is envAt of den (envOf_envAt, den_fun_eq), so the same holds for it.
(thm envOf_lift1 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, Gp :- (List Sk), sg :- (=> Nat Exp), H :- (List Sk), a :- Sk, v :- (Car a), eta :- (HEnv H),
                  h :- (SubTy Gp sg H)]
  (Eq (HEnv Gp)
      (envOf chkf dec encTy n Gp (fn [i :- Nat] (lift 1 0 (sg i))) (List.cons Sk a H) (Prod.mk v eta))
      (envOf chkf dec encTy n Gp sg H eta))
  (rw [(envOf_envAt chkf dec encTy n Gp (fn [i :- Nat] (lift 1 0 (sg i))) (List.cons Sk a H) (Prod.mk v eta))])
  (rw [(envOf_envAt chkf dec encTy n Gp sg H eta)])
  (rw [(den_fun_eq chkf dec encTy n)])
  (exact (envAt_lift chkf dec encTy (denPrev chkf dec encTy n) n a H eta v Gp sg h)))

;; The tail of an environment for pre ++ G: the values of the prefix are
;; discarded, what remains is an environment for G.  Used only to state
;; envOf_past; on a concrete prefix it reduces to the nested Prod.snd.
(kdef envDrop
  (forall [G (List Sk)] (forall [pre (List Sk)] (=> (HEnv (appS pre G)) (HEnv G))))
  (fn [G :- (List Sk), pre :- (List Sk)]
    (List.rec$1$0 Sk (fn [pre :- (List Sk)] (=> (HEnv (appS pre G)) (HEnv G)))
      (fn [ep :- (HEnv G)] ep)
      (fn [s :- Sk, rest :- (List Sk), ih :- (=> (HEnv (appS rest G)) (HEnv G))]
        (fn [ep :- (HEnv (List.cons Sk s (appS rest G)))] (ih (Prod.snd ep))))
      pre)))

;; envOf of j ↦ var (j + |pre|) at any environment of pre ++ G is the tail
;; environment.  Nil is the identity substitution (envOf_id).  At one more
;; binder the shift is the lift of the shorter shift (var_shift_succ), so
;; envOf_lift1 peels it and the induction hypothesis reads the tail.
(thm envOf_past [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                 n :- Nat, G :- (List Sk), pre :- (List Sk)]
  (forall [ep (HEnv (appS pre G))]
    (Eq (HEnv G)
        (envOf chkf dec encTy n G (fn [j :- Nat] (Exp.var (+ j (List.length Sk pre)))) (appS pre G) ep)
        (envDrop G pre ep)))
  (induction pre)
  (intro ep)
  (exact (envOf_id chkf dec encTy n G ep))
  (intro ep)
  (cases ep)
  (have hsub (SubTy G (fn [j :- Nat] (Exp.var (+ j (List.length Sk tail)))) (appS tail G))
    (subOK_ty G (fn [j :- Nat] (Exp.var (+ j (List.length Sk tail)))) (appS tail G) (subOK_past G tail)))
  (have efn (Eq (=> Nat Exp)
                (fn [j :- Nat] (Exp.var (+ j (List.length Sk (List.cons Sk head tail)))))
                (fn [j :- Nat] (lift 1 0 (Exp.var (+ j (List.length Sk tail))))))
    (funext (fn [j :- Nat] (var_shift_succ j (List.length Sk tail)))))
  (rw [efn])
  (exact (Eq.trans
           (envOf_lift1 chkf dec encTy n G (fn [j :- Nat] (Exp.var (+ j (List.length Sk tail)))) (appS tail G) head fst snd hsub)
           (ih_tail snd))))

;; ---------------------------------------------------------------------------
;; sAt k sh: variable 0 ↦ variable k, variable j+1 ↦ variable j+sh.
;;   y1Ty uses sAt 1 3.  The three binders are c₂ : Syn, c₁ : Syn, a : Lbl,
;;   and variable 1 is c₁, so at (b, (a, (l, η))) the environment is (a, η):
;;   V(y1Ty P) there is V(P) at the code a.
;;   y2Ty uses sAt 1 4.  The four binders are y₁, c₂, c₁, a, and variable 1
;;   is c₂, so at (ya, (b, (a, (l, η)))) the environment is (b, η).
;; The head skeleton of the source is syn in both cases; y₁'s skeleton (the
;; extra binder of y2Ty) is an arbitrary s, since sAt 1 4 does not read it.
;; ---------------------------------------------------------------------------

(thm sAt_consSub [k :- Nat, sh :- Nat, i :- Nat]
  (Eq Exp (sAt k sh i) (consSub (Exp.var k) (fn [j :- Nat] (Exp.var (+ j sh))) i))
  (cases i) (rfl) (rfl))

;; Where variable 1 sits in y1Ty's context c₂ :: c₁ :: a :: G: it is c₁.
(thm nthS_at1_y1 [G :- (List Sk)]
  (Eq (Option Sk) (nthS (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 1) (Option.some Sk Sk.syn))
  (exact (Eq.trans (nthS.eq_3 Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)) 0)
                   (nthS.eq_2 Sk.syn (List.cons Sk Sk.lbl G)))))

(thm subOK_sAt13 [G :- (List Sk)]
  (SubOK (List.cons Sk Sk.syn G) (fn [i :- Nat] (sAt 1 3 i))
         (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
  (have e (Eq (=> Nat Exp) (fn [i :- Nat] (sAt 1 3 i)) (consSub (Exp.var 1) (fn [j :- Nat] (Exp.var (+ j 3)))))
    (funext (fn [i :- Nat] (sAt_consSub 1 3 i))))
  (rw [e])
  (exact (subOK_cons (Exp.var 1) Sk.syn G (fn [j :- Nat] (Exp.var (+ j 3)))
           (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
           (SkJ.sVar (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 1 Sk.syn (nthS_at1_y1 G))
           (Eq.trans (skOf_var (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 1) (nthS_at1_y1 G))
           (subOK_past G (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk))))))))

;; ⟦var 1⟧ at (b, (a, (l, η))) in syn :: syn :: lbl :: G is the code a.
(thm den_var1_y1 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, G :- (List Sk), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (Exp.var 1)
                (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) Sk.syn
                (Prod.mk b (Prod.mk a (Prod.mk l en))))
           a)
  (rw [(den_var_eq chkf dec encTy n 1)]))

(thm envOf_sAt13 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, G :- (List Sk), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq (HEnv (List.cons Sk Sk.syn G))
      (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sAt 1 3 j))
             (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
             (Prod.mk b (Prod.mk a (Prod.mk l en))))
      (Prod.mk a en))
  (have e (Eq (=> Nat Exp) (fn [j :- Nat] (sAt 1 3 j)) (consSub (Exp.var 1) (fn [j :- Nat] (Exp.var (+ j 3)))))
    (funext (fn [j :- Nat] (sAt_consSub 1 3 j))))
  (rw [e])
  (change (Eq (HEnv (List.cons Sk Sk.syn G))
              (Prod.mk (den chkf dec encTy n (Exp.var 1)
                            (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) Sk.syn
                            (Prod.mk b (Prod.mk a (Prod.mk l en))))
                       (envOf chkf dec encTy n G (fn [j :- Nat] (Exp.var (+ j 3)))
                              (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
                              (Prod.mk b (Prod.mk a (Prod.mk l en)))))
              (Prod.mk a en)))
  (rw [(den_var1_y1 chkf dec encTy n G b a l en)])
  (rw [(envOf_past chkf dec encTy n G (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk))))
                    (Prod.mk b (Prod.mk a (Prod.mk l en))))]))

;; y2Ty's prefix: an arbitrary skeleton s for y₁, then c₂ c₁ a.
(thm nthS_at1_y2 [s :- Sk, G :- (List Sk)]
  (Eq (Option Sk)
      (nthS (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 1)
      (Option.some Sk Sk.syn))
  (exact (Eq.trans (nthS.eq_3 s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 0)
                   (nthS.eq_2 Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))))

(thm subOK_sAt14 [s :- Sk, G :- (List Sk)]
  (SubOK (List.cons Sk Sk.syn G) (fn [i :- Nat] (sAt 1 4 i))
         (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
  (have e (Eq (=> Nat Exp) (fn [i :- Nat] (sAt 1 4 i)) (consSub (Exp.var 1) (fn [j :- Nat] (Exp.var (+ j 4)))))
    (funext (fn [i :- Nat] (sAt_consSub 1 4 i))))
  (rw [e])
  (exact (subOK_cons (Exp.var 1) Sk.syn G (fn [j :- Nat] (Exp.var (+ j 4)))
           (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
           (SkJ.sVar (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 1 Sk.syn
             (nthS_at1_y2 s G))
           (Eq.trans (skOf_var (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 1)
                     (nthS_at1_y2 s G))
           (subOK_past G (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk)))))))))

(thm den_var1_y2 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, s :- Sk, G :- (List Sk), ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (Exp.var 1)
                (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) Sk.syn
                (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
           b)
  (rw [(den_var_eq chkf dec encTy n 1)]))

(thm envOf_sAt14 [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  n :- Nat, s :- Sk, G :- (List Sk), ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq (HEnv (List.cons Sk Sk.syn G))
      (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sAt 1 4 j))
             (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
             (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
      (Prod.mk b en))
  (have e (Eq (=> Nat Exp) (fn [j :- Nat] (sAt 1 4 j)) (consSub (Exp.var 1) (fn [j :- Nat] (Exp.var (+ j 4)))))
    (funext (fn [j :- Nat] (sAt_consSub 1 4 j))))
  (rw [e])
  (change (Eq (HEnv (List.cons Sk Sk.syn G))
              (Prod.mk (den chkf dec encTy n (Exp.var 1)
                            (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) Sk.syn
                            (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
                       (envOf chkf dec encTy n G (fn [j :- Nat] (Exp.var (+ j 4)))
                              (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
                              (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))
              (Prod.mk b en)))
  (rw [(den_var1_y2 chkf dec encTy n s G ya b a l en)])
  (rw [(envOf_past chkf dec encTy n G
                    (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk)))))
                    (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))]))

;; ---------------------------------------------------------------------------
;; sNodeI.  Variable 0 of P (the code the motive recurses on) becomes
;; snode (var 4) (var 3) (var 2): the label, c₁ and c₂ of the five-binder
;; node context.  Variables j+1 become variable j+5, i.e. they land in Γ
;; past y₂ y₁ c₂ c₁ a.  The two y binders have an arbitrary skeleton s
;; (in the rule, s = skel P); sNodeI does not read them.
;;   source   syn :: G
;;   target   s :: s :: syn :: syn :: lbl :: G
;;   (yb, (ya, (b, (a, (l, η)))))  ↦  (sn l a b, η)
;; ---------------------------------------------------------------------------

;; Where each of var 4, var 3, var 2 sits.  Innermost first:
;; index 0 y₂, 1 y₁, 2 c₂ : Syn, 3 c₁ : Syn, 4 a : Lbl.
(thm nthS_node_lbl [s :- Sk, G :- (List Sk)]
  (Eq (Option Sk)
      (nthS (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 4)
      (Option.some Sk Sk.lbl))
  (exact (Eq.trans (nthS.eq_3 s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 3)
          (Eq.trans (nthS.eq_3 s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 2)
          (Eq.trans (nthS.eq_3 Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)) 1)
          (Eq.trans (nthS.eq_3 Sk.syn (List.cons Sk Sk.lbl G) 0)
                    (nthS.eq_2 Sk.lbl G)))))))

(thm nthS_node_c1 [s :- Sk, G :- (List Sk)]
  (Eq (Option Sk)
      (nthS (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 3)
      (Option.some Sk Sk.syn))
  (exact (Eq.trans (nthS.eq_3 s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 2)
          (Eq.trans (nthS.eq_3 s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 1)
          (Eq.trans (nthS.eq_3 Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)) 0)
                    (nthS.eq_2 Sk.syn (List.cons Sk Sk.lbl G)))))))

(thm nthS_node_c2 [s :- Sk, G :- (List Sk)]
  (Eq (Option Sk)
      (nthS (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 2)
      (Option.some Sk Sk.syn))
  (exact (Eq.trans (nthS.eq_3 s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))) 1)
          (Eq.trans (nthS.eq_3 s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))) 0)
                    (nthS.eq_2 Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))))

;; Index j+5 of the node context is index j of G, hence index j+1 of syn :: G.
(thm nthS_node_tail [s :- Sk, G :- (List Sk), j :- Nat]
  (Eq (Option Sk)
      (nthS (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (+ j 5))
      (nthS G j))
  (exact (nthS_past G j (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk)))))))))

;; Simple typing of sNodeI i at whatever skeleton variable i has in syn :: G.
;; Index 0 is the snode, typed by sSleaf's sibling sSnode from the three
;; lookups above.  Index j+1 is the shifted variable, typed by sVar.
;; Nat.rec$0 rather than cases: the index equation has to be a hypothesis of
;; the branch, not a hypothesis that cases would leave mentioning the
;; original index.
(thm sNodeI_ty [s :- Sk, G :- (List Sk), i :- Nat]
  (forall [s2 Sk] (=> (Eq (Option Sk) (nthS (List.cons Sk Sk.syn G) i) (Option.some Sk s2))
                      (SkJ Bool.false (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                           (sNodeI i) s2)))
  (exact (Nat.rec$0
           (fn [j :- Nat] (forall [s2 Sk]
                            (=> (Eq (Option Sk) (nthS (List.cons Sk Sk.syn G) j) (Option.some Sk s2))
                                (SkJ Bool.false (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                                     (sNodeI j) s2))))
           (fn [s2 :- Sk, h0 :- (Eq (Option Sk) (nthS (List.cons Sk Sk.syn G) Nat.zero) (Option.some Sk s2))]
             (skj_cast Bool.false
               (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
               (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)) Sk.syn s2
               (SkJ.sSnode (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                 (Exp.var 4) (Exp.var 3) (Exp.var 2)
                 (SkJ.sVar (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 4 Sk.lbl (nthS_node_lbl s G))
                 (SkJ.sVar (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 3 Sk.syn (nthS_node_c1 s G))
                 (SkJ.sVar (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) 2 Sk.syn (nthS_node_c2 s G)))
               (someS_inj Sk.syn s2 (Eq.trans (Eq.symm (nthS.eq_2 Sk.syn G)) h0))))
           (fn [j :- Nat, _ :- (forall [s2 Sk]
                                 (=> (Eq (Option Sk) (nthS (List.cons Sk Sk.syn G) j) (Option.some Sk s2))
                                     (SkJ Bool.false (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                                          (sNodeI j) s2))),
                s2 :- Sk, hj :- (Eq (Option Sk) (nthS (List.cons Sk Sk.syn G) (Nat.succ j)) (Option.some Sk s2))]
             (SkJ.sVar (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (+ j 5) s2
               (Eq.trans (nthS_node_tail s G j)
                 (Eq.trans (Eq.symm (nthS.eq_3 Sk.syn G j)) hj))))
           i)))

(thm sNodeI_sko [s :- Sk, G :- (List Sk), i :- Nat]
  (Eq (Option Sk)
      (skOf (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (sNodeI i))
      (nthS (List.cons Sk Sk.syn G) i))
  (exact (Nat.rec$0
           (fn [j :- Nat] (Eq (Option Sk)
               (skOf (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (sNodeI j))
               (nthS (List.cons Sk Sk.syn G) j)))
           (Eq.trans (Eq.refl$1 (Option.some Sk Sk.syn)) (Eq.symm (nthS.eq_2 Sk.syn G)))
           (fn [j :- Nat, _ :- (Eq (Option Sk)
                 (skOf (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (sNodeI j))
                 (nthS (List.cons Sk Sk.syn G) j))]
             (Eq.trans (skOf_var (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) (+ j 5))
               (Eq.trans (nthS_node_tail s G j) (Eq.symm (nthS.eq_3 Sk.syn G j)))))
           i)))

(thm subOK_sNodeI [s :- Sk, G :- (List Sk)]
  (SubOK (List.cons Sk Sk.syn G) (fn [i :- Nat] (sNodeI i))
         (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))))
  (exact (subOK_mk (List.cons Sk Sk.syn G) (fn [i :- Nat] (sNodeI i))
           (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
           (sNodeI_ty s G)
           (sNodeI_sko s G)
           sNodeI_unit)))

;; ⟦snode (var 4) (var 3) (var 2)⟧ at (yb, (ya, (b, (a, (l, η))))) is sn l a b.
(thm den_snode_vars [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                     n :- Nat, s :- Sk, G :- (List Sk), yb :- (Car s), ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq Code
      (den chkf dec encTy n (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2))
           (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) Sk.syn
           (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))
      (Code.sn l a b))
  (rw [(den_snode_at chkf dec encTy n (Exp.var 4) (Exp.var 3) (Exp.var 2)
                     (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) Sk.syn
                     (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))] )
  (rw [(den_var_eq chkf dec encTy n 4) (den_var_eq chkf dec encTy n 3) (den_var_eq chkf dec encTy n 2)]))

(thm envOf_sNodeI [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                   n :- Nat, s :- Sk, G :- (List Sk), yb :- (Car s), ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G)]
  (Eq (HEnv (List.cons Sk Sk.syn G))
      (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sNodeI j))
             (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
             (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))
      (Prod.mk (Code.sn l a b) en))
  (change (Eq (HEnv (List.cons Sk Sk.syn G))
              (Prod.mk (den chkf dec encTy n (sNodeI Nat.zero)
                            (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))) Sk.syn
                            (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))
                       (envOf chkf dec encTy n G (fn [j :- Nat] (sNodeI (+ j 1)))
                              (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                              (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))))
              (Prod.mk (Code.sn l a b) en)))
  (have ht (Eq (=> Nat Exp) (fn [j :- Nat] (sNodeI (+ j 1))) (fn [j :- Nat] (Exp.var (+ j 5))))
    (funext (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 5))))))
  (rw [ht])
  (rw [(den_snode_vars chkf dec encTy n s G yb ya b a l en)])
  (rw [(envOf_past chkf dec encTy n G
                    (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk))))))
                    (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))]))

;; ---------------------------------------------------------------------------
;; Reading V through a motive instance.  leafTy, nodeTy, y1Ty and y2Ty are
;; substitutions of P, not lifts (unlike RecN's stepTy).  V_subst therefore
;; applies directly: V(P[σ]) at η, at skeleton skel P, is V(P) at envOf σ.
;; skel of each instance equals skel P only propositionally (skel_leafTy and
;; its siblings), so the value — a family g of carriers — is carried along
;; that equation by sk_transport, the same move V_stepTy makes after the lift.
;;   V_leafTy, V_nodeTy read a method's result back as V(P) at the code.
;;   V_y1Ty, V_y2Ty embed a recursive result, already in V(P), as the y
;;   entry the node method's context asks for.
;; ---------------------------------------------------------------------------

(thm V_leafTy [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit),
               l :- Nat, en :- (HEnv G), K :- Nat, g :- (forall [s Sk] (Car s)),
               h :- (V chkf dec encTy n (leafTy P) (List.cons Sk Sk.lbl G) (Prod.mk l en) K (skel (leafTy P)) (g (skel (leafTy P))))]
  (V chkf dec encTy n P (List.cons Sk Sk.syn G) (Prod.mk (Code.sl l) en) K (skel P) (g (skel P)))
  (have h1 (V chkf dec encTy n (leafTy P) (List.cons Sk Sk.lbl G) (Prod.mk l en) K (skel P) (g (skel P)))
    (sk_transport (fn [s :- Sk, v :- (Car s)] (V chkf dec encTy n (leafTy P) (List.cons Sk Sk.lbl G) (Prod.mk l en) K s v))
                  g (skel (leafTy P)) (skel P) (skel_leafTy P) h))
  (have h2 (V chkf dec encTy n P (List.cons Sk Sk.syn G)
              (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sLeafI j)) (List.cons Sk Sk.lbl G) (Prod.mk l en))
              K (skel P) (g (skel P)))
    (Eq.mp (V_subst chkf dec encTy n (List.cons Sk Sk.syn G) P der (List.cons Sk Sk.lbl G) (fn [j :- Nat] (sLeafI j)) (Prod.mk l en) (subOK_sLeafI G) K (g (skel P))) h1))
  (exact (Eq.mp (congrArg (fn [e :- (HEnv (List.cons Sk Sk.syn G))]
                            (V chkf dec encTy n P (List.cons Sk Sk.syn G) e K (skel P) (g (skel P))))
                          (envOf_sLeafI chkf dec encTy n G l en)) h2)))

(thm V_nodeTy [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit), s :- Sk,
               yb :- (Car s), ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G), K :- Nat, g :- (forall [s2 Sk] (Car s2)),
               h :- (V chkf dec encTy n (nodeTy P)
                       (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                       (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
                       K (skel (nodeTy P)) (g (skel (nodeTy P))))]
  (V chkf dec encTy n P (List.cons Sk Sk.syn G) (Prod.mk (Code.sn l a b) en) K (skel P) (g (skel P)))
  (have h1 (V chkf dec encTy n (nodeTy P)
              (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
              (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
              K (skel P) (g (skel P)))
    (sk_transport (fn [s2 :- Sk, v :- (Car s2)]
                    (V chkf dec encTy n (nodeTy P)
                       (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                       (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))) K s2 v))
                  g (skel (nodeTy P)) (skel P) (skel_nodeTy P) h))
  (have h2 (V chkf dec encTy n P (List.cons Sk Sk.syn G)
              (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sNodeI j))
                     (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                     (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))))
              K (skel P) (g (skel P)))
    (Eq.mp (V_subst chkf dec encTy n (List.cons Sk Sk.syn G) P der
                    (List.cons Sk s (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))))
                    (fn [j :- Nat] (sNodeI j))
                    (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
                    (subOK_sNodeI s G) K (g (skel P))) h1))
  (exact (Eq.mp (congrArg (fn [e :- (HEnv (List.cons Sk Sk.syn G))]
                            (V chkf dec encTy n P (List.cons Sk Sk.syn G) e K (skel P) (g (skel P))))
                          (envOf_sNodeI chkf dec encTy n s G yb ya b a l en)) h2)))

;; The other direction: a value in V(P) at (a, η) is in V(y1Ty P) at
;; (b, (a, (l, η))), which is where the node method finds c₁.
(thm V_y1Ty [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
             G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit),
             b :- Code, a :- Code, l :- Nat, en :- (HEnv G), K :- Nat, g :- (forall [s Sk] (Car s)),
             h :- (V chkf dec encTy n P (List.cons Sk Sk.syn G) (Prod.mk a en) K (skel P) (g (skel P)))]
  (V chkf dec encTy n (y1Ty P) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
     (Prod.mk b (Prod.mk a (Prod.mk l en))) K (skel (y1Ty P)) (g (skel (y1Ty P))))
  (have h1 (V chkf dec encTy n P (List.cons Sk Sk.syn G)
              (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sAt 1 3 j))
                     (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
                     (Prod.mk b (Prod.mk a (Prod.mk l en))))
              K (skel P) (g (skel P)))
    (Eq.mpr (congrArg (fn [e :- (HEnv (List.cons Sk Sk.syn G))]
                        (V chkf dec encTy n P (List.cons Sk Sk.syn G) e K (skel P) (g (skel P))))
                      (envOf_sAt13 chkf dec encTy n G b a l en)) h))
  (have h2 (V chkf dec encTy n (y1Ty P) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
              (Prod.mk b (Prod.mk a (Prod.mk l en))) K (skel P) (g (skel P)))
    (Eq.mpr (V_subst chkf dec encTy n (List.cons Sk Sk.syn G) P der
                     (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
                     (fn [j :- Nat] (sAt 1 3 j)) (Prod.mk b (Prod.mk a (Prod.mk l en)))
                     (subOK_sAt13 G) K (g (skel P))) h1))
  (exact (sk_transport (fn [s :- Sk, v :- (Car s)]
                         (V chkf dec encTy n (y1Ty P) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G)))
                            (Prod.mk b (Prod.mk a (Prod.mk l en))) K s v))
                       g (skel P) (skel (y1Ty P)) (Eq.symm (skel_y1Ty P)) h2)))

;; Likewise c₂: V(P) at (b, η) is V(y2Ty P) at (ya, (b, (a, (l, η)))).
(thm V_y2Ty [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
             G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit), s :- Sk,
             ya :- (Car s), b :- Code, a :- Code, l :- Nat, en :- (HEnv G), K :- Nat, g :- (forall [s2 Sk] (Car s2)),
             h :- (V chkf dec encTy n P (List.cons Sk Sk.syn G) (Prod.mk b en) K (skel P) (g (skel P)))]
  (V chkf dec encTy n (y2Ty P)
     (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
     (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))) K (skel (y2Ty P)) (g (skel (y2Ty P))))
  (have h1 (V chkf dec encTy n P (List.cons Sk Sk.syn G)
              (envOf chkf dec encTy n (List.cons Sk Sk.syn G) (fn [j :- Nat] (sAt 1 4 j))
                     (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
                     (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))
              K (skel P) (g (skel P)))
    (Eq.mpr (congrArg (fn [e :- (HEnv (List.cons Sk Sk.syn G))]
                        (V chkf dec encTy n P (List.cons Sk Sk.syn G) e K (skel P) (g (skel P))))
                      (envOf_sAt14 chkf dec encTy n s G ya b a l en)) h))
  (have h2 (V chkf dec encTy n (y2Ty P)
              (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
              (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))) K (skel P) (g (skel P)))
    (Eq.mpr (V_subst chkf dec encTy n (List.cons Sk Sk.syn G) P der
                     (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
                     (fn [j :- Nat] (sAt 1 4 j)) (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en))))
                     (subOK_sAt14 s G) K (g (skel P))) h1))
  (exact (sk_transport (fn [s2 :- Sk, v :- (Car s2)]
                         (V chkf dec encTy n (y2Ty P)
                            (List.cons Sk s (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
                            (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))) K s2 v))
                       g (skel P) (skel (y2Ty P)) (Eq.symm (skel_y2Ty P)) h2)))

;; ---------------------------------------------------------------------------
;; The code induction (R4 §3.5: by induction on the code, every result has
;; footprint 0).  lblOk is part of the motive, so a node supplies it to both
;; subcodes.  It reduces on constructors: a leaf's lblOk is the label's
;; bound, a node's is the conjunction band_left / band_right split.
;; ---------------------------------------------------------------------------

(thm recrec_inv [α :- Type, Q :- (=> Code α Prop), lf :- (=> Nat α),
                 nd :- (=> Nat Code Code α α α),
                 hlf :- (forall [l Nat] (=> (Eq Bool (Nat.blt l 100) Bool.true) (Q (Code.sl l) (lf l)))),
                 hnd :- (forall [l Nat] (forall [a Code] (forall [b Code] (forall [ya α] (forall [yb α]
                   (=> (Eq Bool (Nat.blt l 100) Bool.true) (Eq Bool (lblOk a) Bool.true) (Eq Bool (lblOk b) Bool.true)
                       (Q a ya) (Q b yb) (Q (Code.sn l a b) (nd l a b ya yb))))))))]
  (forall [w Code] (=> (Eq Bool (lblOk w) Bool.true)
    (Q w (Code.rec$1 (fn [_ :- Code] α) (fn [l :- Nat] (lf l))
                     (fn [l :- Nat, a :- Code, b :- Code, ya :- α, yb :- α] (nd l a b ya yb)) w))))
  (intro w)
  (induction w)
  (intro hok)
  (exact (hlf l hok))
  (intro hok)
  (have hl (Eq Bool (Nat.blt l 100) Bool.true)
    (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (have hr (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
    (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
  (have ha (Eq Bool (lblOk a) Bool.true) (band_left (lblOk a) (lblOk b) hr))
  (have hb (Eq Bool (lblOk b) Bool.true) (band_right (lblOk a) (lblOk b) hr))
  (exact (hnd l a b
            (Code.rec$1 (fn [_ :- Code] α) (fn [l :- Nat] (lf l))
                        (fn [l :- Nat, a :- Code, b :- Code, ya :- α, yb :- α] (nd l a b ya yb)) a)
            (Code.rec$1 (fn [_ :- Code] α) (fn [l :- Nat] (lf l))
                        (fn [l :- Nat, a :- Code, b :- Code, ya :- α, yb :- α] (nd l a b ya yb)) b)
            hl ha hb (ih_a ha) (ih_b hb))))
