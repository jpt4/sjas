(ns lcert.formal.s52prod
  "Theorem 5.2: dependent pair construction and elimination (R4 §5).

  Both evaluators use the same second-component substitution. At usage
  zero the erasing evaluator replaces the first component by star; S then
  leaves that component unconstrained, as in the existing E relation."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]))

;; The second component is judged at B[x]. Read its relation at skel B,
;; then substitute x's denotation into the environment. This is the same
;; transport for all three usages and for both evaluators.
(prove! 's52_pair_second
  (concat ctx '[A :- Exp, B :- Exp, x :- Exp, y :- Exp, v :- RV,
    hB :- (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit),
    hx :- (SkJ Bool.false G x (skel A)),
    hk :- (Eq (Option Sk) (skOf G x) (Option.some Sk (skel A))),
    hv :- (S52 chkf dec encTy cap erasing (subst1 x B) G en (skel (subst1 x B)) v
            (den chkf dec encTy cap y G (skel (subst1 x B)) en))])
  '(S52 chkf dec encTy cap erasing B (List.cons Sk (skel A) G)
     (Prod.mk (den chkf dec encTy cap x G (skel A) en) en) (skel B) v
     (den chkf dec encTy cap y G (skel B) en))
  '[(have hU (Eq Sk (skel x) Sk.unit)
      (skj_term_unit Bool.false G x (skel A) hx (Eq.refl Bool.false)))
    (have hvB (S52 chkf dec encTy cap erasing (subst1 x B) G en (skel B) v
                (den chkf dec encTy cap y G (skel B) en))
      (sk_transport (fn [s :- Sk, a :- (Car s)]
        (S52 chkf dec encTy cap erasing (subst1 x B) G en s v a))
        (fn [s :- Sk] (den chkf dec encTy cap y G s en))
        (skel (subst1 x B)) (skel B) (skel_subst1 x B hU) hv))
    (exact (Eq.mp (s52_subst1 chkf dec encTy cap erasing G (skel A) B hB x hx hk en v _)
      hvB))])

(def pair-params
  '[r :- U, A :- Exp, B :- Exp, x :- Exp, y :- Exp, xe :- Exp, ye :- Exp,
    hB :- (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit),
    hx :- (SkJ Bool.false G x (skel A)),
    hk :- (Eq (Option Sk) (skOf G x) (Option.some Sk (skel A)))])

;; Pair in eval_n, at every usage, and Pair1/Pairw in evalE_n.
(prove! 's52_pair
  (concat ctx pair-params
    '[hn :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))]
    ['ihx :- (result 'x 'A 'xe) 'ihy :- (result 'y '(subst1 x B) 'ye)])
  (result '(Exp.pair (Exp.tSig r A B) x y) '(Exp.tSig r A B)
          '(Exp.pair (Exp.tSig r A B) xe ye))
  '[(rw [(den_pair_at chkf dec encTy cap (Exp.tSig r A B) x y G (Sk.prod (skel A) (skel B)) en)])
    (refine' (exT RV _ _ ihx _)) (intro vx px)
    (refine' (exT RV _ _ ihy _)) (intro vy py)
    (constructor) (exact (RV.pair vx vy)) (constructor)
    (exact (trace52_ePair chkf dec encTy erasing cap rho (Exp.tSig r A B) xe ye vx vy
      (And.left px) (And.left py)))
    (constructor) (exact vx) (constructor) (exact vy)
    (constructor) (rfl) (constructor)
    (exact (Eq.mpr (entry52_or erasing r
      (S52 chkf dec encTy cap erasing A G en (skel A) vx (den chkf dec encTy cap x G (skel A) en)) hn)
      (And.right px)))
    (exact (s52_pair_second chkf dec encTy cap erasing G en rho A B x y vy hB hx hk (And.right py)))])

;; Pair0 in evalE_n: only the second component runs. The erased component
;; still supplies its denotation to the dependent second component's type.
(prove! 's52_pair0e
  (concat ctx (drop 3 pair-params) '[he :- (Eq Bool erasing Bool.true)]
    ['ihy :- (result 'y '(subst1 x B) 'ye)])
  (result '(Exp.pair (Exp.tSig U.u0 A B) x y) '(Exp.tSig U.u0 A B)
          '(Exp.pair (Exp.tSig U.u0 A B) Exp.star ye))
  '[(subst he)
    (rw [(den_pair_at chkf dec encTy cap (Exp.tSig U.u0 A B) x y G (Sk.prod (skel A) (skel B)) en)])
    (refine' (exT RV _ _ ihy _)) (intro vy py)
    (constructor) (exact (RV.pair RV.star vy)) (constructor)
    (exact (trace52_ePair chkf dec encTy Bool.true cap rho (Exp.tSig U.u0 A B) Exp.star ye RV.star vy
      (trace52_eStar chkf dec encTy Bool.true cap rho) (And.left py)))
    (constructor) (exact RV.star) (constructor) (exact vy)
    (constructor) (rfl) (constructor) (exact True.intro)
    (exact (s52_pair_second chkf dec encTy cap Bool.true G en rho A B x y vy hB hx hk (And.right py)))])
