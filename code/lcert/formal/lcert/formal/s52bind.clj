(ns lcert.formal.s52bind
  "Theorem 5.2: functions and dependent substitution (R4 §5).

  The non-erasing function clause is used at every usage; the erasing
  clause differs only at usage zero. Case hypotheses are local results,
  so the later derivation induction supplies environment extension and
  restriction. No extra assumption about a closure's body is hidden in S."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]))

(defn- at [G en rho t A e]
  (walk/postwalk-replace {'G G 'en en 'rho rho} (result t A e)))
(defn- sr [G en A v a]
  (apply list 'S52 (concat ps [A G en (list 'skel A) v a])))
(def GA '(List.cons Sk (skel A) G))
(def ea '(Prod.mk alpha en))
(defn- bodyrel [a w z] (sr GA (list 'Prod.mk a 'en) 'B w z))
(defn- normal [f phi]
  (list 'forall '[av RV] (list 'forall '[alpha (Car (skel A))]
    (list '=> (rel 'A 'av 'alpha)
      (list 'Exists (list 'fn '[w :- RV]
        (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.ap f 'av) 'w]))
          (bodyrel 'alpha 'w (list phi 'alpha)))))))))

;; Expose the ordinary Pi clause under the exact side condition under
;; which it is valid. Keeping that condition in the motive is necessary
;; because Ansatz's cases does not rewrite previously introduced premises.
(prove! 's52_pi_normal
  (concat ctx '[r :- U, A :- Exp, B :- Exp, f :- RV, phi :- (Car (Sk.arr (skel A) (skel B)))])
  (list '=> '(Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))
    (list 'Eq 'Prop (rel '(Exp.tPi r A B) 'f 'phi) (normal 'f 'phi)))
  '[(cases erasing) (intro hn) (rfl)
    (cases r)
    (intro hn) (cases hn) (exact (Bool.noConfusion h)) (exact (Bool.noConfusion h))
    (intro hn) (rfl) (intro hn) (rfl)])

;; Lam (all usages for eval_n; nonzero usages for evalE_n). The closure is
;; safe immediately, and its application is the body's safe trace.
(prove! 's52_lam
  (concat ctx '[r :- U, A :- Exp, B :- Exp, t :- Exp, te :- Exp,
                hn :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))]
    ['ih :- (list 'forall '[av RV] (list 'forall '[alpha (Car (skel A))]
      (list '=> (rel 'A 'av 'alpha)
        (at GA ea '(List.cons RV av rho) 't 'B 'te))))])
  (result '(Exp.lam r A t) '(Exp.tPi r A B) '(Exp.lam r A te))
  ['(rw [(den_lam_at chkf dec encTy cap r A t G (Sk.arr (skel A) (skel B)) en)])
   '(constructor) '(exact (RV.clos rho te)) '(constructor)
   '(exact (trace52_eLam chkf dec encTy erasing cap rho r A te))
   '(refine' (Eq.mpr (s52_pi_normal chkf dec encTy cap erasing G en rho r A B (RV.clos rho te) _ hn) _))
   '(intro av alpha ha)
   '(refine' (exT RV _ _ (ih av alpha ha) _)) '(intro w hw)
   '(constructor) '(exact w) '(constructor)
   '(exact (trace52_apClos chkf dec encTy erasing cap rho te av w (And.left hw)))
   '(exact (And.right hw))])

;; Lam at usage zero for evalE: quantify over the whole carrier, without
;; an S(A) premise. The body receives star at runtime.
(prove! 's52_lam0e
  (concat ctx '[A :- Exp, B :- Exp, t :- Exp, te :- Exp,
                he :- (Eq Bool erasing Bool.true)]
    ['ih :- (list 'forall '[alpha (Car (skel A))]
      (at GA ea '(List.cons RV RV.star rho) 't 'B 'te))])
  (result '(Exp.lam U.u0 A t) '(Exp.tPi U.u0 A B) '(Exp.lam U.u0 A te))
  ['(subst he)
   '(rw [(den_lam_at chkf dec encTy cap U.u0 A t G (Sk.arr (skel A) (skel B)) en)])
   '(constructor) '(exact (RV.clos rho te)) '(constructor)
   '(exact (trace52_eLam chkf dec encTy Bool.true cap rho U.u0 A te))
   '(intro alpha)
   '(refine' (exT RV _ _ (ih alpha) _)) '(intro w hw)
   '(constructor) '(exact w) '(constructor)
   '(exact (trace52_apClos chkf dec encTy Bool.true cap rho te RV.star w (And.left hw)))
   '(exact (And.right hw))])

;; App needs the same well-formed codomain, skeleton typing and skOf
;; faithfulness as the existing substitution theorem. Each comes from the
;; typing rule's formation premises in the final assembly.
(def app-params
  '[r :- U, A :- Exp, B :- Exp, f :- Exp, u :- Exp, fe :- Exp, ue :- Exp,
    hB :- (SkJ Bool.true (List.cons Sk (skel A) G) B Sk.unit),
    hu :- (SkJ Bool.false G u (skel A)),
    hk :- (Eq (Option Sk) (skOf G u) (Option.some Sk (skel A)))])
(def app-prefix
  '[(have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false G u (skel A) hu (Eq.refl Bool.false)))
    (rw [(skel_subst1 u B hU)])
    (rw [(den_app_some chkf dec encTy cap f u G (skel A) (skel B) en hk)])
    (refine' (exT RV _ _ ihf _)) (intro vf hf)])

(prove! 's52_app
  (concat ctx app-params
    '[hn :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))]
    ['ihf :- (result 'f '(Exp.tPi r A B) 'fe) 'ihu :- (result 'u 'A 'ue)])
  (result '(Exp.app f u) '(subst1 u B) '(Exp.app fe ue))
  (concat app-prefix
    [(list 'have 'hfun (normal 'vf (den 'f '(Sk.arr (skel A) (skel B))))
      '(Eq.mp (s52_pi_normal chkf dec encTy cap erasing G en rho r A B vf _ hn) (And.right hf)))
     '(refine' (exT RV _ _ ihu _)) '(intro vu hv)
     '(refine' (exT RV _ _ (hfun vu (den chkf dec encTy cap u G (skel A) en) (And.right hv)) _))
     '(intro w hw)
     '(constructor) '(exact w) '(constructor)
     '(exact (trace52_eApp chkf dec encTy erasing cap rho fe ue vf vu w (And.left hf) (And.left hv) (And.left hw)))
     '(exact (Eq.mpr (s52_subst1 chkf dec encTy cap erasing G (skel A) B hB u hu hk en w _) (And.right hw)))]))

;; App0 for evalE skips the logical argument's evaluation. Its denotation
;; is still the carrier argument of the erasing Pi clause.
(prove! 's52_app0e
  (concat ctx (drop 3 app-params) '[he :- (Eq Bool erasing Bool.true)]
    ['ihf :- (result 'f '(Exp.tPi U.u0 A B) 'fe)])
  (result '(Exp.app f u) '(subst1 u B) '(Exp.app fe Exp.star))
  (concat ['(subst he)] app-prefix
    ['(have hfun (forall [alpha (Car (skel A))]
       (Exists (fn [w :- RV]
         (And (Trace52 chkf dec encTy cap Bool.true (EvSrc.ap vf RV.star) w)
           (S52 chkf dec encTy cap Bool.true B (List.cons Sk (skel A) G) (Prod.mk alpha en)
             (skel B) w ((den chkf dec encTy cap f G (Sk.arr (skel A) (skel B)) en) alpha))))))
       (And.right hf))
     '(refine' (exT RV _ _ (hfun (den chkf dec encTy cap u G (skel A) en)) _)) '(intro w hw)
     '(constructor) '(exact w) '(constructor)
     '(exact (trace52_eApp chkf dec encTy Bool.true cap rho fe Exp.star vf RV.star w (And.left hf)
       (trace52_eStar chkf dec encTy Bool.true cap rho) (And.left hw)))
     '(exact (Eq.mpr (s52_subst1 chkf dec encTy cap Bool.true G (skel A) B hB u hu hk en w _) (And.right hw)))]))
