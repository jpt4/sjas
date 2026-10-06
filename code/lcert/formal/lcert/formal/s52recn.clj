(ns lcert.formal.s52recn
  "Theorem 5.2: natural-number recursion (R4 §5, RecN).

  The inner induction follows the common runtime/carrier numeral. Its
  invariant is S(P) at the predecessor, with no footprint bound. The step
  relation uses the same substitution and weakening as Lemma 3.6."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]
            [lcert.formal.s52prod]
            [lcert.formal.s52data]))

(def ^:private GN '(List.cons Sk Sk.nat G))
(def ^:private GS '(List.cons Sk (skel P) (List.cons Sk Sk.nat G)))
(def ^:private Qs '(subst (fn [j :- Nat] (sSucc j)) P))
(defn- at [A G en s v a]
  (apply list 'S52 (concat ps [A G en s v a])))
(defn- inv [i v a] (at 'P GN (list 'Prod.mk i 'en) '(skel P) v a))

;; S(stepTy P) at (acc,i,eta) is S(P) at (i+1,eta). The result carrier
;; family g accounts for the skeleton equality without dependent rewriting.
(prove! 's52_stepTy
  '[G :- (List Sk), P :- Exp, der :- (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit),
    sk :- Sk, acc :- (Car sk), i :- Nat, en :- (HEnv G), rv :- RV,
    g :- (forall [s Sk] (Car s)),
    h :- (S52 chkf dec encTy cap erasing (stepTy P)
      (List.cons Sk sk (List.cons Sk Sk.nat G)) (Prod.mk acc (Prod.mk i en))
      (skel (stepTy P)) rv (g (skel (stepTy P))))]
  (inv '(Nat.succ i) 'rv '(g (skel P)))
  [(list 'have 'hQ (list 'SkJ 'Bool.true GN Qs 'Sk.unit)
     (list 'skj_subst 'Bool.true GN 'P 'Sk.unit 'der GN '(fn [j :- Nat] (sSucc j)) '(subOK_sSucc G)))
   (list 'have 'h2 (at Qs GN '(Prod.mk i en) (list 'skel Qs) 'rv (list 'g (list 'skel Qs)))
     (list 'Eq.mp (apply list 's52_lift_fam (concat ps [GN Qs 'hQ 0 'sk '(Prod.mk i en) 'acc 'rv 'g])) 'h))
   (list 'have 'h3 (at Qs GN '(Prod.mk i en) '(skel P) 'rv '(g (skel P)))
     (list 'sk_transport (list 'fn '[s :- Sk, a :- (Car s)] (at Qs GN '(Prod.mk i en) 's 'rv 'a))
       'g (list 'skel Qs) '(skel P) '(skel_subst P (fn [j :- Nat] (sSucc j)) sSucc_unit) 'h2))
   (list 'have 'h4 (at 'P GN (list 'envOf 'chkf 'dec 'encTy 'cap GN '(fn [j :- Nat] (sSucc j)) GN '(Prod.mk i en))
     '(skel P) 'rv '(g (skel P)))
     (list 'Eq.mp (apply list 's52_subst (concat ps [GN 'P 'der GN '(fn [j :- Nat] (sSucc j))
        '(Prod.mk i en) '(subOK_sSucc G) 'rv '(g (skel P))])) 'h3))
   (list 'exact (list 'Eq.mp (list 'congrArg (list 'fn ['q :- (list 'HEnv GN)]
     (at 'P GN 'q '(skel P) 'rv '(g (skel P)))) '(envOf_sSucc chkf dec encTy cap G i en)) 'h4))])

(defn- iter-val [k] (list 'Nat.rec$1 '(fn [_ :- Nat] (Car (skel P))) 'a0 'stepf k))
(defn- iter-result [k]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.iter 'rho 'ste k 'zv) 'v]))
      (inv k 'v (iter-val k))))))
(defn- step-result [i av alpha]
  (list 'Exists (list 'fn '[w :- RV]
    (list 'And (apply list 'Trace52 (concat ps
      [(list 'EvSrc.tm (list 'List.cons 'RV av (list 'List.cons 'RV (list 'RV.nat i) 'rho)) 'ste) 'w]))
      (inv (list 'Nat.succ i) 'w (list 'stepf i alpha))))))

;; Inner induction on the numeral, including the trace of every step.
(prove! 's52_iter
  (concat ctx '[P :- Exp, ste :- Exp, zv :- RV, a0 :- (Car (skel P)),
    stepf :- (=> Nat (Car (skel P)) (Car (skel P)))]
    ['h0 :- (inv 0 'zv 'a0)
     'hs :- (list 'forall '[i Nat] (list 'forall '[av RV] (list 'forall '[alpha (Car (skel P))]
       (list '=> (inv 'i 'av 'alpha) (step-result 'i 'av 'alpha)))))])
  (list 'forall '[count Nat] (iter-result 'count))
  ['(intro count) '(induction count)
   '(constructor) '(exact zv) '(constructor)
   '(exact (trace52_eIterZ chkf dec encTy erasing cap rho ste zv)) '(exact h0)
   '(refine' (exT RV _ _ ih_n _)) '(intro mid hm)
   (list 'refine' (list 'exT 'RV '_ '_ (list 'hs 'n 'mid (iter-val 'n) '(And.right hm)) '_))
   '(intro out ho) '(constructor) '(exact out) '(constructor)
   '(exact (trace52_eIterS chkf dec encTy erasing cap rho ste n zv mid out (And.left hm) (And.left ho)))
   '(exact (And.right ho))])

;; The rule case supplies the invariant from z : P[0] and the body under
;; the predecessor and accumulator. No usage is read here; environment
;; restriction and extension are supplied by the derivation induction.
(def ^:private SF '(fn [i :- Nat, alpha :- (Car (skel P))]
  (den chkf dec encTy cap st (List.cons Sk (skel P) (List.cons Sk Sk.nat G))
    (skel P) (Prod.mk alpha (Prod.mk i en)))))
(def ^:private ZV (den 'z '(skel P)))
(prove! 's52_recN_step
  (concat ctx '[P :- Exp, st :- Exp, ste :- Exp, i :- Nat, av :- RV, alpha :- (Car (skel P)),
    hP :- (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit)]
    ['ih :- (walk/postwalk-replace {'G GS 'en '(Prod.mk alpha (Prod.mk i en))
       'rho '(List.cons RV av (List.cons RV (RV.nat i) rho))} (result 'st '(stepTy P) 'ste))])
  (walk/postwalk-replace {'stepf SF} (step-result 'i 'av 'alpha))
  '[(refine' (exT RV _ _ ih _)) (intro w hw)
    (constructor) (exact w) (constructor) (exact (And.left hw))
    (exact (s52_stepTy chkf dec encTy cap erasing G P hP (skel P) alpha i en w
      (fn [s :- Sk] (den chkf dec encTy cap st (List.cons Sk (skel P) (List.cons Sk Sk.nat G))
        s (Prod.mk alpha (Prod.mk i en)))) (And.right hw)))])

(prove! 's52_recN
  (concat ctx '[P :- Exp, z :- Exp, st :- Exp, nv :- Exp, ze :- Exp, ste :- Exp, ne :- Exp,
    hP :- (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit),
    hn :- (SkJ Bool.false G nv Sk.nat),
    hk :- (Eq (Option Sk) (skOf G nv) (Option.some Sk Sk.nat))]
    ['ihn :- (result 'nv 'Exp.tNat 'ne) 'ihz :- (result 'z '(subst1 Exp.zero P) 'ze)
     'ihs :- (list 'forall '[i Nat] (list 'forall '[av RV] (list 'forall '[alpha (Car (skel P))]
       (list '=> (inv 'i 'av 'alpha)
         (walk/postwalk-replace {'G GS 'en '(Prod.mk alpha (Prod.mk i en))
           'rho '(List.cons RV av (List.cons RV (RV.nat i) rho))}
           (result 'st '(stepTy P) 'ste))))))])
  (result '(Exp.recN P z st nv) '(subst1 nv P) '(Exp.recN P ze ste ne))
  ['(have hU (Eq Sk (skel nv) Sk.unit) (skj_term_unit Bool.false G nv Sk.nat hn (Eq.refl Bool.false)))
   '(rw [(skel_subst1 nv P hU)])
   '(rw [(den_recN_at chkf dec encTy cap P z st nv G (skel P) en)])
   '(refine' (exT RV _ _ ihz _)) '(intro zv hz)
   (list 'have 'hzc (inv (den 'Exp.zero 'Sk.nat) 'zv ZV)
     '(s52_pair_second chkf dec encTy cap erasing G en rho Exp.tNat P Exp.zero z zv hP (SkJ.sZero G) rfl (And.right hz)))
   (list 'have 'hz0 (inv 0 'zv ZV)
     (list 'Eq.mp (list 'congrArg (list 'fn '[i :- Nat] (inv 'i 'zv ZV))
       '(den_zero_nat chkf dec encTy cap G en)) 'hzc))
   (list 'have 'hstep
     (walk/postwalk-replace {'stepf SF}
       (list 'forall '[i Nat] (list 'forall '[av RV] (list 'forall '[alpha (Car (skel P))]
         (list '=> (inv 'i 'av 'alpha) (step-result 'i 'av 'alpha))))))
     '(fn [i :- Nat, av :- RV, alpha :- (Car (skel P)),
           ha :- (S52 chkf dec encTy cap erasing P (List.cons Sk Sk.nat G) (Prod.mk i en) (skel P) av alpha)]
        (s52_recN_step chkf dec encTy cap erasing G en rho P st ste i av alpha hP (ihs i av alpha ha))))
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_iter (concat ps ['G 'en 'rho 'P 'ste 'zv ZV SF 'hz0 'hstep (den 'nv 'Sk.nat)])) '_))
   '(intro v hv) '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eRecN chkf dec encTy erasing cap rho P ze ste ne
     (den chkf dec encTy cap nv G Sk.nat en) zv v
     (s52_value_nat chkf dec encTy cap erasing G en rho nv ne ihn) (And.left hz) (And.left hv)))
   '(exact (Eq.mpr (s52_subst1 chkf dec encTy cap erasing G Sk.nat P hP nv hn hk en v _) (And.right hv)))])
