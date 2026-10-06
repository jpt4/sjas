(ns lcert.formal.s52branch
  "Theorem 5.2: Boolean and finite-label branch selection (R4 §5).

  S identifies the runtime scrutinee with its carrier, so the safe trace
  takes the same branch as the denotation. Dependent motives are moved
  through the already proved substitution fact."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]
            [lcert.formal.s52prod]
            [lcert.formal.s52data]))

 ;; Expose the folded denotation clauses before rewriting their Boolean.
;; rw matches applications, so den_*_at alone does not expose the test.
(doseq [[nm ctor extra] '[[s52_den_ite Exp.ite []] [s52_den_elimB Exp.elimB [P]]]]
  (let [term (apply list ctor (concat extra '[b t e]))]
    (prove! nm
      (concat ctx (mapcat (fn [x] [x :- 'Exp]) extra) '[b :- Exp, t :- Exp, e :- Exp, s :- Sk])
      (list 'Eq '(Car s) (den term 's)
        (list 'Bool.rec$1 '(fn [_ :- Bool] (Car s)) (den 'e 's) (den 't 's) (den 'b 'Sk.bool)))
      [(list 'rw [(apply list (symbol (str "den_" (if (= ctor 'Exp.ite) "ite" "elimB") "_at"))
         (concat '[chkf dec encTy cap] extra '[b t e G s en]))])])))

;; One selected-branch lemma at a time keeps the Boolean equation in the
;; hypothesis used to build the trace. The unused branch is never run.
(doseq [[nm truth picked ih rule] '[[s52_ite_false Bool.false e ihe eIteF]
                                   [s52_ite_true Bool.true t iht eIteT]]]
  (prove! nm
    (concat ctx '[b :- Exp, t :- Exp, e :- Exp, C :- Exp, be :- Exp, te :- Exp, ee :- Exp]
      ['hbv :- (list 'Eq 'Bool (den 'b 'Sk.bool) truth)
       'ihb :- (result 'b 'Exp.tBool 'be)
       ih :- (result picked 'C (if (= picked 't) 'te 'ee))])
    (result '(Exp.ite b t e) 'C '(Exp.ite be te ee))
    ['(rw [(s52_den_ite chkf dec encTy cap erasing G en rho b t e (skel C))]) '(rw [hbv])
     (list 'refine' (list 'exT 'RV '_ '_ ih '_)) '(intro v hv)
     '(constructor) '(exact v) '(constructor)
     (list 'exact (list (symbol (str "trace52_" rule)) 'chkf 'dec 'encTy 'erasing 'cap 'rho 'be 'te 'ee 'v
       (list 's52_trace_cast 'chkf 'dec 'encTy 'cap 'erasing '(EvSrc.tm rho be)
         (list 'RV.bool (den 'b 'Sk.bool)) (list 'RV.bool truth)
         '(s52_value_bool chkf dec encTy cap erasing G en rho b be ihb) '(congrArg RV.bool hbv))
       '(And.left hv)))
     '(exact (And.right hv))]))

(prove! 's52_ite
  (concat ctx '[b :- Exp, t :- Exp, e :- Exp, C :- Exp, be :- Exp, te :- Exp, ee :- Exp]
    ['ihb :- (result 'b 'Exp.tBool 'be) 'iht :- (result 't 'C 'te) 'ihe :- (result 'e 'C 'ee)])
  (result '(Exp.ite b t e) 'C '(Exp.ite be te ee))
  '[(by_cases (den chkf dec encTy cap b G Sk.bool en))
    (exact (s52_ite_false chkf dec encTy cap erasing G en rho b t e C be te ee hc ihb ihe))
    (exact (s52_ite_true chkf dec encTy cap erasing G en rho b t e C be te ee hc ihb iht))])

;; ElimBool changes the result type with the scrutinee. The branch's
;; relation at P[tt] or P[ff] is first read at P in that constant's carrier
;; environment. The equality hbv identifies it with P at the scrutinee.
(doseq [[nm truth c picked ih sc dc rule]
        '[[s52_elimB_false Bool.false Exp.ff e ihe SkJ.sFF den_ff_bool eElimF]
          [s52_elimB_true Bool.true Exp.tt t iht SkJ.sTT den_tt_bool eElimT]]]
  (let [val (den picked '(skel P))
        at (fn [bv] (apply list 'S52 (concat ps ['P '(List.cons Sk Sk.bool G)
                        (list 'Prod.mk bv 'en) '(skel P) 'v val])))]
    (prove! nm
      (concat ctx '[P :- Exp, b :- Exp, t :- Exp, e :- Exp, be :- Exp, te :- Exp, ee :- Exp,
        hP :- (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit),
        hb :- (SkJ Bool.false G b Sk.bool),
        hk :- (Eq (Option Sk) (skOf G b) (Option.some Sk Sk.bool))]
        ['hbv :- (list 'Eq 'Bool (den 'b 'Sk.bool) truth)
         'ihb :- (result 'b 'Exp.tBool 'be)
         ih :- (result picked (list 'subst1 c 'P) (if (= picked 't) 'te 'ee))])
      (result '(Exp.elimB P b t e) '(subst1 b P) '(Exp.elimB P be te ee))
      ['(have hU (Eq Sk (skel b) Sk.unit) (skj_term_unit Bool.false G b Sk.bool hb (Eq.refl Bool.false)))
       '(rw [(skel_subst1 b P hU)])
       '(rw [(s52_den_elimB chkf dec encTy cap erasing G en rho P b t e (skel P))]) '(rw [hbv])
       (list 'refine' (list 'exT 'RV '_ '_ ih '_)) '(intro v hv)
       (list 'have 'hPc (at (den c 'Sk.bool))
         (list 's52_pair_second 'chkf 'dec 'encTy 'cap 'erasing 'G 'en 'rho 'Exp.tBool 'P c picked 'v
           'hP (list sc 'G) 'rfl '(And.right hv)))
       (list 'have 'hPb (at (den 'b 'Sk.bool))
         (list 'Eq.mp (list 'congrArg (list 'fn '[bv :- Bool] (at 'bv))
           (list 'Eq.trans (list dc 'chkf 'dec 'encTy 'cap 'G 'en) '(Eq.symm hbv))) 'hPc))
       '(constructor) '(exact v) '(constructor)
       (list 'exact (list (symbol (str "trace52_" rule)) 'chkf 'dec 'encTy 'erasing 'cap 'rho 'P 'be 'te 'ee 'v
         (list 's52_trace_cast 'chkf 'dec 'encTy 'cap 'erasing '(EvSrc.tm rho be)
           (list 'RV.bool (den 'b 'Sk.bool)) (list 'RV.bool truth)
           '(s52_value_bool chkf dec encTy cap erasing G en rho b be ihb) '(congrArg RV.bool hbv))
         '(And.left hv)))
       '(exact (Eq.mpr (s52_subst1 chkf dec encTy cap erasing G Sk.bool P hP b hb hk en v _) hPb))])))

(prove! 's52_elimB
  (concat ctx '[P :- Exp, b :- Exp, t :- Exp, e :- Exp, be :- Exp, te :- Exp, ee :- Exp,
    hP :- (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit),
    hb :- (SkJ Bool.false G b Sk.bool),
    hk :- (Eq (Option Sk) (skOf G b) (Option.some Sk Sk.bool))]
    ['ihb :- (result 'b 'Exp.tBool 'be)
     'iht :- (result 't '(subst1 Exp.tt P) 'te) 'ihe :- (result 'e '(subst1 Exp.ff P) 'ee)])
  (result '(Exp.elimB P b t e) '(subst1 b P) '(Exp.elimB P be te ee))
  '[(by_cases (den chkf dec encTy cap b G Sk.bool en))
    (exact (s52_elimB_false chkf dec encTy cap erasing G en rho P b t e be te ee hP hb hk hc ihb ihe))
    (exact (s52_elimB_true chkf dec encTy cap erasing G en rho P b t e be te ee hP hb hk hc ihb iht))])
