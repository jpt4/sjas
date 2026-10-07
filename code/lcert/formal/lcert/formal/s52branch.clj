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

;; Branch-list relation at one label. Positions are label minus the
;; starting label; the motive's environment contains the original label.
(defn- branch-rel [label v a]
  (apply list 'S52 (concat ps ['P '(List.cons Sk Sk.lbl G) (list 'Prod.mk label 'en) '(skel P) v a])))
(defn- branch-result [fun label offset a]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.ap fun (list 'RV.lbl offset)) 'v]))
      (branch-rel label 'v a)))))
(def ^:private fcons (den '(Exp.bcons hd t) '(Sk.arr Sk.lbl (skel P))))
(def ^:private ftail (den 't '(Sk.arr Sk.lbl (skel P))))

(prove! 's52_bnil (concat ctx '[P :- Exp])
  (result 'Exp.bnil '(Exp.tBrs P (NL)) 'Exp.bnil)
  '[(constructor) (exact (RV.bnil (skel P))) (constructor)
    (exact (trace52_eBnil chkf dec encTy erasing cap rho (skel P)))
    (intro l hge hlt) (omega)])

;; The head at position zero. Substitution supplies the relation at label
;; k; the equality k=l identifies the motive's environment with label l.
(prove! 's52_bcons_head
  (concat ctx '[P :- Exp, k :- Nat, l :- Nat, hd :- Exp, t :- Exp, vh :- RV, vt :- RV,
    hP :- (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit), heq :- (Eq Nat k l)]
    ['hh :- (rel '(subst1 (Exp.lbl k) P) 'vh (den 'hd '(skel (subst1 (Exp.lbl k) P))))])
  (branch-result '(RV.bcons vh vt) 'l '(- l k) (list fcons '(- l k)))
  ['(rw [(sub_eq_zero k l heq)])
   (list 'rw [(list 'bcons_zero 'chkf 'dec 'encTy 'cap 'hd 't 'G '(skel P) 'en)])
   (list 'have 'hpk (branch-rel (den '(Exp.lbl k) 'Sk.lbl) 'vh (den 'hd '(skel P)))
     '(s52_pair_second chkf dec encTy cap erasing G en rho Exp.tLbl P (Exp.lbl k) hd vh
       hP (SkJ.sLbl G k) rfl hh))
   (list 'have 'hpl (branch-rel 'l 'vh (den 'hd '(skel P)))
     (list 'Eq.mp (list 'congrArg (list 'fn '[j :- Nat] (branch-rel 'j 'vh (den 'hd '(skel P))))
       '(Eq.trans (den_lbl_lbl chkf dec encTy cap G k en) heq)) 'hpk))
   '(constructor) '(exact vh) '(constructor)
   '(exact (trace52_apBconsZ chkf dec encTy erasing cap vh vt))
   '(exact hpl)])

;; A later position delegates to the tail and keeps the same label in P.
(prove! 's52_bcons_tail
  (concat ctx '[P :- Exp, k :- Nat, l :- Nat, hd :- Exp, t :- Exp, vh :- RV, vt :- RV,
    hlt :- (LT.lt k l)]
    ['ht :- (branch-result 'vt 'l '(- l (+ k 1)) (list ftail '(- l (+ k 1))))])
  (branch-result '(RV.bcons vh vt) 'l '(- l k) (list fcons '(- l k)))
  ['(rw [(sub_succ k l hlt)])
   '(rw [(bcons_succ chkf dec encTy cap hd t G (skel P) en (- l (+ k 1)))])
   '(refine' (exT RV _ _ ht _)) '(intro v hv)
   '(constructor) '(exact v) '(constructor)
   '(exact (trace52_apBconsS chkf dec encTy erasing cap vh vt (- l (+ k 1)) v (And.left hv)))
   '(exact (And.right hv))])

(prove! 's52_bcons
  (concat ctx '[P :- Exp, k :- Nat, hd :- Exp, t :- Exp, he :- Exp, te :- Exp,
    hP :- (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit)]
    ['ihh :- (result 'hd '(subst1 (Exp.lbl k) P) 'he)
     'iht :- (result 't '(Exp.tBrs P (+ k 1)) 'te)])
  (result '(Exp.bcons hd t) '(Exp.tBrs P k) '(Exp.bcons he te))
  '[(refine' (exT RV _ _ ihh _)) (intro vh hh)
    (refine' (exT RV _ _ iht _)) (intro vt ht)
    (constructor) (exact (RV.bcons vh vt)) (constructor)
    (exact (trace52_eBcons chkf dec encTy erasing cap rho he te vh vt (And.left hh) (And.left ht)))
    (intro l hge hlt)
    (have hor (Or (Eq Nat k l) (LT.lt k l)) (Nat.eq_or_lt_of_le hge)) (cases hor)
    (exact (s52_bcons_head chkf dec encTy cap erasing G en rho P k l hd t vh vt hP h (And.right hh)))
    (exact (s52_bcons_tail chkf dec encTy cap erasing G en rho P k l hd t vh vt h
      ((And.right ht) l (Nat.succ_le_of_lt h) hlt)))])

;; CaseLbl selects the branch at the scrutinee's identical runtime and
;; carrier label. Its label bound is carried by S(Lbl), so bnil's vacuity
;; cannot be used to manufacture an out-of-range result.
(prove! 's52_caseL
  (concat ctx '[P :- Exp, x :- Exp, bs :- Exp, xe :- Exp, bse :- Exp,
    hP :- (SkJ Bool.true (List.cons Sk Sk.lbl G) P Sk.unit),
    hx :- (SkJ Bool.false G x Sk.lbl),
    hk :- (Eq (Option Sk) (skOf G x) (Option.some Sk Sk.lbl))]
    ['ihx :- (result 'x 'Exp.tLbl 'xe) 'ihb :- (result 'bs '(Exp.tBrs P 0) 'bse)])
  (result '(Exp.caseL P x bs) '(subst1 x P) '(Exp.caseL P xe bse))
  '[(have hU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false G x Sk.lbl hx (Eq.refl Bool.false)))
    (rw [(skel_subst1 x P hU)])
    (rw [(den_caseL_at chkf dec encTy cap P x bs G (skel P) en)])
    (refine' (exT RV _ _ ihb _)) (intro vb hb)
    (refine' (exT RV _ _ ((And.right hb) (den chkf dec encTy cap x G Sk.lbl en)
      (Nat.zero_le _) (s52_valid_lbl chkf dec encTy cap erasing G en rho x xe ihx)) _))
    (intro v hv) (constructor) (exact v) (constructor)
    (exact (trace52_eCaseL chkf dec encTy erasing cap rho P xe bse
      (RV.lbl (den chkf dec encTy cap x G Sk.lbl en)) vb v
      (s52_value_lbl chkf dec encTy cap erasing G en rho x xe ihx) (And.left hb) (And.left hv)))
    (exact (Eq.mpr (s52_subst1 chkf dec encTy cap erasing G Sk.lbl P hP x hx hk en v _) (And.right hv)))])
