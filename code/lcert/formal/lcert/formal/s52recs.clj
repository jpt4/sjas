(ns lcert.formal.s52recs
  "Theorem 5.2: code recursion (R4 §5, RecSyn).

  The inner induction runs on the common runtime/carrier code. Its motive
  carries the code's label bound and S(P) at that code. Each node includes
  the traces of both recursive calls and the node method."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]
            [lcert.formal.s52inst]
            [lcert.formal.s52data]))

(defn- inv [c v a]
  (apply list 'S52 (concat ps ['P '(List.cons Sk Sk.syn G) (list 'Prod.mk c 'en) '(skel P) v a])))
(defn- nest [ctor xs end] (reduce (fn [q x] (list ctor x q)) end (reverse xs)))
(def GL '(List.cons Sk Sk.lbl G))
(def GB '(List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))
(def GN '(List.cons Sk (skel P) (List.cons Sk (skel P) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))))
(def GY '(List.cons Sk (skel (y2Ty P)) (List.cons Sk (skel (y1Ty P)) (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl G))))))
(def EN '(Prod.mk beta (Prod.mk alpha (Prod.mk b (Prod.mk a (Prod.mk l en))))))
(def EY '(Prod.mk (carTo (skel (y2Ty P)) (skel P) (skel_y2Ty P) beta)
  (Prod.mk (carTo (skel (y1Ty P)) (skel P) (skel_y1Ty P) alpha)
    (Prod.mk b (Prod.mk a (Prod.mk l en))))))
(def RN '(List.cons RV br (List.cons RV ar (List.cons RV (RV.code b)
  (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))))))
(defn- method [rho e c aval]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.tm rho e) 'v])) (inv c 'v aval)))))
(defn- cv [c]
  (list 'Code.rec$1 '(fn [_ :- Code] (Car (skel P)))
    '(fn [l :- Nat] (lf l))
    '(fn [l :- Nat, a :- Code, b :- Code, alpha :- (Car (skel P)), beta :- (Car (skel P))]
       (nd l a b alpha beta)) c))
(defn- rec-result [c]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.recs 'rho 'tle 'tne c) 'v])) (inv c 'v (cv c))))))
(def node-pars '[l :- Nat, a :- Code, b :- Code, ar :- RV, br :- RV,
                 alpha :- (Car (skel P)), beta :- (Car (skel P))])
(def node-valid '[hl :- (LT.lt l (NL)), ha :- (Eq Bool (lblOk a) Bool.true), hb :- (Eq Bool (lblOk b) Bool.true)])
(defn- quantify [bs q] (reduce (fn [q [p _ ty]] (list 'forall [p ty] q)) q (reverse (partition 3 bs))))

;; Inner code induction. Every method and both recursive calls are present
;; in the constructed trace, even when the result type forgets their data.
(prove! 's52_recs
  (concat ctx '[P :- Exp, tle :- Exp, tne :- Exp,
    lf :- (=> Nat (Car (skel P))), nd :- (=> Nat Code Code (Car (skel P)) (Car (skel P)) (Car (skel P)))]
    ['hlf :- (list 'forall '[l Nat] (list '=> '(LT.lt l (NL))
      (method '(List.cons RV (RV.lbl l) rho) 'tle '(Code.sl l) '(lf l))))
     'hnd :- (quantify (concat node-pars node-valid)
       (list '=> (inv 'a 'ar 'alpha) (inv 'b 'br 'beta)
         (method RN 'tne '(Code.sn l a b) '(nd l a b alpha beta))))])
  (list 'forall '[c Code] (list '=> '(Eq Bool (lblOk c) Bool.true) (rec-result 'c)))
  ['(intro c) '(induction c)
   '(intro hok)
   '(refine' (exT RV _ _ (hlf l (Eq.mp (Nat.blt_eq l 100) hok)) _)) '(intro v hv)
   '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eRecSL chkf dec encTy erasing cap rho tle tne l v (And.left hv)))
   '(exact (And.right hv))
   '(intro hok)
   '(have hl (LT.lt l (NL)) (Eq.mp (Nat.blt_eq l 100)
     (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok)))
   '(have hab (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
     (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
   '(have ha (Eq Bool (lblOk a) Bool.true) (band_left (lblOk a) (lblOk b) hab))
   '(have hb (Eq Bool (lblOk b) Bool.true) (band_right (lblOk a) (lblOk b) hab))
   '(refine' (exT RV _ _ (ih_a ha) _)) '(intro ar par)
   '(refine' (exT RV _ _ (ih_b hb) _)) '(intro br pbr)
   (list 'refine' (list 'exT 'RV '_ '_ (list 'hnd 'l 'a 'b 'ar 'br (cv 'a) (cv 'b) 'hl 'ha 'hb
     '(And.right par) '(And.right pbr)) '_))
   '(intro v hv) '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eRecSN chkf dec encTy erasing cap rho tle tne l a b ar br v
     (And.left par) (And.left pbr) (And.left hv)))
   '(exact (And.right hv))])

(defn- at-result [G en rho t A e]
  (walk/postwalk-replace {'G G 'en en 'rho rho} (result t A e)))
(def LF '(fn [l :- Nat] (den chkf dec encTy cap tl (List.cons Sk Sk.lbl G) (skel P) (Prod.mk l en))))
(def ND (list 'fn '[l :- Nat, a :- Code, b :- Code, alpha :- (Car (skel P)), beta :- (Car (skel P))]
  (list 'den 'chkf 'dec 'encTy 'cap 'tn GN '(skel P) EN)))

;; The leaf method's type leafTy(P) is P at its leaf code.
(prove! 's52_recS_leaf
  (concat ctx '[P :- Exp, tl :- Exp, tle :- Exp, l :- Nat,
    hP :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
    ['ih :- (at-result GL '(Prod.mk l en) '(List.cons RV (RV.lbl l) rho) 'tl '(leafTy P) 'tle)])
  (method '(List.cons RV (RV.lbl l) rho) 'tle '(Code.sl l)
    (list 'den 'chkf 'dec 'encTy 'cap 'tl GL '(skel P) '(Prod.mk l en)))
  '[(refine' (exT RV _ _ ih _)) (intro v hv)
    (constructor) (exact v) (constructor) (exact (And.left hv))
    (exact (s52_leafTy chkf dec encTy cap erasing v G P hP l en
      (fn [s :- Sk] (den chkf dec encTy cap tl (List.cons Sk Sk.lbl G) s (Prod.mk l en)))
      (And.right hv)))])

;; The node method is judged under y2Ty and y1Ty; the code recursor reads
;; both accumulators at skel P. Transport those two entries, then nodeTy.
(prove! 's52_recS_node
  (concat ctx '[P :- Exp, tn :- Exp, tne :- Exp] node-pars
    '[hP :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit)]
    ['ih :- (at-result GY EY RN 'tn '(nodeTy P) 'tne)])
  (method RN 'tne '(Code.sn l a b) (list 'den 'chkf 'dec 'encTy 'cap 'tn GN '(skel P) EN))
  ['(refine' (exT RV _ _ ih _)) '(intro v hv)
   '(constructor) '(exact v) '(constructor) '(exact (And.left hv))
   (list 'exact (list 's52_nodeTy 'chkf 'dec 'encTy 'cap 'erasing 'v 'G 'P 'hP '(skel P)
     'beta 'alpha 'b 'a 'l 'en (list 'fn '[s :- Sk] (list 'den 'chkf 'dec 'encTy 'cap 'tn GN 's EN))
     '(s52_node_raw chkf dec encTy cap erasing v P G tn beta alpha b a l en (And.right hv))))])

;; RecSyn's rule case: the outer derivation hypotheses supply the leaf and
;; node methods in their related environments. The inner induction uses
;; label bounds from S(Syn), then substitution returns to the type P[c].
(prove! 's52_recS
  (concat ctx '[P :- Exp, tl :- Exp, tn :- Exp, c :- Exp, tle :- Exp, tne :- Exp, ce :- Exp,
    hP :- (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit),
    hc :- (SkJ Bool.false G c Sk.syn), hk :- (Eq (Option Sk) (skOf G c) (Option.some Sk Sk.syn))]
    ['ihc :- (result 'c 'Exp.tSyn 'ce)
     'ihl :- (list 'forall '[l Nat] (list '=> '(LT.lt l (NL))
       (at-result GL '(Prod.mk l en) '(List.cons RV (RV.lbl l) rho) 'tl '(leafTy P) 'tle)))
     'ihn :- (quantify (concat node-pars node-valid)
       (list '=> (inv 'a 'ar 'alpha) (inv 'b 'br 'beta) (at-result GY EY RN 'tn '(nodeTy P) 'tne)))])
  (result '(Exp.recS P tl tn c) '(subst1 c P) '(Exp.recS P tle tne ce))
  ['(have hU (Eq Sk (skel c) Sk.unit) (skj_term_unit Bool.false G c Sk.syn hc (Eq.refl Bool.false)))
   '(rw [(skel_subst1 c P hU)])
   '(rw [(den_recS_at chkf dec encTy cap P tl tn c G (skel P) en)])
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_recs (concat ps ['G 'en 'rho 'P 'tle 'tne LF ND
       '(fn [l :- Nat, hl :- (LT.lt l (NL))]
          (s52_recS_leaf chkf dec encTy cap erasing G en rho P tl tle l hP (ihl l hl)))
       (list 'fn (vec (concat node-pars node-valid
         ['hra :- (inv 'a 'ar 'alpha) 'hrb :- (inv 'b 'br 'beta)]))
         '(s52_recS_node chkf dec encTy cap erasing G en rho P tn tne l a b ar br alpha beta hP
           (ihn l a b ar br alpha beta hl ha hb hra hrb)))
       (den 'c 'Sk.syn) '(s52_valid_syn chkf dec encTy cap erasing G en rho c ce ihc)])) '_))
   '(intro v hv) '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eRecS chkf dec encTy erasing cap rho P tle tne ce (den chkf dec encTy cap c G Sk.syn en) v
     (s52_value_syn chkf dec encTy cap erasing G en rho c ce ihc) (And.left hv)))
   '(exact (Eq.mpr (s52_subst1 chkf dec encTy cap erasing G Sk.syn P hP c hc hk en v _) (And.right hv)))])
