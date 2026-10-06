(ns lcert.formal.s52itr
  "Theorem 5.2: folding certificates (R4 §5, ItR).

  Each leaf safely applies g to its label. Each node safely applies h to
  the token, label, and both recursive results. The relation's Pi clauses
  provide those applications, and weakening removes the method binders."
  (:require [clojure.walk :as walk]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.s52fund :refer [prove! ps ctx den rel trace result]]
            [lcert.formal.s52data]))

;; One weakening at a fixed skeleton. This avoids changing dependent
;; carrier values when a method's lifted annotation is read at skel X.
(prove! 's52_l1s
  '[G :- (List Sk), A :- Exp, der :- (SkJ Bool.true G A Sk.unit), x :- Sk,
    en :- (HEnv G), vx :- (Car x), s :- Sk, e :- (Eq Sk s (skel A)), rv :- RV, v :- (Car s)]
  '(Eq Prop (S52 chkf dec encTy cap erasing (lift 1 0 A) (List.cons Sk x G) (Prod.mk vx en) s rv v)
            (S52 chkf dec encTy cap erasing A G en s rv v))
  '[(subst e) (exact (s52_lift chkf dec encTy cap erasing G A der 0 x en vx rv v))])

(prove! 's52_lsuc
  '[G :- (List Sk), X :- Exp, j :- Nat, der :- (SkJ Bool.true G (lift j 0 X) Sk.unit), x :- Sk,
    en :- (HEnv G), vx :- (Car x), s :- Sk, e :- (Eq Sk s (skel X)), rv :- RV, v :- (Car s)]
  '(Eq Prop (S52 chkf dec encTy cap erasing (lift (+ j 1) 0 X) (List.cons Sk x G) (Prod.mk vx en) s rv v)
            (S52 chkf dec encTy cap erasing (lift j 0 X) G en s rv v))
  '[(rw [(Eq.symm (lift_comp X 1 j 0))])
    (exact (s52_l1s chkf dec encTy cap erasing G (lift j 0 X) der x en vx s
      (Eq.trans e (Eq.symm (skel_lift X j 0))) rv v))])

;; The one-, two-, three- and four-binder instances used by gTy and hTy.
;; Only the repeated equality transports are generated; each is checked
;; as a separate theorem, for arbitrary binder skeletons and values.
(defn- skctx [xs] (reduce (fn [G s] (list 'List.cons 'Sk s G)) 'G xs))
(defn- envctx [xs] (reduce (fn [en v] (list 'Prod.mk v en)) 'en xs))
(doseq [depth (range 1 5)]
  (let [ss (mapv #(symbol (str "s" %)) (range depth))
        vs (mapv #(symbol (str "a" %)) (range depth))
        params (vec (concat '[G :- (List Sk), X :- Exp, hX :- (SkJ Bool.true G X Sk.unit), en :- (HEnv G)]
          (mapcat (fn [s v] [s :- 'Sk v :- (list 'Car s)]) ss vs) '[rv :- RV, alpha :- (Car (skel X))]))
        at (fn [n] (apply list 'S52 (concat ps [(if (zero? n) 'X (list 'lift n 0 'X))
                    (skctx (take n ss)) (envctx (take n vs)) '(skel X) 'rv 'alpha])))
        d1 (list 'skj_weaken 'Bool.true 'G 'X 'Sk.unit 'hX 0 (first ss))
        dj (reduce (fn [h j] (list 'skj_lsuc (skctx (take j ss)) 'X j (nth ss j) h)) d1 (range 1 (dec depth)))
        pf (if (= depth 1)
             (apply list 's52_lift (concat ps ['G 'X 'hX 0 (first ss) 'en (first vs) 'rv 'alpha]))
             (list 'Eq.trans
               (apply list 's52_lsuc (concat ps [(skctx (butlast ss)) 'X (dec depth) dj (last ss)
                 (envctx (butlast vs)) (last vs) '(skel X) 'rfl 'rv 'alpha]))
               (apply list (symbol (str "s52_lift" (dec depth) "_at"))
                 (concat ps ['G 'X 'hX 'en] (mapcat vector (butlast ss) (butlast vs)) ['rv 'alpha]))))]
    (prove! (symbol (str "s52_lift" depth "_at")) params (list 'Eq 'Prop (at depth) (at 0)) [(list 'exact pf)])))

;; At nonzero usages the Pi clause is the same for both evaluators. The
;; explicit skeletons let the fold use skel X throughout the lifted types.
(doseq [[nm usage] '[[s52_apply1_at U.u1] [s52_applyw_at U.uw]]]
  (prove! nm
    '[G :- (List Sk), en :- (HEnv G), A :- Exp, B :- Exp, x :- Sk, y :- Sk,
      f :- RV, phi :- (Car (Sk.arr x y)), av :- RV, alpha :- (Car x)]
    (list '=>
      (apply list 'S52 (concat ps [(list 'Exp.tPi usage 'A 'B) 'G 'en '(Sk.arr x y) 'f 'phi]))
      '(S52 chkf dec encTy cap erasing A G en x av alpha)
      '(Exists (fn [v :- RV]
         (And (Trace52 chkf dec encTy cap erasing (EvSrc.ap f av) v)
           (S52 chkf dec encTy cap erasing B (List.cons Sk x G) (Prod.mk alpha en) y v (phi alpha))))))
    '[(cases erasing) (intro hf ha) (exact (hf av alpha ha))
      (intro hf ha) (exact (hf av alpha ha))]))

(def SG '(Sk.arr Sk.lbl (skel X)))
(def SH '(Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
(def HC '(Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))))
(def C2 '(List.cons Sk Sk.lbl (List.cons Sk Sk.dia G)))
(def E2 '(Prod.mk l (Prod.mk Unit.unit en)))
(defn- fold-den [c]
  (list 'Code.rec$1 '(fn [_ :- Code] (Car (skel X)))
    '(fn [j :- Nat] (fg j))
    '(fn [j :- Nat, a :- Code, b :- Code, ya :- (Car (skel X)), yb :- (Car (skel X))]
       (fh Unit.unit j ya yb)) c))
(defn- folded [c]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And (apply list 'Trace52 (concat ps [(list 'EvSrc.itr 'vg 'vh c) 'v]))
      (rel 'X 'v (fold-den c))))))
(def fold-params
  (concat ctx '[X :- Exp, hX :- (SkJ Bool.true G X Sk.unit), vg :- RV, vh :- RV]
    ['fg :- (list 'Car SG) 'fh :- (list 'Car SH)]))
(def HG (apply list 'S52 (concat ps ['(gTy X) 'G 'en SG 'vg 'fg])))
(def HH (apply list 'S52 (concat ps ['(hTy X) 'G 'en SH 'vh 'fh])))

;; ItR leaf: the related method applied to the finite label is safe, and
;; its codomain lift 1 0 X is X in the original environment.
(prove! 's52_itr_leaf
  (concat fold-params '[l :- Nat, hl :- (LT.lt l (NL))] ['hg :- HG])
  (folded '(Code.sl l))
  '[(refine' (exT RV _ _ (s52_applyw_at chkf dec encTy cap erasing G en Exp.tLbl (lift 1 0 X)
      Sk.lbl (skel X) vg fg (RV.lbl l) l hg (And.intro (Eq.refl (RV.lbl l)) hl)) _))
    (intro v hv) (constructor) (exact v) (constructor)
    (exact (trace52_eItL chkf dec encTy erasing cap vg vh l v (And.left hv)))
    (exact (Eq.mp (s52_lift1_at chkf dec encTy cap erasing G X hX en Sk.lbl l v (fg l))
      (And.right hv)))])

;; ItR node: the recursive results enter h's third and fourth binders.
;; Weakening supplies their S relations under all earlier method binders.
(prove! 's52_itr_node
  (concat fold-params '[l :- Nat, a :- Code, b :- Code, hl :- (LT.lt l (NL))]
    ['hh :- HH 'iha :- (folded 'a) 'ihb :- (folded 'b)])
  (folded '(Code.sn l a b))
  ['(refine' (exT RV _ _ iha _)) '(intro ar ha)
   '(refine' (exT RV _ _ ihb _)) '(intro br hb)
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_apply1_at (concat ps ['G 'en 'Exp.tDia HC 'Sk.dia (nth SH 2)
       'vh 'fh 'RV.token 'Unit.unit 'hh '(Eq.refl RV.token)])) '_))
   '(intro v1 h1)
   '(refine' (exT RV _ _ (s52_applyw_at chkf dec encTy cap erasing
      (List.cons Sk Sk.dia G) (Prod.mk Unit.unit en) Exp.tLbl
      (Exp.tPi U.u1 (lift 2 0 X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))
      Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))) v1 (fh Unit.unit)
      (RV.lbl l) l (And.right h1) (And.intro (Eq.refl (RV.lbl l)) hl)) _))
   '(intro v2 h2)
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_apply1_at (concat ps [C2 E2 '(lift 2 0 X)
       '(Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)) '(skel X) '(Sk.arr (skel X) (skel X))
       'v2 '(fh Unit.unit l) 'ar (fold-den 'a) '(And.right h2)
       (list 'Eq.mpr (apply list 's52_lift2_at (concat ps ['G 'X 'hX 'en 'Sk.dia 'Unit.unit 'Sk.lbl 'l 'ar (fold-den 'a)]))
         '(And.right ha))])) '_))
   '(intro v3 h3)
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_apply1_at (concat ps [(list 'List.cons 'Sk '(skel X) C2) (list 'Prod.mk (fold-den 'a) E2)
       '(lift 3 0 X) '(lift 4 0 X) '(skel X) '(skel X)
       'v3 (list 'fh 'Unit.unit 'l (fold-den 'a)) 'br (fold-den 'b) '(And.right h3)
       (list 'Eq.mpr (apply list 's52_lift3_at (concat ps ['G 'X 'hX 'en 'Sk.dia 'Unit.unit 'Sk.lbl 'l
         '(skel X) (fold-den 'a) 'br (fold-den 'b)])) '(And.right hb))])) '_))
   '(intro v hv) '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eItN chkf dec encTy erasing cap vg vh l a b ar br v1 v2 v3 v
     (And.left ha) (And.left hb) (And.left h1) (And.left h2) (And.left h3) (And.left hv)))
   (list 'exact (list 'Eq.mp (apply list 's52_lift4_at (concat ps ['G 'X 'hX 'en
     'Sk.dia 'Unit.unit 'Sk.lbl 'l '(skel X) (fold-den 'a) '(skel X) (fold-den 'b)
     'v (list 'fh 'Unit.unit 'l (fold-den 'a) (fold-den 'b))])) '(And.right hv)))])

;; Inner induction on the common runtime/carrier tree, with its finite
;; labels. There is no bound on its node count.
(prove! 's52_itr (concat fold-params ['hg :- HG 'hh :- HH])
  (list 'forall '[c Code] (list '=> '(Eq Bool (lblOk c) Bool.true) (folded 'c)))
  '[(intro c) (induction c)
    (intro hok)
    (exact (s52_itr_leaf chkf dec encTy cap erasing G en rho X hX vg vh fg fh l
      (Eq.mp (Nat.blt_eq l 100) hok) hg))
    (intro hok)
    (have hab (Eq Bool (Bool.and (lblOk a) (lblOk b)) Bool.true)
      (band_right (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok))
    (exact (s52_itr_node chkf dec encTy cap erasing G en rho X hX vg vh fg fh l a b
      (Eq.mp (Nat.blt_eq l 100) (band_left (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)) hok)) hh
      (ih_a (band_left (lblOk a) (lblOk b) hab))
      (ih_b (band_right (lblOk a) (lblOk b) hab))))])

;; Rule case: evaluate both methods and the certificate, then compose the
;; inner fold. Skeleton transport exposes the denotation's common carrier.
(prove! 's52_itR
  (concat ctx '[X :- Exp, g :- Exp, h :- Exp, r :- Exp, ge :- Exp, he :- Exp, re :- Exp,
    hX :- (SkJ Bool.true G X Sk.unit)]
    ['ihg :- (result 'g '(gTy X) 'ge) 'ihh :- (result 'h '(hTy X) 'he) 'ihr :- (result 'r 'Exp.tR 're)])
  (result '(Exp.itR X g h r) 'X '(Exp.itR X ge he re))
  ['(rw [(den_itR_at chkf dec encTy cap X g h r G (skel X) en)])
   '(refine' (exT RV _ _ ihg _)) '(intro vg hg)
   '(refine' (exT RV _ _ ihh _)) '(intro vh hh)
   (list 'have 'hg0 (walk/postwalk-replace {'fg (den 'g SG)} HG)
     (list 'sk_transport '(fn [s :- Sk, alpha :- (Car s)] (S52 chkf dec encTy cap erasing (gTy X) G en s vg alpha))
       '(fn [s :- Sk] (den chkf dec encTy cap g G s en)) '(skel (gTy X)) SG '(skel_gTy X) '(And.right hg)))
   (list 'have 'hh0 (walk/postwalk-replace {'fh (den 'h SH)} HH)
     (list 'sk_transport '(fn [s :- Sk, alpha :- (Car s)] (S52 chkf dec encTy cap erasing (hTy X) G en s vh alpha))
       '(fn [s :- Sk] (den chkf dec encTy cap h G s en)) '(skel (hTy X)) SH '(skel_hTy X) '(And.right hh)))
   (list 'refine' (list 'exT 'RV '_ '_
     (apply list 's52_itr (concat ps ['G 'en 'rho 'X 'hX 'vg 'vh (den 'g SG) (den 'h SH)
       'hg0 'hh0 (den 'r 'Sk.cert) '(s52_valid_cert chkf dec encTy cap erasing G en rho r re ihr)])) '_))
   '(intro v hv) '(constructor) '(exact v) '(constructor)
   '(exact (trace52_eItR chkf dec encTy erasing cap rho X ge he re vg vh
     (den chkf dec encTy cap r G Sk.cert en) v (And.left hg) (And.left hh)
     (s52_value_cert chkf dec encTy cap erasing G en rho r re ihr) (And.left hv)))
   '(exact (And.right hv))])
