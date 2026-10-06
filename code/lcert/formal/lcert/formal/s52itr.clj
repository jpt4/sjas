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
