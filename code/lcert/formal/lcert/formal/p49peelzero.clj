(ns lcert.formal.p49peelzero
  "Depth-zero constructor peel with a symbolic unary conclusion.

  The node arm consumes the whole depth witness by a certified reduction
  T(p49dl 0 (snode ...)) -> T(ff) -> Empty. The leaf arm is supplied as a
  genuine Rt derivation; no theorem conclusion is assumed as a premise."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.p49peeldepth]))

(defn- es [xs tail] (reduce #(list 'consE %2 %1) tail (reverse xs)))
(defn- us [xs tail] (reduce #(list 'consU %2 %1) tail (reverse xs)))
(defn- ls [ty xs tail]
  (reduce #(list 'List.cons ty %2 %1) tail (reverse xs)))
(defn- len-prefix [n h]
  (nth (iterate #(list 'congrArg 'Nat.succ %) h) n))
(defn- lookup [kind ty xs tail i]
  (let [[x & rest-xs] xs]
    (if (zero? i) (list (symbol (str kind ".eq_2")) x (ls ty rest-xs tail))
      (list 'Eq.trans (list (symbol (str kind ".eq_3")) x (ls ty rest-xs tail) (dec i))
        (lookup kind ty rest-xs tail (dec i))))))

(kdef p49pz_A Exp (Exp.tT (p49dl 0 (Exp.var 0))))
(kdef p49pz_M (=> Exp Exp)
  (fn [W :- Exp] (p49peel_motive p49pz_A W)))
(kdef p49pz_De (=> Exp (List Exp) (List Exp))
  (fn [W :- Exp, D :- (List Exp)]
    (consE (p49peel_nodeA p49pz_A) (p49peel_Dn p49pz_A W D))))

(thm p49pz_hself [W :- Exp]
  (= (subst1 (Exp.var 0) (p49pz_M W)) (p49peel_body p49pz_A W))
  (exact (p49pd_self (p49peel_body p49pz_A W))))
(thm p49pz_M_eq [F :- (=> Exp Exp), hL :- (p49pd_Lift F)]
  (= (p49pz_M (F (Exp.var 0)))
     (Exp.tPi U.u1 p49pz_A (F (Exp.var 1))))
  (exact (Eq.trans
    (congrArg (fn [b :- Exp]
      (Exp.tPi U.u1 (Exp.tT b) (lift 1 2 (lift 1 0 (F (Exp.var 0))))))
      (p49pd_dl_lift 0 1 1 (Exp.var 0)))
    (congrArg (fn [C :- Exp] (Exp.tPi U.u1 p49pz_A C))
      (p49pd_B_eq F hL)))))
(thm p49pz_M_subst [F :- (=> Exp Exp), hL :- (p49pd_Lift F),
                    hS :- (p49pd_Subst F), sg :- (=> Nat Exp)]
  (= (subst sg (p49pz_M (F (Exp.var 0))))
     (Exp.tPi U.u1 (Exp.tT (p49dl 0 (sg 0))) (F (lift 1 0 (sg 0)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sg e)) (p49pz_M_eq F hL))
    (Eq.trans
      (congrArg (fn [b :- Exp]
        (Exp.tPi U.u1 (Exp.tT b) (subst (upn 1 sg) (F (Exp.var 1)))))
        (p49dl_subst 0 sg (Exp.var 0)))
      (congrArg (fn [C :- Exp]
        (Exp.tPi U.u1 (Exp.tT (p49dl 0 (sg 0))) C))
        (hS (upn 1 sg) (Exp.var 1)))))))
(thm p49pz_LA_eq []
  (= (p49peel_leafA p49pz_A)
     (Exp.tT (p49dl 0 (Exp.sleaf (Exp.var 0)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sLeafI (Exp.tT e)))
      (p49pd_dl_lift 0 1 1 (Exp.var 0)))
    (congrArg Exp.tT (p49dl_subst 0 sLeafI (Exp.var 0))))))
(thm p49pz_NA_eq []
  (= (p49peel_nodeA p49pz_A)
     (Exp.tT (p49dl 0 (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sNodeI (Exp.tT e)))
      (p49pd_dl_lift 0 1 1 (Exp.var 0)))
    (congrArg Exp.tT (p49dl_subst 0 sNodeI (Exp.var 0))))))

(def ^:private mF '(p49pz_M (F (Exp.var 0))))
(def ^:private d1 '[Exp.tSyn Exp.tSyn Exp.tLbl Exp.tSyn])
(def ^:private d2 (into [(list 'y1Ty mF)] d1))
(def ^:private dnF (into [(list 'y2Ty mF)] d2))
(def ^:private deF (into ['(p49peel_nodeA p49pz_A)] dnF))
(def ^:private fp
  '[chkf :- (=> Code Code Bool), D :- (List Exp), F :- (=> Exp Exp),
    hF :- (p49pd_Form chkf F), hL :- (p49pd_Lift F), hS :- (p49pd_Subst F)])
(defn- tlcode [ctx c]
  (let [D (es ctx 'D)]
    (case (first c)
      Exp.var (let [i (second c)]
        (list 'Tl.zVar 'chkf D i (nth ctx i)
          (lookup 'nthE 'Exp ctx 'D i)))
      Exp.sleaf (list 'Tl.zSleaf 'chkf D (second c) (tlcode ctx (second c)))
      Exp.snode (let [[_ l a b] c]
        (list 'Tl.zSnode 'chkf D l a b
          (tlcode ctx l) (tlcode ctx a) (tlcode ctx b))))))
(defn- evform [ctx c]
  (list 'Tl.fT 'chkf (es ctx 'D) (list 'p49dl 0 c)
    (list 'p49dl_typed 'chkf 0 (es ctx 'D) c (tlcode ctx c))))
(defn- skcode [ctx c]
  (let [G (list 'skels (es ctx 'D))
        gs (mapv #(list 'skel %) ctx)]
    (case (first c)
      Exp.var (let [i (second c)]
        (list 'SkJ.sVar G i (nth gs i)
          (lookup 'nthS 'Sk gs '(skels D) i)))
      Exp.snode (let [[_ l a b] c]
        (list 'SkJ.sSnode G l a b
          (skcode ctx l) (skcode ctx a) (skcode ctx b))))))
(defn- form-cast [ctx target he hB]
  (list 'Eq.mp
    (list 'congrArg
      (list 'fn '[t :- Exp] (list 'Tl 'chkf 'Bool.true (es ctx 'D) 't 'Exp.tUnit))
      (list 'Eq.symm he)) hB))
(doseq [[nm ctx target c he]
        [['p49pz_hM '[Exp.tSyn Exp.tSyn] mF '(Exp.var 0) '(p49pz_M_eq F hL)]
         ['p49pz_hY1 d1 (list 'y1Ty mF) '(Exp.var 1)
          '(p49pz_M_subst F hL hS (sAt 1 3))]
         ['p49pz_hY2 d2 (list 'y2Ty mF) '(Exp.var 1)
          '(p49pz_M_subst F hL hS (sAt 1 4))]]]
  (let [A (list 'Exp.tT (list 'p49dl 0 c))
        c1 (list 'Exp.var (inc (second c)))
        B (list 'Exp.tPi 'U.u1 A (list 'F c1))
        hB (list 'Tl.fPi 'chkf (es ctx 'D) 'U.u1 A (list 'F c1)
             (evform ctx c)
             (list 'hF (es (into [A] ctx) 'D) c1
               (tlcode (into [A] ctx) c1)))]
    (a/prove-theorem nm fp
      (list 'Tl 'chkf 'Bool.true (es ctx 'D) target 'Exp.tUnit)
      [(list 'exact (form-cast ctx target he hB))])))
(doseq [[nm ctx target c he]
        [['p49pz_hLA '[Exp.tLbl Exp.tSyn]
          '(p49peel_leafA p49pz_A) '(Exp.sleaf (Exp.var 0)) '(p49pz_LA_eq)]
         ['p49pz_hNA dnF
          '(p49peel_nodeA p49pz_A)
          '(Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)) '(p49pz_NA_eq)]]]
  (a/prove-theorem nm fp
    (list 'Tl 'chkf 'Bool.true (es ctx 'D) target 'Exp.tUnit)
    [(list 'exact (form-cast ctx target he (evform ctx c)))]))
(a/prove-theorem 'p49pz_hNB fp
  (list 'Tl 'chkf 'Bool.true (es deF 'D)
    '(p49peel_nodeB (F (Exp.var 0))) 'Exp.tUnit)
  [(list 'exact
     (form-cast deF '(p49peel_nodeB (F (Exp.var 0)))
       '(p49pd_NB_eq F hL hS)
       (list 'hF (es deF 'D)
         '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))
         (tlcode deF '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))))])

(kdef p49pz_node (=> Exp Exp)
  (fn [W :- Exp]
    (Exp.lam U.u1 (p49peel_nodeA p49pz_A)
      (Exp.abort (p49peel_nodeB W) (Exp.var 0)))))
(kdef p49pz_term (=> Exp Exp Exp)
  (fn [W :- Exp, l :- Exp]
    (Exp.lam U.uw Exp.tSyn
      (Exp.recS (p49pz_M W)
        (Exp.lam U.u1 (p49peel_leafA p49pz_A) l)
        (p49pz_node W) (Exp.var 0)))))

;; The evidence slot is index 0 in De. Its displayed type is the lifted
;; node-instance predicate; the two explicit conversions below identify it
;; with T(p49dl 0 (snode ...)) and then with Empty.
(thm p49pz_NA_lift []
  (= (lift 1 0 (p49peel_nodeA p49pz_A))
     (Exp.tT (p49dl 0
       (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (lift 1 0 e)) (p49pz_NA_eq))
    (congrArg Exp.tT
      (p49pd_dl_lift 0 1 0
        (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)))))))

(let [M '(p49pz_M W)
      ctx ['(p49peel_nodeA p49pz_A) (list 'y2Ty M) (list 'y1Ty M)
           'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn]
      Dc (es ctx 'D)
      v (into ['U.u1] '[U.u1 U.u1 U.uw U.uw U.uw U.u0])
      vlen (len-prefix 7 '(Eq.trans (p49peel_zlen uv) hlen))
      ;; The exact usage tail is Un uv. The extra zero is its outer-code
      ;; slot and the remaining entries are p49peel_z uv.
      U '(consU U.u1 (p49peel_Un uv))
      Cnode '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))
      whole '(Exp.tT (p49dl 0 (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))
      raw '(lift 1 0 (p49peel_nodeA p49pz_A))
      G (list 'skels Dc)
      hc (tlcode ctx Cnode)
      skwhole (list 'SkJ.wT G (list 'p49dl 0 Cnode)
                (list 'p49pd_dl_sk 0 G Cnode (skcode ctx Cnode)))
      cv1 (list 'Cv.cvFwd 'chkf G whole whole '(Exp.tT Exp.ff)
        (list 'Cv.cvRefl 'chkf G whole skwhole
          (list 'p49pd_dl_nbr 0 Cnode 'rfl))
        (list 'p49split_underT 'chkf (list 'p49dl 0 Cnode) 'Exp.ff
          '(p49dl_zero_node chkf (Exp.var 5) (Exp.var 4) (Exp.var 3)))
        (list 'SkJ.wT G 'Exp.ff (list 'SkJ.sFF G)) 'rfl)
      cv2 (list 'Cv.cvFwd 'chkf G whole '(Exp.tT Exp.ff) 'Exp.tEmpty
            cv1 '(p49split_head chkf (Exp.tT Exp.ff) Exp.tEmpty (Hd.tTF chkf))
            (list 'SkJ.wEmpty G) 'rfl)
      hv (list 'Rt.rVar 'chkf Dc U 0 '(p49peel_nodeA p49pz_A) 'U.u1
           vlen (lookup 'nthE 'Exp ctx 'D 0)
           '(nthU.eq_2 U.u1 (p49peel_Un uv)) 'rfl)
      hcast (list 'p49peel_cast 'chkf Dc U U '(Exp.var 0) raw whole hv
              'rfl '(p49pz_NA_lift))]
  (a/prove-theorem 'p49pz_node_absurd
    '[chkf :- (=> Code Code Bool), W :- Exp, D :- (List Exp),
      uv :- (List U), hlen :- (= (lenU uv) (lenE D))]
    '(Rt chkf (p49pz_De W D) (consU U.u1 (p49peel_Un uv))
       (Exp.var 0) Exp.tEmpty)
    (b/lv [(list 'exact
      (list 'Rt.rConv 'chkf Dc U '(Exp.var 0) whole 'Exp.tEmpty hcast
        (list 'Tl.fBase 'chkf Dc 'Exp.tEmpty 'rfl) cv2))])))

;; The only runtime premise of this theorem is the real leaf continuation.
;; In the node arm the whole depth witness is reduced and eliminated above.
(let [dz '(p49peel_Uz uv)
      dc (es '[Exp.tSyn] 'D)
      dl (es '[Exp.tLbl Exp.tSyn] 'D)
      dn (es dnF 'D)
      leafD (es '[(p49peel_leafA p49pz_A) Exp.tLbl Exp.tSyn] 'D)
      leafU (us '[U.u1 U.uw] dz)
      leaf (list 'Exp.lam 'U.u1 '(p49peel_leafA p49pz_A) 'l)
      rec-term '(Exp.recS (p49pz_M (F (Exp.var 0)))
                  (Exp.lam U.u1 (p49peel_leafA p49pz_A) l)
                  (p49pz_node (F (Exp.var 0))) (Exp.var 0))
      leaf-proof (list 'Rt.rLam 'chkf dl (us '[U.uw] dz) 'U.u1
                   '(p49peel_leafA p49pz_A) 'l
                   '(p49peel_leafB (F (Exp.var 0)))
                   '(p49pz_hLA chkf D F hF hL hS) 'hl)
      node-proof '(p49peel_zero_node_typed chkf
                    (p49peel_Dn p49pz_A (F (Exp.var 0)) D)
                    (p49peel_Un uv)
                    (p49peel_nodeA p49pz_A)
                    (p49peel_nodeB (F (Exp.var 0)))
                    (Exp.var 0)
                    (p49pz_hNA chkf D F hF hL hS)
                    (p49pz_hNB chkf D F hF hL hS)
                    (p49pz_node_absurd chkf (F (Exp.var 0)) D uv hlen))
      leaf-scaled (list 'Eq.mp
        (list 'congrArg
          (list 'fn '[v :- (List U)]
            (list 'Rt 'chkf dl (us '[U.uw U.u0] 'v) leaf
              '(leafTy (p49pz_M (F (Exp.var 0))))))
          '(Eq.symm (p49peel_zscale uv))) leaf-proof)
      node-scaled (list 'Eq.mp
        (list 'congrArg
          (list 'fn '[v :- (List U)]
            (list 'Rt 'chkf dn (us '[U.u1 U.u1 U.uw U.uw U.uw U.u0] 'v)
              '(p49pz_node (F (Exp.var 0)))
              '(nodeTy (p49pz_M (F (Exp.var 0))))))
          '(Eq.symm (p49peel_zscale uv))) node-proof)
      rec-use '(vadd (consU U.uw uv)
                 (vadd (vscale U.uw (p49peel_Uz uv))
                       (vscale U.uw (p49peel_Uz uv))))
      rec-proof (list 'Rt.rRecS 'chkf dc (us '[U.uw] 'uv) dz dz
                  mF leaf '(p49pz_node (F (Exp.var 0))) '(Exp.var 0)
                  (list 'Rt.rVar 'chkf dc (us '[U.uw] 'uv) 0
                    'Exp.tSyn 'U.uw '(congrArg Nat.succ hlen)
                    '(nthE.eq_2 Exp.tSyn D) '(nthU.eq_2 U.uw uv) 'rfl)
                  '(p49pz_hM chkf D F hF hL hS)
                  leaf-scaled node-scaled
                  '(p49pz_hY1 chkf D F hF hL hS)
                  '(p49pz_hY2 chkf D F hF hL hS))
      params (into fp
               '[uv :- (List U), hlen :- (= (lenU uv) (lenE D)),
                 l :- Exp,
                 hl :- (Rt chkf
                   (consE (p49peel_leafA p49pz_A)
                     (consE Exp.tLbl (consE Exp.tSyn D)))
                   (consU U.u1 (consU U.uw (p49peel_Uz uv)))
                   l (p49peel_leafB (F (Exp.var 0))))])]
  (a/prove-theorem 'p49pz_peel params
    '(Rt chkf D uv (p49pz_term (F (Exp.var 0)) l)
       (Exp.tPi U.uw Exp.tSyn
         (p49peel_body p49pz_A (F (Exp.var 0)))))
    (b/lv [(list 'exact
      (list 'Rt.rLam 'chkf 'D 'uv 'U.uw 'Exp.tSyn rec-term
        '(p49peel_body p49pz_A (F (Exp.var 0)))
        '(Tl.fBase chkf D Exp.tSyn rfl)
        (list 'p49peel_cast 'chkf dc rec-use (us '[U.uw] 'uv)
          rec-term '(subst1 (Exp.var 0) (p49pz_M (F (Exp.var 0))))
          '(p49peel_body p49pz_A (F (Exp.var 0)))
          rec-proof '(p49peel_rec_usage uv)
          '(p49pz_hself (F (Exp.var 0))))))])))
