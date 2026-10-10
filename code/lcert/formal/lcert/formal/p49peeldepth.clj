(ns lcert.formal.p49peeldepth
  "Successor-depth obligations for the generic Proposition 4.9 peel.
  Depth computations and all arithmetic inductions remain kernel terms."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.syntactic]
            [lcert.formal.p49depth]
            [lcert.formal.p49peel]
            [lcert.formal.p49split]))

(thm p49pd_bound3 [cut :- Nat] (LT.lt 3 (+ cut 5)) (omega))
(thm p49pd_bound2 [cut :- Nat] (LT.lt 2 (+ cut 5)) (omega))

;; Lifting commutes with every depth predicate, including inside the five
;; recSyn branch binders. This is an induction over arbitrary k.
(thm p49pd_dl_lift [k :- Nat]
  (forall [amt Nat] (forall [cut Nat] (forall [c Exp]
    (= (lift amt cut (p49dl k c)) (p49dl k (lift amt cut c))))))
  (induction k)
  (intro amt cut c) (rfl)
  (intro amt cut c)
  (exact (congrArg
    (fn [body :- Exp] (Exp.recS Exp.tBool Exp.tt body (lift amt cut c)))
    (Eq.trans
      (congrArg
        (fn [x :- Exp] (Exp.ite x (lift amt (+ cut 5) (p49dl n (Exp.var 2))) Exp.ff))
        (Eq.trans (ih_n amt (+ cut 5) (Exp.var 3))
          (congrArg (fn [x :- Exp] (p49dl n x))
            (lift_var_below amt (+ cut 5) 3 (p49pd_bound3 cut)))))
      (congrArg (fn [y :- Exp] (Exp.ite (p49dl n (Exp.var 3)) y Exp.ff))
        (Eq.trans (ih_n amt (+ cut 5) (Exp.var 2))
          (congrArg (fn [x :- Exp] (p49dl n x))
            (lift_var_below amt (+ cut 5) 2 (p49pd_bound2 cut)))))))))

(thm p49pd_rec_usage [uv :- (List U)]
  (= (vadd uv (vadd (vscale U.uw (p49peel_z uv))
                   (vscale U.uw (p49peel_z uv)))) uv)
  (exact (Eq.trans
    (congrArg (fn [v :- (List U)] (vadd uv (vadd v v))) (p49peel_zscale uv))
    (Eq.trans (congrArg (fn [v :- (List U)] (vadd uv v)) (p49peel_zadd uv))
      (p49peel_addz uv)))))

;; Finite proof builders abbreviate explicit rule applications only.
(defn- es [xs tail] (reduce #(list 'consE %2 %1) tail (reverse xs)))
(defn- us [xs tail] (reduce #(list 'consU %2 %1) tail (reverse xs)))
(defn- lookup [kind ty xs tail i]
  (let [[x & more] xs
        rest-list (reduce #(list 'List.cons ty %2 %1) tail (reverse more))]
    (if (zero? i) (list (symbol (str kind ".eq_2")) x rest-list)
      (list 'Eq.trans (list (symbol (str kind ".eq_3")) x rest-list (dec i))
        (lookup kind ty more tail (dec i))))))
(defn- len-prefix [n h]
  (nth (iterate #(list 'congrArg 'Nat.succ %) h) n))
(def ^:private nd '[Exp.tBool Exp.tBool Exp.tSyn Exp.tSyn Exp.tLbl])
(def ^:private nu '[U.u1 U.u1 U.uw U.uw U.uw])
(def ^:private z '(p49peel_z uv))
(def ^:private zlen '(Eq.trans (p49peel_zlen uv) hlen))
(defn- base [D A] (list 'Tl.fBase 'chkf D A 'rfl))
(defn- constant [D uv t A hlen]
  (list 'Rt.rConst 'chkf D uv t A hlen 'rfl))
(defn- node-var [v i]
  (list 'Rt.rVar 'chkf (es nd 'D) (us v z) i (nth nd i) (nth v i)
    (len-prefix 5 zlen) (lookup 'nthE 'Exp nd 'D i)
    (lookup 'nthU 'U v z i) 'rfl))

;; A reusable runtime Bool recSyn with zero ambient branch captures. The
;; caller supplies the actual node branch; the tt leaf is constructed here.
(let [Dnode (es nd 'D) Unode (us nu z)
      Dleaf (es '[Exp.tLbl] 'D) Uleaf (us '[U.uw] z)
      leaf (constant Dleaf Uleaf 'Exp.tt 'Exp.tBool (len-prefix 1 zlen))
      leaf-scaled (list 'Eq.mp
        (list 'congrArg (list 'fn '[v :- (List U)]
          (list 'Rt 'chkf Dleaf (us '[U.uw] 'v) 'Exp.tt 'Exp.tBool))
          '(Eq.symm (p49peel_zscale uv))) leaf)
      node-scaled (list 'Eq.mp
        (list 'congrArg (list 'fn '[v :- (List U)]
          (list 'Rt 'chkf Dnode (us nu 'v) 'tn 'Exp.tBool))
          '(Eq.symm (p49peel_zscale uv))) 'hn)
      R '(Exp.recS Exp.tBool Exp.tt tn c)
      U (list 'vadd 'uv (list 'vadd (list 'vscale 'U.uw z) (list 'vscale 'U.uw z)))]
  (a/prove-theorem 'p49pd_rec_bool
    (into '[chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U), c :- Exp, tn :- Exp,
            hlen :- (= (lenU uv) (lenE D)), hc :- (Rt chkf D uv c Exp.tSyn)]
      ['hn :- (list 'Rt 'chkf Dnode Unode 'tn 'Exp.tBool)])
    (list 'Rt 'chkf 'D 'uv R 'Exp.tBool)
    [(list 'exact
       (list 'p49peel_cast 'chkf 'D U 'uv R 'Exp.tBool 'Exp.tBool
         (list 'Rt.rRecS 'chkf 'D 'uv z z 'Exp.tBool 'Exp.tt 'tn 'c 'hc
           (base (es '[Exp.tSyn] 'D) 'Exp.tBool) leaf-scaled node-scaled
           (base (es '[Exp.tSyn Exp.tSyn Exp.tLbl] 'D) 'Exp.tBool)
           (base (es '[Exp.tBool Exp.tSyn Exp.tSyn Exp.tLbl] 'D) 'Exp.tBool))
         '(p49pd_rec_usage uv) 'rfl))]))

;; Runtime typing of dl at depth zero and at a successor. Both child
;; computations are recursive calls to the induction hypothesis, never
;; assumptions about the proposition eventually proved by the eliminator.
(a/prove-theorem 'p49pd_dl_rt_zero
  '[chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U), c :- Exp,
    hlen :- (= (lenU uv) (lenE D)), hc :- (Rt chkf D uv c Exp.tSyn)]
  '(Rt chkf D uv (p49dl 0 c) Exp.tBool)
  [(list 'exact
     (list 'p49pd_rec_bool 'chkf 'D 'uv 'c 'Exp.ff 'hlen 'hc
       (constant (es nd 'D) (us nu z) 'Exp.ff 'Exp.tBool (len-prefix 5 zlen))))])

(let [v1 '[U.u0 U.u0 U.u0 U.uw U.u0]
      v2 '[U.u1 U.u1 U.uw U.u0 U.uw]
      t '(Exp.ite (p49dl j (Exp.var 3)) (p49dl j (Exp.var 2)) Exp.ff)
      ueq (list 'congrArg (list 'fn '[v :- (List U)] (us nu 'v)) '(p49peel_zadd uv))]
  (a/prove-theorem 'p49pd_dl_rt_succ
    '[chkf :- (=> Code Code Bool), j :- Nat,
      ih :- (forall [D (List Exp)] (forall [uv (List U)] (forall [c Exp]
        (=> (= (lenU uv) (lenE D)) (Rt chkf D uv c Exp.tSyn)
            (Rt chkf D uv (p49dl j c) Exp.tBool))))),
      D :- (List Exp), uv :- (List U), c :- Exp,
      hlen :- (= (lenU uv) (lenE D)), hc :- (Rt chkf D uv c Exp.tSyn)]
    '(Rt chkf D uv (p49dl (+ j 1) c) Exp.tBool)
    [(list 'exact
       (list 'p49pd_rec_bool 'chkf 'D 'uv 'c t 'hlen 'hc
         (list 'p49peel_cast 'chkf (es nd 'D) (list 'vadd (us v1 z) (us v2 z)) (us nu z)
           t 'Exp.tBool 'Exp.tBool
           (list 'Rt.rIte 'chkf (es nd 'D) (us v1 z) (us v2 z)
             '(p49dl j (Exp.var 3)) '(p49dl j (Exp.var 2)) 'Exp.ff 'Exp.tBool
             (list 'ih (es nd 'D) (us v1 z) '(Exp.var 3) (len-prefix 5 zlen) (node-var v1 3))
             (list 'ih (es nd 'D) (us v2 z) '(Exp.var 2) (len-prefix 5 zlen) (node-var v2 2))
             (constant (es nd 'D) (us v2 z) 'Exp.ff 'Exp.tBool (len-prefix 5 zlen)))
           ueq 'rfl)))]))

(thm p49pd_dl_rt [chkf :- (=> Code Code Bool), k :- Nat]
  (forall [D (List Exp)] (forall [uv (List U)] (forall [c Exp]
    (=> (= (lenU uv) (lenE D)) (Rt chkf D uv c Exp.tSyn)
        (Rt chkf D uv (p49dl k c) Exp.tBool)))))
  (induction k)
  (intro D uv c hlen hc) (exact (p49pd_dl_rt_zero chkf D uv c hlen hc))
  (intro D uv c hlen hc) (exact (p49pd_dl_rt_succ chkf n ih_n D uv c hlen hc)))

;; Substitution cancels a fresh binder when its action on every variable
;; is a left inverse of that lift. Generalizing over the cutoff makes the
;; statement stable under all syntax constructors' binder depths.
(kdef p49pd_LInv (=> (=> Nat Exp) Nat Prop)
  (fn [sg :- (=> Nat Exp), cut :- Nat]
    (forall [i Nat] (= (subst sg (lift 1 cut (Exp.var i))) (Exp.var i)))))
(thm p49pd_lt_succ [i :- Nat, cut :- Nat, h :- (LT.lt i cut)]
  (LT.lt (+ i 1) (+ cut 1)) (omega))
(thm p49pd_le_succ [i :- Nat, cut :- Nat, h :- (Not (LT.lt i cut))]
  (LE.le (+ cut 1) (+ i 1)) (omega))
(thm p49pd_lt_zero [cut :- Nat] (LT.lt 0 (+ cut 1)) (omega))
(thm p49pd_one_le [i :- Nat] (LE.le 1 (+ i 1)) (omega))

(thm p49pd_linv_low [sg :- (=> Nat Exp), cut :- Nat, hi :- (p49pd_LInv sg cut),
                     i :- Nat, h :- (LT.lt i cut)]
  (= (subst (up sg) (lift 1 (+ cut 1) (Exp.var (+ i 1)))) (Exp.var (+ i 1)))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst (up sg) e))
      (lift_var_below 1 (+ cut 1) (+ i 1) (p49pd_lt_succ i cut h)))
    (congrArg (fn [e :- Exp] (lift 1 0 e))
      (Eq.trans
        (congrArg (fn [e :- Exp] (subst sg e)) (Eq.symm (lift_var_below 1 cut i h)))
        (hi i))))))
(thm p49pd_linv_high [sg :- (=> Nat Exp), cut :- Nat, hi :- (p49pd_LInv sg cut),
                      i :- Nat, h :- (Not (LT.lt i cut))]
  (= (subst (up sg) (lift 1 (+ cut 1) (Exp.var (+ i 1)))) (Exp.var (+ i 1)))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst (up sg) e))
      (lift_var_above 1 (+ cut 1) (+ i 1) (p49pd_le_succ i cut h)))
    (congrArg (fn [e :- Exp] (lift 1 0 e))
      (Eq.trans
        (congrArg (fn [e :- Exp] (subst sg e))
          (Eq.symm (lift_var_above 1 cut i (Nat.le_of_not_lt h))))
        (hi i))))))
(thm p49pd_linv_up [sg :- (=> Nat Exp), cut :- Nat, hi :- (p49pd_LInv sg cut)]
  (p49pd_LInv (up sg) (+ cut 1))
  (intro i) (cases i)
  (exact (congrArg (fn [e :- Exp] (subst (up sg) e))
    (lift_var_below 1 (+ cut 1) 0 (p49pd_lt_zero cut))))
  (have hd (Decidable (LT.lt n cut)) (Nat.decLt n cut)) (cases hd)
  (exact (p49pd_linv_high sg cut hi n h))
  (exact (p49pd_linv_low sg cut hi n h)))
(thm p49pd_linv_upn [n :- Nat]
  (forall [sg (=> Nat Exp)] (forall [cut Nat]
    (=> (p49pd_LInv sg cut) (p49pd_LInv (upn n sg) (+ cut n)))))
  (induction n)
  (intro sg cut h) (exact h)
  (intro sg cut h) (exact (p49pd_linv_up (upn n sg) (+ cut n) (ih_n sg cut h))))

(def ^:private prove-exp! @#'lcert.formal.syntactic/prove-exp!)
(def ^:private congruence @#'lcert.formal.syntactic/congruence)
(prove-exp! 'p49pd_cancel
  '(forall [sg (=> Nat Exp)] (forall [cut Nat]
     (=> (p49pd_LInv sg cut) (= (subst sg (lift 1 cut e)) e))))
  '[sg cut q_linv]
  (fn [ctor fields]
    (if (= ctor 'var) '[(exact (q_linv i))]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty d]]
             (let [sg2 (if (zero? d) 'sg (list 'upn d 'sg))
                   cut2 (if (zero? d) 'cut (list '+ 'cut d))]
               (if (= ty 'Exp)
                 [ty (list 'subst sg2 (list 'lift 1 cut2 f)) f
                  (list (symbol (str "ih_" f)) sg2 cut2
                    (if (zero? d) 'q_linv (list 'p49pd_linv_upn d 'sg 'cut 'q_linv)))]
                 [ty f f nil]))) fields)))])))

(thm p49pd_linv_self []
  (p49pd_LInv (inst1 (Exp.var 0)) 1)
  (intro i) (cases i) (rfl)
  (exact (congrArg (fn [e :- Exp] (subst (inst1 (Exp.var 0)) e))
    (lift_var_above 1 1 (+ n 1) (p49pd_one_le n)))))
(thm p49pd_self [P :- Exp]
  (= (subst1 (Exp.var 0) (lift 1 1 P)) P)
  (exact (p49pd_cancel P (inst1 (Exp.var 0)) 1 (p49pd_linv_self))))
(kdef p49pd_A (=> Nat Exp)
  (fn [j :- Nat] (Exp.tT (p49dl (+ j 1) (Exp.var 0)))))
(thm p49pd_hself [j :- Nat, W :- Exp]
  (= (subst1 (Exp.var 0) (p49peel_motive (p49pd_A j) W))
     (p49peel_body (p49pd_A j) W))
  (exact (p49pd_self (p49peel_body (p49pd_A j) W))))

;; Uniform unary-family interface. This is a formation certificate plus
;; syntactic naturality, NOT an inhabitant or a local peel typing premise.
;; We do not claim the missing general Tl-substitution metatheorem.
(kdef p49pd_Form (=> (=> Code Code Bool) (=> Exp Exp) Prop)
  (fn [chkf :- (=> Code Code Bool), F :- (=> Exp Exp)]
    (forall [D (List Exp)] (forall [c Exp]
      (=> (Tl chkf Bool.false D c Exp.tSyn) (Tl chkf Bool.true D (F c) Exp.tUnit))))))
(kdef p49pd_Lift (=> (=> Exp Exp) Prop)
  (fn [F :- (=> Exp Exp)]
    (forall [n Nat] (forall [cut Nat] (forall [c Exp]
      (= (lift n cut (F c)) (F (lift n cut c))))))))
(kdef p49pd_Subst (=> (=> Exp Exp) Prop)
  (fn [F :- (=> Exp Exp)]
    (forall [sg (=> Nat Exp)] (forall [c Exp]
      (= (subst sg (F c)) (F (subst sg c)))))))

(thm p49pd_B_eq [F :- (=> Exp Exp), hL :- (p49pd_Lift F)]
  (= (lift 1 2 (lift 1 0 (F (Exp.var 0)))) (F (Exp.var 1)))
  (exact (Eq.trans (congrArg (fn [e :- Exp] (lift 1 2 e)) (hL 1 0 (Exp.var 0)))
    (hL 1 2 (Exp.var 1)))))
(thm p49pd_M_eq [j :- Nat, F :- (=> Exp Exp), hL :- (p49pd_Lift F)]
  (= (p49peel_motive (p49pd_A j) (F (Exp.var 0)))
     (Exp.tPi U.u1 (p49pd_A j) (F (Exp.var 1))))
  (exact (Eq.trans
    (congrArg (fn [b :- Exp]
      (Exp.tPi U.u1 (Exp.tT b) (lift 1 2 (lift 1 0 (F (Exp.var 0))))))
      (p49pd_dl_lift (+ j 1) 1 1 (Exp.var 0)))
    (congrArg (fn [C :- Exp] (Exp.tPi U.u1 (p49pd_A j) C)) (p49pd_B_eq F hL)))))

;; Arbitrary motive substitution, factored once for both recursive-result
;; types. sg is a meta-level substitution, not an object-level assumption.
(thm p49pd_M_subst [j :- Nat, F :- (=> Exp Exp), hL :- (p49pd_Lift F),
                    hS :- (p49pd_Subst F), sg :- (=> Nat Exp)]
  (= (subst sg (p49peel_motive (p49pd_A j) (F (Exp.var 0))))
     (Exp.tPi U.u1 (Exp.tT (p49dl (+ j 1) (sg 0))) (F (lift 1 0 (sg 0)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sg e)) (p49pd_M_eq j F hL))
    (Eq.trans
      (congrArg (fn [b :- Exp]
        (Exp.tPi U.u1 (Exp.tT b) (subst (upn 1 sg) (F (Exp.var 1)))))
        (p49dl_subst (+ j 1) sg (Exp.var 0)))
      (congrArg (fn [C :- Exp] (Exp.tPi U.u1 (Exp.tT (p49dl (+ j 1) (sg 0))) C))
        (hS (upn 1 sg) (Exp.var 1)))))))

(thm p49pd_LA_eq [j :- Nat]
  (= (p49peel_leafA (p49pd_A j)) (Exp.tT (p49dl (+ j 1) (Exp.sleaf (Exp.var 0)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sLeafI (Exp.tT e)))
      (p49pd_dl_lift (+ j 1) 1 1 (Exp.var 0)))
    (congrArg Exp.tT (p49dl_subst (+ j 1) sLeafI (Exp.var 0))))))
(thm p49pd_NA_eq [j :- Nat]
  (= (p49peel_nodeA (p49pd_A j))
     (Exp.tT (p49dl (+ j 1) (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst sNodeI (Exp.tT e)))
      (p49pd_dl_lift (+ j 1) 1 1 (Exp.var 0)))
    (congrArg Exp.tT (p49dl_subst (+ j 1) sNodeI (Exp.var 0))))))
(thm p49pd_NB_eq [F :- (=> Exp Exp), hL :- (p49pd_Lift F), hS :- (p49pd_Subst F)]
  (= (p49peel_nodeB (F (Exp.var 0)))
     (F (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))
  (exact (Eq.trans
    (congrArg (fn [e :- Exp] (subst (upn 1 sNodeI) e)) (p49pd_B_eq F hL))
    (hS (upn 1 sNodeI) (Exp.var 1)))))

;; Exact dependent telescopes, innermost first. The pair's X and Y have
;; the same syntax var4: Y is relative to the extra X binder, so it denotes c2.
(kdef p49pd_X (=> Nat Exp) (fn [j :- Nat] (Exp.tT (p49dl j (Exp.var 4)))))
(kdef p49pd_De (=> Nat Exp (List Exp) (List Exp))
  (fn [j :- Nat, W :- Exp, D :- (List Exp)]
    (consE (p49peel_nodeA (p49pd_A j)) (p49peel_Dn (p49pd_A j) W D))))
(def ^:private mF '(p49peel_motive (p49pd_A j) (F (Exp.var 0))))
(def ^:private d1 '[Exp.tSyn Exp.tSyn Exp.tLbl Exp.tSyn])
(def ^:private d2 (into [(list 'y1Ty mF)] d1))
(def ^:private dnF (into [(list 'y2Ty mF)] d2))
(def ^:private deF (into ['(p49peel_nodeA (p49pd_A j))] dnF))
(defn- tlcode [ctx c]
  (let [D (es ctx 'D)]
    (case (first c)
      Exp.var (let [i (second c)]
        (list 'Tl.zVar 'chkf D i (nth ctx i) (lookup 'nthE 'Exp ctx 'D i)))
      Exp.sleaf (list 'Tl.zSleaf 'chkf D (second c) (tlcode ctx (second c)))
      Exp.snode (let [[_ l a b] c]
        (list 'Tl.zSnode 'chkf D l a b (tlcode ctx l) (tlcode ctx a) (tlcode ctx b))))))
(defn- evform [ctx k c]
  (list 'Tl.fT 'chkf (es ctx 'D) (list 'p49dl k c)
    (list 'p49dl_typed 'chkf k (es ctx 'D) c (tlcode ctx c))))
(defn- form-cast [ctx A B he hB]
  (list 'Eq.mp
    (list 'congrArg (list 'fn '[t :- Exp] (list 'Tl 'chkf 'Bool.true (es ctx 'D) 't 'Exp.tUnit))
      (list 'Eq.symm he)) hB))
(def ^:private fp
  '[chkf :- (=> Code Code Bool), j :- Nat, D :- (List Exp), F :- (=> Exp Exp),
    hF :- (p49pd_Form chkf F), hL :- (p49pd_Lift F), hS :- (p49pd_Subst F)])
(doseq [[nm ctx target c he]
        [['p49pd_hM '[Exp.tSyn Exp.tSyn] mF '(Exp.var 0) '(p49pd_M_eq j F hL)]
         ['p49pd_hY1 d1 (list 'y1Ty mF) '(Exp.var 1) '(p49pd_M_subst j F hL hS (sAt 1 3))]
         ['p49pd_hY2 d2 (list 'y2Ty mF) '(Exp.var 1) '(p49pd_M_subst j F hL hS (sAt 1 4))]]]
  (let [A (list 'Exp.tT (list 'p49dl '(+ j 1) c))
        c1 (list 'Exp.var (inc (second c)))
        B (list 'Exp.tPi 'U.u1 A (list 'F c1))
        hB (list 'Tl.fPi 'chkf (es ctx 'D) 'U.u1 A (list 'F c1)
             (evform ctx '(+ j 1) c)
             (list 'hF (es (into [A] ctx) 'D) c1 (tlcode (into [A] ctx) c1)))]
    (a/prove-theorem nm fp (list 'Tl 'chkf 'Bool.true (es ctx 'D) target 'Exp.tUnit)
      [(list 'exact (form-cast ctx target B he hB))])))
(doseq [[nm ctx target c he]
        [['p49pd_hLA '[Exp.tLbl Exp.tSyn] '(p49peel_leafA (p49pd_A j))
          '(Exp.sleaf (Exp.var 0)) '(p49pd_LA_eq j)]
         ['p49pd_hNA dnF '(p49peel_nodeA (p49pd_A j))
          '(Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)) '(p49pd_NA_eq j)]]]
  (a/prove-theorem nm fp (list 'Tl 'chkf 'Bool.true (es ctx 'D) target 'Exp.tUnit)
    [(list 'exact (form-cast ctx target (list 'Exp.tT (list 'p49dl '(+ j 1) c)) he
                   (evform ctx '(+ j 1) c)))]))
(a/prove-theorem 'p49pd_hNB fp
  (list 'Tl 'chkf 'Bool.true (es deF 'D) '(p49peel_nodeB (F (Exp.var 0))) 'Exp.tUnit)
  [(list 'exact (form-cast deF '(p49peel_nodeB (F (Exp.var 0)))
     '(F (Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))) '(p49pd_NB_eq F hL hS)
     (list 'hF (es deF 'D) '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))
       (tlcode deF '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))))])
(doseq [[nm ctx] [['p49pd_hX deF] ['p49pd_hY (into ['(p49pd_X j)] deF)]]]
  (a/prove-theorem nm fp
    (list 'Tl 'chkf 'Bool.true (es ctx 'D) '(p49pd_X j) 'Exp.tUnit)
    [(list 'exact (evform ctx 'j '(Exp.var 4)))]))

(thm p49pd_scale1 [uv :- (List U)] (= (vscale U.u1 uv) uv)
  (induction uv) (rfl)
  (exact (congrArg (fn [v :- (List U)] (consU head v)) ih_tail)))
(thm p49pd_pair_z [uv :- (List U)]
  (= (vadd (vscale U.u1 (p49peel_z uv)) (p49peel_z uv)) (p49peel_z uv))
  (exact (Eq.trans
    (congrArg (fn [v :- (List U)] (vadd v (p49peel_z uv))) (p49pd_scale1 (p49peel_z uv)))
    (p49peel_zadd uv))))

;; Extend the existing splitter's explicit derivation with an erased
;; ambient telescope. This reuses its exact term, motive and arm proofs;
;; the generated extension remains an explicit kernel-checked Rt proof.
;; It is NOT a blanket weakening rule. Only the two vector-combining rules
;; occurring in that fixed derivation need casts, listed exhaustively here.
(defn- prefix-items [xs]
  (if (and (seq? xs) (= 'List.cons (first xs)))
    (into [(nth xs 2)] (prefix-items (nth xs 3))) []))
(defn- plus-u [a b]
  (cond (= a 'U.u0) b (= b 'U.u0) a :else 'U.uw))
(defn- extend-split-proof [p]
  (cond
    (= p '(List.nil Exp)) 'D
    (= p '(List.nil U)) z
    (= p '(List.nil Sk)) '(skels D)
    (not (seq? p)) p
    :else
    (let [xs (mapv extend-split-proof p) tag (first p)
          n (when (#{'Rt.rVar 'Rt.rConst} tag) (count (prefix-items (nth p 2))))
          xs (cond (= tag 'Rt.rVar) (assoc xs 7 (len-prefix n zlen))
                   (= tag 'Rt.rConst) (assoc xs 6 (len-prefix n zlen))
                   :else xs)
          proof (apply list xs)]
      (if (#{'Rt.rPair 'Rt.rElimB} tag)
        (let [uv (mapv plus-u (prefix-items (nth p 3)) (prefix-items (nth p 4)))
              pair? (= tag 'Rt.rPair)
              A (if pair? (list 'Exp.tSig (nth xs 5) (nth xs 6) (nth xs 7))
                    (list 'subst1 (nth xs 6) (nth xs 5)))
              t (if pair? (list 'Exp.pair A (nth xs 8) (nth xs 9))
                    (list 'Exp.elimB (nth xs 5) (nth xs 6) (nth xs 7) (nth xs 8)))
              src (if pair? (list 'vadd (list 'vscale (nth xs 5) (nth xs 3)) (nth xs 4))
                      (list 'vadd (nth xs 3) (nth xs 4)))
              eq (list 'congrArg (list 'fn '[v :- (List U)] (us uv 'v))
                   (list (if pair? 'p49pd_pair_z 'p49peel_zadd) 'uv))]
          (list 'p49peel_cast 'chkf (nth xs 2) src (us uv z) t A A proof eq 'rfl))
        proof))))

(let [t @#'lcert.formal.p49split/split-term
      T @#'lcert.formal.p49split/split-type
      motive @#'lcert.formal.p49split/motive
      ds @#'lcert.formal.p49split/ds
      us0 @#'lcert.formal.p49split/us
      form @#'lcert.formal.p49split/form
      arm @#'lcert.formal.p49split/arm
      arm-proof @#'lcert.formal.p49split/arm-proof
      variable @#'lcert.formal.p49split/variable
      proof (list 'Rt.rLam 'chkf (ds []) (us0 []) 'U.uw 'Exp.tBool
        (nth t 3) (nth T 3) (form [] 'Exp.tBool)
        (list 'Rt.rLam 'chkf (ds ['Exp.tBool]) (us0 ['U.uw]) 'U.uw 'Exp.tBool
          (nth (nth t 3) 3) (nth (nth T 3) 3) (form ['Exp.tBool] 'Exp.tBool)
          (list 'Rt.rElimB 'chkf (ds ['Exp.tBool 'Exp.tBool])
            (us0 ['U.u0 'U.uw]) (us0 ['U.uw 'U.u0]) motive '(Exp.var 1)
            (arm 'Exp.tt) (arm 'Exp.ff)
            (variable ['Exp.tBool 'Exp.tBool] ['U.u0 'U.uw] 1)
            (form ['Exp.tBool 'Exp.tBool 'Exp.tBool] motive)
            (arm-proof 'Exp.tt) (arm-proof 'Exp.ff))))]
  (a/prove-theorem 'p49pd_split_context
    '[chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
      hlen :- (= (lenU uv) (lenE D))]
    '(Rt chkf D (p49peel_z uv) p49split_term p49split_type)
    [(list 'exact (extend-split-proof proof))]))

;; Fixed-shape equality generators used for the three applications below.
;; Each returns [normal-expression, explicit Eq proof]. The only symbolic
;; computation is p49dl, handled by its proved substitution/lifting laws;
;; the generator NEVER unfolds or enumerates the depth parameter j.
(def ^:private fields
  (into {} (map (fn [[c fs]] [(symbol (str "Exp." c)) fs])
               @#'lcert.formal.syntactic/exp-fields)))
(defn- dl-cong [k h] (list 'congrArg (list 'fn '[v :- Exp] (list 'p49dl k 'v)) h))
(defn- lift-normal [n cut e]
  (cond
    (symbol? e) [e (list 'Eq.refl$1 e)]
    (= 'Exp.var (first e))
    (let [v (list 'Exp.var (if (< (second e) cut) (second e) (+ n (second e))))]
      [v (list 'Eq.refl$1 v)])
    (= 'p49dl (first e))
    (let [[_ k c] e [v h] (lift-normal n cut c)]
      [(list 'p49dl k v) (list 'Eq.trans (list 'p49pd_dl_lift k n cut c) (dl-cong k h))])
    :else
    (let [fs (mapv (fn [[_ ty d] x]
                    (if (= ty 'Exp)
                      (let [[v h] (lift-normal n (+ cut d) x)]
                        [ty (list 'lift n (+ cut d) x) v h])
                      [ty x x nil])) (fields (first e)) (rest e))]
      [(apply list (first e) (map #(nth % 2) fs)) (congruence (first e) fs)])))
(defn- up-sigma [[sg values]]
  [(list 'up sg)
   (fn [i]
     (if (zero? i) ['(Exp.var 0) '(Eq.refl$1 (Exp.var 0))]
       (let [[v h] (values (dec i)) [v2 h2] (lift-normal 1 0 v)]
         [v2 (list 'Eq.trans (list 'congrArg '(fn [v :- Exp] (lift 1 0 v)) h) h2)])))])
(defn- sub-normal [[sg values :as sigma] e]
  (cond
    (symbol? e) [e (list 'Eq.refl$1 e)]
    (= 'Exp.var (first e)) (values (second e))
    (= 'p49dl (first e))
    (let [[_ k c] e [v h] (sub-normal sigma c)]
      [(list 'p49dl k v) (list 'Eq.trans (list 'p49dl_subst k sg c) (dl-cong k h))])
    :else
    (let [fs (mapv (fn [[_ ty d] x]
                    (if (= ty 'Exp)
                      (let [sigma2 (nth (iterate up-sigma sigma) d)
                            [v h] (sub-normal sigma2 x)]
                        [ty (list 'subst (first sigma2) x) v h])
                      [ty x x nil])) (fields (first e)) (rest e))]
      [(apply list (first e) (map #(nth % 2) fs)) (congruence (first e) fs)])))
(defn- sub1-normal [u e]
  (sub-normal [(list 'fn '[i :- Nat] (list 'inst1 u 'i))
               (fn [i] (let [v (if (zero? i) u (list 'Exp.var (dec i)))]
                         [v (list 'Eq.refl$1 v)]))] e))

(thm p49pd_zscale [r :- U, uv :- (List U)]
  (= (vscale r (p49peel_z uv)) (p49peel_z uv))
  (induction uv) (rfl)
  (exact (Eq.trans
    (congrArg (fn [s :- U] (consU s (vscale r (p49peel_z tail)))) (umul_zero_right r))
    (congrArg (fn [v :- (List U)] (consU U.u0 v)) ih_tail))))
(thm p49pd_app_z [r :- U, uv :- (List U)]
  (= (vadd (p49peel_z uv) (vscale r (p49peel_z uv))) (p49peel_z uv))
  (exact (Eq.trans
    (congrArg (fn [v :- (List U)] (vadd (p49peel_z uv) v)) (p49pd_zscale r uv))
    (p49peel_zadd uv))))

;; Skeleton typing and branch-list freedom for conversion certificates.
(defn- ss [xs tail] (reduce #(list 'List.cons 'Sk %2 %1) tail (reverse xs)))
(doseq [succ? [false true]]
  (let [g '[Sk.bool Sk.bool Sk.syn Sk.syn Sk.lbl]
        G (ss g 'G)
        varproof (fn [i] (list 'SkJ.sVar G i 'Sk.syn (lookup 'nthS 'Sk g 'G i)))
        t (if succ? '(Exp.ite (p49dl j (Exp.var 3)) (p49dl j (Exp.var 2)) Exp.ff) 'Exp.ff)]
    (a/prove-theorem (if succ? 'p49pd_sk_succ 'p49pd_sk_zero)
      (vec (concat '[G :- (List Sk), c :- Exp, hc :- (SkJ Bool.false G c Sk.syn)]
        (when succ? '[j :- Nat, ih :- (forall [G (List Sk)] (forall [c Exp]
          (=> (SkJ Bool.false G c Sk.syn) (SkJ Bool.false G (p49dl j c) Sk.bool))))])))
      (list 'SkJ 'Bool.false 'G (list 'p49dl (if succ? '(+ j 1) 0) 'c) 'Sk.bool)
      [(list 'exact
         (list 'SkJ.sRecS 'G 'Exp.tBool 'Exp.tt t 'c
           (list 'SkJ.wBool (ss '[Sk.syn] 'G))
           (list 'SkJ.sTT (ss '[Sk.lbl] 'G))
           (if succ? (list 'SkJ.sIte G '(p49dl j (Exp.var 3)) '(p49dl j (Exp.var 2)) 'Exp.ff 'Sk.bool
                       (list 'ih G '(Exp.var 3) (varproof 3))
                       (list 'ih G '(Exp.var 2) (varproof 2)) (list 'SkJ.sFF G))
               (list 'SkJ.sFF G)) 'hc))])))
(thm p49pd_dl_sk [k :- Nat]
  (forall [G (List Sk)] (forall [c Exp]
    (=> (SkJ Bool.false G c Sk.syn) (SkJ Bool.false G (p49dl k c) Sk.bool))))
  (induction k)
  (intro G c hc) (exact (p49pd_sk_zero G c hc))
  (intro G c hc) (exact (p49pd_sk_succ G c hc n ih_n)))
(thm p49pd_dl_nbr [k :- Nat]
  (forall [c Exp] (=> (= (nbr c) Bool.true) (= (nbr (p49dl k c)) Bool.true)))
  (induction k)
  (intro c hc) (exact hc)
  (intro c hc)
  (exact (Eq.trans
    (congrArg (fn [b :- Bool]
      (Bool.and (Bool.and b (Bool.and (nbr (p49dl n (Exp.var 2))) Bool.true)) (nbr c)))
      (ih_n (Exp.var 3) rfl))
    (Eq.trans
      (congrArg (fn [b :- Bool] (Bool.and (Bool.and b Bool.true) (nbr c)))
        (ih_n (Exp.var 2) rfl)) hc))))

;; Type-level and skeleton builders for the normalized application types.
;; Every application still supplies fPi's two formation premises explicitly.
(declare type-form)
(defn- tlbool [ctx e]
  (let [D (es ctx 'D)]
    (cond
      (#{'Exp.tt 'Exp.ff} e) (list 'Tl.zConst 'chkf D e 'Exp.tBool 'rfl)
      (= 'p49dl (first e)) (list 'p49dl_typed 'chkf (second e) D (nth e 2) (tlcode ctx (nth e 2)))
      (= 'Exp.var (first e)) (tlcode ctx e)
      (= 'Exp.ite (first e))
      (let [[_ b t f] e] (list 'Tl.zIte 'chkf D b t f 'Exp.tBool
                           (tlbool ctx b) (tlbool ctx t) (tlbool ctx f))))))
(defn- type-form [ctx A]
  (if (symbol? A) (base (es ctx 'D) A)
    (case (first A)
      Exp.tT (list 'Tl.fT 'chkf (es ctx 'D) (second A) (tlbool ctx (second A)))
      (Exp.tPi Exp.tSig)
      (let [[tag r X Y] A]
        (list (if (= tag 'Exp.tPi) 'Tl.fPi 'Tl.fSig) 'chkf (es ctx 'D) r X Y
          (type-form ctx X) (type-form (into [X] ctx) Y))))))
(defn- skcode [ctx c]
  (let [gs (mapv #(list 'skel %) ctx) G (list 'skels (es ctx 'D))]
    (case (first c)
      Exp.var (let [i (second c)] (list 'SkJ.sVar G i (nth gs i) (lookup 'nthS 'Sk gs '(skels D) i)))
      Exp.snode (let [[_ l a b] c] (list 'SkJ.sSnode G l a b (skcode ctx l) (skcode ctx a) (skcode ctx b))))))
(defn- skbool [ctx b]
  (let [G (list 'skels (es ctx 'D))]
    (cond
      (= b 'Exp.ff) (list 'SkJ.sFF G)
      (= 'p49dl (first b)) (list 'p49pd_dl_sk (second b) G (nth b 2) (skcode ctx (nth b 2)))
      (= 'Exp.ite (first b))
      (let [[_ a c d] b] (list 'SkJ.sIte G a c d 'Sk.bool (skbool ctx a) (skbool ctx c) (skbool ctx d))))))
(defn- nbr-bool [e]
  (cond
    (= e 'Exp.ff) 'rfl
    (= 'p49dl (first e)) (list 'p49pd_dl_nbr (second e) (nth e 2) 'rfl)
    (= 'Exp.ite (first e))
    (let [[_ a b _] e]
      (list 'Eq.trans
        (list 'congrArg (list 'fn '[v :- Bool] (list 'Bool.and 'v (list 'Bool.and (list 'nbr b) 'Bool.true))) (nbr-bool a))
        (list 'congrArg '(fn [v :- Bool] (Bool.and v Bool.true)) (nbr-bool b))))))

(defn- rtvar [ctx v i]
  (list 'Rt.rVar 'chkf (es ctx 'D) (us v z) i (nth ctx i) (nth v i)
    (len-prefix (count ctx) zlen)
    (lookup 'nthE 'Exp ctx 'D i) (lookup 'nthU 'U v z i) 'rfl))
(defn- times-u [r u]
  (cond (= r 'U.u0) 'U.u0 (= u 'U.u0) 'U.u0 (= r 'U.u1) u :else 'U.uw))
(defn- apply-rt
  "Input function [term,type,usage-prefix,proof] and argument [term,prefix,proof]."
  [ctx [f T vf hf] [u vu hu]]
  (let [[_ r A B] T
        t (list 'Exp.app f u)
        [C ec] (sub1-normal u B)
        v (mapv plus-u vf (mapv #(times-u r %) vu))
        src (list 'vadd (us vf z) (list 'vscale r (us vu z)))
        eu (list 'congrArg (list 'fn '[v :- (List U)] (us v 'v)) (list 'p49pd_app_z r 'uv))
        hp (list 'Rt.rApp 'chkf (es ctx 'D) (us vf z) (us vu z) r f u A B 'rfl hf hu
             (type-form ctx A) (type-form (into [A] ctx) B))]
    [t C v (list 'p49peel_cast 'chkf (es ctx 'D) src (us v z) t
             (list 'subst1 u B) C hp eu ec)]))

(kdef p49pd_q (=> Nat Exp)
  (fn [j :- Nat]
    (Exp.app (Exp.app (Exp.app p49split_term (p49dl j (Exp.var 4)))
                       (p49dl j (Exp.var 3))) (Exp.var 0))))

;; This is the actual hq premise of p49peel_typed. No runtime depth,
;; splitter, conversion, or formation premise is left to the parent.
;; W is completely opaque here: its recursive-result slots have usage 0.
(let [M '(p49peel_motive (p49pd_A j) W)
      ctx ['(p49peel_nodeA (p49pd_A j)) (list 'y2Ty M) (list 'y1Ty M)
           'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn]
      Dc (es ctx 'D)
      zeroes (vec (repeat 7 'U.u0))
      v1 (assoc zeroes 4 'U.uw) v2 (assoc zeroes 3 'U.uw)
      ve (assoc zeroes 0 'U.u1)
      Cnode '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))
      whole (list 'Exp.tT (list 'p49dl '(+ j 1) Cnode))
      b1 '(p49dl j (Exp.var 4)) b2 '(p49dl j (Exp.var 3))
      conj-b (list 'Exp.ite b1 b2 'Exp.ff)
      E (list 'Exp.tT conj-b)
      raw-E '(lift 1 0 (p49peel_nodeA (p49pd_A j)))
      [normal-E e-lift] (lift-normal 1 0
        '(Exp.tT (p49dl (+ j 1) (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2)))))
      ee (list 'Eq.trans (list 'congrArg '(fn [t :- Exp] (lift 1 0 t)) '(p49pd_NA_eq j)) e-lift)
      he (list 'p49peel_cast 'chkf Dc (us ve z) (us ve z) '(Exp.var 0) raw-E normal-E
           (rtvar ctx ve 0) 'rfl ee)
      sk-src (list 'SkJ.wT (list 'skels Dc) (second whole) (skbool ctx (second whole)))
      sk-dst (list 'SkJ.wT (list 'skels Dc) conj-b (skbool ctx conj-b))
      cv (list 'Cv.cvFwd 'chkf (list 'skels Dc) whole whole E
           (list 'Cv.cvRefl 'chkf (list 'skels Dc) whole sk-src (nbr-bool (second whole)))
           (list 'p49split_underT 'chkf (second whole) conj-b
             '(p49dl_node chkf j (Exp.var 5) (Exp.var 4) (Exp.var 3)))
           sk-dst (nbr-bool conj-b))
      he-conv (list 'Rt.rConv 'chkf Dc (us ve z) '(Exp.var 0) whole E he (type-form ctx E) cv)
      rtdepth (fn [i v]
                (list 'p49pd_dl_rt 'chkf 'j Dc (us v z) (list 'Exp.var i)
                  (len-prefix 7 zlen) (rtvar ctx v i)))
      hf (list 'p49pd_split_context 'chkf Dc (us zeroes 'uv) (len-prefix 7 'hlen))
      s0 ['p49split_term @#'lcert.formal.p49split/split-type zeroes hf]
      s1 (apply-rt ctx s0 [b1 v1 (rtdepth 4 v1)])
      s2 (apply-rt ctx s1 [b2 v2 (rtdepth 3 v2)])
      s3 (apply-rt ctx s2 ['(Exp.var 0) ve he-conv])]
  (a/prove-theorem 'p49pd_hq
    '[chkf :- (=> Code Code Bool), j :- Nat, W :- Exp,
      D :- (List Exp), uv :- (List U), hlen :- (= (lenU uv) (lenE D))]
    '(Rt chkf (p49pd_De j W D) (p49peel_Uq uv) (p49pd_q j)
       (Exp.tSig U.u1 (p49pd_X j) (p49pd_X j)))
    (b/lv [(list 'exact (nth s3 3))])))

(kdef p49pd_term (=> Nat Exp Exp Exp Exp)
  (fn [j :- Nat, W :- Exp, l :- Exp, n :- Exp]
    (p49peel_term (p49pd_A j) W l (p49pd_q j) n)))

;; The parent-facing successor peel. All local formation, motive
;; substitution and splitter obligations are discharged above. The two Rt
;; premises are exactly the leaf instance and smaller-potential node
;; continuation; neither is the resulting recSyn or all-depth theorem.
(let [dz '(p49peel_Uz uv)
      leafctx (es '[(p49peel_leafA (p49pd_A j)) Exp.tLbl Exp.tSyn] 'D)
      nodectx (es (into ['(p49pd_X j) '(p49pd_X j)] deF) 'D)
      params (into fp
               (concat '[uv :- (List U), hlen :- (= (lenU uv) (lenE D)), l :- Exp, n :- Exp]
                 ['hl :- (list 'Rt 'chkf leafctx (us '[U.u1 U.uw] dz) 'l '(p49peel_leafB (F (Exp.var 0))))
                  'hn :- (list 'Rt 'chkf nodectx
                           '(consU U.u1 (consU U.u1 (p49peel_Uk uv))) 'n
                           '(lift 2 0 (p49peel_nodeB (F (Exp.var 0)))))]))
      args '[chkf j D F hF hL hS]]
  (a/prove-theorem 'p49pd_peel params
    '(Rt chkf D uv (p49pd_term j (F (Exp.var 0)) l n)
       (Exp.tPi U.uw Exp.tSyn (p49peel_body (p49pd_A j) (F (Exp.var 0)))))
    [(list 'exact
       (apply list 'p49peel_typed
         (concat '[chkf D uv (p49pd_A j) (F (Exp.var 0)) (p49pd_X j) (p49pd_X j)
                   l (p49pd_q j) n hlen (p49pd_hself j (F (Exp.var 0)))]
           (map #(apply list % args)
             '[p49pd_hM p49pd_hY1 p49pd_hY2 p49pd_hLA p49pd_hNA p49pd_hNB p49pd_hX p49pd_hY])
           '[hl (p49pd_hq chkf j (F (Exp.var 0)) D uv hlen) hn])))]))
