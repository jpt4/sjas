(ns lcert.formal-test.p49peel
  "Generic dependent constructor elimination, independent of depth evaluation."
  (:require [clojure.test :refer [deftest is]]
            [ansatz.core :as a]
            [lcert.formal.base :as b]
            [lcert.formal.p49peel]))

;; Integration witnesses for the generic theorem. These are explicit rule
;; derivations, not assumed premises. The first component of the node pair
;; is the supplied evidence; the continuation returns that component.
;; A second instance includes an unused linear ambient token: all branch
;; usages for it are zero, while the scrutinee carries its original usage.
(defn- ls [ty xs]
  (reduce #(list 'List.cons ty %2 %1) (list 'List.nil ty) (reverse xs)))
(defn- ds [xs] (ls 'Exp xs))
(defn- us [xs] (ls 'U xs))
(defn- look [kind ty xs i]
  (let [[x & tail] xs]
    (if (zero? i) (list (symbol (str kind ".eq_2")) x (ls ty tail))
      (list 'Eq.trans (list (symbol (str kind ".eq_3")) x (ls ty tail) (dec i))
        (look kind ty tail (dec i))))))
(defn- variable [D uv i]
  (list 'Rt.rVar 'chkf (ds D) (us uv) i (nth D i) (nth uv i)
    'rfl (look 'nthE 'Exp D i) (look 'nthU 'U uv i) 'rfl))
(defn- unit-form [D] (list 'Tl.fBase 'chkf (ds D) 'Exp.tUnit 'rfl))
(defn- arrow-form [D]
  (list 'Tl.fPi 'chkf (ds D) 'U.u1 'Exp.tUnit 'Exp.tUnit
    (unit-form D) (unit-form (into ['Exp.tUnit] D))))
(def ^:private sigma '(Exp.tSig U.u1 Exp.tUnit Exp.tUnit))
(def ^:private pair-term (list 'Exp.pair sigma '(Exp.var 0) 'Exp.star))
(b/kdef! 'p49peel_unit_term 'Exp
  (list 'p49peel_term 'Exp.tUnit 'Exp.tUnit '(Exp.var 0) pair-term '(Exp.var 1)))

(doseq [[nm ambient ambient-usage]
        [['p49peel_unit_instance [] []]
         ['p49peel_unit_ambient_instance ['Exp.tDia] ['U.u1]]]]
  (let [P '(Exp.tPi U.u1 Exp.tUnit Exp.tUnit)
        dc (into ['Exp.tSyn] ambient)
        dl (into ['Exp.tLbl] dc)
        dy1 (into ['Exp.tSyn 'Exp.tSyn 'Exp.tLbl] dc)
        dy2 (into [P] dy1)
        dn (into [P] dy2)
        de (into ['Exp.tUnit] dn)
        dz (into ['U.u0] (repeat (count ambient) 'U.u0))
        uq (into ['U.u1 'U.u0 'U.u0 'U.uw 'U.uw 'U.u0] dz)
        uk (into ['U.u0 'U.u1 'U.u1 'U.uw 'U.uw 'U.uw] dz)
        zq (vec (repeat (count de) 'U.u0))
        hp (list 'Rt.rPair 'chkf (ds de) (us uq) (us zq) 'U.u1
             'Exp.tUnit 'Exp.tUnit '(Exp.var 0) 'Exp.star 'rfl
             (unit-form de) (unit-form (into ['Exp.tUnit] de))
             (variable de uq 0)
             (list 'Rt.rConst 'chkf (ds de) (us zq) 'Exp.star 'Exp.tUnit 'rfl 'rfl))]
    (a/prove-theorem nm '[chkf :- (=> Code Code Bool)]
      (list 'Rt 'chkf (ds ambient) (us ambient-usage) 'p49peel_unit_term
        '(Exp.tPi U.uw Exp.tSyn (Exp.tPi U.u1 Exp.tUnit Exp.tUnit)))
      [(list 'exact
         (list 'p49peel_typed 'chkf (ds ambient) (us ambient-usage)
           'Exp.tUnit 'Exp.tUnit 'Exp.tUnit 'Exp.tUnit '(Exp.var 0) pair-term '(Exp.var 1)
           'rfl 'rfl (arrow-form (into ['Exp.tSyn] dc))
           (arrow-form dy1) (arrow-form dy2) (unit-form dl) (unit-form dn)
           (unit-form de) (unit-form de) (unit-form (into ['Exp.tUnit] de))
           (variable (into ['Exp.tUnit] dl) (into ['U.u1 'U.uw] dz) 0)
           hp (variable (into ['Exp.tUnit 'Exp.tUnit] de) (into ['U.u1 'U.u1] uk) 1)))])))

(deftest generic-peel
  (is (b/has? 'p49peel_typed))
  (is (b/has? 'p49peel_node_typed))
  (is (b/has? 'p49peel_zero_node_typed))
  ;; A closed constructed instance must pass all the actual formation,
  ;; substitution, usage and continuation obligations of the generic rule.
  (is (b/has? 'p49peel_unit_instance))
  (is (b/has? 'p49peel_unit_ambient_instance))
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (List.nil Exp) (List.nil U) p49peel_unit_term
           (Exp.tPi U.uw Exp.tSyn Exp.tEmpty))
        '[(exact (p49peel_unit_instance chkf))]))
  ;; The theorem does not remove the evidence binder from its result.
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (List.nil Exp) (List.nil U) p49peel_unit_term
           (Exp.tPi U.uw Exp.tSyn Exp.tUnit))
        '[(exact (p49peel_unit_instance chkf))]))
  ;; In the let body, the original unsplit evidence is erased. It cannot
  ;; replace either freshly bound component (even though all are unit here).
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        '(Rt chkf (consE Exp.tUnit (consE Exp.tUnit (consE Exp.tUnit (List.nil Exp))))
           (consU U.u1 (consU U.u1 (consU U.u0 (List.nil U)))) (Exp.var 2) Exp.tUnit)
        [(list 'exact (variable ['Exp.tUnit 'Exp.tUnit 'Exp.tUnit] ['U.u1 'U.u1 'U.u0] 2))])))

;; A genuinely code-dependent conclusion: W(c) is the identity-function
;; type T(chk c cbot) -o T(chk c cbot). No checker reduction is used.
;; Instantiating the generic theorem checks code indices across recSyn's
;; five binders, the whole evidence, both pair fields, and W's own binder.
(defn- shift-code [c]
  (case (first c)
    Exp.var (list 'Exp.var (inc (second c)))
    Exp.sleaf (list 'Exp.sleaf (shift-code (second c)))
    Exp.snode (apply list 'Exp.snode (map shift-code (rest c)))
    Exp.lbl c))
(defn- evidence [c] (list 'Exp.tT (list 'Exp.chk c '(Exp.sleaf (Exp.lbl 15)))))
(defn- identity-type [c]
  (list 'Exp.tPi 'U.u1 (evidence c) (evidence (shift-code c))))
(defn- identity-term [c]
  (list 'Exp.lam 'U.u1 (evidence c) '(Exp.var 0)))
(defn- motive-at [c]
  (list 'Exp.tPi 'U.u1 'Exp.tUnit (identity-type (shift-code c))))
(declare formed)
(defn- tl-code [D c]
  (case (first c)
    Exp.var (let [i (second c)]
      (list 'Tl.zVar 'chkf (ds D) i (nth D i) (look 'nthE 'Exp D i)))
    Exp.lbl (list 'Tl.zConst 'chkf (ds D) c 'Exp.tLbl 'rfl)
    Exp.sleaf (list 'Tl.zSleaf 'chkf (ds D) (second c) (tl-code D (second c)))
    Exp.snode (let [[_ a c1 c2] c]
      (list 'Tl.zSnode 'chkf (ds D) a c1 c2 (tl-code D a) (tl-code D c1) (tl-code D c2)))))
(defn- formed [D A]
  (if (symbol? A) (list 'Tl.fBase 'chkf (ds D) A 'rfl)
    (case (first A)
      Exp.tPi (let [[_ r X Y] A]
        (list 'Tl.fPi 'chkf (ds D) r X Y (formed D X) (formed (into [X] D) Y)))
      Exp.tT (let [[_ [_ c d]] A]
        (list 'Tl.fT 'chkf (ds D) (second A)
          (list 'Tl.zChk 'chkf (ds D) c d (tl-code D c) (tl-code D d)))))))
(defn- identity-proof [D uv c]
  (list 'Rt.rLam 'chkf (ds D) (us uv) 'U.u1 (evidence c) '(Exp.var 0)
    (evidence (shift-code c)) (formed D (evidence c))
    (variable (into [(evidence c)] D) (into ['U.u1] uv) 0)))

(let [W (identity-type '(Exp.var 0))
      M (motive-at '(Exp.var 0))
      Yt (motive-at '(Exp.var 1))
      dc ['Exp.tSyn] dl ['Exp.tLbl 'Exp.tSyn]
      dy1 ['Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn]
      dy2 (into [Yt] dy1) dn (into [Yt] dy2)
      de (into ['Exp.tUnit] dn) dp (into ['Exp.tUnit 'Exp.tUnit] de)
      uq ['U.u1 'U.u0 'U.u0 'U.uw 'U.uw 'U.u0 'U.u0]
      uk ['U.u1 'U.u1 'U.u0 'U.u1 'U.u1 'U.uw 'U.uw 'U.uw 'U.u0]
      zq (vec (repeat 7 'U.u0))
      lc '(Exp.sleaf (Exp.var 1))
      nc '(Exp.snode (Exp.var 7) (Exp.var 6) (Exp.var 5))
      l (identity-term lc) n (identity-term nc)
      hp (list 'Rt.rPair 'chkf (ds de) (us uq) (us zq) 'U.u1
           'Exp.tUnit 'Exp.tUnit '(Exp.var 0) 'Exp.star 'rfl
           (unit-form de) (unit-form (into ['Exp.tUnit] de))
           (variable de uq 0)
           (list 'Rt.rConst 'chkf (ds de) (us zq) 'Exp.star 'Exp.tUnit 'rfl 'rfl))]
  (b/kdef! 'p49peel_dependent_term 'Exp (list 'p49peel_term 'Exp.tUnit W l pair-term n))
  (a/prove-theorem 'p49peel_dependent_instance '[chkf :- (=> Code Code Bool)]
    (list 'Rt 'chkf (ds []) (us []) 'p49peel_dependent_term
      (list 'Exp.tPi 'U.uw 'Exp.tSyn (motive-at '(Exp.var 0))))
    [(list 'exact
       (list 'p49peel_typed 'chkf (ds []) (us []) 'Exp.tUnit W 'Exp.tUnit 'Exp.tUnit
         l pair-term n 'rfl 'rfl (formed (into ['Exp.tSyn] dc) M)
         (formed dy1 Yt) (formed dy2 Yt) (unit-form dl) (unit-form dn)
         (formed de (identity-type '(Exp.snode (Exp.var 5) (Exp.var 4) (Exp.var 3))))
         (unit-form de) (unit-form (into ['Exp.tUnit] de))
         (identity-proof (into ['Exp.tUnit] dl) ['U.u1 'U.uw 'U.u0] lc)
         hp (identity-proof dp uk nc)))]))

(deftest dependent-peel
  (is (b/has? 'p49peel_dependent_instance))
  ;; Shifting the evidence type by the wrong number of binders changes the
  ;; result; it is not made valid by the constructor-elimination theorem.
  (is (b/rejects? '[chkf :- (=> Code Code Bool)]
        (list 'Rt 'chkf (ds []) (us []) 'p49peel_dependent_term
          (list 'Exp.tPi 'U.uw 'Exp.tSyn
            (list 'Exp.tPi 'U.u1 'Exp.tUnit (identity-type '(Exp.var 0)))))
        '[(exact (p49peel_dependent_instance chkf))])))

(deftest splitter-can-evaluate-child-bounds
  ;; Applying the closed Boolean splitter evaluates both depth predicates.
  ;; Its budget must therefore make c1 (index 4) and c2 (index 3) available.
  (let [P '(Exp.tPi U.u1 Exp.tUnit Exp.tUnit)
        de ['Exp.tUnit P P 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl 'Exp.tSyn]
        expected ['U.u1 'U.u0 'U.u0 'U.uw 'U.uw 'U.u0 'U.u0]]
    (doseq [i [3 4]]
      (is (not (b/rejects? '[chkf :- (=> Code Code Bool)]
                 (list 'Rt 'chkf (ds de) '(p49peel_Uq (List.nil U)) (list 'Exp.var i) 'Exp.tSyn)
                 [(list 'exact (variable de expected i))]))))))
