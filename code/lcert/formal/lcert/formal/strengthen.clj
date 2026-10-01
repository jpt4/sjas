(ns lcert.formal.strengthen
  "F2 — Lemma 2.3 (strengthening), R4-metatheory.md §2:
      Γ, x :ρ A, Γ′ ⊢ t :¹ B  and  x ∉ FV(t)  ⟹  Γ, x :₀ A, Γ′ ⊢ t :¹ B.

  rt_strengthen: Rt chkf D us t A → freshF t c = true → Rt chkf D (zeroUF us c) t A,
  for the variable c (de Bruijn) not free in t.  By induction on the
  derivation, as the paper's proof: set c's usage to 0 in every judgment.  No
  Var instance is for c; the axioms accept any usages of the right length;
  sums and scalings of 0 are 0 (zero_vadd, zero_vscale); type-level premises
  ignore usages.  Under a binder the variable is c + 1.

  rt_mask: the same for every variable a mask selects, at once."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.skeletons]
            [lcert.formal.skof]
            [lcert.formal.derivations]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(defn- ren [f] (if (= f 'c) 'cx f))
(defn- at [k] (if (zero? k) 'cut (list '+ 'cut k)))
(defn- fr-terms [fields]  ;; [[expr-of-field-freshness] ...] for Exp fields
  (for [[f ty k] fields :when (= ty 'Exp)] (list (list 'freshF (ren f)) (at k))))
(defn- and-chain [xs] (if (= 1 (count xs)) (first xs) (list 'Bool.and (first xs) (and-chain (rest xs)))))
(defn- clause [[ctor fields]]
  (let [pat (if (seq fields) (apply list ctor (map (comp ren first) fields)) ctor)
        body (cond (= ctor 'var) '(Bool.not (Nat.beq i cut))
                   (empty? (fr-terms fields)) 'true
                   :else (and-chain (fr-terms fields)))]
    [pat (list 'fn '[cut :- Nat] body)]))
;; freshF e c: the variable c does not occur free in e (under k binders the
;; variable is c + k).  Generated from syntactic.clj's constructor table, as
;; model.clj's closedF is written by hand; a field named c is renamed cx.
(eval (list 'a/defn 'freshF '[e :- Exp] '(=> Nat Bool) (apply list 'match 'e (map (comp vec clause) exp-fields))))

;; fresh_<ctor>: freshness of a constructor splits into its Exp fields'
;; (at their binder depths), as a nested And; one per constructor with at
;; least two Exp fields (with one, freshF of the field is definitional).
(defn- split [xs h]
  (if (= 1 (count xs)) h
      (let [x0 (first xs) R (and-chain (rest xs))]
        (list 'And.intro (list 'band_left x0 R h) (split (rest xs) (list 'band_right x0 R h))))))
(defn- and-props [xs] (if (= 1 (count xs)) (list 'Eq 'Bool (first xs) 'Bool.true) (list 'And (list 'Eq 'Bool (first xs) 'Bool.true) (and-props (rest xs)))))
(doseq [[ctor fields] exp-fields :let [xs (fr-terms fields)] :when (and (>= (count xs) 2) (not (lcert.formal.base/has? (str "fresh_" ctor))))]
  (let [fs (map (comp ren first) fields)
        term (apply list (symbol (str "Exp." ctor)) fs)]
    (a/prove-theorem (symbol (str "fresh_" ctor))
      (lv (into (vec (mapcat (fn [[f ty _]] [(ren f) :- ty]) fields)) ['cut :- 'Nat 'hfr :- (list 'Eq 'Bool (list (list 'freshF term) 'cut) 'Bool.true)]))
      (lv (and-props xs))
      (lv [(list 'exact (split xs 'hfr))]))))


;; zeroUF us c: us with position c set to usage 0 (structural on us, so it
;; computes under conses).
(kdef zeroUF (=> (List U) Nat (List U))
  (fn [us :- (List U)]
    (List.rec$1$0 U (fn [_ :- (List U)] (=> Nat (List U))) (fn [c :- Nat] (List.nil U))
      (fn [x :- U, rest :- (List U), ih :- (=> Nat (List U))]
        (fn [c :- Nat] (Nat.rec$1 (fn [_ :- Nat] (List U)) (List.cons U U.u0 rest) (fn [j :- Nat, _ :- (List U)] (List.cons U x (ih j))) c)))
      us)))

;; Its laws: it commutes with vadd and vscale, keeps the length, and leaves
;; every other entry.
(thm zero_vadd_step [a :- U, b :- U, xs :- (List U), ys :- (List U), j :- Nat,
                       ih :- (forall [y (List U)] (forall [c Nat] (Eq (List U) (zeroUF (vadd xs y) c) (vadd (zeroUF xs c) (zeroUF y c)))))]
  (Eq (List U) (zeroUF (vadd (List.cons U a xs) (List.cons U b ys)) (Nat.succ j))
               (vadd (zeroUF (List.cons U a xs) (Nat.succ j)) (zeroUF (List.cons U b ys) (Nat.succ j))))
  (exact (congrArg (fn [v :- (List U)] (List.cons U (uadd a b) v)) (ih ys j))))
(thm zero_vadd [x :- (List U)]
  (forall [y (List U)] (forall [c Nat] (Eq (List U) (zeroUF (vadd x y) c) (vadd (zeroUF x c) (zeroUF y c)))))
  (induction x)
  (intro y c) (rfl)
  (intro y c) (cases y) (cases c) (rfl) (rfl)
  (cases c) (rfl)
  (refine' (zero_vadd_step _ _ _ _ _ ih_tail)))
(thm zero_vscale [r :- U, x :- (List U)]
  (forall [c Nat] (Eq (List U) (zeroUF (vscale r x) c) (vscale r (zeroUF x c))))
  (induction x)
  (intro c) (rfl)
  (intro c) (cases c)
  (exact (congrArg (fn [v :- U] (List.cons U v (vscale r tail))) (Eq.symm (umul_zero_right r))))
  (exact (congrArg (fn [v :- (List U)] (List.cons U (umul r head) v)) (ih_tail n))))

(thm zero_len [us :- (List U)] (forall [c Nat] (Eq Nat (lenU (zeroUF us c)) (lenU us)))
  (induction us)
  (intro c) (rfl)
  (intro c) (cases c) (rfl)
  (exact (congrArg (fn [q :- Nat] (+ q 1)) (ih_tail n))))
(thm zero_nth_step [x :- U, rest :- (List U), n :- Nat, k :- Nat,
                      ih :- (forall [c Nat] (forall [j Nat] (=> (Eq Bool (Nat.beq j c) Bool.false) (Eq (Option U) (nthU (zeroUF rest c) j) (nthU rest j))))),
                      h :- (Eq Bool (Nat.beq (Nat.succ k) (Nat.succ n)) Bool.false)]
  (Eq (Option U) (nthU (zeroUF (List.cons U x rest) (Nat.succ n)) (Nat.succ k)) (nthU (List.cons U x rest) (Nat.succ k)))
  (exact (Eq.trans (nthU.eq_3 x (zeroUF rest n) k) (Eq.trans (ih n k h) (Eq.symm (nthU.eq_3 x rest k))))))
(thm zero_nth [us :- (List U)]
  (forall [c Nat] (forall [j Nat] (=> (Eq Bool (Nat.beq j c) Bool.false) (Eq (Option U) (nthU (zeroUF us c) j) (nthU us j)))))
  (induction us)
  (intro c j h) (rfl)
  (intro c j h) (cases c) (cases j)
  (exact (Bool.noConfusion h))
  (exact (Eq.trans (nthU.eq_3 U.u0 tail n) (Eq.symm (nthU.eq_3 head tail n))))
  (cases j)
  (exact (Eq.trans (nthU.eq_2 head (zeroUF tail n)) (Eq.symm (nthU.eq_2 head tail))))
  (refine' (zero_nth_step _ _ _ _ ih_tail h)))

;; --- Lemma 2.3 ---------------------------------------------------------------------

(thm rt_ucast [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), us2 :- (List U), t :- Exp, A :- Exp,
                 h :- (Rt chkf D us t A), p :- (Eq (List U) us us2)]
  (Rt chkf D us2 t A) (subst p) (exact h))
(thm not_true_false [b :- Bool, h :- (Eq Bool (Bool.not b) Bool.true)] (Eq Bool b Bool.false)
  (revert h) (cases b) (intro h) (rfl) (intro h) (exact (Bool.noConfusion h)))
(defn- Z [x] (list 'zeroUF x 'cz))
(defn- zq [E]
  (let [lu '(List U)]
    (cond (symbol? E) [(Z E) (list 'Eq.refl$1 (Z E))]
          (= (first E) 'vscale) (let [[_ r X] E [l p] (zq X)]
                                  [(list 'vscale r l) (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vscale r 'v)) p) (list 'Eq.symm (list 'zero_vscale r X 'cz)))])
          (= (first E) 'vadd) (let [[_ X Y] E [l1 p1] (zq X) [l2 p2] (zq Y)]
                                [(list 'vadd l1 l2)
                                 (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd 'v l2)) p1)
                                       (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd (Z X) 'v)) p2)
                                             (list 'Eq.symm (list 'zero_vadd X Y 'cz))))]))))
;; (RC E h): h concludes at zq(E)'s l; cast to zeroUF E cz
;; (UC h pre E): h at (pre … (zeroUF E cz)) — cast to (pre … l) where pre wraps conses
;; (FR lemma args i n): the i-th of n freshness conjuncts
(defn- cz+ [k] (if (zero? k) 'cz (list '+ 'cz k)))
(defn- expand [form]
  (walk/prewalk
    (fn [x]
      (cond
        (and (seq? x) (= (first x) 'RC)) (let [[_ E h] x [l p] (zq E)] (list 'rt_ucast 'chkf '_ l (Z E) '_ '_ h p))
        (and (seq? x) (= (first x) 'UC)) (let [[_ h wrap E] x [l p] (zq E)
                                               w (fn [v] (walk/postwalk-replace {'HOLE v} wrap))]
                                           (list 'rt_ucast 'chkf '_ (w (Z E)) (w l) '_ '_ h
                                                 (list 'congrArg (list 'fn '[v :- (List U)] (w 'v)) (list 'Eq.symm p))))
        (and (seq? x) (= (first x) 'FR)) (let [[_ lem args i n] x
                                               base (concat (list lem) args (list 'cz 'hfr))
                                               f (fn f [t j] (if (zero? j) (if (= i (dec n)) t (list 'And.left t)) (f (list 'And.right t) (dec j))))]
                                           (if (= i (dec n)) (reduce (fn [t _] (list 'And.right t)) base (range i)) (f base i)))
        (and (seq? x) (= (first x) 'C+)) (cz+ (second x))
        :else x))
    form))
;; One proof term per rule of Rt.  Each rule is rebuilt at the zeroed premise
;; vectors; (RC E h) casts its conclusion's vector to zeroUF E (zero_vadd,
;; zero_vscale); (UC h wrap E) casts an IH whose vector is zeroUF of a scaled
;; vector under binders; (FR lemma args i n) is the i-th freshness conjunct of
;; the conclusion's term; (C+ k) is the position under k binders.  Var: the
;; variable is not cz, so its entry is unchanged (zero_nth).
(def ^:private cases
  '[;; rVar, rConst
    (Rt.rVar chkf D (zeroUF us cz) i A r (Eq.trans (zero_len us cz) hl) hA
       (Eq.trans (zero_nth us cz i (not_true_false (Nat.beq i cz) hfr)) hu) hr)
    (Rt.rConst chkf D (zeroUF us cz) t A (Eq.trans (zero_len us cz) hl) h)
    ;; rLam, rApp0, rApp
    (Rt.rLam chkf D (zeroUF us cz) r A t B hA (ih_ht (C+ 1) (FR fresh_lam [r A t] 1 2)))
    (Rt.rApp0 chkf D (zeroUF us cz) f u A B (ih_hf cz (FR fresh_app [f u] 0 2)) hu hA hB)
    (RC (vadd us1 (vscale r us2)) (Rt.rApp chkf D (zeroUF us1 cz) (zeroUF us2 cz) r f u A B hr
         (ih_hf cz (FR fresh_app [f u] 0 2)) (ih_hu cz (FR fresh_app [f u] 1 2)) hA hB))
    ;; rPair0, rPair, rLet
    (Rt.rPair0 chkf D (zeroUF us cz) A B x y hA hB hx (ih_hy cz (FR fresh_pair [(Exp.tSig U.u0 A B) x y] 2 3)))
    (RC (vadd (vscale r us1) us2) (Rt.rPair chkf D (zeroUF us1 cz) (zeroUF us2 cz) r A B x y hr hA hB
         (ih_hx cz (FR fresh_pair [(Exp.tSig r A B) x y] 1 3)) (ih_hy cz (FR fresh_pair [(Exp.tSig r A B) x y] 2 3))))
    (RC (vadd us1 us2) (Rt.rLet chkf D (zeroUF us1 cz) (zeroUF us2 cz) r A B C p t
         (ih_hp cz (FR fresh_letp [C p t] 1 3)) hC hA hB (ih_ht (C+ 2) (FR fresh_letp [C p t] 2 3))))
    ;; rAbort, rConv
    (Rt.rAbort chkf D (zeroUF us cz) A t (ih_ht cz (FR fresh_abort [A t] 1 2)) hA)
    (Rt.rConv chkf D (zeroUF us cz) t A B (ih_ht cz hfr) hB hc)
    ;; rIte, rElimB, rSucc, rRecN
    (RC (vadd us1 us2) (Rt.rIte chkf D (zeroUF us1 cz) (zeroUF us2 cz) b t e C
         (ih_hb cz (FR fresh_ite [b t e] 0 3)) (ih_ht cz (FR fresh_ite [b t e] 1 3)) (ih_he cz (FR fresh_ite [b t e] 2 3))))
    (RC (vadd us1 us2) (Rt.rElimB chkf D (zeroUF us1 cz) (zeroUF us2 cz) P b t e
         (ih_hb cz (FR fresh_elimB [P b t e] 1 4)) hP (ih_ht cz (FR fresh_elimB [P b t e] 2 4)) (ih_he cz (FR fresh_elimB [P b t e] 3 4))))
    (Rt.rSucc chkf D (zeroUF us cz) n (ih_h cz hfr))
    (RC (vadd us1 (vadd us2 (vscale U.uw us3))) (Rt.rRecN chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) P z s n
         (ih_hn cz (FR fresh_recN [P z s n] 3 4)) hP (ih_hz cz (FR fresh_recN [P z s n] 1 4))
         (UC (ih_hs (C+ 2) (FR fresh_recN [P z s n] 2 4)) (List.cons U U.u1 (List.cons U U.uw HOLE)) (vscale U.uw us3))))
    ;; rCaseL, rBnil, rBcons
    (RC (vadd us1 us2) (Rt.rCaseL chkf D (zeroUF us1 cz) (zeroUF us2 cz) P x bs
         (ih_hx cz (FR fresh_caseL [P x bs] 1 3)) hP (ih_hb cz (FR fresh_caseL [P x bs] 2 3))))
    (Rt.rBnil chkf D (zeroUF us cz) P (Eq.trans (zero_len us cz) hl))
    (Rt.rBcons chkf D (zeroUF us cz) P k h t (ih_hh cz (FR fresh_bcons [h t] 0 2)) (ih_ht cz (FR fresh_bcons [h t] 1 2)) hP)
    ;; rSleaf, rSnode, rRecS
    (Rt.rSleaf chkf D (zeroUF us cz) x (ih_h cz hfr))
    (RC (vadd us1 (vadd us2 us3)) (Rt.rSnode chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) x c1 c2
         (ih_hx cz (FR fresh_snode [x c1 c2] 0 3)) (ih_h1 cz (FR fresh_snode [x c1 c2] 1 3)) (ih_h2 cz (FR fresh_snode [x c1 c2] 2 3))))
    (RC (vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3))) (Rt.rRecS chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) P tl tn c
         (ih_hc cz (FR fresh_recS [P tl tn c] 3 4)) hP
         (UC (ih_hl (C+ 1) (FR fresh_recS [P tl tn c] 1 4)) (List.cons U U.uw HOLE) (vscale U.uw us2))
         (UC (ih_hn (C+ 5) (FR fresh_recS [P tl tn c] 2 4))
             (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw HOLE))))) (vscale U.uw us3))
         hY1 hY2))
    ;; rLeaf, rNode, rItR, rPrn
    (Rt.rLeaf chkf D (zeroUF us cz) x (ih_h cz hfr))
    (RC (vadd us1 (vadd us2 (vadd us3 us4))) (Rt.rNode chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) (zeroUF us4 cz) d x r1 r2
         (ih_hd cz (FR fresh_node [d x r1 r2] 0 4)) (ih_hx cz (FR fresh_node [d x r1 r2] 1 4))
         (ih_h1 cz (FR fresh_node [d x r1 r2] 2 4)) (ih_h2 cz (FR fresh_node [d x r1 r2] 3 4))))
    (RC (vadd (vscale U.uw us1) (vadd (vscale U.uw us2) us3)) (Rt.rItR chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) X g h r hX
         (UC (ih_hg cz (FR fresh_itR [X g h r] 1 4)) HOLE (vscale U.uw us1))
         (UC (ih_hh cz (FR fresh_itR [X g h r] 2 4)) HOLE (vscale U.uw us2))
         (ih_hr cz (FR fresh_itR [X g h r] 3 4))))
    (Rt.rPrn chkf D (zeroUF us cz) r (ih_h cz hfr))
    ;; rChk, rH1, rRefl, rInsp
    (RC (vadd us1 us2) (Rt.rChk chkf D (zeroUF us1 cz) (zeroUF us2 cz) c d
         (ih_hc cz (FR fresh_chk [c d] 0 2)) (ih_hd cz (FR fresh_chk [c d] 1 2))))
    (RC (vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5))))
        (Rt.rH1 chkf D (zeroUF us1 cz) (zeroUF us2 cz) (zeroUF us3 cz) (zeroUF us4 cz) (zeroUF us5 cz) r s c e1 e2
         (ih_hr cz (FR fresh_h1 [r s c e1 e2] 0 5)) (ih_hs cz (FR fresh_h1 [r s c e1 e2] 1 5))
         (UC (ih_hc cz (FR fresh_h1 [r s c e1 e2] 2 5)) HOLE (vscale U.uw us3))
         (ih_h1 cz (FR fresh_h1 [r s c e1 e2] 3 5)) (ih_h2 cz (FR fresh_h1 [r s c e1 e2] 4 5))))
    (RC (vadd us1 us2) (Rt.rRefl chkf D (zeroUF us1 cz) (zeroUF us2 cz) X cd r e hb
         (ih_hr cz (FR fresh_refl [X r e] 1 3)) (ih_he cz (FR fresh_refl [X r e] 2 3))))
    (RC (vadd us1 (vadd (vscale U.uw us0) us2)) (Rt.rInsp chkf D (zeroUF us1 cz) (zeroUF us0 cz) (zeroUF us2 cz) X r c t1 t2
         (ih_hr cz (FR fresh_insp [X r c t1 t2] 1 5))
         (UC (ih_hc cz (FR fresh_insp [X r c t1 t2] 2 5)) HOLE (vscale U.uw us0))
         hX hF1 hF2
         (ih_h1 (C+ 2) (FR fresh_insp [X r c t1 t2] 3 5)) (ih_h2 (C+ 2) (FR fresh_insp [X r c t1 t2] 4 5))))])
(a/prove-theorem 'rt_strengthen
  '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)]
  '(forall [cz Nat] (=> (Eq Bool ((freshF t0) cz) Bool.true) (Rt chkf D0 (zeroUF us0 cz) t0 A0)))
  (lv (into ['(induction der)] (mapcat (fn [c] ['(intro cz hfr) (list 'refine' (expand c))]) cases))))

;; --- Lemma 2.3 for a set of variables at once --------------------------------------------

;; rt_mask: every variable a mask g selects, if none is free in t, can be
;; lowered to usage 0 at once — Lemma 2.3 applied to each; Theorem 3's
;; refinement needs it for all the unused tokens of Θₘ.  maskUF us g zeroes
;; the positions g selects; under a binder the mask shifts (gsh: the new
;; variable is never masked), and maskUF (r :: us) (gsh g) = r :: maskUF us g
;; holds by rfl.  The proof is rt_strengthen's table (cases) with the position
;; replaced by the mask: each premise's freshness becomes a function over the
;; masked positions (fresh-fn, built with sh_ok under binders), and Var reads
;; g i = false off the hypothesis (mask_var_false).  The mask is named gm: ItR
;; has a field g.
;; maskUF us g: every position j with g j = true set to usage 0
(kdef maskUF (=> (List U) (=> Nat Bool) (List U))
  (fn [us :- (List U)]
    (List.rec$1$0 U (fn [_ :- (List U)] (=> (=> Nat Bool) (List U))) (fn [g :- (=> Nat Bool)] (List.nil U))
      (fn [x :- U, rest :- (List U), ih :- (=> (=> Nat Bool) (List U))]
        (fn [g :- (=> Nat Bool)] (List.cons U (Bool.rec$1 (fn [_ :- Bool] U) x U.u0 (g 0)) (ih (fn [j :- Nat] (g (+ j 1)))))))
      us)))
;; the mask under one binder: the new variable is never masked
(kdef gsh (=> (=> Nat Bool) Nat Bool)
  (fn [g :- (=> Nat Bool), j :- Nat] (Nat.rec$1 (fn [_ :- Nat] Bool) Bool.false (fn [k :- Nat, _ :- Bool] (g k)) j)))
(thm mask_cons [r :- U, us :- (List U), g :- (=> Nat Bool)]
  (Eq (List U) (maskUF (List.cons U r us) (gsh g)) (List.cons U r (maskUF us g)))
  (rfl))
(thm sel_uadd [bb :- Bool, a :- U, b :- U]
  (Eq U (Bool.rec$1 (fn [_ :- Bool] U) (uadd a b) U.u0 bb) (uadd (Bool.rec$1 (fn [_ :- Bool] U) a U.u0 bb) (Bool.rec$1 (fn [_ :- Bool] U) b U.u0 bb)))
  (cases bb) (rfl) (rfl))
(thm sel_umul [bb :- Bool, r :- U, a :- U]
  (Eq U (Bool.rec$1 (fn [_ :- Bool] U) (umul r a) U.u0 bb) (umul r (Bool.rec$1 (fn [_ :- Bool] U) a U.u0 bb)))
  (cases bb) (rfl) (exact (Eq.symm (umul_zero_right r))))

(thm mask_vadd_step [a :- U, b :- U, xs :- (List U), ys :- (List U), g :- (=> Nat Bool),
                       ih :- (forall [y (List U)] (forall [g (=> Nat Bool)] (Eq (List U) (maskUF (vadd xs y) g) (vadd (maskUF xs g) (maskUF y g)))))]
  (Eq (List U) (maskUF (vadd (List.cons U a xs) (List.cons U b ys)) g) (vadd (maskUF (List.cons U a xs) g) (maskUF (List.cons U b ys) g)))
  (exact (Eq.trans (congrArg (fn [v :- U] (List.cons U v (maskUF (vadd xs ys) (fn [j :- Nat] (g (+ j 1)))))) (sel_uadd (g 0) a b))
                   (congrArg (fn [v :- (List U)] (List.cons U (uadd (Bool.rec$1 (fn [_ :- Bool] U) a U.u0 (g 0)) (Bool.rec$1 (fn [_ :- Bool] U) b U.u0 (g 0))) v))
                             (ih ys (fn [j :- Nat] (g (+ j 1))))))))
(thm mask_vadd [x :- (List U)]
  (forall [y (List U)] (forall [g (=> Nat Bool)] (Eq (List U) (maskUF (vadd x y) g) (vadd (maskUF x g) (maskUF y g)))))
  (induction x)
  (intro y g) (rfl)
  (intro y g) (cases y) (rfl)
  (refine' (mask_vadd_step _ _ _ _ _ ih_tail)))
(thm mask_vscale [r :- U, x :- (List U)]
  (forall [g (=> Nat Bool)] (Eq (List U) (maskUF (vscale r x) g) (vscale r (maskUF x g))))
  (induction x)
  (intro g) (rfl)
  (intro g)
  (exact (Eq.trans (congrArg (fn [v :- U] (List.cons U v (maskUF (vscale r tail) (fn [j :- Nat] (g (+ j 1)))))) (sel_umul (g 0) r head))
                   (congrArg (fn [v :- (List U)] (List.cons U (umul r (Bool.rec$1 (fn [_ :- Bool] U) head U.u0 (g 0))) v)) (ih_tail (fn [j :- Nat] (g (+ j 1))))))))
(thm mask_len [us :- (List U)] (forall [g (=> Nat Bool)] (Eq Nat (lenU (maskUF us g)) (lenU us)))
  (induction us) (intro g) (rfl) (intro g) (exact (congrArg (fn [q :- Nat] (+ q 1)) (ih_tail (fn [j :- Nat] (g (+ j 1)))))))
(thm mask_nth [us :- (List U)]
  (forall [g (=> Nat Bool)] (forall [i Nat] (=> (Eq Bool (g i) Bool.false) (Eq (Option U) (nthU (maskUF us g) i) (nthU us i)))))
  (induction us)
  (intro g i h) (rfl)
  (intro g i h) (cases i)
  (exact (Eq.trans (nthU.eq_2 (Bool.rec$1 (fn [_ :- Bool] U) head U.u0 (g 0)) (maskUF tail (fn [j :- Nat] (g (+ j 1)))))
           (Eq.trans (congrArg (fn [bb :- Bool] (Option.some U (Bool.rec$1 (fn [_ :- Bool] U) head U.u0 bb))) h) (Eq.symm (nthU.eq_2 head tail)))))
  (exact (Eq.trans (nthU.eq_3 (Bool.rec$1 (fn [_ :- Bool] U) head U.u0 (g 0)) (maskUF tail (fn [j :- Nat] (g (+ j 1)))) n)
           (Eq.trans (ih_tail (fn [j :- Nat] (g (+ j 1))) n h) (Eq.symm (nthU.eq_3 head tail n))))))
;; freshness under the shifted mask
(thm sh_ok [g :- (=> Nat Bool), P :- (=> Nat Prop), h :- (forall [j Nat] (=> (Eq Bool (g j) Bool.true) (P (+ j 1))))]
  (forall [j Nat] (=> (Eq Bool (gsh g j) Bool.true) (P j)))
  (intro j) (cases j) (intro hj) (exact (Bool.noConfusion hj)) (intro hj) (exact (h n hj)))

(thm beq_refl [i :- Nat] (Eq Bool (Nat.beq i i) Bool.true) (induction i) (rfl) (exact ih_n))
(thm bfalse [b :- Bool] (=> (=> (Eq Bool b Bool.true) False) (Eq Bool b Bool.false))
  (cases b) (all_goals (intro h)) (exact (False.elim (h (Eq.refl$1 Bool.true)))) (exact (Eq.refl$1 Bool.false)))
(thm mask_var_false [g :- (=> Nat Bool), i :- Nat,
                       hfr :- (forall [j Nat] (=> (Eq Bool (g j) Bool.true) (Eq Bool ((freshF (Exp.var i)) j) Bool.true)))]
  (Eq Bool (g i) Bool.false)
  (exact (bfalse (g i) (fn [h :- (Eq Bool (g i) Bool.true)]
           (Bool.noConfusion (Eq.trans (congrArg Bool.not (Eq.symm (beq_refl i))) (hfr i h)))))))

(defn- gk [k] (if (zero? k) 'gm (list 'gsh (gk (dec k)))))
(defn- frj [lem args i n] ;; freshness of field i at cut j, from hfr j hj
  (let [base (concat (list lem) args (list 'j (list 'hfr 'j 'hj)))
        f (fn f [t m] (if (zero? m) (if (= i (dec n)) t (list 'And.left t)) (f (list 'And.right t) (dec m))))]
    (if (= i (dec n)) (reduce (fn [t _] (list 'And.right t)) base (range i)) (f base i))))
(defn- exp-args [lem args]
  (let [ctor (symbol (subs (name lem) (count "fresh_")))
        fields (second (first (filter #(= (first %) ctor) exp-fields)))]
    (for [[a [_ ty _]] (map vector args fields) :when (= ty 'Exp)] a)))
(defn- fresh-fn [k lem args i n]
  (let [s (nth (exp-args lem args) i)
        P (fn [off] (list 'Eq 'Bool (list (list 'freshF s) (if (zero? off) 'j (list '+ 'j off))) 'Bool.true))
        base (list 'fn ['j :- 'Nat 'hj :- (list 'Eq 'Bool (list 'gm 'j) 'Bool.true)] (frj lem args i n))]
    (loop [d 1 t base]
      (if (> d k) t
          (recur (inc d) (list 'sh_ok (gk (dec d)) (list 'fn '[j :- Nat] (P (- k d))) t))))))
(defn- M [x] (list 'maskUF x 'gm))
(defn- mq [E]
  (let [lu '(List U)]
    (cond (symbol? E) [(M E) (list 'Eq.refl$1 (M E))]
          (= (first E) 'vscale) (let [[_ r X] E [l p] (mq X)]
                                  [(list 'vscale r l) (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vscale r 'v)) p) (list 'Eq.symm (list 'mask_vscale r X 'gm)))])
          (= (first E) 'vadd) (let [[_ X Y] E [l1 p1] (mq X) [l2 p2] (mq Y)]
                                [(list 'vadd l1 l2)
                                 (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd 'v l2)) p1)
                                       (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd (M X) 'v)) p2)
                                             (list 'Eq.symm (list 'mask_vadd X Y 'gm))))]))))
(defn- ih-call? [x] (and (seq? x) (symbol? (first x)) (.startsWith (name (first x)) "ih_")))
(defn- mexpand [form]
  (walk/prewalk
    (fn [x]
      (cond
        (and (seq? x) (= (first x) 'RC)) (let [[_ E h] x [l p] (mq E)] (list 'rt_ucast 'chkf '_ l (M E) '_ '_ h p))
        (and (seq? x) (= (first x) 'UC)) (let [[_ h wrap E] x [l p] (mq E)
                                               w (fn [v] (walk/postwalk-replace {'HOLE v} wrap))]
                                           (list 'rt_ucast 'chkf '_ (w (M E)) (w l) '_ '_ h
                                                 (list 'congrArg (list 'fn '[v :- (List U)] (w 'v)) (list 'Eq.symm p))))
        (ih-call? x) (let [[ih pos fr] x
                           k (if (= pos 'cz) 0 (second pos))]
                       (list ih (gk k) (if (= fr 'hfr) 'hfr (let [[_ lem args i n] fr] (fresh-fn k lem args i n)))))
        (= x 'zeroUF) 'maskUF
        (= x 'zero_len) 'mask_len
        (= x 'cz) 'gm
        :else x))
    form))

(def ^:private mcases
  (into ['(Rt.rVar chkf D (maskUF us gm) i A r (Eq.trans (mask_len us gm) hl) hA
           (Eq.trans (mask_nth us gm i (mask_var_false gm i hfr)) hu) hr)]
        (map mexpand (rest cases))))
(a/prove-theorem 'rt_mask
  '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)]
  '(forall [gm (=> Nat Bool)] (=> (forall [j Nat] (=> (Eq Bool (gm j) Bool.true) (Eq Bool ((freshF t0) j) Bool.true)))
                                 (Rt chkf D0 (maskUF us0 gm) t0 A0)))
  (lv (into ['(induction der)] (mapcat (fn [c] ['(intro gm hfr) (list 'refine' c)]) mcases))))
