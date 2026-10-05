(ns lcert.formal.enclabels
  "F7 — the labels of encodings (ADR-0006).

  Codes are trees over the finite label set L. The paper's L is the
  encoding labels — 97 of them, positions 0–96 of lcert.syntax/
  encoding-labels — plus labels free for programs; the formalization fixes
  |L| = NL = 100 and asks codes held as Syn or R values to have every label
  below NL (lblOk, skel.clj).

  This namespace proves that the encodings introduce no label of their own
  outside the encoding labels: for every bound nb ≥ 97, an encoding has every
  label below nb as soon as the data it encodes does — the label constants
  of an expression (lblsE) and any raw code it carries. So the encodings stay
  inside L whatever |L| is, as long as L contains the encoding labels: now
  (nb = NL = 100, lblOk_lblBelow) and if the free labels are later made a
  parameter.

  - lblBelow nb c: every label of the code c is below nb.
  - lblsE nb e: every label constant ℓ of the expression e is below nb (the
    typing rules already ask ℓ < NL: constTyped).
  - lblBelow_encNat: unary numbers (labels 2, 3, 0) are below every nb ≥ 97.
  - lblBelow_encE: lblsE nb e = true → lblBelow nb (encE e) = true, one lemma
    per constructor, generated from encE's own clauses (read from encode.clj),
    so the theorem cannot drift from the encoder.

  encE's pseudo-type tBrs, which earlier took label 100 (outside L), now takes
  label 11 (:isType), which the expression encoding uses nowhere else
  (encode.clj)."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.check-hd :as h]
            [lcert.formal.encode]))

;; ---------------------------------------------------------------------------
;; The bound and its basic facts.

(kdef lblBelow (=> Nat Code Bool)
  (fn [nb :- Nat, c :- Code]
    (Code.rec$1 (fn [_ :- Code] Bool)
      (fn [l :- Nat] (Nat.blt l nb))
      (fn [l :- Nat, a :- Code, b :- Code, ia :- Bool, ib :- Bool] (Bool.and (Nat.blt l nb) (Bool.and ia ib)))
      c)))

;; lblOk (skel.clj) is the bound at NL = 100.
(thm lblOk_lblBelow [c :- Code] (Eq Bool (lblOk c) (lblBelow 100 c))
  (induction c)
  (rfl)
  (have q (Eq Bool (Bool.and (Nat.blt l 100) (Bool.and (lblOk a) (lblOk b)))
                   (Bool.and (Nat.blt l 100) (Bool.and (lblBelow 100 a) (lblBelow 100 b))))
    (congrArg (fn [p :- Bool] (Bool.and (Nat.blt l 100) p)) (congr (congrArg Bool.and ih_a) ih_b)))
  (exact q))

(thm band_tt2 [x :- Bool, y :- Bool, hx :- (Eq Bool x Bool.true), hy :- (Eq Bool y Bool.true)]
  (Eq Bool (Bool.and x y) Bool.true)
  (exact (Eq.trans (congrArg (fn [z :- Bool] (Bool.and z y)) hx) hy)))

;; A literal label L ≤ 96 is below every nb ≥ 97 (hL by computation).
(thm blt_lit [L :- Nat, nb :- Nat, hL :- (Eq Bool (Nat.ble (+ L 1) 97) Bool.true), hk :- (LE.le 97 nb)]
  (Eq Bool (Nat.blt L nb) Bool.true)
  (exact (Nat.ble_eq_true_of_le (Nat.le_trans (Nat.le_of_ble_eq_true hL) hk))))

;; uLbl r L0 is L0, L0+1 or L0+2 (the three usages of Π, Σ, λ).
(thm uLbl_blt [r :- U, L0 :- Nat, nb :- Nat, hL :- (Eq Bool (Nat.ble (+ L0 3) 97) Bool.true), hk :- (LE.le 97 nb)]
  (Eq Bool (Nat.blt (uLbl r L0) nb) Bool.true)
  (have h3 (LE.le (+ L0 3) 97) (Nat.le_of_ble_eq_true hL))
  (cases r)
  (change (Eq Bool (Nat.blt L0 nb) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega)
  (change (Eq Bool (Nat.blt (+ L0 1) nb) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega)
  (change (Eq Bool (Nat.blt (+ L0 2) nb) Bool.true)) (refine' (Nat.ble_eq_true_of_le _)) (omega))

;; Unary numbers: zero is sl 2, succ j is sn 3 ⌜j⌝ (sl 0).
(thm lblBelow_encNat [nb :- Nat, hk :- (LE.le 97 nb)]
  (forall [j Nat] (Eq Bool (lblBelow nb (encNat j)) Bool.true))
  (intro j) (induction j)
  (exact (blt_lit 2 nb (Eq.refl Bool.true) hk))
  (have q (Eq Bool (Bool.and (Nat.blt 3 nb) (Bool.and (lblBelow nb (encNat n)) (lblBelow nb (Code.sl 0)))) Bool.true)
    (band_tt2 (Nat.blt 3 nb) (Bool.and (lblBelow nb (encNat n)) (lblBelow nb (Code.sl 0)))
      (blt_lit 3 nb (Eq.refl Bool.true) hk)
      (band_tt2 (lblBelow nb (encNat n)) (lblBelow nb (Code.sl 0)) ih_n (blt_lit 0 nb (Eq.refl Bool.true) hk))))
  (exact q))

;; ---------------------------------------------------------------------------
;; Expressions: the data predicate, and the bound for encE.

;; Exp's constructors and fields, and encE's clauses, read from the sources
;; (syntax.clj's a/inductive Exp; encode.clj's kdef encE) so that the
;; generated statements and proofs follow the definitions exactly.
(defn- read-forms [f] (read-string (str "[" (slurp (io/resource f)) "]")))
(def ^:private exp-ctors
  (let [form (first (filter #(and (seq? %) (= 'a/inductive (first %)) (= 'Exp (second %)))
                            (read-forms "lcert/formal/syntax.clj")))]
    (vec (for [c (drop 3 form)] [(first c) (vec (map vec (rest c)))]))))
(def ^:private enc-clauses
  (let [form (first (filter #(and (seq? %) (= 'kdef (first %)) (= 'encE (second %)))
                            (read-forms "lcert/formal/encode.clj")))
        rec (nth (nth form 3) 2)]               ; (fn [e :- Exp] (Exp.rec$1 motive c1 … e))
    (vec (butlast (drop 2 rec)))))

(defn- exp-fields [fields] (vec (for [[x T] fields :when (= T 'Exp)] x)))

;; lblsE nb e: every label constant of e below nb.
(b/kdef! 'lblsE '(=> Nat Exp Bool)
  (list 'fn '[nb :- Nat, ex :- Exp]
    (concat (list 'Exp.rec$1 '(fn [_ :- Exp] Bool))
      (for [[ctor fields] exp-ctors]
        (let [ihs (for [x (exp-fields fields)] [(symbol (str "ih_" x)) 'Bool])
              body (if (= ctor 'lbl) '(Nat.blt l nb) (h/f7-and (map first ihs)))]
          (if (seq (concat fields ihs)) (list 'fn (h/f7-params (concat fields ihs)) body) body)))
      ['ex])))

(defn- clause-body
  "encE's clause for a constructor, with each child's code i_x replaced by
  (encE x)."
  [[ctor fields] clause]
  (if (and (seq? clause) (= 'fn (first clause)))
    (walk/postwalk-replace (into {} (for [x (exp-fields fields)] [(symbol (str "i_" x)) (list 'encE x)]))
                           (nth clause 2))
    clause))

(defn- label-proof [L]
  (cond (integer? L) (list 'blt_lit L 'nb '(Eq.refl Bool.true) 'hk)
        (and (seq? L) (= 'uLbl (first L))) (list 'uLbl_blt (nth L 1) (nth L 2) 'nb '(Eq.refl Bool.true) 'hk)
        :else 'hs))                              ; the label constant l itself: lblsE nb (lbl l) is Nat.blt l nb

(defn- below-proof
  "A proof that lblBelow nb c = true, for c a piece of a clause body."
  [c child-proof]
  (cond
    (and (seq? c) (= 'Code.sl (first c))) (label-proof (second c))
    (and (seq? c) (= 'Code.sn (first c)))
    (let [[_ L x y] c]
      (list 'band_tt2 (list 'Nat.blt L 'nb) (list 'Bool.and (list 'lblBelow 'nb x) (list 'lblBelow 'nb y))
            (label-proof L)
            (list 'band_tt2 (list 'lblBelow 'nb x) (list 'lblBelow 'nb y) (below-proof x child-proof) (below-proof y child-proof))))
    (and (seq? c) (= 'encNat (first c))) (list 'lblBelow_encNat 'nb 'hk (second c))
    (and (seq? c) (= 'encE (first c))) (child-proof (second c))
    :else (throw (ex-info "unexpected piece of an encE clause" {:piece c}))))

(defn- motive [x] (list '=> (list 'Eq 'Bool (list 'lblsE 'nb x) 'Bool.true)
                           (list 'Eq 'Bool (list 'lblBelow 'nb (list 'encE x)) 'Bool.true)))

(doseq [[[ctor fields :as c] clause] (map vector exp-ctors enc-clauses)]
  (let [xs (exp-fields fields)
        v (h/f7-app (symbol (str "Exp." ctor)) (map first fields))
        checks (map #(list 'lblsE 'nb %) xs)
        child (fn [x] (list (symbol (str "ih_" x)) (h/f7-projection checks (.indexOf ^java.util.List xs x) 'hs)))
        body (clause-body c clause)]
    (a/prove-theorem (symbol (str "lblE_" ctor))
      (lv (h/f7-params (concat [['nb 'Nat] ['hk '(LE.le 97 nb)]] fields
                               (for [x xs] [(symbol (str "ih_" x)) (motive x)])
                               [['hs (list 'Eq 'Bool (list 'lblsE 'nb v) 'Bool.true)]])))
      (lv (list 'Eq 'Bool (list 'lblBelow 'nb (list 'encE v)) 'Bool.true))
      (lv [(list 'have 'q_lbl (list 'Eq 'Bool (list 'lblBelow 'nb body) 'Bool.true) (below-proof body child))
           '(exact q_lbl)]))))

(a/prove-theorem 'lblBelow_encE '[nb :- Nat, hk :- (LE.le 97 nb), ex :- Exp] (motive 'ex)
  (lv (into ['(induction ex)]
            (mapcat (fn [[ctor fields]]
                      ['(intro hs)
                       (list 'exact (apply list (symbol (str "lblE_" ctor)) 'nb 'hk
                                           (concat (map first fields) (map #(symbol (str "ih_" %)) (exp-fields fields)) ['hs])))])
                    exp-ctors))))
