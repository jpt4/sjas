(ns lcert.formal.p49depth
  "Proposition 4.9: syntax of the depth bound and its substitution algebra.
  These lemmas do not construct inhabitants at positive depths."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.p49zero]))

;; Primitive recursion in the kernel's Nat, not a Clojure enumeration of
;; codes. The node arm has five binders: y2, y1, c2, c1, label.
(kdef p49dl (=> Nat Exp Exp)
  (fn [k :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] (=> Exp Exp))
      (fn [c :- Exp] (Exp.recS Exp.tBool Exp.tt Exp.ff c))
      (fn [j :- Nat, prev :- (=> Exp Exp)]
        (fn [c :- Exp]
          (Exp.recS Exp.tBool Exp.tt
            (Exp.ite (prev (Exp.var 3)) (prev (Exp.var 2)) Exp.ff) c))) k)))

(kdef P49 (=> Nat Exp)
  (fn [k :- Nat]
    (Exp.tPi U.uw Exp.tSyn
      (Exp.tPi U.u1 (Exp.tT (p49dl k (Exp.var 0)))
        (Exp.tPi U.u1 (Exp.tT (Exp.chk (Exp.var 1) (cbot))) Exp.tEmpty)))))

;; ‹lemma p49dl_zero_agrees: statement and proof elided, lines 25–26›
;; ‹lemma P49_zero_agrees: statement and proof elided, lines 27–27›

;; Generalized over the substitution: the recursive calls in a recSyn
;; node arm see upn 5 sigma, whose first five variables remain bound.
;; ‹lemma p49dl_subst: statement and proof elided, lines 31–44›

;; Rule-sized head certificates: no code enumeration, checker evaluation,
;; or expansion at a concrete positive k is involved.
;; ‹lemma p49dl_leaf: statement and proof elided, lines 48–53›

;; ‹lemma p49dl_zero_node: statement and proof elided, lines 55–57›

;; instL is well-founded, so use its proved structural counterpart instead
;; of asking reflexivity to evaluate list lookup in the recSyn contractum.
;; ‹lemma p49dl_node_subst: statement and proof elided, lines 61–72›

;; ‹lemma p49dl_node: statement and proof elided, lines 74–90›

;; Formation at arbitrary depth, in arbitrary ambient telescopes. The
;; builders below merely write out the fixed recSyn rule premises. The
;; induction over k itself is inside the kernel.
(defn- p49d-ctx [xs]
  (reduce (fn [tail x] (list 'List.cons 'Exp x tail)) 'D (reverse xs)))
(defn- p49d-lookup [xs i]
  (let [[x & rest-xs] xs]
    (if (zero? i) (list 'nthE.eq_2 x (p49d-ctx rest-xs))
      (list 'Eq.trans (list 'nthE.eq_3 x (p49d-ctx rest-xs) (dec i))
        (p49d-lookup rest-xs (dec i))))))
(defn- p49d-var [xs i]
  (list 'Tl.zVar 'chkf (p49d-ctx xs) i (nth xs i) (p49d-lookup xs i)))
(defn- p49d-base [xs A] (list 'Tl.fBase 'chkf (p49d-ctx xs) A 'rfl))
(defn- p49d-const [xs e A] (list 'Tl.zConst 'chkf (p49d-ctx xs) e A 'rfl))

(doseq [successor? [false true]]
  (let [xs '[Exp.tBool Exp.tBool Exp.tSyn Exp.tSyn Exp.tLbl]
        tn (if successor? '(Exp.ite (p49dl j (Exp.var 3)) (p49dl j (Exp.var 2)) Exp.ff) 'Exp.ff)
        params (vec (concat '[chkf :- (=> Code Code Bool)]
          (when successor?
            '[j :- Nat, hi :- (forall [D (List Exp)] (forall [c Exp]
               (=> (Tl chkf Bool.false D c Exp.tSyn)
                   (Tl chkf Bool.false D (p49dl j c) Exp.tBool))))])
          '[D :- (List Exp), c :- Exp, hc :- (Tl chkf Bool.false D c Exp.tSyn)]))]
    (a/prove-theorem (if successor? 'p49dl_typed_succ 'p49dl_typed_zero) params
      (list 'Tl 'chkf 'Bool.false 'D (list 'p49dl (if successor? '(+ j 1) 0) 'c) 'Exp.tBool)
      [(list 'exact
         (list 'Tl.zRecS 'chkf 'D 'Exp.tBool 'Exp.tt tn 'c 'hc
           (p49d-base '[Exp.tSyn] 'Exp.tBool)
           (p49d-const '[Exp.tLbl] 'Exp.tt 'Exp.tBool)
           (if successor?
             (list 'Tl.zIte 'chkf (p49d-ctx xs)
               '(p49dl j (Exp.var 3)) '(p49dl j (Exp.var 2)) 'Exp.ff 'Exp.tBool
               (list 'hi (p49d-ctx xs) '(Exp.var 3) (p49d-var xs 3))
               (list 'hi (p49d-ctx xs) '(Exp.var 2) (p49d-var xs 2))
               (p49d-const xs 'Exp.ff 'Exp.tBool))
             (p49d-const xs 'Exp.ff 'Exp.tBool))))])))

;; ‹lemma p49dl_typed: statement and proof elided, lines 130–136›

;; P49 k is a formed closed type for every k. No existence of an inhabitant
;; follows from formation. Its bound uses the previous depth in both children.
(let [A '(Exp.tT (p49dl k (Exp.var 0)))
      B '(Exp.tT (Exp.chk (Exp.var 1) (cbot)))
      body (list 'Exp.tPi 'U.u1 A (list 'Exp.tPi 'U.u1 B 'Exp.tEmpty))]
  (a/prove-theorem 'P49_formed '[chkf :- (=> Code Code Bool), k :- Nat, D :- (List Exp)]
    '(Tl chkf Bool.true D (P49 k) Exp.tUnit)
    [(list 'exact
       (list 'Tl.fPi 'chkf 'D 'U.uw 'Exp.tSyn body
         (p49d-base [] 'Exp.tSyn)
         (list 'Tl.fPi 'chkf (p49d-ctx '[Exp.tSyn]) 'U.u1 A
           (list 'Exp.tPi 'U.u1 B 'Exp.tEmpty)
           (list 'Tl.fT 'chkf (p49d-ctx '[Exp.tSyn]) '(p49dl k (Exp.var 0))
             (list 'p49dl_typed 'chkf 'k (p49d-ctx '[Exp.tSyn]) '(Exp.var 0) (p49d-var '[Exp.tSyn] 0)))
           (list 'Tl.fPi 'chkf (p49d-ctx [A 'Exp.tSyn]) 'U.u1 B 'Exp.tEmpty
             (list 'Tl.fT 'chkf (p49d-ctx [A 'Exp.tSyn]) '(Exp.chk (Exp.var 1) (cbot))
               (list 'Tl.zChk 'chkf (p49d-ctx [A 'Exp.tSyn]) '(Exp.var 1) '(cbot)
                 (p49d-var [A 'Exp.tSyn] 1)
                 (list 'Tl.zSleaf 'chkf (p49d-ctx [A 'Exp.tSyn]) '(Exp.lbl 15)
                   (p49d-const [A 'Exp.tSyn] '(Exp.lbl 15) 'Exp.tLbl))))
             (p49d-base [B A 'Exp.tSyn] 'Exp.tEmpty)))))]))

;; The completed fragment expressed using the general family's name.
;; ‹lemma prop49_zero: statement and proof elided, lines 161–166›
