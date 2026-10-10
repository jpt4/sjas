(ns lcert.formal.p49peel
  "Generic curried bounded constructor elimination for Proposition 4.9.

  Only judgment.clj is loaded: A can be T(p49dl j (var 0)), but the
  combinator does not unfold or assume a particular depth predicate.
  W is the unary conclusion in Syn::D. Its evidence binder is inserted by
  lift, so evidence is curried INSIDE each recSyn arm, never captured by
  recSyn's omega-scaled ambient context.

  p49peel_typed takes genuine leaf instances, a typed pair-producing term
  under the node evidence, and a continuation under the pair's two binders.
  It constructs both lambdas, the pair elimination, recSyn, and the outer
  code lambda. No premise asserts the resulting bounded eliminator.

  Instantiation still must prove the displayed formation premises, the
  substitution identity hself, and the pair/continuation premises.
  In particular the pair premise includes the depth-node conversion and
  application of the Boolean splitter; this file does not assert them.
  The terminal continuation is the externally supplied smaller-potential
  induction instance, not either recSyn recursive result."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.judgment]))

;; Local structural zero vectors keep this module out of the much larger
;; weakening/semantic closure. They erase all ambient branch resources.
(a/defn p49peel_z [us :- (List U)] (List U)
  (match us [nil (List.nil U)]
    [(cons x xs) (List.cons U U.u0 (p49peel_z xs))]))
(thm p49peel_zscale [us :- (List U)]
  (= (vscale U.uw (p49peel_z us)) (p49peel_z us))
  (induction us) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u0 v)) ih_tail)))
(thm p49peel_zadd [us :- (List U)]
  (= (vadd (p49peel_z us) (p49peel_z us)) (p49peel_z us))
  (induction us) (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u0 v)) ih_tail)))
(thm p49peel_addz [us :- (List U)]
  (= (vadd us (p49peel_z us)) us)
  (induction us) (rfl)
  (exact (Eq.trans
    (congrArg (fn [r :- U] (List.cons U r (vadd tail (p49peel_z tail))))
      (uadd_zero_right head))
    (congrArg (fn [v :- (List U)] (List.cons U head v)) ih_tail))))
(thm p49peel_zlen [us :- (List U)]
  (= (lenU (p49peel_z us)) (lenU us))
  (induction us) (rfl) (exact (congrArg Nat.succ ih_tail)))

;; Propositional transport changes only indices of an existing derivation.
;; These are Eq eliminations, not extra conversion or typing rules.
(thm p49peel_cast [chkf :- (=> Code Code Bool), D :- (List Exp),
                  us :- (List U), vs :- (List U), t :- Exp, A :- Exp, B :- Exp,
                  ht :- (Rt chkf D us t A), hu :- (= us vs), hA :- (= A B)]
  (Rt chkf D vs t B)
  (exact (Eq.mp (congrArg (fn [C :- Exp] (Rt chkf D vs t C)) hA)
    (Eq.mp (congrArg (fn [v :- (List U)] (Rt chkf D v t A)) hu) ht))))

(kdef p49peel_body (=> Exp Exp Exp)
  (fn [A :- Exp, W :- Exp] (Exp.tPi U.u1 A (lift 1 0 W))))
(kdef p49peel_motive (=> Exp Exp Exp)
  (fn [A :- Exp, W :- Exp] (lift 1 1 (p49peel_body A W))))
(kdef p49peel_leafA (=> Exp Exp)
  (fn [A :- Exp] (subst sLeafI (lift 1 1 A))))
(kdef p49peel_leafB (=> Exp Exp)
  (fn [W :- Exp] (subst (upn 1 sLeafI) (lift 1 2 (lift 1 0 W)))))
(kdef p49peel_nodeA (=> Exp Exp)
  (fn [A :- Exp] (subst sNodeI (lift 1 1 A))))
(kdef p49peel_nodeB (=> Exp Exp)
  (fn [W :- Exp] (subst (upn 1 sNodeI) (lift 1 2 (lift 1 0 W)))))

;; The exact substitutions below compute by definition at the outer Pi;
;; no reduction of an arbitrary W or A is claimed.
(thm p49peel_leaf_shape [A :- Exp, W :- Exp]
  (= (leafTy (p49peel_motive A W))
     (Exp.tPi U.u1 (p49peel_leafA A) (p49peel_leafB W))) (rfl))
(thm p49peel_node_shape [A :- Exp, W :- Exp]
  (= (nodeTy (p49peel_motive A W))
     (Exp.tPi U.u1 (p49peel_nodeA A) (p49peel_nodeB W))) (rfl))

;; Small data builders only abbreviate finite constructor/rule expressions.
;; All symbolic lists, types, equalities, and proofs remain kernel terms.
(defn- es [xs tail] (reduce #(list 'consE %2 %1) tail (reverse xs)))
(defn- us [xs tail] (reduce #(list 'consU %2 %1) tail (reverse xs)))
(def ^:private dc (es '[Exp.tSyn] 'D))
(def ^:private dl (es '[Exp.tLbl] dc))
(def ^:private m '(p49peel_motive A W))
(def ^:private dn (es [(list 'y2Ty m) (list 'y1Ty m) 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl] dc))
(def ^:private dz (us '[U.u0] '(p49peel_z uv)))
(def ^:private un (us '[U.u1 U.u1 U.uw U.uw U.uw] dz))
(def ^:private de (es '[(p49peel_nodeA A)] dn))
(def ^:private dp (es '[Y X] de))
;; The splitter evaluates the two child depth predicates. Their Syn
;; variables therefore need omega usage in q as well as in the continuation.
;; The whole evidence is linear and both recSyn result variables are erased.
(def ^:private uq (us '[U.u1 U.u0 U.u0 U.uw U.uw U.u0] dz))
(def ^:private uk (us '[U.u0 U.u1 U.u1 U.uw U.uw U.uw] dz))

(kdef p49peel_node (=> Exp Exp Exp Exp Exp)
  (fn [E :- Exp, C :- Exp, q :- Exp, n :- Exp]
    (Exp.lam U.u1 E (Exp.letp C q n))))

;; General pair-elimination arm. C is in E::D; X,Y are the Sigma domain
;; and dependent codomain in E::D and X::E::D respectively. The equality
;; forces the split and continuation budgets to sum to 1::uv exactly.
(thm p49peel_node_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
   uq :- (List U), uk :- (List U), E :- Exp, C :- Exp, X :- Exp, Y :- Exp,
   q :- Exp, n :- Exp,
   hE :- (Tl chkf Bool.true D E Exp.tUnit),
   hC :- (Tl chkf Bool.true (consE E D) C Exp.tUnit),
   hX :- (Tl chkf Bool.true (consE E D) X Exp.tUnit),
   hY :- (Tl chkf Bool.true (consE X (consE E D)) Y Exp.tUnit),
   hq :- (Rt chkf (consE E D) uq q (Exp.tSig U.u1 X Y)),
   hn :- (Rt chkf (consE Y (consE X (consE E D)))
             (consU U.u1 (consU U.u1 uk)) n (lift 2 0 C)),
   hu :- (= (vadd uq uk) (consU U.u1 uv))]
  (Rt chkf D uv (p49peel_node E C q n) (Exp.tPi U.u1 E C))
  (exact (Rt.rLam chkf D uv U.u1 E (Exp.letp C q n) C hE
    (p49peel_cast chkf (consE E D) (vadd uq uk) (consU U.u1 uv)
      (Exp.letp C q n) C C
      (Rt.rLet chkf (consE E D) uq uk U.u1 X Y C q n hq hC hX hY hn)
      hu rfl))))

;; The zero-depth alternative requires an actual derivation of absurdity
;; from the whole evidence (obtained by the depth-zero head reductions).
;; Merely having a formation proof of the evidence is insufficient.
(thm p49peel_zero_node_typed
  [chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
   E :- Exp, C :- Exp, q :- Exp,
   hE :- (Tl chkf Bool.true D E Exp.tUnit),
   hC :- (Tl chkf Bool.true (consE E D) C Exp.tUnit),
   hq :- (Rt chkf (consE E D) (consU U.u1 uv) q Exp.tEmpty)]
  (Rt chkf D uv (Exp.lam U.u1 E (Exp.abort C q)) (Exp.tPi U.u1 E C))
  (exact (Rt.rLam chkf D uv U.u1 E (Exp.abort C q) C hE
    (Rt.rAbort chkf (consE E D) (consU U.u1 uv) C q hq hC))))

;; Named exact contexts allow the parent to state helper lemmas without
;; reverse-engineering a generated theorem's telescope.
(doseq [[nm ty pars body]
        [['p49peel_Dc '(=> (List Exp) (List Exp)) '[D :- (List Exp)] dc]
         ['p49peel_Dl '(=> (List Exp) (List Exp)) '[D :- (List Exp)] dl]
         ['p49peel_Dn '(=> Exp Exp (List Exp) (List Exp)) '[A :- Exp, W :- Exp, D :- (List Exp)] dn]
         ['p49peel_Uz '(=> (List U) (List U)) '[uv :- (List U)] dz]
         ['p49peel_Un '(=> (List U) (List U)) '[uv :- (List U)] un]
         ['p49peel_Uq '(=> (List U) (List U)) '[uv :- (List U)] uq]
         ['p49peel_Uk '(=> (List U) (List U)) '[uv :- (List U)] uk]]]
  (b/kdef! nm ty (list 'fn pars body)))

;; Usage normalizations are proved for arbitrary tails, not tested at
;; bounded lengths. Only the scrutinee carries the original ambient uv.
(thm p49peel_rec_usage [uv :- (List U)]
  (= (vadd (consU U.uw uv)
       (vadd (vscale U.uw (p49peel_Uz uv)) (vscale U.uw (p49peel_Uz uv))))
     (consU U.uw uv))
  (exact (Eq.trans
    (congrArg (fn [v :- (List U)] (consU U.uw (vadd uv (vadd v v))))
      (p49peel_zscale uv))
    (Eq.trans
      (congrArg (fn [v :- (List U)] (consU U.uw (vadd uv v)))
        (p49peel_zadd uv))
      (congrArg (fn [v :- (List U)] (consU U.uw v)) (p49peel_addz uv))))))
(thm p49peel_pair_usage [uv :- (List U)]
  (= (vadd (p49peel_Uq uv) (p49peel_Uk uv)) (consU U.u1 (p49peel_Un uv)))
  (exact (congrArg
    (fn [v :- (List U)]
      (consU U.u1 (consU U.u1 (consU U.u1
        (consU U.uw (consU U.uw (consU U.uw (consU U.u0 v))))))))
    (p49peel_zadd uv))))

(kdef p49peel_term (=> Exp Exp Exp Exp Exp Exp)
  (fn [A :- Exp, W :- Exp, l :- Exp, q :- Exp, n :- Exp]
    (Exp.lam U.uw Exp.tSyn
      (Exp.recS (p49peel_motive A W)
        (Exp.lam U.u1 (p49peel_leafA A) l)
        (p49peel_node (p49peel_nodeA A) (p49peel_nodeB W) q n)
        (Exp.var 0)))))

;; Main typed peel: every assumption below is a local formation, a
;; syntactic equality, a leaf instance, a split, or a node continuation.
;; No hypothesis is an Rt judgment for p49peel_term or for recSyn.
(let [params
      (vec (concat
        '[chkf :- (=> Code Code Bool), D :- (List Exp), uv :- (List U),
          A :- Exp, W :- Exp, X :- Exp, Y :- Exp, l :- Exp, q :- Exp, n :- Exp,
          hlen :- (= (lenU uv) (lenE D)),
          hself :- (= (subst1 (Exp.var 0) (p49peel_motive A W)) (p49peel_body A W))]
        ['hM :- (list 'Tl 'chkf 'Bool.true (es '[Exp.tSyn] dc) m 'Exp.tUnit)
         'hY1 :- (list 'Tl 'chkf 'Bool.true (es '[Exp.tSyn Exp.tSyn Exp.tLbl] dc) (list 'y1Ty m) 'Exp.tUnit)
         'hY2 :- (list 'Tl 'chkf 'Bool.true (es [(list 'y1Ty m) 'Exp.tSyn 'Exp.tSyn 'Exp.tLbl] dc) (list 'y2Ty m) 'Exp.tUnit)
         'hLA :- (list 'Tl 'chkf 'Bool.true dl '(p49peel_leafA A) 'Exp.tUnit)
         'hNA :- (list 'Tl 'chkf 'Bool.true dn '(p49peel_nodeA A) 'Exp.tUnit)
         'hNB :- (list 'Tl 'chkf 'Bool.true de '(p49peel_nodeB W) 'Exp.tUnit)
         'hX :- (list 'Tl 'chkf 'Bool.true de 'X 'Exp.tUnit)
         'hY :- (list 'Tl 'chkf 'Bool.true (es '[X] de) 'Y 'Exp.tUnit)
         'hl :- (list 'Rt 'chkf (es '[(p49peel_leafA A)] dl) (us '[U.u1 U.uw] dz) 'l '(p49peel_leafB W))
         'hq :- (list 'Rt 'chkf de uq 'q '(Exp.tSig U.u1 X Y))
         'hn :- (list 'Rt 'chkf dp (us '[U.u1 U.u1] uk) 'n '(lift 2 0 (p49peel_nodeB W)))]))
      leaf '(Exp.lam U.u1 (p49peel_leafA A) l)
      node '(p49peel_node (p49peel_nodeA A) (p49peel_nodeB W) q n)
      rec-term (list 'Exp.recS m leaf node '(Exp.var 0))
      rec-use (list 'vadd (us '[U.uw] 'uv) (list 'vadd (list 'vscale 'U.uw dz) (list 'vscale 'U.uw dz)))
      leaf-proof (list 'Rt.rLam 'chkf dl (us '[U.uw] dz) 'U.u1 '(p49peel_leafA A) 'l '(p49peel_leafB W) 'hLA 'hl)
      node-proof (list 'p49peel_node_typed 'chkf dn un uq uk '(p49peel_nodeA A) '(p49peel_nodeB W) 'X 'Y 'q 'n
                   'hNA 'hNB 'hX 'hY 'hq 'hn '(p49peel_pair_usage uv))
      ;; Omega scaling of the erased ambient tail needs one equality cast
      ;; per branch, since arbitrary vectors are not definitionally zero.
      leaf-scaled (list 'Eq.mp
        (list 'congrArg (list 'fn '[v :- (List U)] (list 'Rt 'chkf dl (us '[U.uw U.u0] 'v) leaf (list 'leafTy m)))
          '(Eq.symm (p49peel_zscale uv))) leaf-proof)
      node-scaled (list 'Eq.mp
        (list 'congrArg (list 'fn '[v :- (List U)] (list 'Rt 'chkf dn (us '[U.u1 U.u1 U.uw U.uw U.uw U.u0] 'v) node (list 'nodeTy m)))
          '(Eq.symm (p49peel_zscale uv))) node-proof)
      rec-proof (list 'Rt.rRecS 'chkf dc (us '[U.uw] 'uv) dz dz m leaf node '(Exp.var 0)
                  (list 'Rt.rVar 'chkf dc (us '[U.uw] 'uv) 0 'Exp.tSyn 'U.uw
                    '(congrArg Nat.succ hlen) (list 'nthE.eq_2 'Exp.tSyn 'D) '(nthU.eq_2 U.uw uv) 'rfl)
                  'hM leaf-scaled node-scaled 'hY1 'hY2)]
  (a/prove-theorem 'p49peel_typed params
    '(Rt chkf D uv (p49peel_term A W l q n)
       (Exp.tPi U.uw Exp.tSyn (p49peel_body A W)))
    [(list 'exact
       (list 'Rt.rLam 'chkf 'D 'uv 'U.uw 'Exp.tSyn rec-term '(p49peel_body A W)
         '(Tl.fBase chkf D Exp.tSyn rfl)
         (list 'p49peel_cast 'chkf dc rec-use (us '[U.uw] 'uv) rec-term
           (list 'subst1 '(Exp.var 0) m) '(p49peel_body A W) rec-proof
           '(p49peel_rec_usage uv) 'hself)))]))
