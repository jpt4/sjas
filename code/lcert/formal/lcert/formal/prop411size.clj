(ns lcert.formal.prop411size
  "A10, reopened 2026-10-15: the exact F7 size of Proposition 4.11's
  certificate family.

  `sh4_cn_cert n` (prop411poly) is the canonical F7 certificate of the
  sharing chain c_n.  This namespace proves its node count exactly,

    cnodes (sh4_cn_cert n) = 16 n² + 97 n + 73,

  and evaluates it in the kernel at n = 2, 4, 6, 8 for R4 §4.10's table.
  The method is sizegen's: one generated equation per derivation-tree layer,
  one fact per atom, omega on the resulting linear identity, then induction.

  The derivation tree's own node count enters the certificate too: F7
  stores the decoder's fuel `htDT T₁ + htDT T₂ + 1` in unary, and `htDT`
  counts the nodes of a derivation tree (certenc's `define-fueled!`)."
  (:require [ansatz.core :as a]
            [clojure.walk]
            [lcert.formal.base :refer [thm lv]]
            [lcert.formal.sizegen :as sg]
            [lcert.formal.prop411cert]
            [lcert.formal.prop411poly]))

;; A λ over the chain's body: F7 spends one node on it, plus the body.
(def ^:private lam-code '(Code.sn 50 (Code.sl 20) (encE (sh4_open n))))

(def ^:private open-facts
  "Size facts for the n-dependent fields: the chain's body and its λ."
  {'(cnodes (encE (Exp.lam U.uw Exp.tSyn (sh4_open n))))
   [(sg/nodes-of lam-code) (sg/cnodes-eq lam-code)]
   '(cnodes (encE (sh4_open n)))
   ['(+ 1 (+ n (+ n (+ n (+ n (+ n (+ n n))))))) '(sh4_open_enc_linear n)]})

;; One successor layer of the open derivation tree, in the context of d
;; older let binders: sh4_open_tree_succ's right side at D = sh4_syns d.
;; It adds 14 nodes per chain step recorded in its terms, 18 per older
;; binder recorded in its contexts and usages, and 81 fixed.
;; sh4_open_tree_succ's right side, over the older context D.
(def ^:private succ-tree
  '(DT.rApp (List.cons Exp Exp.tSyn D)
                (vzero (lenE (List.cons Exp Exp.tSyn D)))
                (List.cons U U.uw (vzero (lenE D))) U.uw
                (Exp.lam U.uw Exp.tSyn (sh4_open n))
                (Exp.snode (Exp.lbl 0) (Exp.var 0) (Exp.var 0))
                Exp.tSyn Exp.tSyn
                (DT.rLam (List.cons Exp Exp.tSyn D)
                  (vzero (lenE (List.cons Exp Exp.tSyn D))) U.uw Exp.tSyn
                  (sh4_open n) Exp.tSyn
                  (DT.fBase (List.cons Exp Exp.tSyn D) Exp.tSyn)
                  (sh4_open_tree n (List.cons Exp Exp.tSyn D)))
                (DT.rSnode (List.cons Exp Exp.tSyn D)
                  (vzero (lenE (List.cons Exp Exp.tSyn D)))
                  (List.cons U U.uw (vzero (lenE D)))
                  (List.cons U U.uw (vzero (lenE D)))
                  (Exp.lbl 0) (Exp.var 0) (Exp.var 0)
                  (DT.rConst (List.cons Exp Exp.tSyn D)
                    (vzero (lenE (List.cons Exp Exp.tSyn D)))
                    (Exp.lbl 0) Exp.tLbl)
                  (DT.rVar (List.cons Exp Exp.tSyn D)
                    (List.cons U U.uw (vzero (lenE D))) 0 Exp.tSyn U.uw)
                  (DT.rVar (List.cons Exp Exp.tSyn D)
                    (List.cons U U.uw (vzero (lenE D))) 0 Exp.tSyn U.uw))
                (DT.fBase (List.cons Exp Exp.tSyn D) Exp.tSyn)
                (DT.fBase (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn D))
                  Exp.tSyn)))

(let [C '(List.cons Exp Exp.tSyn (sh4_syns d))
      tree (clojure.walk/postwalk-replace {'D '(sh4_syns d)} succ-tree)
      facts (merge open-facts
              {(list 'cnodes (list 'encLE C))
               ['(Nat.succ d) '(sh4_encLE_head d)]
               (list 'cnodes (list 'encLE (list 'List.cons 'Exp 'Exp.tSyn C)))
               ['(Nat.succ (Nat.succ d)) '(sh4_encLE_head (Nat.succ d))]
               (list 'cnodes (list 'encLU (list 'vzero (list 'lenE C))))
               ['(Nat.succ d)
                '(Eq.trans (congrArg (fn [k :- Nat] (cnodes (encLU (vzero k))))
                             (congrArg Nat.succ (sh4_syns_len d)))
                           (sh4_encLU_vzero (Nat.succ d)))]
               '(cnodes (encLU (List.cons U U.uw (vzero (lenE (sh4_syns d))))))
               ['(Nat.succ d)
                '(Eq.trans (congrArg (fn [k :- Nat]
                                       (cnodes (encLU (List.cons U U.uw (vzero k)))))
                             (sh4_syns_len d))
                           (sh4_encLU_head d))]
               (list 'cnodes (list 'encDT (list 'sh4_open_tree 'n C)))
               ['(cnodes (encDT (sh4_open_tree n (sh4_syns (Nat.succ d))))) '(rfl)]})]
  (sg/size-eq! 'sh4_open_tree_nodes_step '[n :- Nat, d :- Nat] '[n d]
    '(sh4_open_tree (Nat.succ n) (sh4_syns d)) tree
    '(sh4_open_tree_succ n (sh4_syns d))
    '(+ (cnodes (encDT (sh4_open_tree n (sh4_syns (Nat.succ d)))))
        (+ (Nat.mul n 14) (+ (Nat.mul d 18) 81)))
    facts))

;; The closed form of the open tree's size, by induction on the chain
;; length, with the number of older binders general.  The step's arithmetic
;; is a lemma over plain variables: omega must not see the trees' sizes as
;; atoms (it compares atoms by unfolding them).
(thm sh4_open_nodes_arith [n :- Nat, d :- Nat, x :- Nat, x2 :- Nat,
                           P :- Nat, P2 :- Nat, Q :- Nat, Q1 :- Nat, Q2 :- Nat,
                           hs :- (Eq Nat x2 (+ x (+ (Nat.mul n 14) (+ (Nat.mul d 18) 81)))),
                           hi :- (Eq Nat x (+ (Nat.mul P 16)
                                            (+ (Nat.mul Q2 18)
                                               (+ (Nat.mul n 67) (+ (Nat.mul (Nat.succ d) 2) 7))))),
                           sq :- (Eq Nat P2 (+ P (+ n (+ n 1)))),
                           m1 :- (Eq Nat Q1 (+ Q d)),
                           m2 :- (Eq Nat Q2 (+ Q n))]
  (Eq Nat x2 (+ (Nat.mul P2 16)
                (+ (Nat.mul Q1 18)
                   (+ (Nat.mul (Nat.succ n) 67) (+ (Nat.mul d 2) 7)))))
  (omega))

(thm sh4_open_base_arith [d :- Nat, x :- Nat, Z0 :- Nat, Z1 :- Nat,
                          h0 :- (Eq Nat x (+ (Nat.mul d 2) 7)),
                          z0 :- (Eq Nat Z0 0), z1 :- (Eq Nat Z1 0)]
  (Eq Nat x (+ (Nat.mul Z0 16) (+ (Nat.mul Z1 18) (+ (Nat.mul 0 67) (+ (Nat.mul d 2) 7)))))
  (omega))

(thm sh4_open_tree_nodes [n :- Nat]
  (forall [d Nat]
    (Eq Nat (cnodes (encDT (sh4_open_tree n (sh4_syns d))))
      (+ (Nat.mul (Nat.mul n n) 16)
         (+ (Nat.mul (Nat.mul n d) 18)
            (+ (Nat.mul n 67) (+ (Nat.mul d 2) 7))))))
  (induction n)
  (intro d)
  (exact (sh4_open_base_arith d (cnodes (encDT (sh4_open_tree 0 (sh4_syns d))))
           (Nat.mul 0 0) (Nat.mul 0 d)
           (sh4_open_tree_nodes_zero d) (Nat.zero_mul 0) (Nat.zero_mul d)))
  (intro d)
  (exact (sh4_open_nodes_arith n d
           (cnodes (encDT (sh4_open_tree n (sh4_syns (Nat.succ d)))))
           (cnodes (encDT (sh4_open_tree (Nat.succ n) (sh4_syns d))))
           (Nat.mul n n) (Nat.mul (Nat.succ n) (Nat.succ n))
           (Nat.mul n d) (Nat.mul (Nat.succ n) d) (Nat.mul n (Nat.succ d))
           (sh4_open_tree_nodes_step n d) (ih_n (Nat.succ d))
           (sh4_sq_succ n) (Nat.succ_mul n d) (Nat.mul_succ n d))))

;; The derivation tree's node count, which F7 stores as fuel: nine nodes
;; per chain step, and the final variable.
(sg/ht-eq! 'sh4_open_tree_ht_step '[n :- Nat, D :- (List Exp)]
  '(sh4_open_tree (Nat.succ n) D) succ-tree '(sh4_open_tree_succ n D)
  '(+ (htDT (sh4_open_tree n (List.cons Exp Exp.tSyn D))) 9))

(thm sh4_ht_step_arith [n :- Nat, r :- Nat, r2 :- Nat,
                        hs :- (Eq Nat r2 (+ r 9)), ih :- (Eq Nat r (+ (Nat.mul n 9) 1))]
  (Eq Nat r2 (+ (Nat.mul (Nat.succ n) 9) 1))
  (omega))

(thm sh4_open_tree_ht [n :- Nat]
  (forall [D (List Exp)]
    (Eq Nat (htDT (sh4_open_tree n D)) (+ (Nat.mul n 9) 1)))
  (induction n)
  (intro D)
  (rfl)
  (intro D)
  (exact (sh4_ht_step_arith n (htDT (sh4_open_tree n (List.cons Exp Exp.tSyn D)))
           (htDT (sh4_open_tree (Nat.succ n) D))
           (sh4_open_tree_ht_step n D) (ih_n (List.cons Exp Exp.tSyn D)))))

;; The closed chain's tree applies the open chain's λ to the leaf.  Its size
;; is the open tree's at d = 0, plus 14 per chain step recorded in its two
;; terms, plus 42 fixed.
;; sh4_cn_tree's body.
(def ^:private cn-tree
  '(DT.rApp (List.nil Exp) (List.nil U) (List.nil U) U.uw
              (Exp.lam U.uw Exp.tSyn (sh4_open n)) (Exp.sleaf (Exp.lbl 0))
              Exp.tSyn Exp.tSyn
              (DT.rLam (List.nil Exp) (List.nil U) U.uw Exp.tSyn
                (sh4_open n) Exp.tSyn
                (DT.fBase (List.nil Exp) Exp.tSyn)
                (sh4_open_tree n (List.nil Exp)))
              (DT.rSleaf (List.nil Exp) (List.nil U) (Exp.lbl 0)
                (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl 0) Exp.tLbl))
              (DT.fBase (List.nil Exp) Exp.tSyn)
              (DT.fBase (List.cons Exp Exp.tSyn (List.nil Exp)) Exp.tSyn)))

(let [tree cn-tree
      facts (merge open-facts
              {'(cnodes (encDT (sh4_open_tree n (List.nil Exp))))
               ['(cnodes (encDT (sh4_open_tree n (sh4_syns 0)))) '(rfl)]})]
  (sg/size-eq! 'sh4_cn_tree_nodes '[n :- Nat] '[n]
    '(sh4_cn_tree n) tree '(rfl)
    '(+ (cnodes (encDT (sh4_open_tree n (sh4_syns 0))))
        (+ (Nat.mul n 14) 42))
    facts))

(sg/ht-eq! 'sh4_cn_tree_ht_step '[n :- Nat]
  '(sh4_cn_tree n) cn-tree '(rfl)
  '(+ (htDT (sh4_open_tree n (List.nil Exp))) 7))

(thm sh4_cn_ht_arith [n :- Nat, r :- Nat, r2 :- Nat,
                      hs :- (Eq Nat r2 (+ r 7)), ih :- (Eq Nat r (+ (Nat.mul n 9) 1))]
  (Eq Nat r2 (+ (Nat.mul n 9) 8))
  (omega))

(thm sh4_cn_tree_ht [n :- Nat]
  (Eq Nat (htDT (sh4_cn_tree n)) (+ (Nat.mul n 9) 8))
  (exact (sh4_cn_ht_arith n (htDT (sh4_open_tree n (List.nil Exp))) (htDT (sh4_cn_tree n))
           (sh4_cn_tree_ht_step n) (sh4_open_tree_ht n (List.nil Exp)))))

;; Proposition 4.11, second bullet, under F7: the certificate of c_n has
;; exactly 16 n² + 97 n + 73 nodes.  Besides the typing tree, the frame holds
;; the unary fuel (the two trees' node counts plus one), the budget 0, the
;; term (7n + 5 nodes), the type and the formation tree.
(let [fuel '(+ (+ (htDT (sh4_cn_tree n)) (htDT sh4_c0_form)) 1)]
  (sg/cert-size! 'prop411_cert_size '[n :- Nat] '[n]
    '(sh4_cn_cert n)
    '{m 0, t (sh4_cn n), A Exp.tSyn, T1 (sh4_cn_tree n), T2 sh4_c0_form}
    '(+ (Nat.mul (Nat.mul n n) 16) (+ (Nat.mul n 97) 73))
    {(list 'cnodes (list 'encN fuel)) [fuel (list 'cnodes_encNat fuel)]
     '(cnodes (encE (sh4_cn n)))
     ['(+ 5 (+ n (+ n (+ n (+ n (+ n (+ n n))))))) '(sh4_cn_enc_linear n)]
     '(cnodes (encDT (sh4_cn_tree n)))
     ['(+ (cnodes (encDT (sh4_open_tree n (sh4_syns 0)))) (+ (Nat.mul n 14) 42))
      '(sh4_cn_tree_nodes n)]}
    '[(have o (Eq Nat (cnodes (encDT (sh4_open_tree n (sh4_syns 0))))
                (+ (Nat.mul (Nat.mul n n) 16)
                   (+ (Nat.mul (Nat.mul n 0) 18)
                      (+ (Nat.mul n 67) (+ (Nat.mul 0 2) 7)))))
        (sh4_open_tree_nodes n 0))
      (have z (Eq Nat (Nat.mul n 0) 0) (Nat.mul_zero n))
      (have h1 (Eq Nat (htDT (sh4_cn_tree n)) (+ (Nat.mul n 9) 8))
        (sh4_cn_tree_ht n))
      (have h2 (Eq Nat (htDT sh4_c0_form) 1) (rfl))]))

;; R4 §4.10's table, under F7.  Each count is computed by the kernel from
;; the certificate itself, independently of the closed form above.
(doseq [[nm n v] '[[prop411_cert_2 2 331] [prop411_cert_4 4 717]
                   [prop411_cert_6 6 1231] [prop411_cert_8 8 1873]]]
  (a/prove-theorem nm (lv '[])
    (lv (list 'Eq 'Nat (list 'cnodes (list 'sh4_cn_cert n)) v))
    (lv '[(rfl)])))

;; "c_n's certificate grows quadratically."
(thm sh4_quad_arith [n :- Nat, c :- Nat, P :- Nat, P1 :- Nat,
                     hs :- (Eq Nat c (+ (Nat.mul P 16) (+ (Nat.mul n 97) 73))),
                     sq :- (Eq Nat P1 (+ P (+ n (+ n 1))))]
  (LE.le c (Nat.mul P1 97))
  (omega))

(thm prop411_cert_quadratic [n :- Nat]
  (LE.le (cnodes (sh4_cn_cert n))
         (Nat.mul (Nat.mul (+ n 1) (+ n 1)) 97))
  (exact (sh4_quad_arith n (cnodes (sh4_cn_cert n)) (Nat.mul n n) (Nat.mul (+ n 1) (+ n 1))
           (prop411_cert_size n) (sh4_sq_succ n))))

;; "A term's certificate can be exponentially smaller than the certificate
;; of its normal form."  Under F7 the quadratic overtakes the exponential
;; lower bound at n = 12: for every n ≥ 12, c_n's certificate is smaller
;; than every F7 certificate of its normal form (prop411_literal_lower), for
;; any budget and any typing and formation trees.
(thm sh4_dom_arith [k :- Nat, s :- Nat, s2 :- Nat, p :- Nat, p2 :- Nat,
                    hk :- (LE.le 1 k),
                    ih :- (LE.le (+ (Nat.mul s 16) (+ (Nat.mul k 97) 75)) p),
                    hp :- (Eq Nat p2 (+ p p)),
                    sq :- (Eq Nat s2 (+ s (+ k (+ k 1))))]
  (LE.le (+ (Nat.mul s2 16) (+ (Nat.mul (Nat.succ k) 97) 75)) p2)
  (omega))

(thm sh4_pow_dominates [n :- Nat]
  (LE.le (+ (Nat.mul (Nat.mul (+ n 12) (+ n 12)) 16) (+ (Nat.mul (+ n 12) 97) 75))
         (sh4_pow (+ n 12)))
  (induction n)
  (decide)
  (exact (sh4_dom_arith (+ n 12) (Nat.mul (+ n 12) (+ n 12))
           (Nat.mul (+ (Nat.succ n) 12) (+ (Nat.succ n) 12))
           (sh4_pow (+ n 12)) (sh4_pow (+ (Nat.succ n) 12))
           (Nat.le_add_left 1 (+ n 11))
           ih_n (sh4_pow_succ (+ n 12)) (sh4_sq_succ (+ n 12)))))

(thm sh4_gap_arith [k :- Nat, a :- Nat, b :- Nat, p :- Nat, s :- Nat,
                    hs :- (Eq Nat a (+ (Nat.mul s 16) (+ (Nat.mul k 97) 73))),
                    hl :- (LE.le (Nat.sub p 1) b),
                    hd :- (LE.le (+ (Nat.mul s 16) (+ (Nat.mul k 97) 75)) p)]
  (LT.lt a b)
  (omega))

(thm prop411_exponential_gap [n :- Nat, m :- Nat, T1 :- DT, T2 :- DT]
  (LT.lt (cnodes (sh4_cn_cert (+ n 12)))
         (cnodes (encCert m (codeTerm (sh4_bush (+ n 12))) Exp.tSyn T1 T2)))
  (exact (sh4_gap_arith (+ n 12) (cnodes (sh4_cn_cert (+ n 12)))
           (cnodes (encCert m (codeTerm (sh4_bush (+ n 12))) Exp.tSyn T1 T2))
           (sh4_pow (+ n 12)) (Nat.mul (+ n 12) (+ n 12))
           (prop411_cert_size (+ n 12))
           (prop411_literal_lower (+ n 12) m T1 T2)
           (sh4_pow_dominates n))))

;; The normal form's certificate.  The literal codeTerm c has one
;; conversion-free derivation: Snode at each internal node, Sleaf at each
;; leaf, Const for each label.  codeTree c is that derivation's tree, and its
;; F7 certificate is the one R4's table counts.
(lcert.formal.base/kdef! 'codeTree '(=> Code DT)
  '(fn [c :- Code]
     (Code.rec$1 (fn [_ :- Code] DT)
       (fn [l :- Nat]
         (DT.rSleaf (List.nil Exp) (List.nil U) (Exp.lbl l)
           (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl l) Exp.tLbl)))
       (fn [l :- Nat, a :- Code, b :- Code, ia :- DT, ib :- DT]
         (DT.rSnode (List.nil Exp) (List.nil U) (List.nil U) (List.nil U)
           (Exp.lbl l) (codeTerm a) (codeTerm b)
           (DT.rConst (List.nil Exp) (List.nil U) (Exp.lbl l) Exp.tLbl)
           ia ib))
       c)))

(lcert.formal.base/kdef! 'sh4_lit_cert '(=> Nat Code)
  '(fn [n :- Nat]
     (encCert 0 (codeTerm (sh4_bush n)) Exp.tSyn (codeTree (sh4_bush n)) sh4_c0_form)))

;; At each tabulated n: Check accepts the normal form's certificate, and the
;; kernel counts its nodes.  Values are computed by reduction when this
;; namespace loads and must match the table written here (nil: not yet
;; recorded).
(def ^:private lit-table '{2 152, 4 780, 6 3772, 8 17660})
(doseq [n (sort (keys lit-table))]
  (let [v (sg/kernel-nat (list 'cnodes (list 'sh4_lit_cert n)))]
    (when-let [want (lit-table n)]
      (assert (= v want) (str "sh4_lit_cert " n ": kernel count " v ", table " want)))
    (println "  sh4_lit_cert" n "has" v "nodes")
    (a/prove-theorem (symbol (str "prop411_lit_cert_" n)) (lv '[])
      (lv (list 'And
                (list 'Eq 'Bool (list 'Check 'decCert (list 'sh4_lit_cert n) '(encE Exp.tSyn)) 'Bool.true)
                (list 'Eq 'Nat (list 'cnodes (list 'sh4_lit_cert n)) v)))
      (lv [(list 'exact (list 'And.intro
                         (list 'check_cert_ok 0 (list 'codeTerm (list 'sh4_bush n)) 'Exp.tSyn
                               (list 'codeTree (list 'sh4_bush n)) 'sh4_c0_form '(rfl) '(rfl) '(rfl))
                         '(rfl)))]))))
