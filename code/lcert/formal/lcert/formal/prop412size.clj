(ns lcert.formal.prop412size
  "A06, reopened 2026-10-15: the exact F7 size of Proposition 4.12(1)'s
  certificate family.

  `sh4_doubler_cert n` (prop412poly) is the canonical F7 certificate of the
  recN doubler applied to the unary numeral n.  Its node count is
  s(n) + 3n + C, where s(n) is the numeral's proof tree (n(n − 1)/2 + 4n + 4
  nodes): the numeral appears once more in the tree, once in the term, and
  the fuel adds the tree's height, which grows by one per successor.  So

    2 · cnodes (sh4_doubler_cert n) = n² + 13 n + K,

  polynomial in n, because the numeral is unary; the exact closed form is
  the polynomial statement.  The method is sizegen's.
  The fixed parts (the recursor's motive, base and step, and their
  derivations) are closed codes: their sizes are computed by the kernel."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm lv]]
            [lcert.formal.sizegen :as sg]
            [lcert.formal.prop412poly]))

;; Constants of the closed form, computed by kernel reduction at load and
;; checked against the values written here (nil: not yet recorded).
(def ^:private recorded '{KD 290, KT 23, H0 26, C 362, K 732})
(defn- kernel-const [nm form]
  (let [v (sg/kernel-nat form)]
    (when-let [want (recorded nm)]
      (assert (= v want) (str nm ": kernel value " v ", recorded " want)))
    (println " " nm "=" v)
    v))

;; The numeral's proof tree: s(n) with 2 s(n) = n² + 7n + 8.  The step's
;; arithmetic is a lemma over plain variables, so omega never unfolds a size.
(thm sh4_num_nodes_arith [n :- Nat, x :- Nat, x2 :- Nat, P :- Nat, P1 :- Nat,
                          hs :- (Eq Nat x2 (+ x (+ n 4))),
                          ih :- (Eq Nat (+ x x) (+ P (+ (Nat.mul n 7) 8))),
                          sq :- (Eq Nat P1 (+ P (+ n (+ n 1))))]
  (Eq Nat (+ x2 x2) (+ P1 (+ (Nat.mul (Nat.succ n) 7) 8)))
  (omega))

(thm sh4_num_tree_nodes [n :- Nat]
  (Eq Nat (+ (cnodes (encDT (sh4_num_tree n))) (cnodes (encDT (sh4_num_tree n))))
          (+ (Nat.mul n n) (+ (Nat.mul n 7) 8)))
  (induction n)
  (rfl)
  (exact (sh4_num_nodes_arith n (cnodes (encDT (sh4_num_tree n)))
           (cnodes (encDT (sh4_num_tree (Nat.succ n))))
           (Nat.mul n n) (Nat.mul (Nat.succ n) (Nat.succ n))
           (sh4_num_tree_size_step n) ih_n (sh4_sq_succ n))))

;; The doubler's term and tree height grow by one per successor.
(let [KT (kernel-const 'KT '(cnodes (encE (sh4_doubler (sh4_num 0)))))
      H0 (kernel-const 'H0 '(htDT (sh4_doubler_tree 0)))]
  (a/prove-theorem 'sh4_doubler_term_size (lv '[n :- Nat])
    (lv (list 'Eq 'Nat '(cnodes (encE (sh4_doubler (sh4_num n)))) (list '+ 'n KT)))
    (lv ['(induction n)
         '(rfl)
         (list 'have 'hs '(Eq Nat (cnodes (encE (sh4_doubler (sh4_num (Nat.succ n)))))
                                (+ (cnodes (encE (sh4_doubler (sh4_num n)))) 1))
               '(sh4_doubler_term_size_step n))
         '(rewrite [hs])
         '(rewrite [ih_n])
         '(try (clear hs))
         '(try (clear ih_n))
         '(omega)]))
  (a/prove-theorem 'sh4_doubler_tree_ht (lv '[n :- Nat])
    (lv (list 'Eq 'Nat '(htDT (sh4_doubler_tree n)) (list '+ 'n H0)))
    (lv ['(induction n)
         '(rfl)
         (list 'have 'hs '(Eq Nat (htDT (sh4_doubler_tree (Nat.succ n)))
                                (+ (htDT (sh4_doubler_tree n)) 1))
               '(sh4_doubler_tree_ht_step n))
         '(rewrite [hs])
         '(rewrite [ih_n])
         '(try (clear hs))
         '(try (clear ih_n))
         '(omega)])))

;; The doubler's derivation tree: one rRecN node over the numeral, its proof
;; tree, and closed subtrees for the motive, base and step.  This is the
;; body prop412poly's sh4_doubler_tree was defined with.
(def ^:private doubler-tree
  (let [tree (vec lcert.formal.prop412poly/doubler-tree-form)]
    (apply list (assoc tree 8 '(sh4_num n) 9 '(sh4_num_tree n)))))

(let [KD (kernel-const 'KD '(Nat.sub (cnodes (encDT (sh4_doubler_tree 0))) 4))]
  (sg/size-eq! 'sh4_doubler_tree_nodes '[n :- Nat] '[n]
    '(sh4_doubler_tree n) doubler-tree '(rfl)
    (list '+ '(cnodes (encDT (sh4_num_tree n))) (list '+ 'n KD))
    {'(cnodes (encE (sh4_num n))) ['n '(sh4_num_enc_linear n)]}))

;; The certificate: s(n) + 3n + C nodes.
(let [C (kernel-const 'C '(Nat.sub (cnodes (sh4_doubler_cert 0)) 4))
      fuel '(+ (+ (htDT (sh4_doubler_tree n)) (htDT sh4_doubler_form_tree)) 1)
      KT (sg/kernel-nat '(cnodes (encE (sh4_doubler (sh4_num 0)))))
      KD (sg/kernel-nat '(Nat.sub (cnodes (encDT (sh4_doubler_tree 0))) 4))
      H0 (sg/kernel-nat '(htDT (sh4_doubler_tree 0)))
      Hf (sg/kernel-nat '(htDT sh4_doubler_form_tree))]
  (sg/cert-size! 'sh4_doubler_cert_nodes '[n :- Nat] '[n]
    '(sh4_doubler_cert n)
    '{m 0, t (sh4_doubler (sh4_num n)), A (sh4_M), T1 (sh4_doubler_tree n),
      T2 sh4_doubler_form_tree}
    (list '+ '(cnodes (encDT (sh4_num_tree n))) (list '+ '(Nat.mul n 3) C))
    {(list 'cnodes (list 'encN fuel)) [fuel (list 'cnodes_encNat fuel)]
     '(cnodes (encE (sh4_doubler (sh4_num n)))) [(list '+ 'n KT) '(sh4_doubler_term_size n)]
     '(cnodes (encDT (sh4_doubler_tree n)))
     [(list '+ '(cnodes (encDT (sh4_num_tree n))) (list '+ 'n KD)) '(sh4_doubler_tree_nodes n)]}
    [(list 'have 'h1 (list 'Eq 'Nat '(htDT (sh4_doubler_tree n)) (list '+ 'n H0))
           '(sh4_doubler_tree_ht n))
     (list 'have 'h2 (list 'Eq 'Nat '(htDT sh4_doubler_form_tree) Hf) '(rfl))]))

;; Proposition 4.12(1) under F7: 2 · |certificate| = n² + 13n + K.
(let [K (kernel-const 'K '(+ (cnodes (sh4_doubler_cert 0)) (cnodes (sh4_doubler_cert 0))))
      C (sg/kernel-nat '(Nat.sub (cnodes (sh4_doubler_cert 0)) 4))]
  (a/prove-theorem 'sh4_dcert_arith
    (lv ['n :- 'Nat 'c :- 'Nat 's :- 'Nat 'P :- 'Nat
         'hc :- (list 'Eq 'Nat 'c (list '+ 's (list '+ '(Nat.mul n 3) C)))
         'hs :- '(Eq Nat (+ s s) (+ P (+ (Nat.mul n 7) 8)))])
    (lv (list 'Eq 'Nat '(+ c c) (list '+ 'P (list '+ '(Nat.mul n 13) K))))
    (lv '[(omega)]))
  (a/prove-theorem 'prop412_cert_size (lv '[n :- Nat])
    (lv (list 'Eq 'Nat '(+ (cnodes (sh4_doubler_cert n)) (cnodes (sh4_doubler_cert n)))
              (list '+ '(Nat.mul n n) (list '+ '(Nat.mul n 13) K))))
    (lv '[(exact (sh4_dcert_arith n (cnodes (sh4_doubler_cert n)) (cnodes (encDT (sh4_num_tree n)))
                   (Nat.mul n n) (sh4_doubler_cert_nodes n) (sh4_num_tree_nodes n)))])))

;; R4 §4.10's three counts, by kernel evaluation of the certificates.
(def ^:private table '{4 400, 8 450, 12 516})
(doseq [n (sort (keys table))]
  (let [v (sg/kernel-nat (list 'cnodes (list 'sh4_doubler_cert n)))]
    (when-let [want (table n)]
      (assert (= v want) (str "sh4_doubler_cert " n ": kernel count " v ", table " want)))
    (println "  sh4_doubler_cert" n "has" v "nodes")
    (a/prove-theorem (symbol (str "prop412_cert_" n)) (lv '[])
      (lv (list 'Eq 'Nat (list 'cnodes (list 'sh4_doubler_cert n)) v))
      (lv '[(rfl)]))))
