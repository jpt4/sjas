(ns lcert.examples
  "Programs that make results of R4-metatheory.md executable.

    not-form              the draft's §2.7 example
    h-from-h1             Proposition 4.10 (from review R4-02): H° from H₁,
                          at the constant budget of one fixed certificate
    roll-form, out-form   the destructor of §4.7 (from review R4-03), definable
                          from itR and dependent pairs
    parse-form            a typed parser threading a supply of tokens through a
                          code received at runtime (review RR2-10)
    bounded-con           Proposition 4.9: bounded code consistency, by case
                          analysis, with no tokens

  Surface forms are built as Clojure data, so that repeated pieces (the types
  V and K, the term roll) can be spliced in rather than abbreviated: the
  language itself has no top-level definitions."
  (:require [clojure.walk :as walk]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]
            [lcert.check :as c]
            [lcert.kernel :as k]))

(def not-form '(fn [x w Bool] (if x ff tt)))

;; ---------------------------------------------------------------------------
;; Proposition 4.10: H° from H₁.

(def zero-lolli-zero
  "⊢ λ(x :₁ 0). x : 0 ⊸ 0 — the fixed certificate's program."
  '(fn [x 1 Void] x))

(def h-type
  "H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0, in abstract syntax."
  [:Pi 1 [:R] [:Pi 1 [:T [:chk [:print [:var 0]] s/c-bot-term]] [:Empty]]])

(defn h-from-h1
  "The term λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). H₁ r lit_v c⊥ e ⋆, where v
  certifies 0 ⊸ 0.  Its budget K is v's size: a constant.  The refutation
  certificate r is H₁'s certificate of the type 0; v certifies its negation."
  []
  (let [code (e/enc-deriv (t/check-top 0 (s/parse-term [] zero-lolli-zero)))
        kk (k/nodes code)
        lit (s/shift (c/certificate-literal code) 2)]  ; under the binders r, e
    {:budget kk
     :term [:lam 1 [:R]
            [:lam 1 [:T [:chk [:print [:var 0]] s/c-bot-term]]
             [:h1 [:var 1] lit s/c-bot-term [:var 0] [:star]]]]}))

;; ---------------------------------------------------------------------------
;; The definable destructor for R (R4-metatheory.md §4.7).
;;
;;   K := ◇ ⊗ (R ⊗ R)                          a node's token and children
;;   V := Σ(b :ω Bool). Σ(a :ω Lbl). (T(b) ⊸ K)  a leaf (b = ff) or a node
;;
;; roll : V ⊸ R rebuilds a tree from its view; out : R ⊸ V computes the view,
;; by iteration, rolling the children back up as it goes.

(def K-type '(tensor Dia (tensor R R)))

(def V-type (walk/postwalk-replace {'K K-type}
                                   '(Sigma [b w Bool] (Sigma [a w Lbl] (-o (T b) K)))))

(defn- splice [form] (walk/postwalk-replace {'K K-type 'V V-type} form))

(defn- view-case
  "Eliminate the view in `p` (a V): on a node, `node-body` with d, u, v bound
  to the token and the children; on a leaf, (leaf a).  Result type R."
  [p node-body]
  (splice
   (list 'let-pair 'R '[b q] p
         (list 'let-pair 'R '[a f] 'q
               (list (list 'elim-bool '[x (-o (-o (T x) K) R)] 'b
                           (list 'fn '[g 1 (-o (T tt) K)]
                                 (list 'let-pair 'R '[d uv] '(g star)
                                       (list 'let-pair 'R '[u v] 'uv node-body)))
                           '(fn [g 1 (-o (T ff) K)] (leaf a)))
                     'f)))))

(def roll-form
  "roll : V ⊸ R."
  (splice (list 'fn '[p 1 V] (view-case 'p '(node d a u v)))))

(defn- leaf-view [a]
  (splice (list 'pair 'V 'ff
                (list 'pair '(Sigma [a2 w Lbl] (-o (T ff) K)) a
                      '(fn [e 1 (T ff)] (abort K e))))))

(defn- node-view [d a u v]
  (splice (list 'pair 'V 'tt
                (list 'pair '(Sigma [a2 w Lbl] (-o (T tt) K)) a
                      (list 'fn '[e 1 (T tt)]
                            (list 'pair 'K d (list 'pair '(tensor R R) u v)))))))

(def out-form
  "out : R ⊸ V, by itR: each node's token goes into its view, and the
  children's views are rolled back into trees."
  (splice
   (list 'fn '[r 1 R]
         (list 'itr 'V
               (list 'fn '[l w Lbl] (leaf-view 'l))
               (list 'fn '[d 1 Dia]
                     (list 'fn '[l w Lbl]
                           (list 'fn '[u 1 V]
                                 (list 'fn '[v 1 V]
                                       (node-view 'd 'l (list roll-form 'u) (list roll-form 'v))))))
               'r))))

(defn roll-out "The program (roll (out r))." [r] (list roll-form (list out-form r)))

(defn left-child "The program that takes a node apart and returns its left child." [r]
  (view-case (list out-form r) 'u))

;; ---------------------------------------------------------------------------
;; Parsing a code into a certificate (review RR2-10).
;;
;; parse : Π(c :ω Syn). R ⊸ R ⊗ R, by recursion on the code.  Its argument is
;; a *supply*: a certificate used only for its tokens, read along its right
;; spine.  A leaf of the code costs nothing.  A node takes the supply apart
;; with `out`, spends the top token on the node, discards the supply's left
;; child, and threads the right spine through the two subcodes, left first.
;; The result is the certificate and the unused supply.
;;
;; Adequacy, proved outside the calculus by induction on c: if the supply's
;; right spine has at least nodes(c) nodes, the certificate prints back to c
;; and exactly nodes(c) spine nodes are used.  With too short a supply, an
;; exhausted node becomes a leaf, so print reveals the failure.

(def parse-form
  (splice
   (list 'fn '[c w Syn]
         (list 'rec-syn '[z (-o R (tensor R R))]
               '[a] '(fn [s0 1 R] (pair (tensor R R) (leaf a) s0))
               '[a c1 c2 y1 y2]
               (list 'fn '[s0 1 R]
                     (list 'let-pair '(tensor R R) '[b q] (list out-form 's0)
                           (list 'let-pair '(tensor R R) '[a2 f] 'q
                                 (list (list 'elim-bool '[x (-o (-o (T x) K) (tensor R R))] 'b
                                             '(fn [g 1 (-o (T tt) K)]
                                                (let-pair (tensor R R) [d uv] (g star)
                                                  (let-pair (tensor R R) [u rest] uv
                                                    (let-pair (tensor R R) [t1 s1] (y1 rest)
                                                      (let-pair (tensor R R) [t2 s2] (y2 s1)
                                                        (pair (tensor R R) (node d a t1 t2) s2))))))
                                             '(fn [g 1 (-o (T ff) K)] (pair (tensor R R) (leaf a) (leaf a2))))
                                       'f))))
               'c))))

(defn parse-then
  "The program: parse `code-form` with `supply-form`, bind the certificate to
  t and the unused supply to rest, and return `body`, of type C."
  [code-form supply-form C body]
  (list 'let-pair C '[t rest] (list parse-form code-form supply-form) body))

;; ---------------------------------------------------------------------------
;; Proposition 4.9 at depth k: Π(c :ω Syn). T(depthLeq_k c) ⊸ T(chk′ c c⊥) ⊸ 0.

(defn depth-leq
  "A closed Syn → Bool deciding depth ≤ k, unrolled at the meta level so that
  no recursive result is used inside another recursion."
  [k]
  (if (zero? k)
    '(fn [c w Syn] (rec-syn [x Bool] [a] tt [a c1 c2 y1 y2] ff c))
    (list 'fn '[c w Syn]
          (list 'rec-syn '[x Bool] '[a] 'tt
                ['a 'c1 'c2 'y1 'y2]
                (list 'and (list (depth-leq (dec k)) 'c1) (list (depth-leq (dec k)) 'c2))
                'c))))

(defn bounded-con-type-form [k]
  (list 'Pi '[c w Syn] (list '-o (list 'T (list (depth-leq k) 'c)) '(T (chk c c-bot)) 'Void)))

(defn bounded-con-type [k] (s/parse-type [] (bounded-con-type-form k)))

(defn bounded-con
  "The proof of Proposition 4.9 at k = 0: case analysis on the code, and on
  the label of a leaf, until every case is a closed code on which chk′
  computes: a leaf of any label is not a refutation certificate."
  [k]
  (when-not (zero? k)
    (throw (ex-info "only depth 0 is generated here" {:k k})))
  (let [dl (depth-leq 0)
        leaf-branch (fn [l]
                      (list 'fn [(symbol "h") 1 (list 'T (list dl (list 'sleaf l)))]
                            (list 'fn '[e 1 (T (chk (sleaf LBL) c-bot))] 'e)))
        branches (into {} (for [l s/labels]
                            [l (walk/postwalk-replace {'LBL l} (leaf-branch l))]))]
    (list 'fn '[c w Syn]
          (list 'rec-syn
                ['x (list '-o (list 'T (list dl 'x)) '(T (chk x c-bot)) 'Void)]
                '[a]
                (list 'case-lbl
                      ['z (list '-o (list 'T (list dl '(sleaf z))) '(T (chk (sleaf z) c-bot)) 'Void)]
                      'a branches)
                '[a c1 c2 y1 y2]
                (list 'fn ['h 1 (list 'T (list dl '(snode a c1 c2)))]
                      '(fn [e 1 (T (chk (snode a c1 c2) c-bot))] (abort Void h)))
                'c))))
