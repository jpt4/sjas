(ns lcert.verified
  "The Ansatz-verified kernel of λᶜᵉʳᵗ₀ (ADR-0005; R4-metatheory.md §7).

  Loading this namespace runs Ansatz's elaborator and its Lean-4-compatible
  kernel on every definition and theorem below; a proof the kernel rejects
  aborts the load.  Nothing here downloads anything: the environment is
  Ansatz's bundled Lean Init (a/load-init!), and the suite sets
  ANSATZ_OFFLINE=1.

  Codes as a kernel type.  Ansatz does not accept an inductive whose leaf
  constructor has fields, so a code is encoded over a field-less terminator:
    CT := tend | cell (lbl : Nat) (inner : Bool) (x y : CT)
  A leaf [:sl l] is (cell l false tend tend); a node [:sn l a b] is
  (cell l true a b).  Labels are their positions in lcert.syntax/labels.  At
  runtime tend is nil and a cell is the vector [lbl inner x y].

  Verified here:
    ctxlen_le_nodes   ∀t. ctxlen t ≤ nodes t        (a context of m entries
                                                     has at least m nodes, E3)
    fstlen_le_nodes   ∀t. fstlen t ≤ nodes t
    strict_overhead   ∀l j p. budget (cell l true j p) < nodes (cell l true j p)
                      — Lemma 2.7 for every derivation-shaped code
    h1_budget         m₁ < k₁ ∧ m₂ < k₂ ∧ k₁ + k₂ ≤ n → m₁ + m₂ < n
                      — the budget descent of the H₁ case of Lemma 3.6
    not_cert_nodes    the draft's §2.7 certificate of `not` has 35 nodes
    not_cert_budget   … and declares budget 0

  How the proofs are written, and why.  Ansatz 0.2.115 generates equation
  lemmas cleanly for ctxlen, fstlen and budget, but for nodes — two recursive
  results combined by Nat.add — it emits lemmas stated through the raw
  structural-recursion encoding, which omega cannot use.  So each case is
  closed by `change` to the definitional unfolding (checked by the kernel)
  followed by omega; Boolean case splits are done in separate lemmas where the
  Boolean is a parameter; and inductions close the base case first and then
  cite the step lemma.  Found by the spikes of 2026-09-26 (see the log)."
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as nm]
            [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.typing :as t]))

(a/load-init!)

;; ---------------------------------------------------------------------------
;; Definitions.  Each compiles to an ordinary Clojure function of the same
;; name in this namespace.

(a/inductive CT [] (tend) (cell [lbl Nat] [inner Bool] [x CT] [y CT]))

;; 1 for an internal node, 0 for a leaf.  Keeping this `if` out of the
;; recursive definitions keeps their equation lemmas usable.
(a/defn b2n [b :- Bool] Nat (if b 1 0))

;; One more entry on a context spine, or none at a leaf.
(a/defn ctxstep [b :- Bool, n :- Nat] Nat (if b (+ 1 n) 0))

;; Internal nodes.
(a/defn nodes [t :- CT] Nat
  (match t
    [tend 0]
    [(cell lbl inner x y) (+ (b2n inner) (+ (nodes x) (nodes y)))]))

;; Length of the first-child spine of internal nodes: for a context, its
;; number of entries.
(a/defn ctxlen [t :- CT] Nat
  (match t
    [tend 0]
    [(cell lbl inner x y) (ctxstep inner (ctxlen x))]))

;; The context length of a node's first child.
(a/defn fstlen [t :- CT] Nat
  (match t
    [tend 0]
    [(cell lbl inner x y) (ctxlen x)]))

;; A derivation [:sn rule J prems] declares the context of its judgment J,
;; whose first child is that context: budget is fstlen of the first child.
(a/defn budget [t :- CT] Nat
  (match t
    [tend 0]
    [(cell lbl inner x y) (fstlen x)]))

;; ---------------------------------------------------------------------------
;; Theorems.

(a/theorem ctxlen_le_nodes_tend []
  (<= (ctxlen CT.tend) (nodes CT.tend))
  (decide))

(a/theorem ctxlen_le_nodes_leaf [l :- Nat, x :- CT, y :- CT]
  (<= (ctxlen (CT.cell l false x y)) (nodes (CT.cell l false x y)))
  (change (<= 0 (+ 0 (+ (nodes x) (nodes y)))))
  (omega))

(a/theorem ctxlen_le_nodes_node [l :- Nat, x :- CT, y :- CT, ih :- (<= (ctxlen x) (nodes x))]
  (<= (ctxlen (CT.cell l true x y)) (nodes (CT.cell l true x y)))
  (change (<= (+ 1 (ctxlen x)) (+ 1 (+ (nodes x) (nodes y)))))
  (omega))

(a/theorem ctxlen_le_nodes_cell [l :- Nat, i :- Bool, x :- CT, y :- CT, ih :- (<= (ctxlen x) (nodes x))]
  (<= (ctxlen (CT.cell l i x y)) (nodes (CT.cell l i x y)))
  (cases i)
  (exact (ctxlen_le_nodes_leaf l x y))
  (exact (ctxlen_le_nodes_node l x y ih)))

(a/theorem ctxlen_le_nodes [t :- CT]
  (<= (ctxlen t) (nodes t))
  (induction t)
  (all_goals (try (exact ctxlen_le_nodes_tend)))
  (exact (ctxlen_le_nodes_cell lbl inner x y ih_x)))

(a/theorem fstlen_le_nodes_cell [l :- Nat, i :- Bool, x :- CT, y :- CT]
  (<= (fstlen (CT.cell l i x y)) (nodes (CT.cell l i x y)))
  (change (<= (ctxlen x) (+ (b2n i) (+ (nodes x) (nodes y)))))
  (have hc (<= (ctxlen x) (nodes x)) (ctxlen_le_nodes x))
  (omega))

(a/theorem fstlen_le_nodes [t :- CT]
  (<= (fstlen t) (nodes t))
  (induction t)
  (all_goals (try (decide)))
  (exact (fstlen_le_nodes_cell lbl inner x y)))

;; Lemma 2.7: a derivation code declares fewer tokens than it has nodes.
(a/theorem strict_overhead [l :- Nat, j :- CT, p :- CT]
  (< (budget (CT.cell l true j p)) (nodes (CT.cell l true j p)))
  (change (< (fstlen j) (+ 1 (+ (nodes j) (nodes p)))))
  (have h (<= (fstlen j) (nodes j)) (fstlen_le_nodes j))
  (omega))

;; The budget descent of the H₁ case of Lemma 3.6.
(a/theorem h1_budget [m1 :- Nat, k1 :- Nat, m2 :- Nat, k2 :- Nat, n :- Nat,
                      h1 :- (< m1 k1), h2 :- (< m2 k2), h3 :- (<= (+ k1 k2) n)]
  (< (+ m1 m2) n)
  (omega))

;; ---------------------------------------------------------------------------
;; Codes as CT values, and the draft's example computed by the kernel.

(defn code->ct
  "A code value as a runtime CT value: nil for tend, [lbl inner x y] for a cell."
  [c]
  (case (first c)
    :sl [(s/label-index (second c)) false nil nil]
    :sn [(s/label-index (second c)) true (code->ct (nth c 2)) (code->ct (nth c 3))]))

(defn- ct-form
  "A code as a CT constructor expression, for use in a theorem statement."
  [c]
  (case (first c)
    :sl (list 'CT.cell (s/label-index (second c)) false 'CT.tend 'CT.tend)
    :sn (list 'CT.cell (s/label-index (second c)) true (ct-form (nth c 2)) (ct-form (nth c 3)))))

(def not-certificate
  "The code of the draft's §2.7 derivation of `not`, built by the type checker
  (lcert.typing-test shows it is exactly the draft's)."
  (e/enc-deriv (t/check-top 0 (s/parse-term [] '(fn [x w Bool] (if x ff tt))))))

;; Kernel computation (rfl) of the draft's two numbers.
(eval (list 'ansatz.core/theorem 'not_cert_nodes []
            (list '= (list 'nodes (ct-form not-certificate)) 35) '(rfl)))
(eval (list 'ansatz.core/theorem 'not_cert_budget []
            (list '= (list 'budget (ct-form not-certificate)) 0) '(rfl)))

(def theorem-names
  ["ctxlen_le_nodes" "fstlen_le_nodes" "strict_overhead" "h1_budget"
   "not_cert_nodes" "not_cert_budget"])

(defn declared?
  "Is the named declaration in the Ansatz kernel environment?"
  [s]
  (some? (env/lookup (a/env) (nm/from-string s))))
