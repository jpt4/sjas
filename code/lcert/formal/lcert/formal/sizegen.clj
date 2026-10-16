(ns lcert.formal.sizegen
  "Generators for exact F7 certificate sizes (A06 and A10, reopened
  2026-10-15).  No kernel constants are declared here: these functions write
  proof scripts, and the kernel checks every equation they produce.

  A certificate's node count is a sum over its codes.  Normalizing a whole
  derivation tree's sum in one tactic call did not finish (LOG,
  2026-10-15, A06 and A10).  The method here keeps every tactic goal a
  linear identity over a few atoms:
  - `expand` writes one layer of a derivation tree as its F7 code, using the
    encoder's own constructor table, so the code is F7's by construction;
  - certcanon's `nodes-of` reads off the node count of that code as a sum of
    atoms `(cnodes X)`, and `cnodes-eq` proves the reading;
  - each atom gets a fact `(cnodes X) = v`: a size lemma, supplied by the
    caller, or, for a closed X, its value computed by kernel reduction and
    re-checked by `rfl`;
  - omega closes the goal from those facts."
  (:require [ansatz.core :as a]
            [clojure.walk]
            [ansatz.kernel.expr :as e]
            [ansatz.kernel.reduce :as red]
            [ansatz.surface.elaborate :as el]
            [lcert.formal.base :refer [lv]]
            [lcert.formal.certenc]
            [lcert.formal.certcanon]))

;; The encoder and canonical-format generators, so that the expansion below
;; is the encoder's own definition, not a copy.
(def dt-ctors @#'lcert.formal.certenc/dt-ctors)
(def ty-info @@#'lcert.formal.certenc/ty-info)
(def fuel-info @@#'lcert.formal.certenc/fuel-info)
(def chain-enc @#'lcert.formal.certenc/chain-enc)
(def nodes-of @#'lcert.formal.certcanon/nodes-of)
(def cnodes-eq @#'lcert.formal.certcanon/cnodes-eq)
(def cert-expr @#'lcert.formal.certcanon/cert-expr)

(defn expand
  "The F7 code of a derivation-tree term, as a surface form.  A constructor
  application becomes F7's constructor chain over its fields' codes, with
  DT fields expanded in turn; any other DT term t stays as (encDT t)."
  [term]
  (if-let [[i fields] (when (seq? term)
                        (first (keep-indexed
                                 (fn [i [nm fields]]
                                   (when (= (first term) (symbol (str "DT." nm)))
                                     [i fields]))
                                 dt-ctors)))]
    (chain-enc i (map (fn [[_ T] arg]
                        (if (= T 'DT)
                          (expand arg)
                          (list (first (or (ty-info T) (fuel-info T))) arg)))
                      fields (rest term)))
    (list 'encDT term)))

(defn atoms
  "The distinct atoms (cnodes X) of an arithmetic form, in first-seen order."
  [form]
  (distinct (filter #(and (seq? %) (= 'cnodes (first %)))
                    (tree-seq seq? seq form))))

(defn- kernel-le?
  "Does the Java kernel type checker reduce (Nat.ble form k) to true?"
  [form k]
  (let [env @a/ansatz-env
        tc (ansatz.kernel.TypeChecker. env)]
    (.isDefEq tc (el/elaborate env (lv (list 'Nat.ble form k)))
                 (el/elaborate env 'Bool.true))))

(defn kernel-nat
  "The value of a closed Nat-valued surface form.  First by the reducer's
  weak head normal form, read through literals and Nat.succ; when that
  exceeds its depth limit (large certificates), by binary search on
  Nat.ble decided by the Java kernel type checker.  Used only to choose the
  value v of a fact (cnodes X) = v; the fact itself is checked by rfl."
  [form]
  (let [env @a/ansatz-env
        by-whnf (fn []
                  (loop [x (el/elaborate env (lv form)) acc 0]
                    (let [w (red/whnf env x)]
                      (cond
                        (e/lit-nat? w) (+ acc (long (e/lit-nat-val w)))
                        :else
                        (let [[head args] (e/get-app-fn-args w)
                              hname (when (e/const? head) (str (e/const-name head)))]
                          (cond
                            (and (= "Nat.succ" hname) (= 1 (count args))) (recur (first args) (inc acc))
                            (and (= "Nat.zero" hname) (empty? args)) acc
                            :else (throw (ex-info "kernel-nat: not a numeral"
                                                  {:form form :whnf (str w)}))))))))]
    (try (by-whnf)
         (catch clojure.lang.ExceptionInfo ex
           (if (= :depth-exceeded (:cause (ex-data ex)))
             (let [hi (loop [h 1] (if (kernel-le? form h) h (recur (* 2 h))))]
               (loop [lo 0 hi hi]            ; invariant: form ≤ hi, not form ≤ lo - 1
                 (if (>= lo hi) hi
                   (let [mid (quot (+ lo hi) 2)]
                     (if (kernel-le? form mid) (recur lo mid) (recur (inc mid) hi))))))
             (throw ex))))))

(defn closed?
  "True when the form mentions none of the generator's variables."
  [vars form]
  (not-any? (set vars) (flatten (if (seq? form) form [form]))))

(defn fact-haves
  "have-forms for the facts of every atom in sum.  facts maps an atom to
  [value proof]; a closed atom (no symbol from vars) without a supplied fact
  gets its kernel-computed value and the proof (rfl).  Any other atom without
  a fact is an error naming it, raised before the kernel is called."
  ([nm vars sum facts] (fact-haves nm vars sum facts #{}))
  ([nm vars sum facts opaque-ok]
  (let [ats (remove opaque-ok (atoms sum))
        fact (fn [at] (or (facts at)
                          (when (closed? vars at) [(kernel-nat at) '(rfl)])))
        fs (map fact ats)
        missing (keep (fn [[at f]] (when-not f at)) (map vector ats fs))]
    (when (seq missing)
      (throw (ex-info (str nm ": atoms without a size fact") {:atoms (vec missing)})))
    (map-indexed (fn [i [at [v p]]]
                   (list 'have (symbol (str "f_" i)) (list 'Eq 'Nat at v) p))
                 (map vector ats fs)))))

(defn- solve-tactics
  "Tactics that close goal (Eq Nat lhs-atom rhs), given q_n : lhs-atom = sum.
  omega interns atoms up to whnf and compares them pairwise by definitional
  equality; atoms such as cnodes (encDT (sh4_open_tree n D)) unfold to large
  stuck terms, and that comparison did not finish (2026-10-15).  So:
  - the goal is rewritten by q_n, then by each fact and each extra have
    (h : atom = value), one rewrite per tactic, inside try: an atom already
    gone needs no rewrite;
  - a fact whose value mentions another fact's atom is rewritten first;
  - any atom of rhs without a fact (an opaque size, such as the recursive
    subtree's) needs no fact and is generalized to a fresh variable;
  so omega sees only variables.  omega also reads every hypothesis in the
  context, so the facts, q_n and the generalization equations are cleared
  before it runs."
  [nm vars sum facts extra rhs]
  (let [opaque (remove #(contains? facts %) (atoms rhs))
        haves (vec (fact-haves nm vars sum facts (set opaque)))
        atom-of (fn [h] (nth (nth h 2) 2))
        value-of (fn [h] (nth (nth h 2) 3))
        inner (set (mapcat (comp atoms value-of) haves))
        ordered (concat (remove #(inner (atom-of %)) haves) (filter #(inner (atom-of %)) haves))]
    (concat haves extra
            [(list 'rewrite ['q_n])]
            (for [h (concat ordered extra)] (list 'try (list 'rewrite [(second h)])))
            (map-indexed (fn [i at] (list 'generalize (symbol (str "X_" i)) (symbol (str "hX_" i)) at))
                         opaque)
            (for [h (concat ['q_n 'q_e] (map second haves) (map second extra)
                            (map #(symbol (str "hX_" %)) (range (count opaque))))]
              (list 'try (list 'clear h)))
            ['(omega)])))

(defn size-eq!
  "Prove nm : cnodes (encDT lhs) = goal, given tree-proof : lhs = tree,
  where tree is a constructor application.  vars are the theorem's variable
  names; facts and extra (further have-forms) supply the rest."
  ([nm params vars lhs tree tree-proof goal facts]
   (size-eq! nm params vars lhs tree tree-proof goal facts []))
  ([nm params vars lhs tree tree-proof goal facts extra]
   (let [code (expand tree)
         sum (nodes-of code)]
     (a/prove-theorem nm (lv params)
       (lv (list 'Eq 'Nat (list 'cnodes (list 'encDT lhs)) goal))
       (lv (vec (concat
         [(list 'have 'q_t (list 'Eq 'DT lhs tree) tree-proof)
          (list 'have 'q_e (list 'Eq 'Code (list 'encDT tree) code) '(rfl))
          (list 'have 'q_n (list 'Eq 'Nat (list 'cnodes (list 'encDT lhs)) sum)
                (list 'Eq.trans '(congrArg (fn [z :- DT] (cnodes (encDT z))) q_t)
                      (list 'Eq.trans '(congrArg cnodes q_e) (cnodes-eq code))))]
         (solve-tactics nm vars sum facts extra goal))))))))

(defn cert-size!
  "Prove nm : cnodes c = goal, where c unfolds by rfl to
  encCert m t A T1 T2 (certcanon's frame, read from its own expression).
  args maps the frame's symbols m t A T1 T2 to forms."
  [nm params vars c args goal facts extra]
  (let [frame (clojure.walk/postwalk-replace args cert-expr)
        sum (nodes-of frame)]
    (a/prove-theorem nm (lv params)
      (lv (list 'Eq 'Nat (list 'cnodes c) goal))
      (lv (vec (concat
        [(list 'have 'q_e (list 'Eq 'Code c frame) '(rfl))
         (list 'have 'q_n (list 'Eq 'Nat (list 'cnodes c) sum)
               (list 'Eq.trans '(congrArg cnodes q_e) (cnodes-eq frame)))]
        (solve-tactics nm vars sum facts extra goal)))))))

(defn ctor-app?
  "True when term is a DT constructor application."
  [term]
  (and (seq? term) (some (fn [[nm _]] (= (first term) (symbol (str "DT." nm)))) dt-ctors)))

(defn ht-form
  "htDT of a derivation-tree term, unfolded through its constructors the way
  certenc's define-fueled! defines it: a node's height is one more than the
  sum of its DT fields' heights (sum-terms order); any other DT term t stays
  as (htDT t)."
  [term]
  (if-let [[_ fields] (when (seq? term)
                        (first (filter (fn [[nm _]] (= (first term) (symbol (str "DT." nm))))
                                       dt-ctors)))]
    (list '+ (reduce (fn [acc t] (list '+ t acc)) 0
                     (reverse (for [[[_ T] arg] (map vector fields (rest term)) :when (= T 'DT)]
                                (ht-form arg))))
          1)
    (list 'htDT term)))

(defn ht-eq!
  "Prove nm : htDT lhs = goal, given tree-proof : lhs = tree.  The tree is
  unfolded by ht-form (checked by rfl); every remaining (htDT t) is
  generalized to a variable, and omega closes the linear goal."
  [nm params lhs tree tree-proof goal]
  (let [form (ht-form tree)
        hts (distinct (filter #(and (seq? %) (= 'htDT (first %))) (tree-seq seq? seq (list form goal))))]
    (a/prove-theorem nm (lv params)
      (lv (list 'Eq 'Nat (list 'htDT lhs) goal))
      (lv (vec (concat
        [(list 'have 'q_t (list 'Eq 'DT lhs tree) tree-proof)
         (list 'have 'q_h (list 'Eq 'Nat (list 'htDT lhs) form)
               (list 'Eq.trans '(congrArg htDT q_t) (list 'Eq.refl form)))
         '(rewrite [q_h])
         '(try (clear q_h))]
        (map-indexed (fn [i h] (list 'generalize (symbol (str "H_" i)) (symbol (str "hH_" i)) h)) hts)
        (map-indexed (fn [i _] (list 'try (list 'clear (symbol (str "hH_" i))))) hts)
        ['(omega)]))))))
