(ns lcert.pa
  "Peano arithmetic, and its embedding into λᶜᵉʳᵗ₀ at budget 0.

  This is step 1 of the proof that λᶜᵉʳᵗ₀ cannot prove its own consistency
  over codes (R4-metatheory.md §6, Proposition 5).  Step 1 needs: a PA-proof
  of 0 = 1 becomes, by a primitive recursive translation, a closed refutation
  in λᶜᵉʳᵗ₀.  This namespace implements that translation, so that its
  templates — one per axiom scheme and rule — are checked by the type checker
  and by Check rather than only argued on paper.

  PA here is a Hilbert system for classical first-order logic in the
  connectives →, ⊥, ∀ and the relation =, over the terms
      0 = [:z]    S t = [:s t]    t + u = [:+ t u]    t · u = [:* t u]    x
  and the formulas
      [:= t u]   [:bot]   [:-> φ ψ]   [:all x φ]
  (¬φ is φ → ⊥; ∧, ∨ and ∃ are the usual classical abbreviations).  Its
  axioms are the schemes of `instance`, and its rules modus ponens and
  generalization.  With only →, ⊥ and ∀, every formula is already in the
  negative fragment, so the translation needs no double negations: atoms are
  decidable.

  The translation.  A PA variable x becomes the λᶜᵉʳᵗ₀ variable v_x.  A term
  becomes a Nat term, using closed definitions PLUS and TIMES; an atom s = t
  becomes T(EQ s t); ⊥ becomes Void; → and ∀ become Π at usage ω.  A proof
  line φ becomes a *closed* term of the universal closure of φ, so every
  derivation lives at budget 0 in the empty context, where everything may be
  used any number of times.

  Two places need care with usages, and are where the reviews of 2026-09-26
  found the earlier outline wrong:
    - induction (IND) and the transport lemma recurse at a packaged motive
      !P = Σ(p :ω P). Unit, because recN hands its step the recursive result
      at usage 1, while a PA step uses its hypothesis freely;
    - stability, ((φ → ⊥) → ⊥) → φ, is proved by recursion on φ: for atoms by
      case analysis on the Boolean, then lifted through → and ∀."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [lcert.syntax :as s]
            [lcert.reduce :as r]))

(defn- fail [msg data] (throw (ex-info msg (assoc data :type :lcert/pa))))

;; ---------------------------------------------------------------------------
;; PA syntax: variables, substitution.

(defn term-vars [t]
  (cond (symbol? t) #{t}
        (= :z (first t)) #{}
        :else (apply set/union (map term-vars (rest t)))))

(defn free-vars [phi]
  (case (first phi)
    := (set/union (term-vars (nth phi 1)) (term-vars (nth phi 2)))
    :bot #{}
    :-> (set/union (free-vars (nth phi 1)) (free-vars (nth phi 2)))
    :all (disj (free-vars (nth phi 2)) (nth phi 1))))

(defn subst-term [t x u]
  (cond (symbol? t) (if (= t x) u t)
        (= :z (first t)) t
        :else (into [(first t)] (map #(subst-term % x u) (rest t)))))

(defn subst
  "Replace the free occurrences of x in phi by the term u (no capture check;
  see substitutable?)."
  [phi x u]
  (case (first phi)
    := [:= (subst-term (nth phi 1) x u) (subst-term (nth phi 2) x u)]
    :bot phi
    :-> [:-> (subst (nth phi 1) x u) (subst (nth phi 2) x u)]
    :all (if (= (nth phi 1) x) phi [:all (nth phi 1) (subst (nth phi 2) x u)])))

(defn substitutable?
  "Is t free for x in phi: does no free occurrence of x lie in the scope of a
  quantifier binding a variable of t?"
  [t x phi]
  (let [tv (term-vars t)]
    (letfn [(ok? [f bound]
              (case (first f)
                := (or (not (contains? (free-vars f) x)) (empty? (set/intersection tv bound)))
                :bot true
                :-> (and (ok? (nth f 1) bound) (ok? (nth f 2) bound))
                :all (or (= (nth f 1) x) (ok? (nth f 2) (conj bound (nth f 1))))))]
      (ok? phi #{}))))

;; ---------------------------------------------------------------------------
;; The axiom schemes.

(defn instance
  "The formula an axiom justification denotes."
  [[tag & ps]]
  (case tag
    :A1 (let [[f g] ps] [:-> f [:-> g f]])
    :A2 (let [[f g h] ps] [:-> [:-> f [:-> g h]] [:-> [:-> f g] [:-> f h]]])
    :DN (let [[f] ps] [:-> [:-> [:-> f [:bot]] [:bot]] f])
    :A4 (let [[x f t] ps] [:-> [:all x f] (subst f x t)])
    :A5 (let [[x f g] ps] [:-> [:all x [:-> f g]] [:-> f [:all x g]]])
    :E1 (let [[x] ps] [:= x x])
    :E2 (let [[x y z atom] ps] [:-> [:= x y] [:-> (subst atom z x) (subst atom z y)]])
    :Q1 (let [[x] ps] [:-> [:= [:s x] [:z]] [:bot]])
    :Q2 (let [[x y] ps] [:-> [:= [:s x] [:s y]] [:= x y]])
    :Q3 (let [[x] ps] [:= [:+ x [:z]] x])
    :Q4 (let [[x y] ps] [:= [:+ x [:s y]] [:s [:+ x y]]])
    :Q5 (let [[x] ps] [:= [:* x [:z]] [:z]])
    :Q6 (let [[x y] ps] [:= [:* x [:s y]] [:+ [:* x y] x]])
    :IND (let [[x f] ps] [:-> (subst f x [:z])
                              [:-> [:all x [:-> f (subst f x [:s x])]] [:all x f]]])))

(defn- side-conditions-hold? [[tag & ps]]
  (case tag
    :A4 (let [[x f t] ps] (and (symbol? x) (substitutable? t x f)))
    :A5 (let [[x f _] ps] (and (symbol? x) (not (contains? (free-vars f) x))))
    :E1 (symbol? (first ps))
    :E2 (let [[x y z atom] ps] (and (every? symbol? [x y z]) (= := (first atom))))
    (:Q1 :Q3 :Q5) (symbol? (first ps))
    (:Q2 :Q4 :Q6) (every? symbol? ps)
    :IND (symbol? (first ps))
    true))

(def axiom-tags #{:A1 :A2 :DN :A4 :A5 :E1 :E2 :Q1 :Q2 :Q3 :Q4 :Q5 :Q6 :IND})

(defn check-proof
  "Check a PA proof: a vector of lines {:formula φ :by just}, where just is an
  axiom justification, [:MP i j] (line i is φ, line j is φ → this line), or
  [:Gen i x] (this line is ∀x of line i).  Returns true or throws."
  [lines]
  (doseq [[k {:keys [formula by]}] (map-indexed vector lines)]
    (let [tag (first by)
          earlier (fn [i] (if (and (int? i) (< -1 i k)) (:formula (nth lines i))
                              (fail (str "line " k " cites line " i) {:line k})))]
      (cond
        (axiom-tags tag)
        (do (when-not (side-conditions-hold? by) (fail (str "line " k ": side condition fails") {:line k :by by}))
            (when-not (= formula (instance by)) (fail (str "line " k ": not that axiom instance") {:line k :by by})))
        (= tag :MP)
        (let [[_ i j] by]
          (when-not (= (earlier j) [:-> (earlier i) formula])
            (fail (str "line " k ": modus ponens does not apply") {:line k :by by})))
        (= tag :Gen)
        (let [[_ i x] by]
          (when-not (and (symbol? x) (= formula [:all x (earlier i)]))
            (fail (str "line " k ": generalization does not apply") {:line k :by by})))
        :else (fail (str "line " k ": unknown justification") {:line k :by by}))))
  true)

;; ---------------------------------------------------------------------------
;; Closed definitions, as surface forms.  Each is closed, so it can be spliced
;; anywhere; `expand` replaces the abbreviations by their definitions.

(def ^:private base-defs
  (array-map
   'ISZERO '(fn [b w Nat] (rec-nat [q Bool] tt [k y] ff b))
   'PRED '(fn [b w Nat] (rec-nat [q Nat] zero [k y] k b))
   ;; EQ recurses on its first argument; the recursive result h is used once,
   ;; inside an ordinary λ, never inside an ω-scaled method
   'EQ '(fn [a w Nat] (rec-nat [q (-> Nat Bool)] ISZERO
                               [k h] (fn [b w Nat] (if (ISZERO b) ff (h (PRED b))))
                               a))
   ;; PLUS takes its first argument at usage 1: it is the recursion's base,
   ;; used once.  TIMES's step passes it the step's recursive result, which
   ;; recN supplies at usage 1; an ω-argument there would be a usage error
   ;; (found by the tests of this namespace).
   'PLUS '(fn [a 1 Nat] (fn [b w Nat] (rec-nat [q Nat] a [k y] (succ y) b)))
   'TIMES '(fn [a w Nat] (fn [b w Nat] (rec-nat [q Nat] zero [k y] (PLUS y a) b)))
   ;; reflexivity: T(EQ x x), by recursion; EQ (S k) (S k) converts to EQ k k
   'REFL '(fn [x w Nat] (rec-nat [x (T (EQ x x))] star [k y] y x))))

(defn- Q-type
  "Q(t) := Π(y :ω Nat). Π(f :ω Nat → Bool). T(EQ t y) → T(f t) → T(f y)."
  [t]
  (list 'Pi '[y w Nat] (list 'Pi '[f w (-> Nat Bool)]
                             (list '-> (list 'T (list 'EQ t 'y)) (list 'T (list 'f t)) '(T (f y))))))

(def ^:private transport-def
  "TRANSPORT : Π(x :ω Nat). Q(x), by induction on x at the packaged motive
  !Q(x) = Σ(q :ω Q(x)). Unit, with a case analysis on y inside each case.
  The inner case analysis is a recN whose method is ω-scaled; it may use q
  only because the packaging returns q at usage ω."
  (let [base (list 'fn '[y w Nat]
                   (list 'rec-nat ['y (list 'Pi '[f w (-> Nat Bool)]
                                            '(-> (T (EQ zero y)) (T (f zero)) (T (f y))))]
                         '(fn [f w (-> Nat Bool)] (fn [e w (T (EQ zero zero))] (fn [a w (T (f zero))] a)))
                         '[y1 r] '(fn [f w (-> Nat Bool)]
                                    (fn [e w (T (EQ zero (succ y1)))]
                                      (fn [a w (T (f zero))] (abort (T (f (succ y1))) e))))
                         'y))
        step (list 'fn '[y w Nat]
                   (list 'rec-nat ['y (list 'Pi '[f w (-> Nat Bool)]
                                            '(-> (T (EQ (succ x1) y)) (T (f (succ x1))) (T (f y))))]
                         '(fn [f w (-> Nat Bool)]
                            (fn [e w (T (EQ (succ x1) zero))]
                              (fn [a w (T (f (succ x1)))] (abort (T (f zero)) e))))
                         '[y1 r] '(fn [f w (-> Nat Bool)]
                                    (fn [e w (T (EQ (succ x1) (succ y1)))]
                                      (fn [a w (T (f (succ x1)))]
                                        (q y1 (fn [z w Nat] (f (succ z))) e a))))
                         'y))
        pack (fn [t] (list 'Sigma ['q 'w (Q-type t)] 'Unit))]
    (list 'fn '[x w Nat]
          (list 'let-pair (Q-type 'x) '[q u]
                (list 'rec-nat ['x (pack 'x)]
                      (list 'pair (pack 'zero) base 'star)
                      '[x1 h]
                      (list 'let-pair (pack '(succ x1)) '[q u] 'h
                            (list 'pair (pack '(succ x1)) step 'star))
                      'x)
                'q))))

(def ^:private defs
  "The definitions, each fully expanded (definitions may use earlier ones)."
  (reduce (fn [acc [k v]] (assoc acc k (walk/postwalk-replace acc v)))
          {}
          (concat base-defs [['TRANSPORT transport-def]])))

(defn expand
  "Replace the abbreviations EQ, PLUS, TIMES, ISZERO, PRED, REFL and TRANSPORT
  in a surface form by their closed definitions."
  [form]
  (walk/postwalk-replace defs form))

(defn computes?
  "Do the two surface terms (abbreviations allowed) have the same normal form?"
  [form expected]
  (= (r/nf (s/parse-term [] (expand form))) (r/nf (s/parse-term [] (expand expected)))))

;; ---------------------------------------------------------------------------
;; The translation of terms and formulas.

(defn vsym "The λᶜᵉʳᵗ₀ variable of PA variable x." [x] (symbol (str "v_" (name x))))

(defn tr-term [t]
  (cond (symbol? t) (vsym t)
        :else (case (first t)
                :z 'zero
                :s (list 'succ (tr-term (nth t 1)))
                :+ (list 'PLUS (tr-term (nth t 1)) (tr-term (nth t 2)))
                :* (list 'TIMES (tr-term (nth t 1)) (tr-term (nth t 2))))))

(defn tr-formula [phi]
  (case (first phi)
    := (list 'T (list 'EQ (tr-term (nth phi 1)) (tr-term (nth phi 2))))
    :bot 'Void
    :-> (list '-> (tr-formula (nth phi 1)) (tr-formula (nth phi 2)))
    :all (list 'Pi [(vsym (nth phi 1)) 'w 'Nat] (tr-formula (nth phi 2)))))

(defn closure-vars "The free variables of phi, in the fixed order of the closure." [phi]
  (sort-by name (free-vars phi)))

(defn- close-over [vars body]
  (reduce (fn [b x] (list 'fn [(vsym x) 'w 'Nat] b)) body (reverse vars)))

(defn closure-type-abbrev
  "The translated universal closure of phi, abbreviations unexpanded."
  [phi]
  (reduce (fn [b x] (list 'Pi [(vsym x) 'w 'Nat] b)) (tr-formula phi) (reverse (closure-vars phi))))

(defn closure-type "The translated universal closure of phi, as a surface type." [phi]
  (expand (closure-type-abbrev phi)))

;; ---------------------------------------------------------------------------
;; Stability: a term of ((⟦φ⟧ → Void) → Void) → ⟦φ⟧, in the scope of φ's free
;; variables.  This is what makes the classical axiom DN hold.

(defn stab [phi]
  (let [F (tr-formula phi)]
    (case (first phi)
      := (let [b (list 'EQ (tr-term (nth phi 1)) (tr-term (nth phi 2)))]
           (list 'elim-bool ['z0 '(-> (-> (-> (T z0) Void) Void) (T z0))] b
                 '(fn [k w (-> (-> (T tt) Void) Void)] star)
                 '(fn [k w (-> (-> (T ff) Void) Void)] (k (fn [w0 w (T ff)] w0)))))
      :bot '(fn [k w (-> (-> Void Void) Void)] (k (fn [w0 w Void] w0)))
      :-> (let [[_ f g] phi
                A (tr-formula f) B (tr-formula g)]
            (list 'fn ['k 'w (list '-> (list '-> F 'Void) 'Void)]
                  (list 'fn ['a 'w A]
                        (list (stab g)
                              (list 'fn ['nb 'w (list '-> B 'Void)]
                                    (list 'k (list 'fn ['f0 'w F] '(nb (f0 a)))))))))
      :all (let [[_ x f] phi
                 vx (vsym x) B (tr-formula f)]
             (list 'fn ['k 'w (list '-> (list '-> F 'Void) 'Void)]
                   (list 'fn [vx 'w 'Nat]
                         (list (stab f)
                               (list 'fn ['nb 'w (list '-> B 'Void)]
                                     (list 'k (list 'fn ['f0 'w F] (list 'nb (list 'f0 vx))))))))))))

;; ---------------------------------------------------------------------------
;; The templates: one closed term per axiom instance.

(defn- template-body
  "The body of an axiom's term, in the scope of the instance's free variables."
  [[tag & ps]]
  (case tag
    :A1 (let [[f g] ps] (list 'fn ['a 'w (tr-formula f)] (list 'fn ['b 'w (tr-formula g)] 'a)))
    :A2 (let [[f g h] ps]
          (list 'fn ['p 'w (tr-formula [:-> f [:-> g h]])]
                (list 'fn ['q 'w (tr-formula [:-> f g])]
                      (list 'fn ['a 'w (tr-formula f)] '(p a (q a))))))
    :DN (stab (first ps))
    :A4 (let [[x f t] ps
              ;; if x is not free in f, t's variables do not occur in the
              ;; instance, so the closure does not bind them; any argument
              ;; then serves, since the result type does not mention x
              keep (free-vars (instance (into [tag] ps)))
              t' (reduce (fn [u v] (if (contains? keep v) u (subst-term u v [:z]))) t (term-vars t))]
          (list 'fn ['p 'w (tr-formula [:all x f])] (list 'p (tr-term t'))))
    :A5 (let [[x f g] ps
              vx (vsym x)]
          (list 'fn ['p 'w (tr-formula [:all x [:-> f g]])]
                (list 'fn ['a 'w (tr-formula f)]
                      (list 'fn [vx 'w 'Nat] (list 'p vx 'a)))))
    :E1 (list 'REFL (vsym (first ps)))
    :E2 (let [[x y z atom] ps
              [_ s t] atom]
          (list 'fn ['e 'w (list 'T (list 'EQ (vsym x) (vsym y)))]
                (list 'fn ['p 'w (tr-formula (subst atom z x))]
                      (list 'TRANSPORT (vsym x) (vsym y)
                            (list 'fn [(vsym z) 'w 'Nat] (list 'EQ (tr-term s) (tr-term t)))
                            'e 'p))))
    :Q1 (list 'fn ['e 'w (tr-formula [:= [:s (first ps)] [:z]])] 'e)
    :Q2 (let [[x y] ps] (list 'fn ['e 'w (tr-formula [:= [:s x] [:s y]])] 'e))
    :Q3 (list 'REFL (vsym (first ps)))
    :Q4 (let [[x y] ps] (list 'REFL (list 'succ (list 'PLUS (vsym x) (vsym y)))))
    :Q5 '(REFL zero)
    :Q6 (let [[x y] ps] (list 'REFL (list 'PLUS (list 'TIMES (vsym x) (vsym y)) (vsym x))))
    :IND (let [[x f] ps
               vx (vsym x)
               F (tr-formula f)
               F0 (tr-formula (subst f x [:z]))
               FS (tr-formula (subst f x [:s x]))
               pack (fn [A] (list 'Sigma ['p 'w A] 'Unit))]
           (list 'fn ['b 'w F0]
                 (list 'fn ['st 'w (list 'Pi [vx 'w 'Nat] (list '-> F FS))]
                       (list 'fn [vx 'w 'Nat]
                             (list 'let-pair F '[p u]
                                   (list 'rec-nat [vx (pack F)]
                                         (list 'pair (pack F0) 'b 'star)
                                         [vx 'h]
                                         (list 'let-pair (pack FS) '[p u] 'h
                                               (list 'pair (pack FS) (list 'st vx 'p) 'star))
                                         vx)
                                   'p)))))))

(defn axiom-term
  "The closed term (surface syntax, expanded) of an axiom instance, ascribed
  its translated universal closure."
  [just]
  (when-not (side-conditions-hold? just) (fail "side condition fails" {:by just}))
  (let [phi (instance just)]
    (expand (list 'the (closure-type-abbrev phi)
                  (close-over (closure-vars phi) (template-body just))))))

;; ---------------------------------------------------------------------------
;; Proofs.

(declare fix-apply)

(defn- line-sym [i] (symbol (str "L" i)))

(defn- line-term
  "The term of proof line k, referring to earlier lines by name."
  [lines k]
  (let [{:keys [formula by]} (nth lines k)
        fm (fn [i] (:formula (nth lines i)))
        inst (fn [vars keep] (map (fn [v] (if (contains? keep v) (vsym v) 'zero)) vars))]
    (case (first by)
      :MP (let [[_ i j] by
                keep (free-vars formula)
                ;; variables of the premises that the conclusion lacks are
                ;; instantiated at zero: the premises are universally closed
                body (list (apply list (line-sym j) (inst (closure-vars (fm j)) keep))
                           (apply list (line-sym i) (inst (closure-vars (fm i)) keep)))]
            (list 'the (closure-type-abbrev formula)
                  (close-over (closure-vars formula) (fix-apply body))))
      :Gen (let [[_ i x] by
                 inner (list 'fn [(vsym x) 'w 'Nat]
                             (fix-apply (apply list (line-sym i) (map vsym (closure-vars (fm i))))))]
             (list 'the (closure-type-abbrev formula)
                   (close-over (closure-vars formula) inner)))
      ;; an axiom
      (let [phi (instance by)]
        (list 'the (closure-type-abbrev phi)
              (close-over (closure-vars phi) (template-body by)))))))

(defn- fix-apply
  "A line with no free variables is referred to bare, not applied to nothing."
  [form]
  (walk/prewalk (fn [x] (if (and (seq? x) (= 1 (count x)) (symbol? (first x))) (first x) x)) form))

(defn proof-term
  "The closed term (surface syntax, expanded) of a checked PA proof: the
  lines bound in order by let, at usage ω since a line may be cited more
  than once, ending with the last line.  Each line is a term of its
  translated universal closure in the context of the earlier lines; an
  axiom's is closed."
  [lines]
  (check-proof lines)
  (let [bindings (vec (mapcat (fn [k] [(line-sym k) 'w (closure-type-abbrev (:formula (nth lines k)))
                                       (line-term lines k)])
                              (range (count lines))))]
    (expand (list 'let bindings (line-sym (dec (count lines)))))))

(defn refutation-term
  "A term of T(EQ 0 (S 0)), the translation of 0 = 1, ascribed Void: the two
  types convert, so a PA-proof of 0 = 1 becomes a closed refutation."
  [t]
  (list 'the 'Void t))
