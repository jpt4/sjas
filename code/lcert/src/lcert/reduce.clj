(ns lcert.reduce
  "Conversion in λᶜᵉʳᵗ₀ (R4-metatheory.md §1.5).

  The single steps are β (functions and pairs), ι (the eliminators, print),
  δ (chk′ on two closed canonical codes computes Check), and T (T(tt) is 1,
  T(ff) is 0).  H₁, reflect and inspect have no step: they compute only in
  the evaluator.

  The type checker needs *recorded* conversions: an explicit derivation keeps
  every conversion as a chain of types, each one step from the next in one
  direction or the other, so that Check can validate it structurally without
  normalizing (R4-metatheory.md §1.6).  `conv-chain` produces such chains by
  normalizing both sides; `one-step?` and `chain-valid?` are what Check uses.

  δ needs Check itself.  To avoid a cycle between namespaces, it reaches Check
  through the dynamic var *check*, which defaults to lcert.check/check.  Check
  calls back into this namespace only on codes strictly smaller than the one
  it is checking (E4), so the recursion terminates."
  (:require [lcert.syntax :as s]))

(def ^:dynamic *check*
  "The function (fn [code type-code] boolean) that δ-steps call."
  (fn [c d] ((requiring-resolve 'lcert.check/check) c d)))

(defn contract
  "The contractum of `e` if `e` is itself a redex, else nil."
  [e]
  (when (s/expr? e)
    (case (first e)
      :app (let [[_ f u] e]
             (when (= :lam (first f)) (s/subst (nth f 3) u)))
      :let (let [[_ _C p t] e]
             (when (= :pair (first p)) (s/subst-many t [(nth p 2) (nth p 3)])))
      :if (let [[_ b t f] e]
            (condp = b [:tt] t [:ff] f nil))
      :elimBool (let [[_ _P b t1 t2] e]
                  (condp = b [:tt] t1 [:ff] t2 nil))
      :recN (let [[_ P z st n] e]
              (case (first n)
                :zero z
                :succ (s/subst-many st [(second n) [:recN P z st (second n)]])
                nil))
      :caseLbl (let [[_ _P a bs] e]
                 (when (= :lbl (first a)) (nth bs (s/label-index (second a)))))
      :recSyn (let [[_ P tl tn c] e]
                (case (first c)
                  :sleaf (s/subst tl (second c))
                  :snode (let [[_ a c1 c2] c]
                           (s/subst-many tn [a c1 c2 [:recSyn P tl tn c1] [:recSyn P tl tn c2]]))
                  nil))
      :itR (let [[_ X g h r] e]
             (case (first r)
               :leaf [:app g (second r)]
               :node (let [[_ d a r1 r2] r]
                       [:app [:app [:app [:app h d] a] [:itR X g h r1]] [:itR X g h r2]])
               nil))
      :print (let [[_ r] e]
               (case (first r)
                 :leaf [:sleaf (second r)]
                 :node (let [[_ _d a r1 r2] r] [:snode a [:print r1] [:print r2]])
                 nil))
      :chk (let [[_ c d] e
                 c' (s/term->code c)
                 d' (s/term->code d)]
             (when (and c' d') (if (*check* c' d') [:tt] [:ff])))
      :T (condp = (second e) [:tt] [:Unit] [:ff] [:Empty] nil)
      nil)))

(declare step)

(defn- step-vec
  "One step in the first element of a vector of terms that has one."
  [v]
  (loop [i 0]
    (when (< i (count v))
      (if-let [x (step (nth v i))]
        (assoc v i x)
        (recur (inc i))))))

(defn step
  "One leftmost-outermost step, or nil if `e` is normal."
  [e]
  (when (s/expr? e)
    (or (contract e)
        (loop [i 1]
          (when (< i (count e))
            (let [x (nth e i)
                  x' (cond (s/expr? x) (step x)
                           (vector? x) (step-vec x)
                           :else nil)]
              (if x' (assoc e i x') (recur (inc i)))))))))

(def default-fuel
  "Steps allowed before normalization gives up.  Well-typed terms normalize;
  the fuel only guards against a bug feeding in an ill-typed term."
  1000000)

(defn normalize
  "The recorded sequence [e e1 ... en] of leftmost-outermost steps, en normal."
  ([e] (normalize e default-fuel))
  ([e fuel]
   (loop [e e acc [e] n 0]
     (when (>= n fuel)
       (throw (ex-info "normalization ran out of fuel" {:fuel fuel})))
     (if-let [e' (step e)]
       (recur e' (conj acc e') (inc n))
       acc))))

(defn nf "The normal form of `e`." [e] (peek (normalize e)))

(defn convertible? [a b] (or (= a b) (= (nf a) (nf b))))

(defn conv-chain
  "A chain [a ... b] of single steps, through the common normal form, or nil
  if a and b are not convertible.  Equal types give the one-element chain."
  [a b]
  (if (= a b)
    [a]
    (let [sa (normalize a) sb (normalize b)]
      (when (= (peek sa) (peek sb))
        (vec (concat sa (rest (rseq sb))))))))

(declare one-step?)

(defn- one-step-vec? [v w]
  (and (= (count v) (count w))
       (let [diffs (remove #(= (nth v %) (nth w %)) (range (count v)))]
         (and (= 1 (count diffs))
              (one-step? (nth v (first diffs)) (nth w (first diffs)))))))

(defn one-step?
  "True iff `y` results from `x` by contracting exactly one redex: at the root,
  or inside exactly one argument, the rest being equal."
  [x y]
  (and (s/expr? x)
       (or (= y (contract x))
           (and (vector? y)
                (= (count x) (count y))
                (= (first x) (first y))
                (let [diffs (remove #(= (nth x %) (nth y %)) (range 1 (count x)))]
                  (and (= 1 (count diffs))
                       (let [a (nth x (first diffs)) b (nth y (first diffs))]
                         (cond (s/expr? a) (one-step? a b)
                               (vector? a) (one-step-vec? a b)
                               :else false))))))))

(defn chain-valid?
  "True iff every consecutive pair of `chain` is one step apart, in either
  direction.  The chain must be non-empty."
  [chain]
  (and (seq chain)
       (every? (fn [[a b]] (or (one-step? a b) (one-step? b a)))
               (partition 2 1 chain))))
