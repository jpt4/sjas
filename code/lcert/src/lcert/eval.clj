(ns lcert.eval
  "The evaluators of λᶜᵉʳᵗ₀ (R4-metatheory.md §5).

  The language runs on evalᴱₙ, the *erasing* budgeted evaluator:
    - it evaluates a program erased from its typing derivation: the argument
      of every application at a usage-0 Π, and the first component of every
      pair at a usage-0 Σ, are replaced by ⋆ (they are never needed at
      runtime);
    - tokens are distinct runtime objects, n of them for a program run in Θₙ;
    - reflect_D v e runs the program a certificate v encodes, when the
      certificate is within the budget cap (‖v‖ ≤ n) and Check accepts it at
      ⌜D⌝.  It erases the decoded derivation, and gives the program m of v's
      own tokens (m < ‖v‖ by strict overhead), taken in preorder;
    - inspect branches on Check and hands the certificate to the branch;
    - H₁, H and abort return defaults.  A typed program never reaches them
      (Theorem 5.2), and *unreachable* lets the tests confirm it.

  Erasure matters.  An erased position may build a certificate from one token
  used any number of times (review R4-04); the non-erasing evaluator of the
  metatheory's Theorem 4 would then physically build it.  That evaluator is
  kept here, as the {:erase? false} option, only to test that the two agree on
  data results (Theorem 4′).

  Runtime values:
    Unit ⋆ → :star     Bool → true/false     Nat → a long     Lbl → a keyword
    Syn → [:sl l] / [:sn l a b]
    ◇ → a Token        R → [:rl l] / [:rn token l r1 r2]
    Π → a Clojure fn   Σ → [:pv a b]"
  (:require [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.check :as c]
            [lcert.kernel :as k]
            [lcert.typing :as t]))

(defrecord Token [id])

(defn token "A runtime token." [id] (->Token id))

;; ---------------------------------------------------------------------------
;; Certificate values.

(defn nodes
  "Internal nodes of an R value."
  [v]
  (if (= :rn (first v)) (+ 1 (nodes (nth v 3)) (nodes (nth v 4))) 0))

(defn tokens
  "The tokens of an R value (or of a pair holding R values), in preorder."
  [v]
  (cond
    (not (vector? v)) []
    (= :rn (first v)) (into [(second v)] (concat (tokens (nth v 3)) (tokens (nth v 4))))
    (= :pv (first v)) (into (tokens (second v)) (tokens (nth v 2)))
    :else []))

(defn assert-linear!
  "Throw if some token object occurs twice in value v: a well-typed program
  run by the erasing evaluator never duplicates a token.  The walk sees
  certificate trees and pairs; tokens captured inside a closure are not
  visible to it (review E2), so a value of function type is not checked."
  [v]
  (let [ts (tokens v)]
    (when (not= (count ts) (count (distinct ts)))
      (throw (ex-info "a token occurs twice in a value" {:type :lcert/linearity :tokens ts})))
    v))

(defn print-value "print: forget the tokens of an R value." [v]
  (case (first v)
    :rl [:sl (second v)]
    :rn [:sn (nth v 2) (print-value (nth v 3)) (print-value (nth v 4))]))

;; ---------------------------------------------------------------------------
;; Defaults, by skeleton (R4-metatheory.md §3.1: computable choices).

(defn default-value [sk]
  (case (first sk)
    :Unit :star, :Bool false, :Nat 0, :Lbl (first s/labels)
    :Syn [:sl (first s/labels)], :R [:rl (first s/labels)]
    ;; a fresh phantom token each time, so two defaults never share one
    :Dia (token (keyword (gensym "phantom")))
    :Fn (let [v (default-value (nth sk 2))] (fn [_] v))
    :Prod [:pv (default-value (second sk)) (default-value (nth sk 2))]))

;; ---------------------------------------------------------------------------
;; Erasure, from a typing derivation.

(defn erase
  "The runtime term of runtime derivation d, with usage-0 positions replaced
  by ⋆.  Type annotations are kept, but never evaluated."
  [d]
  (let [p (:prems d)
        er (fn [i] (erase (nth p i)))
        t (:term d)]
    (case (:rule d)
      (:Var :Unit :TT :FF :Zero :Lbl) t
      :Conv (er 0)
      :Lam (let [[_ u A _] t] [:lam u A (er 1)])
      :App (let [u (second (:type (nth p 0)))]
             [:app (er 0) (if (= u 0) [:star] (er 1))])
      :Pair (let [[_ S] t u (second S)]
              [:pair S (if (= u 0) [:star] (er 1)) (er 2)])
      :Let [:let (second t) (er 0) (er 2)]
      :Abort [:abort (second t) (er 0)]
      :If [:if (er 0) (er 1) (er 2)]
      :ElimBool [:elimBool (second t) (er 0) (er 2) (er 3)]
      :Succ [:succ (er 0)]
      :RecN [:recN (second t) (er 2) (er 3) (er 0)]
      :CaseLbl [:caseLbl (second t) (er 0) (mapv erase (drop 2 p))]
      :SLeaf [:sleaf (er 0)]
      :SNode [:snode (er 0) (er 1) (er 2)]
      :RecSyn [:recSyn (second t) (er 2) (er 3) (er 0)]
      :Leaf [:leaf (er 0)]
      :Node [:node (er 0) (er 1) (er 2) (er 3)]
      :ItR [:itR (second t) (er 1) (er 2) (er 3)]
      :Print [:print (er 0)]
      :Chk [:chk (er 0) (er 1)]
      :H1 [:h1 (er 0) (er 1) (er 2) (er 3) (er 4)]
      :Reflect [:reflect (second t) (er 0) (er 1)]
      :Inspect [:inspect (second t) (er 0) (er 1) (er 3) (er 4)])))

;; ---------------------------------------------------------------------------
;; Evaluation.

(declare eval-deriv)

(def ^:dynamic *unreachable*
  "When bound to a function, evaluation calls it with :abort, :h1 or :H on
  entering an abort, H₁ or H (reflect at 0) node, before evaluating the
  node's arguments.  Theorem 5.2 of the metatheory says a typed program
  never enters one, under either evaluator; the tests bind this to catch
  any that does.  Unbound (nil), nothing is called, and such a node returns
  the default of its type's skeleton, as in the metatheory's evaluators."
  nil)

(defn- entered-unreachable
  "Report entry into an abort, H₁ or H node to *unreachable*, if bound."
  [kind]
  (when-let [f *unreachable*] (f kind)))

(defn- ev
  "Evaluate term t in environment env (a vector, innermost last) under the
  budget cap n.  opts: {:erase? bool}.  With :erase? true, t is an erased
  term and certificate nodes are checked never to reuse a token."
  [t env n opts]
  (let [go (fn [x] (ev x env n opts))
        under (fn [vals x] (ev x (into env vals) n opts))]
    (case (first t)
      :var (nth env (- (count env) 1 (second t)))
      :star :star
      :tt true
      :ff false
      :zero 0
      :succ (inc (go (second t)))
      :lbl (second t)
      ;; call-by-value: the argument runs first (review E1); in a typed
      ;; program it cannot produce a value, so the default is never observed
      :abort (do (entered-unreachable :abort)
                 (go (nth t 2))
                 (default-value (c/skel (second t))))
      :if (let [[_ b x y] t] (if (go b) (go x) (go y)))
      :elimBool (let [[_ _P b x y] t] (if (go b) (go x) (go y)))
      :recN (let [[_ _P z st nn] t
                  k (go nn)]
              (loop [i 0 acc (go z)]
                (if (< i k) (recur (inc i) (under [i acc] st)) acc)))
      :caseLbl (let [[_ _P a bs] t] (go (nth bs (s/label-index (go a)))))
      :sleaf [:sl (go (second t))]
      :snode (let [[_ a c1 c2] t] [:sn (go a) (go c1) (go c2)])
      :recSyn (let [[_ _P tl tn cc] t]
                (letfn [(rec [code]
                          (case (first code)
                            :sl (under [(second code)] tl)
                            :sn (let [[_ l a b] code]
                                  (under [l a b (rec a) (rec b)] tn))))]
                  (rec (go cc))))
      :leaf [:rl (go (second t))]
      :node (let [[_ d a r1 r2] t
                  tok (go d) l (go a) v1 (go r1) v2 (go r2)]
              (when (:erase? opts)
                (when (some #{tok} (concat (tokens v1) (tokens v2)))
                  (throw (ex-info "a token was used twice" {:type :lcert/linearity :token tok}))))
              [:rn tok l v1 v2])
      :itR (let [[_ _X g h rr] t
                 gv (go g) hv (go h)]
             (letfn [(rec [v]
                       (case (first v)
                         :rl (gv (second v))
                         :rn (let [[_ tok l a b] v]
                               ((((hv tok) l) (rec a)) (rec b)))))]
               (rec (go rr))))
      :print (print-value (go (second t)))
      :lam (let [[_ _u _A body] t] (fn [v] (ev body (conj env v) n opts)))
      :app (let [[_ f a] t] ((go f) (go a)))
      :pair (let [[_ _S a b] t] [:pv (go a) (go b)])
      :let (let [[_ _C p body] t
                 [_ x y] (go p)]
             (under [x y] body))
      :chk (let [[_ cc dd] t] (c/check (go cc) (go dd)))
      :h1 (do (entered-unreachable :h1) (doseq [x (rest t)] (go x)) :star)
      :reflect (let [[_ D rr ev0] t
                     _ (when (= [:Empty] D) (entered-unreachable :H))
                     v (go rr)
                     _ (go ev0)]
                 (if (and (<= (nodes v) n) (c/check (print-value v) (e/enc-exp D)))
                   ;; run the certified program on m of v's own tokens
                   (let [dd (e/dec-deriv (print-value v))
                         m (count (:ctx dd))
                         toks (vec (take m (tokens v)))]
                     (eval-deriv dd m (assoc opts :tokens toks)))
                   (default-value (c/skel D))))
      :inspect (let [[_ _X rr cc t1 t2] t
                     v (go rr)
                     ok (c/check (print-value v) (go cc))]
                 (under [v :star] (if ok t1 t2)))
      (throw (ex-info (str "cannot evaluate " (first t)) {:term t})))))

(defn eval-deriv
  "Evaluate the program of runtime derivation d, whose context is Θₘ, under
  budget cap n.  The m tokens are fresh unless opts supplies them as :tokens.
  With {:erase? true} (the default) the erasing evaluator runs, and the
  result is checked to hold no token twice."
  ([d n] (eval-deriv d n {:erase? true}))
  ([d n opts]
   (let [opts (merge {:erase? true} opts)
         m (count (:ctx d))
         toks (or (:tokens opts) (mapv #(token (inc %)) (range m)))
         term (if (:erase? opts) (erase d) (:term d))
         v (ev term toks n (dissoc opts :tokens))]
     (if (:erase? opts) (assert-linear! v) v))))

(defn run
  "Type check term t in Θₙ and evaluate it with n fresh tokens (erasing)."
  [n t]
  (eval-deriv (t/check-top n t) n))
