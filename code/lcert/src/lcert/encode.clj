(ns lcert.encode
  "The encoding ⌜·⌝ of expressions, contexts, judgments and explicit
  derivations into codes, and its inverse.

  The encoding extends the table of the draft's §2.7
  (nachlass/refinement/R4-certificate-calculus.md) to the whole of λᶜᵉʳᵗ₀,
  and is designed to have the properties E1–E5 of R4-metatheory.md §1.6:

    E1  injective and decodable by structural recursion (dec-* below);
    E2  a derivation is [:sn rule J premises], J its conclusion;
    E3  a judgment contains its context and term as disjoint subtrees, and a
        context of m entries is m nested :ext nodes, one per entry;
    E4  every variable occurrence and every constructor of arity ≥ 1 is at
        least one internal node, so a literal code term encodes to at least
        as many nodes as the code it denotes;
    E5  ⌜Π(x:₁A).0⌝ = [:sn :arrow1 ⌜A⌝ ⌜0⌝], so neg-code of ⌜A⌝ is ⌜A ⊸ 0⌝.

  The draft's own labels are kept (:has :pair :isType :empty :var :zero :succ
  :nil :cons :bool :arrow :lam :if :branches :tt :ff, and the rule names), so
  its `not` example encodes to exactly its 35 nodes.  Its `ext_ω` is :extw and
  its `arrow` is the ω-arrow.

  Derivation records, the in-memory form of explicit derivations:
    {:rule  rule-label
     :j     :has (runtime typing) | :has0 (type-level typing) | :type
     :ctx   [[usage type] ...]      outermost entry first
     :term  t                       (absent when :j is :type)
     :type  A
     :prems [derivation ...]        in the order of the rule table
     :chain [A0 A1 ... Ak]}         (:Conv only; see lcert.check)"
  (:require [lcert.syntax :as s]))

;; ---------------------------------------------------------------------------
;; Building blocks.

(defn sl [l] [:sl l])
(defn sn [l a b] [:sn l a b])

(def nil-code (sl :nil))

(defn enc-list
  "A list as a spine of :cons nodes ending in (leaf nil) (draft §2.7)."
  [xs]
  (if (empty? xs) nil-code (sn :cons (first xs) (enc-list (rest xs)))))

(defn enc-nat
  "A de Bruijn index in unary: 0 = (leaf zero), i+1 = node(succ, i, leaf nil)."
  [i]
  (if (zero? i) (sl :zero) (sn :succ (enc-nat (dec i)) nil-code)))

(def ^:private by-usage
  {:ext   {0 :ext0 1 :ext1 :w :extw}
   :arrow {0 :arrow0 1 :arrow1 :w :arrow}
   :sigma {0 :sigma0 1 :sigma1 :w :sigmaw}
   :lam   {0 :lam0 1 :lam1 :w :lam}})

(def ^:private usage-of
  "Inverse of by-usage: label → [family usage]."
  (into {} (for [[fam m] by-usage [u l] m] [l [fam u]])))

(def ^:private base-type-labels
  {[:Empty] :t0 [:Unit] :t1 [:Bool] :bool [:Nat] :nat
   [:Lbl] :lbl [:Syn] :syn [:Dia] :dia [:R] :cert})

(def ^:private base-type-of (into {} (map (fn [[t l]] [l t]) base-type-labels)))

(def ^:private nullary-term-labels
  {[:star] :star [:tt] :tt [:ff] :ff [:zero] :nzero})

(def ^:private nullary-term-of (into {} (map (fn [[t l]] [l t]) nullary-term-labels)))

;; ---------------------------------------------------------------------------
;; Expressions (types and terms share one encoding; their labels are disjoint).

(defn enc-exp
  "⌜e⌝ for a type or term e."
  [e]
  (let [enc enc-exp
        args (fn [& xs] (reduce (fn [acc x] (sn :args x acc)) (reverse xs)))]
    (if-let [l (or (base-type-labels e) (nullary-term-labels e))]
      (sl l)
      (let [[tag & xs] e]
        (case tag
          :T (sn :T (enc (first xs)) nil-code)
          :Pi (let [[u A B] xs] (sn (get-in by-usage [:arrow u]) (enc A) (enc B)))
          :Sigma (let [[u A B] xs] (sn (get-in by-usage [:sigma u]) (enc A) (enc B)))
          :var (sn :var (enc-nat (first xs)) nil-code)
          :abort (let [[A t] xs] (sn :abort (enc A) (enc t)))
          :if (let [[b t f] xs] (sn :if (enc b) (sn :branches (enc t) (enc f))))
          :elimBool (let [[P b t1 t2] xs]
                      (sn :elimBool (enc P) (sn :args (enc b) (sn :branches (enc t1) (enc t2)))))
          :succ (sn :nsucc (enc (first xs)) nil-code)
          :recN (let [[P z st n] xs] (sn :recN (enc P) (args (enc z) (enc st) (enc n))))
          :lbl (sn :lblc (sl (first xs)) nil-code)
          :caseLbl (let [[P a bs] xs]
                     (sn :caseLbl (enc P) (sn :args (enc a) (enc-list (map enc bs)))))
          :sleaf (sn :sleaf (enc (first xs)) nil-code)
          :snode (let [[a c1 c2] xs] (sn :snode (enc a) (args (enc c1) (enc c2))))
          :recSyn (let [[P tl tn c] xs] (sn :recSyn (enc P) (args (enc tl) (enc tn) (enc c))))
          :leaf (sn :rleaf (enc (first xs)) nil-code)
          :node (let [[d a r1 r2] xs] (sn :rnode (enc d) (args (enc a) (enc r1) (enc r2))))
          :itR (let [[X g h r] xs] (sn :itR (enc X) (args (enc g) (enc h) (enc r))))
          :print (sn :print (enc (first xs)) nil-code)
          :lam (let [[u A t] xs] (sn (get-in by-usage [:lam u]) (enc A) (enc t)))
          :app (let [[f a] xs] (sn :app (enc f) (enc a)))
          :pair (let [[S a b] xs] (sn :tpair (enc S) (args (enc a) (enc b))))
          :let (let [[C p t] xs] (sn :let (enc C) (args (enc p) (enc t))))
          :chk (let [[c d] xs] (sn :chk (enc c) (enc d)))
          :h1 (let [[r s' c e1 e2] xs] (sn :h1 (enc r) (args (enc s') (enc c) (enc e1) (enc e2))))
          :reflect (let [[D r ev] xs] (sn :reflect (enc D) (args (enc r) (enc ev))))
          :inspect (let [[X r c t1 t2] xs]
                     (sn :inspect (enc X) (args (enc r) (enc c) (sn :branches (enc t1) (enc t2)))))
          (throw (ex-info (str "cannot encode " (pr-str e)) {:exp e})))))))

(defn enc-ctx
  "A context: (leaf empty) or one :ext node per entry, usage in the label."
  [ctx]
  (reduce (fn [acc [u A]] (sn (get-in by-usage [:ext u]) acc (enc-exp A)))
          (sl :empty) ctx))

(defn enc-judg
  "The conclusion judgment of a derivation record."
  [{:keys [j ctx term type]}]
  (case j
    :has  (sn :has  (enc-ctx ctx) (sn :pair (enc-exp term) (enc-exp type)))
    :has0 (sn :has0 (enc-ctx ctx) (sn :pair (enc-exp term) (enc-exp type)))
    :type (sn :isType (enc-ctx ctx) (enc-exp type))))

(defn enc-deriv
  "⌜D⌝ for a derivation record: node(rule, judgment, list(premises)).  A
  conversion node carries its chain of types as one more list element."
  [{:keys [rule prems chain] :as d}]
  (sn rule (enc-judg d)
      (enc-list (concat (map enc-deriv prems)
                        (when (= rule :Conv) [(enc-list (map enc-exp chain))])))))

(def c-bot "c⊥ = ⌜0⌝." (sl :t0))

(defn neg-code "neg c = node(arrow₁, c, ⌜0⌝), the code of A ⊸ 0 when c = ⌜A⌝."
  [c]
  (sn :arrow1 c c-bot))

;; ---------------------------------------------------------------------------
;; Decoding.  dec-exp and friends throw on malformed input; dec-deriv returns
;; nil instead, so Check can treat every non-derivation as simply rejected.

(defn- bad [what c] (throw (ex-info (str "malformed " what) {:code c})))

(defn- node-of
  "Destructure an internal node with label l, or fail."
  [what l c]
  (if (and (vector? c) (= :sn (first c)) (= l (second c)) (= 4 (count c)))
    [(nth c 2) (nth c 3)]
    (bad what c)))

(defn- leaf-of [what l c]
  (when-not (= c (sl l)) (bad what c)))

(defn dec-list [c]
  (loop [c c acc []]
    (cond
      (= c nil-code) acc
      (and (vector? c) (= :sn (first c)) (= :cons (second c)) (= 4 (count c)))
      (recur (nth c 3) (conj acc (nth c 2)))
      :else (bad "list" c))))

(defn dec-nat [c]
  (loop [c c n 0]
    (cond
      (= c (sl :zero)) n
      :else (let [[i z] (node-of "index" :succ c)]
              (leaf-of "index" :nil z)
              (recur i (inc n))))))

(defn- dec-args
  "Decode k arguments nested as :args nodes (the inverse of enc-exp's args)."
  [k c]
  (if (= k 1)
    [c]
    (let [[a more] (node-of "arguments" :args c)]
      (into [a] (dec-args (dec k) more)))))

(defn dec-exp
  "The expression a code encodes; throws on a code that encodes none."
  [c]
  (when-not (vector? c) (bad "expression" c))
  (let [dec dec-exp
        unary (fn [l tag] (let [[a z] (node-of (name tag) l c)]
                            (leaf-of (name tag) :nil z)
                            [tag (dec a)]))]
    (case (first c)
      :sl (or (base-type-of (second c)) (nullary-term-of (second c)) (bad "expression" c))
      :sn (let [[_ l a b] c]
            (if-let [[fam u] (usage-of l)]
              (case fam
                :arrow [:Pi u (dec a) (dec b)]
                :sigma [:Sigma u (dec a) (dec b)]
                :lam [:lam u (dec a) (dec b)]
                (bad "expression" c))
              (case l
                :T (unary :T :T)
                :var (let [[i z] (node-of "var" :var c)] (leaf-of "var" :nil z) [:var (dec-nat i)])
                :abort [:abort (dec a) (dec b)]
                :if (let [[t f] (node-of "branches" :branches b)] [:if (dec a) (dec t) (dec f)])
                :elimBool (let [[bb br] (node-of "arguments" :args b)
                                [t1 t2] (node-of "branches" :branches br)]
                            [:elimBool (dec a) (dec bb) (dec t1) (dec t2)])
                :nsucc (unary :nsucc :succ)
                :recN (let [[z st n] (dec-args 3 b)] [:recN (dec a) (dec z) (dec st) (dec n)])
                :lblc (let [[l2 z] (node-of "label" :lblc c)]
                        (leaf-of "label" :nil z)
                        (if (and (= :sl (first l2)) (s/label? (second l2)))
                          [:lbl (second l2)]
                          (bad "label" c)))
                :caseLbl (let [[sc bs] (node-of "arguments" :args b)
                               bs (mapv dec (dec-list bs))]
                           (when (not= (count bs) (count s/labels)) (bad "caseLbl branches" c))
                           [:caseLbl (dec a) (dec sc) bs])
                :sleaf (unary :sleaf :sleaf)
                :snode (let [[c1 c2] (dec-args 2 b)] [:snode (dec a) (dec c1) (dec c2)])
                :recSyn (let [[tl tn sc] (dec-args 3 b)] [:recSyn (dec a) (dec tl) (dec tn) (dec sc)])
                :rleaf (unary :rleaf :leaf)
                :rnode (let [[lb r1 r2] (dec-args 3 b)] [:node (dec a) (dec lb) (dec r1) (dec r2)])
                :itR (let [[g h r] (dec-args 3 b)] [:itR (dec a) (dec g) (dec h) (dec r)])
                :print (unary :print :print)
                :app [:app (dec a) (dec b)]
                :tpair (let [[x y] (dec-args 2 b)] [:pair (dec a) (dec x) (dec y)])
                :let (let [[p t] (dec-args 2 b)] [:let (dec a) (dec p) (dec t)])
                :chk [:chk (dec a) (dec b)]
                :h1 (let [[s' cc e1 e2] (dec-args 4 b)] [:h1 (dec a) (dec s') (dec cc) (dec e1) (dec e2)])
                :reflect (let [[r ev] (dec-args 2 b)] [:reflect (dec a) (dec r) (dec ev)])
                :inspect (let [[r cc br] (dec-args 3 b)
                               [t1 t2] (node-of "branches" :branches br)]
                           [:inspect (dec a) (dec r) (dec cc) (dec t1) (dec t2)])
                (bad "expression" c))))
      (bad "expression" c))))

(defn dec-ctx [c]
  (loop [c c acc ()]
    (if (= c (sl :empty))
      (vec acc)
      (let [[fam u] (usage-of (second c))]
        (when-not (= fam :ext) (bad "context" c))
        (let [[rest-ctx A] (node-of "context" (second c) c)]
          (recur rest-ctx (cons [u (dec-exp A)] acc)))))))

(defn dec-judg
  "Decode a judgment into the judgment fields of a derivation record."
  [c]
  (case (second c)
    (:has :has0) (let [[g p] (node-of "judgment" (second c) c)
                       [t A] (node-of "judgment" :pair p)]
                   {:j (if (= :has (second c)) :has :has0)
                    :ctx (dec-ctx g) :term (dec-exp t) :type (dec-exp A)})
    :isType (let [[g A] (node-of "judgment" :isType c)]
              {:j :type :ctx (dec-ctx g) :type (dec-exp A)})
    (bad "judgment" c)))

(def rule-labels
  #{:Var :Unit :TT :FF :Zero :Lbl :Lam :App :Pair :Let :Abort :Conv
    :If :ElimBool :Succ :RecN :CaseLbl :SLeaf :SNode :RecSyn
    :Leaf :Node :ItR :Print :Chk :H1 :Reflect :Inspect
    :EmptyF :UnitF :BoolF :NatF :LblF :SynF :DiaF :RF :TF :PiF :SigmaF})

(defn- dec-deriv* [c]
  (let [rule (second c)]
    (when-not (contains? rule-labels rule) (bad "rule" c))
    (let [[j ps] (node-of "derivation" rule c)
          items (dec-list ps)
          [prems chain] (if (= rule :Conv)
                          (do (when (empty? items) (bad "conversion" c))
                              [(butlast items) (mapv dec-exp (dec-list (last items)))])
                          [items nil])]
      (cond-> (merge {:rule rule} (dec-judg j) {:prems (mapv dec-deriv* prems)})
        (= rule :Conv) (assoc :chain chain)))))

(defn dec-deriv
  "The derivation record a code encodes, or nil if it encodes none."
  [c]
  (try (when (s/code? c) (dec-deriv* c))
       (catch clojure.lang.ExceptionInfo _ nil)))
