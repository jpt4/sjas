(ns lcert.syntax
  "Abstract syntax of λᶜᵉʳᵗ₀, the core certificate calculus of
  nachlass/refinement/R4-metatheory.md (§1), and its surface parser.

  READ THIS FIRST.  λᶜᵉʳᵗ₀ is a small dependently typed language with
  *usages* (0 = erased, 1 = at most once at runtime, ω = unrestricted).
  Programs may build *codes* (free syntax trees, type Syn) and *certificates*
  (the same trees, type R, where every internal node consumes one *token* of
  the resource type ◇).  The language has a built-in checker `chk′` that
  decides whether a code encodes a derivation of the language itself, and
  constants (H₁, reflect, whose case D = 0 is H) that let a program rely on
  the language's own consistency.

  Representation.  Types and terms share one representation: a vector whose
  first element is a keyword tag.  Variables are de Bruijn indices: [:var 0]
  is the innermost bound variable.  Usages are 0, 1 or :w.

    Types   [:Empty] [:Unit] [:Bool] [:Nat] [:Lbl] [:Syn] [:Dia] [:R]
            [:T b]  [:Pi u A B]  [:Sigma u A B]        (B binds one variable)
    Terms   [:var i] [:star] [:abort A t]
            [:tt] [:ff] [:if b t e] [:elimBool P b t1 t2]        (P binds 1)
            [:zero] [:succ n] [:recN P z s n]          (P binds 1, s binds 2)
            [:lbl l] [:caseLbl P a [t_l ...]]    (one branch per label of L)
            [:sleaf a] [:snode a c1 c2] [:recSyn P tl tn c]
                                            (P binds 1, tl binds 1, tn binds 5)
            [:leaf a] [:node d a r1 r2] [:itR X g h r] [:print r]
            [:lam u A t] [:app f u]                          (t binds 1)
            [:pair S a b] [:let C p t]          (S is the Σ type; t binds 2)
            [:chk c d] [:h1 r s c e1 e2] [:reflect D r e]
            [:inspect X r c t1 t2]                   (t1 and t2 bind 2 each)

  Binding order.  Where a position binds several variables, the first-named
  is outermost.  recN's step s sees [:var 1] = the predecessor x and
  [:var 0] = the recursive result y; recSyn's node method sees, from outermost,
  the label a, the subcodes c1 c2, and the recursive results y1 y2; let's body
  sees x ([:var 1]) and y ([:var 0]); inspect's branches see the certificate
  x ([:var 1]) and the evidence e ([:var 0]).

  Code values (the runtime values of Syn, and what the encoding produces) are
  [:sl l] for a leaf and [:sn l c1 c2] for an internal node, l a label.")

;; ---------------------------------------------------------------------------
;; Usages: the semiring {0, 1, ω} of R4-metatheory.md §1.1.

(def usages #{0 1 :w})

(defn u+
  "Usage addition: 0 + ρ = ρ, 1 + 1 = ω, ω + ρ = ω."
  [a b]
  (cond (= a 0) b
        (= b 0) a
        :else :w))

(defn u*
  "Usage multiplication: 0·ρ = 0, 1·ρ = ρ, ω·ω = ω (so ω·1 = ω, ω·0 = 0)."
  [a b]
  (cond (or (= a 0) (= b 0)) 0
        (= a 1) b
        (= b 1) a
        :else :w))

(defn u<=
  "The affine order 0 ≤ 1 ≤ ω."
  [a b]
  (let [rank {0 0, 1 1, :w 2}]
    (<= (rank a) (rank b))))

;; ---------------------------------------------------------------------------
;; The label set L.  Codes are trees over L, and derivations are encoded with
;; these labels, so L must contain every label the encoding uses
;; (lcert.encode).  A few further labels are left for user programs.  The
;; order is fixed: it gives caseLbl its branch order and the Ansatz kernel its
;; numbering of labels.

(def encoding-labels
  [;; lists, unary numbers, contexts, judgments
   :nil :cons :zero :succ :empty :ext0 :ext1 :extw
   :has :has0 :pair :isType :args :branches :var
   ;; types
   :t0 :t1 :bool :nat :lbl :syn :dia :cert :T
   :arrow0 :arrow1 :arrow :sigma0 :sigma1 :sigmaw
   ;; terms
   :star :abort :tt :ff :if :elimBool :nzero :nsucc :recN :lblc :caseLbl
   :sleaf :snode :recSyn :rleaf :rnode :itR :print
   :lam0 :lam1 :lam :app :tpair :let :chk :h1 :reflect :inspect
   ;; rules
   :Var :Unit :TT :FF :Zero :Lbl :Lam :App :Pair :Let :Abort :Conv
   :If :ElimBool :Succ :RecN :CaseLbl :SLeaf :SNode :RecSyn
   :Leaf :Node :ItR :Print :Chk :H1 :Reflect :Inspect
   :EmptyF :UnitF :BoolF :NatF :LblF :SynF :DiaF :RF :TF :PiF :SigmaF])

(def user-labels
  "Labels with no role in the encoding, free for programs to use."
  [:a :b :c])

(def labels (vec (concat encoding-labels user-labels)))

(def label-index
  "Map (usable as a function) from a label to its position in `labels`."
  (into {} (map-indexed (fn [i l] [l i]) labels)))

(defn label? [x] (contains? label-index x))

;; ---------------------------------------------------------------------------
;; Binding structure, and the de Bruijn operations built on it.

(def binders
  "For each tag, how many variables each argument position binds.  Positions
  count the arguments after the tag.  Tags absent here bind nothing."
  {:Pi       [0 0 1]      ; [:Pi u A B]
   :Sigma    [0 0 1]      ; [:Sigma u A B]
   :lam      [0 0 1]      ; [:lam u A t]
   :elimBool [1 0 0 0]    ; [:elimBool P b t1 t2]
   :recN     [1 0 2 0]    ; [:recN P z s n]
   :caseLbl  [1 0 0]      ; [:caseLbl P a branches]
   :recSyn   [1 1 5 0]    ; [:recSyn P tl tn c]
   :let      [0 0 2]      ; [:let C p t]
   :inspect  [0 0 0 2 2]}) ; [:inspect X r c t1 t2]

(defn expr?
  "True for a type or term: a vector headed by a keyword tag."
  [x]
  (and (vector? x) (keyword? (first x))))

(defn map-vars
  "Rebuild `e`, replacing every variable occurrence [:var i] that sits under
  `d` binders (counted from where the walk started) by (g i d).  This one
  traversal implements shifting, substitution and free-variable collection."
  ([e g] (map-vars e g 0))
  ([e g d]
   (let [tag (first e)]
     (case tag
       :var (g (second e) d)
       :lbl e
       (let [bs (binders tag)]
         (into [tag]
               (map-indexed
                (fn [pos arg]
                  (let [k (+ d (if bs (nth bs pos 0) 0))]
                    (cond
                      (expr? arg) (map-vars arg g k)
                      ;; caseLbl's branch vector: a plain vector of terms
                      (vector? arg) (mapv #(map-vars % g k) arg)
                      ;; usages, and anything else that is not syntax
                      :else arg)))
                (rest e))))))))

(defn shift
  "Add `amount` to every free index of `e` that is at least `cutoff`."
  ([e amount] (shift e amount 0))
  ([e amount cutoff]
   (if (zero? amount)
     e
     (map-vars e (fn [i d] (if (>= i (+ cutoff d)) [:var (+ i amount)] [:var i]))))))

(defn subst-many
  "Substitute the innermost (count vs) free variables of `e` simultaneously.
  `vs` is in binding order, outermost first: (last vs) replaces [:var 0].
  Remaining free indices are lowered by (count vs)."
  [e vs]
  (let [k (count vs)]
    (map-vars e (fn [i d]
                  (cond
                    (< i d) [:var i]
                    (< i (+ d k)) (shift (nth vs (- k 1 (- i d))) d)
                    :else [:var (- i k)])))))

(defn subst
  "Substitute `v` for [:var 0] in `e`, lowering the other free indices."
  [e v]
  (subst-many e [v]))

(defn free-vars
  "The set of free de Bruijn indices of `e`, relative to its own top."
  [e]
  (let [acc (volatile! #{})]
    (map-vars e (fn [i d] (when (>= i d) (vswap! acc conj (- i d))) [:var i]))
    @acc))

(defn closed? [e] (empty? (free-vars e)))

;; ---------------------------------------------------------------------------
;; Code values and code terms.

(defn code?
  "True for a code value: [:sl l] or [:sn l c1 c2] with labels from L."
  [c]
  (and (vector? c)
       (case (first c)
         :sl (and (= 2 (count c)) (label? (second c)))
         :sn (and (= 4 (count c)) (label? (second c)) (code? (nth c 2)) (code? (nth c 3)))
         false)))

(defn code->term
  "The closed canonical term that denotes the code value `c`."
  [c]
  (case (first c)
    :sl [:sleaf [:lbl (second c)]]
    :sn [:snode [:lbl (second c)] (code->term (nth c 2)) (code->term (nth c 3))]))

(defn term->code
  "The code a closed canonical code term denotes, or nil if `t` is not one.
  Canonical code terms are built from sleaf, snode and label constants only;
  they are what the δ-rule for chk′ requires (R4-metatheory.md §1.5)."
  [t]
  (when (expr? t)
    (case (first t)
      :sleaf (let [[_ a] t]
               (when (and (expr? a) (= :lbl (first a))) [:sl (second a)]))
      :snode (let [[_ a c1 c2] t]
               (when (and (expr? a) (= :lbl (first a)))
                 (let [k1 (term->code c1) k2 (term->code c2)]
                   (when (and k1 k2) [:sn (second a) k1 k2]))))
      nil)))

(defn canonical-code-term? [t] (some? (term->code t)))

;; ---------------------------------------------------------------------------
;; Derived forms used by the typing rules and by programs.

(def c-bot-term
  "c⊥ = ⌜0⌝, the code of the empty type, as a term."
  [:sleaf [:lbl :t0]])

(defn neg-term
  "neg c = snode arrow₁ c ⌜0⌝: the code of A ⊸ 0 when c is the code of A."
  [c]
  [:snode [:lbl :arrow1] c c-bot-term])

(defn not-term [b] [:if b [:ff] [:tt]])

(defn and-term [a b] [:if a b [:ff]])

(def base-data-types
  "The types reflect may target (R4-metatheory.md §1.2): T(b) is excluded."
  #{[:Empty] [:Unit] [:Bool] [:Nat] [:Lbl] [:Syn] [:R]})

;; ---------------------------------------------------------------------------
;; The surface syntax: EDN forms with named variables.
;;
;;   Types: Void Unit Bool Nat Lbl Syn Dia R   (T b)
;;          (Pi [x u A] B)  (Sigma [x u A] B)       u one of 0 1 w
;;          (-> A B ...) = Π at ω, (-o A B ...) = Π at 1   (right-nested)
;;          (tensor A B) = Σ at 1,  (prod A B) = Σ at ω    (non-dependent)
;;   Terms: x  star tt ff zero  n (a numeral)  :label  c-bot
;;          (fn [x u A ...] body)   (f a b ...)   (if b t e)
;;          (elim-bool [x P] b t1 t2)   (succ n)   (rec-nat [x P] z [x y] s n)
;;          (case-lbl [x P] a {:l t ... :else t})
;;          (sleaf a) (snode a c1 c2) (rec-syn [x P] [a] tl [a c1 c2 y1 y2] tn c)
;;          (leaf a) (node d a r1 r2) (itr X g h r) (print r)
;;          (pair S a b) (let-pair C [x y] p t) (abort A t)
;;          (chk c d) (H1 r s c e1 e2) (reflect D r e) (H r e)
;;          (inspect X r c [x e] t1 [x e] t2)
;;          (code A) = the literal code of the closed type A
;;          (code-literal c) = the literal term of a code value c, e.g. [:sl :a]
;;          (neg c) (not b) (and a b)
;;   A program's tokens are named $1 .. $n (see token-scope).

(defn- fail [msg data] (throw (ex-info msg data)))

(defn token-scope
  "The scope of a program run with budget n: tokens $1 .. $n, $n innermost."
  [n]
  (mapv #(symbol (str "$" %)) (range 1 (inc n))))

(defn- lookup [scope sym]
  (let [i (.lastIndexOf ^java.util.List scope sym)]
    (when (neg? i) (fail (str "unbound variable " sym) {:sym sym :scope scope}))
    [:var (- (count scope) 1 i)]))

(defn- parse-usage [u]
  (case u
    0 0, 1 1, w :w, :w :w
    (fail (str "not a usage: " u " (use 0, 1 or w)") {:usage u})))

(declare parse-term)

(defn parse-type
  "Parse a surface type in `scope` (a vector of names, innermost last)."
  [scope form]
  (cond
    (symbol? form)
    (case form
      Void [:Empty], Unit [:Unit], Bool [:Bool], Nat [:Nat]
      Lbl [:Lbl], Syn [:Syn], Dia [:Dia], R [:R]
      (fail (str "unknown type " form) {:form form}))

    (seq? form)
    (let [[head & args] form
          binder (fn [tag [[x u a] body]]
                   [tag (parse-usage u) (parse-type scope a)
                    (parse-type (conj scope x) body)])
          arrow (fn arrow [tag u types]
                  (if (= 1 (count types))
                    (parse-type scope (first types))
                    [tag u (parse-type scope (first types))
                     ;; the codomain sits under the binder: extend the scope
                     ;; with a name no program can write
                     (parse-type (conj scope '_) (cons head (rest types)))]))]
      (case head
        T (do (when (not= 1 (count args)) (fail "T takes one argument" {:form form}))
              [:T (parse-term scope (first args))])
        Pi (binder :Pi args)
        Sigma (binder :Sigma args)
        -> (arrow :Pi :w args)
        -o (arrow :Pi 1 args)
        tensor [:Sigma 1 (parse-type scope (first args)) (parse-type (conj scope '_) (second args))]
        prod [:Sigma :w (parse-type scope (first args)) (parse-type (conj scope '_) (second args))]
        (fail (str "unknown type former " head) {:form form})))

    :else (fail (str "not a type: " (pr-str form)) {:form form})))

(defn- numeral [n] (nth (iterate (fn [t] [:succ t]) [:zero]) n))

(defn- parse-lbl-branches
  "caseLbl needs a branch for every label; a map may give some explicitly and
  the rest through :else."
  [scope m]
  (let [explicit (dissoc m :else)]
    (doseq [l (keys explicit)]
      (when-not (label? l) (fail (str "not a label: " l) {:label l})))
    (mapv (fn [l]
            (let [f (get explicit l (get m :else ::missing))]
              (when (= f ::missing) (fail (str "case-lbl misses label " l) {:label l}))
              (parse-term scope f)))
          labels)))

(defn- parse-fn [scope [bs & body]]
  (when (not= 1 (count body)) (fail "fn takes one body" {:binders bs}))
  (when-not (and (vector? bs) (pos? (count bs)) (zero? (mod (count bs) 3)))
    (fail "fn binders are [x u A ...]" {:binders bs}))
  (let [[x u a & more] bs
        inner (if (seq more) (list* 'fn (vec more) body) (first body))]
    [:lam (parse-usage u) (parse-type scope a) (parse-term (conj scope x) inner)]))

(defn parse-term
  "Parse a surface term in `scope` (a vector of names, innermost last)."
  [scope form]
  (cond
    (symbol? form)
    (case form
      star [:star], tt [:tt], ff [:ff], zero [:zero]
      c-bot c-bot-term
      (lookup scope form))

    (keyword? form)
    (if (label? form) [:lbl form] (fail (str "not a label: " form) {:form form}))

    (and (integer? form) (not (neg? form))) (numeral form)

    (seq? form)
    (let [[head & args] form
          p (fn [x] (parse-term scope x))
          ty (fn [x] (parse-type scope x))
          under (fn [names x] (parse-term (into scope names) x))
          motive (fn [[x P]] (parse-type (conj scope x) P))]
      (case head
        fn (parse-fn scope args)
        if (let [[b t e] args] [:if (p b) (p t) (p e)])
        elim-bool (let [[bm b t1 t2] args] [:elimBool (motive bm) (p b) (p t1) (p t2)])
        succ [:succ (p (first args))]
        rec-nat (let [[bm z [x y] s n] args]
                  [:recN (motive bm) (p z) (under [x y] s) (p n)])
        case-lbl (let [[bm a m] args]
                   [:caseLbl (motive bm) (p a) (parse-lbl-branches scope m)])
        sleaf [:sleaf (p (first args))]
        snode (let [[a c1 c2] args] [:snode (p a) (p c1) (p c2)])
        rec-syn (let [[bm [a] tl [a2 c1 c2 y1 y2] tn c] args]
                  [:recSyn (motive bm) (under [a] tl) (under [a2 c1 c2 y1 y2] tn) (p c)])
        leaf [:leaf (p (first args))]
        node (let [[d a r1 r2] args] [:node (p d) (p a) (p r1) (p r2)])
        itr (let [[X g h r] args] [:itR (ty X) (p g) (p h) (p r)])
        print [:print (p (first args))]
        pair (let [[S a b] args] [:pair (ty S) (p a) (p b)])
        let-pair (let [[C [x y] pr t] args] [:let (ty C) (p pr) (under [x y] t)])
        abort (let [[A t] args] [:abort (ty A) (p t)])
        chk (let [[c d] args] [:chk (p c) (p d)])
        H1 (let [[r s c e1 e2] args] [:h1 (p r) (p s) (p c) (p e1) (p e2)])
        reflect (let [[D r e] args] [:reflect (ty D) (p r) (p e)])
        H (let [[r e] args] [:reflect [:Empty] (p r) (p e)])
        inspect (let [[X r c [x1 e1] t1 [x2 e2] t2] args]
                  [:inspect (ty X) (p r) (p c) (under [x1 e1] t1) (under [x2 e2] t2)])
        code-literal (let [c (first args)]
                       (when-not (code? c) (fail "code-literal needs a code value" {:form form}))
                       (code->term c))
        code (let [A (ty (first args))]
               (when-not (closed? A) (fail "code needs a closed type" {:form form}))
               (code->term ((requiring-resolve 'lcert.encode/enc-exp) A)))
        neg (neg-term (p (first args)))
        not (not-term (p (first args)))
        and (and-term (p (first args)) (p (second args)))
        ;; anything else is an application, curried and left-nested
        (reduce (fn [f a] [:app f (p a)]) (p head) args)))

    :else (fail (str "not a term: " (pr-str form)) {:form form})))
