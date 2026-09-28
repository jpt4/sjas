(ns lcert.typing
  "The type checker of λᶜᵉʳᵗ₀.  It follows the rule table of
  nachlass/refinement/R4-metatheory.md §1.4, and builds an explicit derivation
  (a derivation record, see lcert.encode) for every judgment it establishes.

  How usages are handled.  A context Γ is a vector of [usage type] entries,
  outermost first; it holds the usages *declared* by the binders.  Each
  derivation records its own conclusion context, and the rule table requires
  those recorded contexts to add up exactly: App concludes Γ₁ + ρΓ₂ from
  premises in Γ₁ and Γ₂, and so on.  The checker builds them bottom-up:

    - an ω-declared variable is recorded at ω everywhere (ω + ω = ω);
    - a 0-declared variable is recorded at 0 everywhere;
    - a 1-declared variable is recorded at 1 in the premise that uses it and
      at 0 elsewhere, so sums reveal double use (1 + 1 = ω), which is then
      rejected against the declared 1.

  Where the rules demand an exact context — a binder's own entry, the shared
  context of the branches of if, elimBool, caseLbl and inspect, and the root
  context Θₙ — an unused 1-declared variable is *raised* from 0 to 1 (`raise`).
  Raising pushes the extra usage into one premise that is not ω-scaled, down
  to an axiom, and axioms accept any context (the affine reading; this is the
  metatheory's admissible subusaging).

  Type-level judgments (mode 0) and formation judgments ignore usages; their
  contexts are recorded with every usage 0 (the metatheory's context
  convention, §1.4).

  Conversions are recorded as Conv nodes carrying a chain of types, one
  reduction step apart (lcert.reduce/conv-chain)."
  (:require [lcert.syntax :as s]
            [lcert.reduce :as r]
            [lcert.encode :as e]))

(defn- err [msg data]
  (throw (ex-info msg (assoc data :type :lcert/type-error))))

;; ---------------------------------------------------------------------------
;; Contexts: vectors of [usage type], outermost first.  Entry p's type is
;; relative to the entries before it.

(defn- lookup
  "Usage and type of variable i, the type made relative to all of Γ."
  [Γ i]
  (let [n (count Γ)]
    (when-not (< -1 i n) (err (str "unbound variable index " i) {:index i}))
    (let [[u A] (nth Γ (- n 1 i))]
      [u (s/shift A (inc i))])))

(defn- zero-ctx [Γ] (mapv (fn [[_ A]] [0 A]) Γ))

(defn- base-ctx
  "The recorded context of an axiom: ω-declared entries at ω, the rest at 0."
  [Γ]
  (mapv (fn [[u A]] [(if (= u :w) :w 0) A]) Γ))

(defn- ctx+ [& ctxs]
  (reduce (fn [a b] (mapv (fn [[u A] [v _]] [(s/u+ u v) A]) a b)) ctxs))

(defn- ctx* [rho ctx] (mapv (fn [[u A]] [(s/u* rho u) A]) ctx))

(defn- ctx-join [& ctxs]
  (reduce (fn [a b] (mapv (fn [[u A] [v _]] [(if (s/u<= u v) v u) A]) a b)) ctxs))

(defn- prefix [ctx n] (subvec ctx 0 n))

(defn- check-within
  "Reject a recorded context that uses some variable more than declared."
  [ctx Γ what]
  (doseq [i (range (count Γ))]
    (let [used (first (nth ctx i)) declared (first (nth Γ i))]
      (when-not (s/u<= used declared)
        (err (str "variable used beyond its declared usage in " what
                  ": index " (- (count Γ) 1 i) " declared " declared ", used " used)
             {:position i :declared declared :used used}))))
  ctx)

(defn- check-omega-scaled
  "An ω-scaled premise (an iteration method, the code of H₁ or inspect) may use
  no 1-declared variable: it may run many times."
  [d Γ what]
  (doseq [i (range (count Γ))]
    (when (and (= 1 (first (nth (:ctx d) i))) (= 1 (first (nth Γ i))))
      (err (str "a usage-1 variable is used in " what
                ", which may run more than once: index " (- (count Γ) 1 i))
           {:position i})))
  d)

;; ---------------------------------------------------------------------------
;; Raising a derivation's recorded context (admissible subusaging).

(def ^:private axiom-rules #{:Var :Unit :TT :FF :Zero :Lbl})

(def ^:private absorbing-premise
  "For each non-axiom runtime rule, the premise that absorbs a raised usage:
  one whose context is neither ω-scaled nor type-level."
  {:Lam 1 :App 0 :Pair 2 :Let 0 :Abort 0 :Conv 0 :If 0 :ElimBool 0 :Succ 0
   :RecN 0 :CaseLbl 0 :SLeaf 0 :SNode 0 :RecSyn 0 :Leaf 0 :Node 0 :ItR 3
   :Print 0 :Chk 0 :H1 0 :Reflect 0 :Inspect 0})

(defn raise
  "Return runtime derivation `d` with its recorded context raised to `target`,
  which must agree with it except for higher usages."
  [d target]
  (let [cur (:ctx d)]
    (cond
      (= cur target) d
      (axiom-rules (:rule d)) (assoc d :ctx target)
      :else
      (let [k (absorbing-premise (:rule d))
            p (nth (:prems d) k)
            ;; raise the premise at every position where the target differs;
            ;; positions are counted from the outermost entry, so they agree
            ;; between a conclusion and a premise with a longer context
            pctx (reduce (fn [pc i]
                           (if (= (nth cur i) (nth target i))
                             pc
                             (assoc pc i [(first (nth target i)) (second (nth pc i))])))
                         (:ctx p) (range (count target)))]
        (assoc d :ctx target :prems (assoc (:prems d) k (raise p pctx)))))))

(defn- raise-binders
  "Raise the innermost entries of d's context to the declared ones."
  [d declared]
  (let [ctx (:ctx d)
        n (- (count ctx) (count declared))]
    (raise d (into (prefix ctx n) declared))))

;; ---------------------------------------------------------------------------
;; Formation, conversion, and typing.

(declare synth conv)

(def ^:private base-type-rules
  {:Empty :EmptyF :Unit :UnitF :Bool :BoolF :Nat :NatF :Lbl :LblF
   :Syn :SynF :Dia :DiaF :R :RF})

(defn tf
  "A derivation of Γ ⊢ A type, recorded in the all-zero context."
  [Γ A]
  (let [z (zero-ctx Γ)
        node (fn [rule prems] {:rule rule :j :type :ctx z :type A :prems prems})]
    (if-let [rule (and (s/expr? A) (= 1 (count A)) (base-type-rules (first A)))]
      (node rule [])
      (case (and (s/expr? A) (first A))
        :T (node :TF [(conv (synth 0 Γ (second A)) [:Bool] Γ)])
        :Pi (let [[_ u A1 B] A] (node :PiF [(tf Γ A1) (tf (conj Γ [u A1]) B)]))
        :Sigma (let [[_ u A1 B] A] (node :SigmaF [(tf Γ A1) (tf (conj Γ [u A1]) B)]))
        (err (str "not a type: " (pr-str A)) {:type-expr A})))))

(defn conv
  "Derivation `d` retyped at `B` (well-formed in Γ): unchanged if its type is
  already B, else wrapped in a Conv node with a recorded chain."
  [d B Γ]
  (let [A (:type d)]
    (cond
      (= A B) d
      :else (if-let [chain (r/conv-chain A B)]
              {:rule :Conv :j (:j d) :ctx (:ctx d) :term (:term d) :type B
               :prems [d (tf Γ B)] :chain chain}
              (err (str "type mismatch: expected " (pr-str B) ", found " (pr-str A))
                   {:expected B :actual A :term (:term d)})))))

(defn- expect-head [d tag what]
  (let [A (:type d)]
    (when-not (and (s/expr? A) (= tag (first A)))
      (err (str what " expects a " (name tag) " type, found " (pr-str A))
           {:actual A :term (:term d)}))
    A))

(defn synth
  "The derivation of `t` in context Γ, in mode m (1 = runtime, 0 = type
  level).  Its :type is the synthesized type."
  [m Γ t]
  (let [rt (= m 1)
        z (zero-ctx Γ)
        j (if rt :has :has0)
        ;; a runtime premise of the same mode, and its expected-type variant
        sub (fn [t'] (synth m Γ t'))
        sub-at (fn [t' B] (conv (synth m Γ t') B Γ))
        ;; the conclusion context of a node whose runtime premises add up
        total (fn [what & ctxs] (if rt (check-within (apply ctx+ ctxs) Γ what) z))
        axiom (fn [rule type] {:rule rule :j j :ctx (if rt (base-ctx Γ) z)
                               :term t :type type :prems []})
        mk (fn [rule ctx type prems] {:rule rule :j j :ctx ctx :term t :type type :prems prems})
        ;; ω-scaled premises: typed in Γ, then required to use no 1-variable
        omega-sub (fn [t' B what]
                    (let [d (conv (synth m Γ t') B Γ)]
                      (if rt (check-omega-scaled d Γ what) d)))
        n (count Γ)]
    (when-not (s/expr? t) (err (str "not a term: " (pr-str t)) {:term t}))
    (case (first t)
      :var (let [i (second t) [u A] (lookup Γ i)]
             (when (and rt (= u 0))
               (err (str "erased variable used at runtime: index " i) {:index i}))
             {:rule :Var :j j :term t :type A :prems []
              ;; recorded: this variable at 1 (or ω if so declared), the
              ;; other entries as in an axiom; the entry keeps its own type,
              ;; relative to its prefix, as every recorded context does
              :ctx (if rt
                     (assoc (base-ctx Γ) (- n 1 i) [(if (= u :w) :w 1) (second (nth Γ (- n 1 i)))])
                     z)})
      :star (axiom :Unit [:Unit])
      :tt (axiom :TT [:Bool])
      :ff (axiom :FF [:Bool])
      :zero (axiom :Zero [:Nat])
      :lbl (do (when-not (s/label? (second t)) (err "not a label" {:term t}))
               (axiom :Lbl [:Lbl]))

      :lam (let [[_ u A body] t
                 dA (tf Γ A)
                 Γx (conj Γ [u A])
                 db (synth m Γx body)
                 db (if rt (raise-binders db [[u A]]) db)]
             (mk :Lam (if rt (prefix (:ctx db) n) z) [:Pi u A (:type db)] [dA db]))

      :app (let [[_ f a] t
                 df (sub f)
                 [_ u A B] (expect-head df :Pi "application")
                 da (if (= u 0) (conv (synth 0 Γ a) A Γ) (sub-at a A))]
             (mk :App (if (= u 0) (total "an application" (:ctx df))
                          (total "an application" (:ctx df) (ctx* u (:ctx da))))
                 (s/subst B a) [df da]))

      :pair (let [[_ S a b] t
                  _ (when-not (and (s/expr? S) (= :Sigma (first S)))
                      (err "a pair is annotated with its Σ type" {:term t}))
                  [_ u A B] S
                  dS (tf Γ S)
                  da (if (= u 0) (conv (synth 0 Γ a) A Γ) (sub-at a A))
                  db (sub-at b (s/subst B a))]
              (mk :Pair (if (= u 0) (total "a pair" (:ctx db))
                            (total "a pair" (ctx* u (:ctx da)) (:ctx db)))
                  S [dS da db]))

      :let (let [[_ C p body] t
                 dp (sub p)
                 [_ u A B] (expect-head dp :Sigma "let")
                 dC (tf Γ C)
                 Γxy (conj Γ [u A] [1 B])
                 db (conv (synth m Γxy body) (s/shift C 2) Γxy)
                 db (if rt (raise-binders db [[u A] [1 B]]) db)]
             (mk :Let (total "let" (:ctx dp) (if rt (prefix (:ctx db) n) z)) C [dp dC db]))

      :abort (let [[_ A t'] t
                   dt (sub-at t' [:Empty])]
               (mk :Abort (if rt (:ctx dt) z) A [dt (tf Γ A)]))

      :if (let [[_ b t1 t2] t
                db (sub-at b [:Bool])
                d1 (sub t1)
                d2 (sub-at t2 (:type d1))
                [d1 d2 g2] (if rt (let [g (ctx-join (:ctx d1) (:ctx d2))]
                                    [(raise d1 g) (raise d2 g) g])
                               [d1 d2 z])]
            (mk :If (total "if" (:ctx db) g2) (:type d1) [db d1 d2]))

      :elimBool (let [[_ P b t1 t2] t
                      db (sub-at b [:Bool])
                      dP (tf (conj Γ [0 [:Bool]]) P)
                      d1 (sub-at t1 (s/subst P [:tt]))
                      d2 (sub-at t2 (s/subst P [:ff]))
                      [d1 d2 g2] (if rt (let [g (ctx-join (:ctx d1) (:ctx d2))]
                                          [(raise d1 g) (raise d2 g) g])
                                     [d1 d2 z])]
                  (mk :ElimBool (total "elimBool" (:ctx db) g2) (s/subst P b) [db dP d1 d2]))

      :succ (let [dn (sub-at (second t) [:Nat])]
              (mk :Succ (if rt (:ctx dn) z) [:Nat] [dn]))

      :recN (let [[_ P z0 st nn] t
                  dn (sub-at nn [:Nat])
                  dP (tf (conj Γ [0 [:Nat]]) P)
                  dz (sub-at z0 (s/subst P [:zero]))
                  Γs (conj Γ [:w [:Nat]] [1 P])
                  ds (conv (synth m Γs st) (s/subst (s/shift P 2 1) [:succ [:var 1]]) Γs)
                  ds (if rt (-> (raise-binders ds [[:w [:Nat]] [1 P]])
                                (check-omega-scaled Γ "the step of recN"))
                         ds)]
              (mk :RecN (total "recN" (:ctx dn) (:ctx dz) (if rt (prefix (:ctx ds) n) z))
                  (s/subst P nn) [dn dP dz ds]))

      :caseLbl (let [[_ P a bs] t
                     _ (when-not (= (count bs) (count s/labels))
                         (err "caseLbl needs one branch per label" {:term t}))
                     da (sub-at a [:Lbl])
                     dP (tf (conj Γ [0 [:Lbl]]) P)
                     dbs (mapv (fn [l b] (sub-at b (s/subst P [:lbl l]))) s/labels bs)
                     [dbs g2] (if rt (let [g (apply ctx-join (map :ctx dbs))]
                                       [(mapv #(raise % g) dbs) g])
                                  [dbs z])]
                 (mk :CaseLbl (total "caseLbl" (:ctx da) g2) (s/subst P a) (into [da dP] dbs)))

      :sleaf (let [da (sub-at (second t) [:Lbl])]
               (mk :SLeaf (if rt (:ctx da) z) [:Syn] [da]))

      :snode (let [[_ a c1 c2] t
                   da (sub-at a [:Lbl]) d1 (sub-at c1 [:Syn]) d2 (sub-at c2 [:Syn])]
               (mk :SNode (total "snode" (:ctx da) (:ctx d1) (:ctx d2)) [:Syn] [da d1 d2]))

      :recSyn (let [[_ P tl tn c] t
                    dc (sub-at c [:Syn])
                    dP (tf (conj Γ [0 [:Syn]]) P)
                    ;; the leaf method: context Γ, a :ω Lbl
                    Γl (conj Γ [:w [:Lbl]])
                    dl (conv (synth m Γl tl) (s/subst (s/shift P 1 1) [:sleaf [:var 0]]) Γl)
                    ;; the node method: Γ, a c1 c2 :ω, y1 :₁ P[c1], y2 :₁ P[c2]
                    P1 (s/subst (s/shift P 3 1) [:var 1])
                    P2 (s/subst (s/shift P 4 1) [:var 1])
                    Γn (conj Γ [:w [:Lbl]] [:w [:Syn]] [:w [:Syn]] [1 P1] [1 P2])
                    dn (conv (synth m Γn tn)
                             (s/subst (s/shift P 5 1) [:snode [:var 4] [:var 3] [:var 2]]) Γn)
                    [dl dn] (if rt
                              [(check-omega-scaled dl Γ "the leaf method of recSyn")
                               (-> (raise-binders dn [[:w [:Lbl]] [:w [:Syn]] [:w [:Syn]] [1 P1] [1 P2]])
                                   (check-omega-scaled Γ "the node method of recSyn"))]
                              [dl dn])]
                (mk :RecSyn (total "recSyn" (:ctx dc)
                                   (if rt (prefix (:ctx dl) n) z) (if rt (prefix (:ctx dn) n) z))
                    (s/subst P c) [dc dP dl dn]))

      :leaf (let [da (sub-at (second t) [:Lbl])]
              (mk :Leaf (if rt (:ctx da) z) [:R] [da]))

      :node (let [[_ d a r1 r2] t
                  dd (sub-at d [:Dia]) da (sub-at a [:Lbl])
                  d1 (sub-at r1 [:R]) d2 (sub-at r2 [:R])]
              (mk :Node (total "a certificate node" (:ctx dd) (:ctx da) (:ctx d1) (:ctx d2))
                  [:R] [dd da d1 d2]))

      :itR (let [[_ X g h rr] t
                 dX (tf Γ X)
                 dg (omega-sub g [:Pi :w [:Lbl] (s/shift X 1)] "the leaf method of itR")
                 dh (omega-sub h [:Pi 1 [:Dia] [:Pi :w [:Lbl] [:Pi 1 (s/shift X 2)
                                                             [:Pi 1 (s/shift X 3) (s/shift X 4)]]]]
                               "the node method of itR")
                 dr (sub-at rr [:R])]
             (mk :ItR (total "itR" (:ctx dg) (:ctx dh) (:ctx dr)) X [dX dg dh dr]))

      :print (let [dr (sub-at (second t) [:R])]
               (mk :Print (if rt (:ctx dr) z) [:Syn] [dr]))

      :chk (let [[_ c d] t
                 dc (sub-at c [:Syn]) dd (sub-at d [:Syn])]
             (mk :Chk (total "chk" (:ctx dc) (:ctx dd)) [:Bool] [dc dd]))

      :h1 (let [[_ rr ss c e1 e2] t
                dr (sub-at rr [:R])
                ds (sub-at ss [:R])
                dc (omega-sub c [:Syn] "the code argument of H1")
                de1 (sub-at e1 [:T [:chk [:print rr] c]])
                de2 (sub-at e2 [:T [:chk [:print ss] (s/neg-term c)]])]
            (mk :H1 (total "H1" (:ctx dr) (:ctx ds) (:ctx dc) (:ctx de1) (:ctx de2))
                [:Empty] [dr ds dc de1 de2]))

      :reflect (let [[_ D rr ev] t
                     _ (when-not (s/base-data-types D)
                         (err (str "reflect targets base data types only, not " (pr-str D)) {:term t}))
                     dr (sub-at rr [:R])
                     de (sub-at ev [:T [:chk [:print rr] (s/code->term (e/enc-exp D))]])]
                 (mk :Reflect (total "reflect" (:ctx dr) (:ctx de)) D [dr de]))

      :inspect (let [[_ X rr c t1 t2] t
                     dr (sub-at rr [:R])
                     dc (omega-sub c [:Syn] "the code argument of inspect")
                     dX (tf Γ X)
                     ok [:chk [:print [:var 0]] (s/shift c 1)]
                     b1 [[1 [:R]] [1 [:T ok]]]
                     b2 [[1 [:R]] [1 [:T (s/not-term ok)]]]
                     Γ1 (into Γ b1) Γ2 (into Γ b2)
                     d1 (conv (synth m Γ1 t1) (s/shift X 2) Γ1)
                     d2 (conv (synth m Γ2 t2) (s/shift X 2) Γ2)
                     [d1 d2 g2] (if rt
                                  (let [d1 (raise-binders d1 b1)
                                        d2 (raise-binders d2 b2)
                                        g (ctx-join (prefix (:ctx d1) n) (prefix (:ctx d2) n))]
                                    [(raise d1 (into g b1)) (raise d2 (into g b2)) g])
                                  [d1 d2 z])]
                 (mk :Inspect (total "inspect" (:ctx dr) (:ctx dc) g2) X [dr dc dX d1 d2]))

      (err (str "unknown term former " (first t)) {:term t}))))

(defn token-context "Θₙ: n tokens, each at usage 1." [n] (vec (repeat n [1 [:Dia]])))

(defn check-top
  "The derivation of Θₙ ⊢ t :¹ A, its root context exactly Θₙ.  Throws a
  type error (ex-data :type :lcert/type-error) if t is ill-typed."
  [n t]
  (let [Γ (token-context n)]
    (raise (synth 1 Γ t) Γ)))
