(ns lcert.check
  "Check(c, d), the language's own proof checker (R4-metatheory.md §1.6).

  Check(c, d) is true iff:
    1. the code c decodes to an explicit derivation (lcert.encode/dec-deriv)
       all of whose nodes are valid instances of the rule table;
    2. its root judgment is Θₘ ⊢ t :¹ A, a runtime judgment whose context is
       m tokens at usage 1;
    3. A is closed;
    4. the encoding of A is the code d.

  This namespace re-checks every rule instance *locally*, from the recorded
  judgments alone.  It shares no logic with the type checker (lcert.typing),
  which builds derivations; the tests check that the two agree.

  Conversion nodes carry chains of types.  Each chain element must be well
  formed at the level of simple-type skeletons (review R4-01: otherwise a
  chain could pass through an expression the model cannot interpret), and
  consecutive elements must be one reduction step apart.  A δ-step inside a
  chain calls Check on a strictly smaller code (E4), so Check terminates.

  Every code Check accepts satisfies strict overhead, budget < nodes
  (Lemma 2.7).  That is a theorem about the encoding, and Check asserts it:
  a violation is reported as a defect of the implementation, never as a
  rejection."
  (:require [lcert.syntax :as s]
            [lcert.encode :as e]
            [lcert.reduce :as r]
            [lcert.kernel :as k]))

;; ---------------------------------------------------------------------------
;; Simple-type skeletons (R4-metatheory.md Lemma 2.5).  Skeletons contain no
;; terms, so a skeleton context needs no shifting.

(defn skel
  "The skeleton of a type: 0, 1 and T(b) become Unit; Π and Σ become Fn and Prod."
  [A]
  (case (first A)
    (:Empty :Unit :T) [:Unit]
    (:Bool :Nat :Lbl :Syn :Dia :R) [(first A)]
    :Pi [:Fn (skel (nth A 2)) (skel (nth A 3))]
    :Sigma [:Prod (skel (nth A 2)) (skel (nth A 3))]))

(declare stype)

(defn skel-wf?
  "Is A a type whose terms are all simply typed, in skeleton context S?"
  [S A]
  (case (first A)
    (:Empty :Unit :Bool :Nat :Lbl :Syn :Dia :R) (= 1 (count A))
    :T (= [:Bool] (stype S (second A)))
    (:Pi :Sigma) (let [[_ u A1 B] A]
                   (and (s/usages u) (skel-wf? S A1) (skel-wf? (conj S (skel A1)) B)))
    false))

(defn stype
  "The skeleton type of term t in skeleton context S (innermost last), or nil."
  [S t]
  (let [ty (fn [x] (stype S x))
        is (fn [x sk] (= sk (stype S x)))
        wf (fn [A] (skel-wf? S A))
        under (fn [sks x] (stype (into S sks) x))]
    (case (first t)
      :var (let [i (second t)] (when (and (int? i) (< -1 i (count S))) (nth S (- (count S) 1 i))))
      :star [:Unit]
      (:tt :ff) [:Bool]
      :zero [:Nat]
      :lbl (when (s/label? (second t)) [:Lbl])
      :succ (when (is (second t) [:Nat]) [:Nat])
      :abort (let [[_ A x] t] (when (and (wf A) (is x [:Unit])) (skel A)))
      :if (let [[_ b x y] t sx (ty x)] (when (and sx (is b [:Bool]) (is y sx)) sx))
      :elimBool (let [[_ P b x y] t]
                  (when (skel-wf? (conj S [:Bool]) P)
                    (let [sp (skel P)] (when (and (is b [:Bool]) (is x sp) (is y sp)) sp))))
      :recN (let [[_ P z st nn] t]
              (when (skel-wf? (conj S [:Nat]) P)
                (let [sp (skel P)]
                  (when (and (is nn [:Nat]) (is z sp) (= sp (under [[:Nat] sp] st))) sp))))
      :caseLbl (let [[_ P a bs] t]
                 (when (and (skel-wf? (conj S [:Lbl]) P) (vector? bs) (= (count bs) (count s/labels)))
                   (let [sp (skel P)] (when (and (is a [:Lbl]) (every? #(is % sp) bs)) sp))))
      :sleaf (when (is (second t) [:Lbl]) [:Syn])
      :snode (let [[_ a c1 c2] t] (when (and (is a [:Lbl]) (is c1 [:Syn]) (is c2 [:Syn])) [:Syn]))
      :recSyn (let [[_ P tl tn c] t]
                (when (skel-wf? (conj S [:Syn]) P)
                  (let [sp (skel P)]
                    (when (and (is c [:Syn]) (= sp (under [[:Lbl]] tl))
                               (= sp (under [[:Lbl] [:Syn] [:Syn] sp sp] tn)))
                      sp))))
      :leaf (when (is (second t) [:Lbl]) [:R])
      :node (let [[_ d a r1 r2] t]
              (when (and (is d [:Dia]) (is a [:Lbl]) (is r1 [:R]) (is r2 [:R])) [:R]))
      :itR (let [[_ X g h rr] t]
             (when (wf X)
               (let [sx (skel X)]
                 (when (and (is g [:Fn [:Lbl] sx])
                            (is h [:Fn [:Dia] [:Fn [:Lbl] [:Fn sx [:Fn sx sx]]]])
                            (is rr [:R]))
                   sx))))
      :print (when (is (second t) [:R]) [:Syn])
      :chk (let [[_ c d] t] (when (and (is c [:Syn]) (is d [:Syn])) [:Bool]))
      :lam (let [[_ u A b] t]
             (when (and (s/usages u) (wf A))
               (when-let [sb (under [(skel A)] b)] [:Fn (skel A) sb])))
      :app (let [[_ f a] t sf (ty f)]
             (when (and (= :Fn (first sf)) (is a (second sf))) (nth sf 2)))
      :pair (let [[_ S2 a b] t]
              (when (and (= :Sigma (first S2)) (wf S2))
                (let [sk (skel S2)] (when (and (is a (second sk)) (is b (nth sk 2))) sk))))
      :let (let [[_ C p b] t sp (ty p)]
             (when (and (wf C) (= :Prod (first sp))
                        (= (skel C) (under [(second sp) (nth sp 2)] b)))
               (skel C)))
      :h1 (let [[_ rr ss c e1 e2] t]
            (when (and (is rr [:R]) (is ss [:R]) (is c [:Syn]) (is e1 [:Unit]) (is e2 [:Unit]))
              [:Unit]))
      :reflect (let [[_ D rr ev] t]
                 (when (and (s/base-data-types D) (is rr [:R]) (is ev [:Unit])) (skel D)))
      :inspect (let [[_ X rr c t1 t2] t]
                 (when (wf X)
                   (let [sx (skel X)]
                     (when (and (is rr [:R]) (is c [:Syn])
                                (= sx (under [[:R] [:Unit]] t1)) (= sx (under [[:R] [:Unit]] t2)))
                       sx))))
      nil)))

;; ---------------------------------------------------------------------------
;; Contexts in derivation records: vectors of [usage type], outermost first.

(defn- tel [G] (mapv second G))
(defn- zero-ctx [G] (mapv (fn [[_ A]] [0 A]) G))
(defn- ctx+ [& cs] (reduce (fn [a b] (mapv (fn [[u A] [v _]] [(s/u+ u v) A]) a b)) cs))
(defn- ctx* [rho G] (mapv (fn [[u A]] [(s/u* rho u) A]) G))
(defn- omega-scaled? [G] (every? #(#{0 :w} (first %)) G))
(defn- prefix [G n] (subvec G 0 (min n (count G))))
(defn- suffix [G n] (subvec G (min n (count G))))

(defn- typing?
  "Premise P concludes a typing judgment of mode jm, over telescope T, of term
  t at type A.  Type-level judgments must record all-zero contexts."
  [P jm T t A]
  (and (map? P) (= jm (:j P)) (= T (tel (:ctx P))) (= t (:term P)) (= A (:type P))
       (or (= jm :has) (= (:ctx P) (zero-ctx (:ctx P))))))

(defn- form?
  "Premise P concludes the formation judgment of A over telescope T."
  [P T A]
  (and (map? P) (= :type (:j P)) (= T (tel (:ctx P))) (= A (:type P))
       (= (:ctx P) (zero-ctx (:ctx P)))))

(defn- chain-typed?
  "Every chain element is skeleton-well-formed in the context, and all share
  one skeleton (review R4-01)."
  [chain T]
  (let [S (mapv skel T)]
    (and (every? #(skel-wf? S %) chain)
         (apply = (map skel chain)))))

(def ^:private base-formation
  {:EmptyF [:Empty] :UnitF [:Unit] :BoolF [:Bool] :NatF [:Nat]
   :LblF [:Lbl] :SynF [:Syn] :DiaF [:Dia] :RF [:R]})

(defn- formation-ok? [{:keys [rule type prems]} T]
  (let [ps prems np (count ps)]
    (if-let [A (base-formation rule)]
      (and (= type A) (zero? np))
      (case rule
        :TF (and (= :T (first type)) (= 1 np) (typing? (ps 0) :has0 T (second type) [:Bool]))
        (:PiF :SigmaF) (let [[tag u A1 B] type]
                         (and (= tag (if (= rule :PiF) :Pi :Sigma)) (s/usages u) (= 2 np)
                              (form? (ps 0) T A1) (form? (ps 1) (conj T A1) B)))
        false))))

(defn- typing-ok?
  "The local validity of a typing node, runtime (:has) or type level (:has0).
  In type-level nodes every recorded context is all-zero, so the context
  equations below hold as 0 = 0 + 0."
  [{:keys [rule j ctx term type prems chain]}]
  (let [G ctx n (count G) T (tel G) rt (= j :has)
        ps prems np (count ps)
        u* (fn [u] (if rt u 0))                 ; a binder's recorded usage
        sub? (fn [i t A] (typing? (ps i) j T t A))
        at (fn [i] (:ctx (ps i)))]
    (case rule
      ;; axioms accept any context: unused variables are discarded
      :Var (let [[tag i] term]
             (and (= tag :var) (zero? np) (int? i) (< -1 i n)
                  (let [[u B] (nth G (- n 1 i))]
                    (and (or (not rt) (#{1 :w} u))
                         (= type (s/shift B (inc i)))))))
      :Unit (and (= term [:star]) (= type [:Unit]) (zero? np))
      :TT (and (= term [:tt]) (= type [:Bool]) (zero? np))
      :FF (and (= term [:ff]) (= type [:Bool]) (zero? np))
      :Zero (and (= term [:zero]) (= type [:Nat]) (zero? np))
      :Lbl (and (= :lbl (first term)) (= 2 (count term)) (s/label? (second term))
                (= type [:Lbl]) (zero? np))

      :Lam (let [[tag u A1 body] term]
             (and (= tag :lam) (s/usages u) (= 2 np)
                  (form? (ps 0) T A1)
                  (typing? (ps 1) j (conj T A1) body (:type (ps 1)))
                  (= type [:Pi u A1 (:type (ps 1))])
                  (= (at 1) (conj G [(u* u) A1]))))

      :App (let [[tag f a] term FT (:type (ps 0))]
             (and (= tag :app) (= 2 np) (= :Pi (first FT))
                  (let [[_ u A1 B] FT]
                    (and (sub? 0 f FT)
                         (= type (s/subst B a))
                         (if (= u 0)
                           (and (typing? (ps 1) :has0 T a A1) (= G (at 0)))
                           (and (sub? 1 a A1) (= G (ctx+ (at 0) (ctx* u (at 1))))))))))

      :Pair (let [[tag S a b] term]
              (and (= tag :pair) (= 3 np) (= :Sigma (first S)) (= type S)
                   (let [[_ u A1 B] S]
                     (and (form? (ps 0) T S)
                          (if (= u 0) (typing? (ps 1) :has0 T a A1) (sub? 1 a A1))
                          (sub? 2 b (s/subst B a))
                          (= G (if (= u 0) (at 2) (ctx+ (ctx* u (at 1)) (at 2))))))))

      :Let (let [[tag C p body] term ST (:type (ps 0))]
             (and (= tag :let) (= 3 np) (= type C) (= :Sigma (first ST))
                  (let [[_ u A1 B] ST]
                    (and (sub? 0 p ST)
                         (form? (ps 1) T C)
                         (typing? (ps 2) j (conj T A1 B) body (s/shift C 2))
                         (= (suffix (at 2) n) [[(u* u) A1] [(u* 1) B]])
                         (= G (ctx+ (at 0) (prefix (at 2) n)))))))

      :Abort (let [[tag A1 t'] term]
               (and (= tag :abort) (= 2 np) (= type A1)
                    (sub? 0 t' [:Empty]) (form? (ps 1) T A1) (= G (at 0))))

      :Conv (and (= 2 np) (vector? chain) (seq chain)
                 (let [A0 (:type (ps 0))]
                   (and (sub? 0 term A0)
                        (form? (ps 1) T type)
                        (= G (at 0))
                        (= (first chain) A0) (= (peek chain) type)
                        (chain-typed? chain T)
                        ;; last: δ-steps inside may call Check recursively
                        (r/chain-valid? chain))))

      :If (let [[tag b t1 t2] term]
            (and (= tag :if) (= 3 np)
                 (sub? 0 b [:Bool]) (sub? 1 t1 type) (sub? 2 t2 type)
                 (= (at 1) (at 2))
                 (= G (ctx+ (at 0) (at 1)))))

      :ElimBool (let [[tag P b t1 t2] term]
                  (and (= tag :elimBool) (= 4 np) (= type (s/subst P b))
                       (sub? 0 b [:Bool]) (form? (ps 1) (conj T [:Bool]) P)
                       (sub? 2 t1 (s/subst P [:tt])) (sub? 3 t2 (s/subst P [:ff]))
                       (= (at 2) (at 3))
                       (= G (ctx+ (at 0) (at 2)))))

      :Succ (and (= :succ (first term)) (= 1 np) (= type [:Nat])
                 (sub? 0 (second term) [:Nat]) (= G (at 0)))

      :RecN (let [[tag P z st nn] term]
              (and (= tag :recN) (= 4 np) (= type (s/subst P nn))
                   (sub? 0 nn [:Nat]) (form? (ps 1) (conj T [:Nat]) P)
                   (sub? 2 z (s/subst P [:zero]))
                   (typing? (ps 3) j (conj T [:Nat] P) st (s/subst (s/shift P 2 1) [:succ [:var 1]]))
                   (= (suffix (at 3) n) [[(u* :w) [:Nat]] [(u* 1) P]])
                   (omega-scaled? (prefix (at 3) n))
                   (= G (ctx+ (at 0) (at 2) (prefix (at 3) n)))))

      :CaseLbl (let [[tag P a bs] term]
                 (and (= tag :caseLbl) (vector? bs) (= (count bs) (count s/labels))
                      (= np (+ 2 (count bs))) (= type (s/subst P a))
                      (sub? 0 a [:Lbl]) (form? (ps 1) (conj T [:Lbl]) P)
                      (every? true? (map (fn [i l b] (sub? (+ 2 i) b (s/subst P [:lbl l])))
                                         (range) s/labels bs))
                      (apply = (map :ctx (drop 2 ps)))
                      (= G (ctx+ (at 0) (at 2)))))

      :SLeaf (and (= :sleaf (first term)) (= 1 np) (= type [:Syn])
                  (sub? 0 (second term) [:Lbl]) (= G (at 0)))

      :SNode (let [[tag a c1 c2] term]
               (and (= tag :snode) (= 3 np) (= type [:Syn])
                    (sub? 0 a [:Lbl]) (sub? 1 c1 [:Syn]) (sub? 2 c2 [:Syn])
                    (= G (ctx+ (at 0) (at 1) (at 2)))))

      :RecSyn (let [[tag P tl tn c] term
                    P1 (s/subst (s/shift P 3 1) [:var 1])
                    P2 (s/subst (s/shift P 4 1) [:var 1])]
                (and (= tag :recSyn) (= 4 np) (= type (s/subst P c))
                     (sub? 0 c [:Syn]) (form? (ps 1) (conj T [:Syn]) P)
                     (typing? (ps 2) j (conj T [:Lbl]) tl (s/subst (s/shift P 1 1) [:sleaf [:var 0]]))
                     (= (suffix (at 2) n) [[(u* :w) [:Lbl]]])
                     (omega-scaled? (prefix (at 2) n))
                     (typing? (ps 3) j (conj T [:Lbl] [:Syn] [:Syn] P1 P2) tn
                              (s/subst (s/shift P 5 1) [:snode [:var 4] [:var 3] [:var 2]]))
                     (= (suffix (at 3) n)
                        [[(u* :w) [:Lbl]] [(u* :w) [:Syn]] [(u* :w) [:Syn]] [(u* 1) P1] [(u* 1) P2]])
                     (omega-scaled? (prefix (at 3) n))
                     (= G (ctx+ (at 0) (prefix (at 2) n) (prefix (at 3) n)))))

      :Leaf (and (= :leaf (first term)) (= 1 np) (= type [:R])
                 (sub? 0 (second term) [:Lbl]) (= G (at 0)))

      :Node (let [[tag d a r1 r2] term]
              (and (= tag :node) (= 4 np) (= type [:R])
                   (sub? 0 d [:Dia]) (sub? 1 a [:Lbl]) (sub? 2 r1 [:R]) (sub? 3 r2 [:R])
                   (= G (ctx+ (at 0) (at 1) (at 2) (at 3)))))

      :ItR (let [[tag X g h rr] term
                 gt [:Pi :w [:Lbl] (s/shift X 1)]
                 ht [:Pi 1 [:Dia] [:Pi :w [:Lbl] [:Pi 1 (s/shift X 2) [:Pi 1 (s/shift X 3) (s/shift X 4)]]]]]
             (and (= tag :itR) (= 4 np) (= type X)
                  (form? (ps 0) T X)
                  (sub? 1 g gt) (omega-scaled? (at 1))
                  (sub? 2 h ht) (omega-scaled? (at 2))
                  (sub? 3 rr [:R])
                  (= G (ctx+ (at 1) (at 2) (at 3)))))

      :Print (and (= :print (first term)) (= 1 np) (= type [:Syn])
                  (sub? 0 (second term) [:R]) (= G (at 0)))

      :Chk (let [[tag c d] term]
             (and (= tag :chk) (= 2 np) (= type [:Bool])
                  (sub? 0 c [:Syn]) (sub? 1 d [:Syn]) (= G (ctx+ (at 0) (at 1)))))

      :H1 (let [[tag rr ss c e1 e2] term]
            (and (= tag :h1) (= 5 np) (= type [:Empty])
                 (sub? 0 rr [:R]) (sub? 1 ss [:R])
                 (sub? 2 c [:Syn]) (omega-scaled? (at 2))
                 (sub? 3 e1 [:T [:chk [:print rr] c]])
                 (sub? 4 e2 [:T [:chk [:print ss] (s/neg-term c)]])
                 (= G (ctx+ (at 0) (at 1) (at 2) (at 3) (at 4)))))

      :Reflect (let [[tag D rr ev] term]
                 (and (= tag :reflect) (= 2 np) (s/base-data-types D) (= type D)
                      (sub? 0 rr [:R])
                      (sub? 1 ev [:T [:chk [:print rr] (s/code->term (e/enc-exp D))]])
                      (= G (ctx+ (at 0) (at 1)))))

      :Inspect (let [[tag X rr c t1 t2] term
                     ok [:chk [:print [:var 0]] (s/shift c 1)]
                     b1 [[:R] [:T ok]]
                     b2 [[:R] [:T (s/not-term ok)]]]
                 (and (= tag :inspect) (= 5 np) (= type X)
                      (sub? 0 rr [:R])
                      (sub? 1 c [:Syn]) (omega-scaled? (at 1))
                      (form? (ps 2) T X)
                      (typing? (ps 3) j (into T b1) t1 (s/shift X 2))
                      (typing? (ps 4) j (into T b2) t2 (s/shift X 2))
                      (= (suffix (at 3) n) (mapv (fn [A] [(u* 1) A]) b1))
                      (= (suffix (at 4) n) (mapv (fn [A] [(u* 1) A]) b2))
                      (= (prefix (at 3) n) (prefix (at 4) n))
                      (= G (ctx+ (at 0) (at 1) (prefix (at 3) n)))))
      false)))

(defn- node-ok? [{:keys [j ctx] :as D}]
  (let [T (tel ctx)]
    (and (or (= j :has) (= ctx (zero-ctx ctx)))   ; type level: all-zero contexts
         (case j
           :type (formation-ok? D T)
           (:has :has0) (typing-ok? D)
           false))))

(defn valid?
  "Is every node of derivation record D a valid rule instance?"
  [D]
  (and (node-ok? D) (every? valid? (:prems D))))

(defn check
  "Check(c, d) for codes c and d: true iff c encodes a valid derivation of a
  runtime judgment Θₘ ⊢ t :¹ A with A closed and ⌜A⌝ = d.  Never throws on bad
  input; throws only on a violation of strict overhead, which would be a
  defect of the encoding (R4-metatheory.md Lemma 2.7)."
  [c d]
  (let [accepted
        (try
          (boolean
           (when (and (s/code? c) (s/code? d))
             (when-let [D (e/dec-deriv c)]
               (and (= :has (:j D))
                    (every? #(= [1 [:Dia]] %) (:ctx D))
                    (s/closed? (:type D))
                    (= d (e/enc-exp (:type D)))
                    (valid? D)))))
          (catch Exception _ false)
          ;; a pathologically deep code exhausts the stack: reject it too
          ;; (review E3); Check's recursion is otherwise well founded
          (catch StackOverflowError _ false))]
    (when (and accepted (not (< (k/budget c) (k/nodes c))))
      (throw (ex-info "strict overhead violated: a defect of the encoding"
                      {:type :lcert/defect :budget (k/budget c) :nodes (k/nodes c)})))
    accepted))

(defn certificate-literal
  "The term that builds code c's tree as a certificate, taking tokens
  $1 … $n in preorder (n = nodes c).  It is typed in Θₙ, where $n is
  [:var 0]."
  [c]
  (let [n (k/nodes c)
        counter (volatile! 0)]
    (letfn [(lit [x]
              (case (first x)
                :sl [:leaf [:lbl (second x)]]
                :sn (let [i (vswap! counter inc)]
                      [:node [:var (- n i)] [:lbl (second x)] (lit (nth x 2)) (lit (nth x 3))])))]
      (lit c))))
