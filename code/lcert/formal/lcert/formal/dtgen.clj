(ns lcert.formal.dtgen
  "F6.3b — a generator of derivation data for concrete λᶜᵉʳᵗ₀ terms.

  F7 gave a checker for derivation trees, dtCheck chkf tree J : Bool, and a
  soundness theorem, dtCheck_sound: dtCheck chkf tree J = true → Holds chkf J,
  for every chkf and with no hypothesis (check_der.clj). So a typing
  judgment about a *concrete* term can be proved by reflection: build a tree,
  and let the kernel evaluate dtCheck to true (the tactic is rfl).

  This namespace builds the trees. It is ordinary Clojure, outside the trust
  base: a wrong tree makes dtCheck return false and the proof fail, never
  succeed. It mirrors the formal definitions it must agree with — lift,
  subst, subst1, substL, stepTy, skel (syntax.clj, judgment.clj, skel.clj),
  and the head rules of conv.clj — and takes every constructor's field order
  from the formal rule tables (check_dt's dt-rules, check_skj's skj-rules,
  check_hd's hd-rules), so the data cannot drift from the rules.

  Representation. An expression is a vector [tag & fields] whose tag is the
  keyword of an Exp constructor: [:var 3], [:lam :uw [:tNat] body],
  [:tNat]. Usages are :u0 :u1 :uw; Nat fields are integers. A context is a
  vector of types, innermost (index 0) first, as the formal List Exp.

  What it builds, for a context D, a usage vector us, a term e and a type A:
  - (rt D us e A): a tree for Rt D us e A. Usages are allocated top-down from
    the free variables of each premise (alloc); conversions are inserted
    where a synthesized type differs from the one required (conv-dt).
  - (tl-form D A) and (tl0-check D e A): trees for Tl true D A 1 and
    Tl false D e A (formation and type-level typing).
  - (cv-dt D A B): a Cv chain, A →* X ←* B, found by weak-head reduction and
    comparison of heads (conv-steps), each element with its skeleton tree.
  - (skj-ty G A), (skj G e): skeleton trees.

  Surface syntax (compile-term), with named variables, for writing terms:
    (λ r [x A] body)   (Π r [x A] B)   (Σ r [x A] B)   (ap f a b …)
    (recN [x P] z [k y s] n)   (elimB [z P] b t e)   (ite b t e)
    (letp C p [a b] t)   (pair S x y)   (abort A t)   (succ n)   (T b)
    Nat Bool Unit Empty ⋆ tt ff zero, and names of closed constants
  (looked up in the map passed as consts)."
  (:require [lcert.formal.check-dt :as dt]
            [lcert.formal.check-skj :as skj]
            [lcert.formal.check-hd :as hd]))

;; ---------------------------------------------------------------------------
;; Exp: the constructor table and the operations of syntax.clj.

;; Each field is :U (a usage), :N (a Nat), or an integer: an Exp field under
;; that many binders (as liftF and substF of syntax.clj descend).
(def ctor-table
  {:tEmpty [] :tUnit [] :tBool [] :tNat [] :tLbl [] :tSyn [] :tDia [] :tR []
   :tT [0] :tPi [:U 0 1] :tSig [:U 0 1]
   :var [:N] :star [] :abort [0 0] :tt [] :ff []
   :ite [0 0 0] :elimB [1 0 0 0] :zero [] :succ [0] :recN [1 0 2 0]
   :lbl [:N] :caseL [1 0 0] :bnil [] :bcons [0 0] :sleaf [0] :snode [0 0 0]
   :recS [1 1 5 0] :leaf [0] :node [0 0 0 0] :itR [0 0 0 0] :prn [0]
   :lam [:U 0 1] :app [0 0] :pair [0 0 0] :letp [0 0 2] :chk [0 0]
   :h1 [0 0 0 0 0] :refl [0 0 0] :insp [0 0 0 2 2] :tBrs [1 :N]})

(defn- kinds [e]
  (or (ctor-table (first e)) (throw (ex-info "dtgen: unknown constructor" {:e e}))))

(defn emap
  "Rebuild e, applying (f child binders) to each Exp field."
  [f e]
  (into [(first e)] (map (fn [k x] (if (integer? k) (f x k) x)) (kinds e) (rest e))))

(defn lift [k c e]
  (if (= :var (first e))
    (let [i (second e)] (if (< i c) e [:var (+ i k)]))
    (emap (fn [x b] (lift k (+ c b) x)) e)))

(defn up [s] (fn [i] (if (zero? i) [:var 0] (lift 1 0 (s (dec i))))))
(defn upn [n s] (if (zero? n) s (up (upn (dec n) s))))

(defn subst [s e]
  (if (= :var (first e)) (s (second e)) (emap (fn [x b] (subst (upn b s) x)) e)))

(defn inst1 [u] (fn [i] (if (zero? i) u [:var (dec i)])))
(defn subst1 [u e] (subst (inst1 u) e))
(defn instL [us] (fn [i] (if (< i (count us)) (nth us i) [:var (- i (count us))])))
(defn substL [us e] (subst (instL us) e))
(defn s-succ [i] (if (zero? i) [:succ [:var 0]] [:var i]))
(defn step-ty "judgment.clj's stepTy: P[succ x/x], under the result binder." [P]
  (lift 1 0 (subst s-succ P)))

(defn skel [A]
  (case (first A)
    (:tEmpty :tUnit :tT) [:unit]
    :tBool [:bool] :tNat [:nat] :tLbl [:lbl] :tSyn [:syn] :tDia [:dia] :tR [:cert]
    :tPi [:arr (skel (nth A 2)) (skel (nth A 3))]
    :tSig [:prod (skel (nth A 2)) (skel (nth A 3))]
    :tBrs [:arr [:lbl] (skel (nth A 1))]
    [:unit]))

(def base-types #{:tEmpty :tUnit :tBool :tNat :tLbl :tSyn :tDia :tR})

(defn- shift-down [k vars] (set (keep #(when (>= % k) (- % k)) vars)))

;; Positions that are types: they are checked by Tl, which records no
;; usage, so a variable occurring only there needs none.
(def ^:private type-positions
  {:lam #{0} :pair #{0} :letp #{0} :recN #{0} :elimB #{0} :caseL #{0}
   :recS #{0} :abort #{0} :itR #{0} :refl #{0} :insp #{0}})

(defn rfv
  "The free variables of e at runtime positions (type annotations excluded)."
  [e]
  (if (= :var (first e)) #{(second e)}
    (let [skip (type-positions (first e) #{})]
      (loop [ks (kinds e) xs (rest e) pos 0 acc #{}]
        (if (empty? ks) acc
          (let [k (first ks)]
            (if (integer? k)
              (recur (rest ks) (rest xs) (inc pos)
                     (if (skip pos) acc (into acc (shift-down k (rfv (first xs))))))
              (recur (rest ks) (rest xs) pos acc))))))))

;; Exp fields by position (as conv.clj's child numbers them: Exp fields only).
(defn- exp-slots [e] (vec (keep-indexed (fn [i k] (when (integer? k) (inc i))) (kinds e))))
(defn child [e i] (nth e (nth (exp-slots e) i)))
(defn set-child [e i x] (assoc e (nth (exp-slots e) i) x))
(defn get-at [e p] (reduce child e p))
(defn set-at [e p x]
  (if (empty? p) x (set-child e (first p) (set-at (child e (first p)) (rest p) x))))

;; ---------------------------------------------------------------------------
;; Printing, as kernel surface terms.

(def usx {:u0 'U.u0 :u1 'U.u1 :uw 'U.uw})

(def ^:dynamic *names*
  "Closed expressions to print as their kernel constants (e.g. EQ's body as
  trEQ). The kernel unfolds a constant when it must, so this changes only
  the size of the printed tree, which otherwise repeats EQ in every type of
  every conversion chain."
  {})

(defn sx [e]
  (if-let [nm (*names* e)] nm
  (let [h (symbol (str "Exp." (name (first e))))]
    (if (empty? (rest e)) h
      (apply list h (map (fn [k x] (cond (= k :U) (usx x) (= k :N) x :else (sx x))) (kinds e) (rest e)))))))

(defn list-sx [T xs] (reduce (fn [tail x] (list 'List.cons T x tail)) (list 'List.nil T) (reverse xs)))
(defn D-sx [D] (list-sx 'Exp (map sx D)))
(defn us-sx [us] (list-sx 'U (map usx us)))
(defn sk-sx [s]
  (let [h (symbol (str "Sk." (name (first s))))]
    (if (empty? (rest s)) h (apply list h (map sk-sx (rest s))))))
(defn path-sx [p] (list-sx 'Nat p))

;; Constructor applications in rule-table field order. vals maps field names
;; to printed values; a missing field is an error, never a default.
(defn- rule-of [rules nm] (or (first (filter #(= nm (first %)) rules))
                              (throw (ex-info "dtgen: no such rule" {:rule nm}))))
(defn- build [ctor fields vals]
  (let [args (for [[x _] fields]
               (if (contains? vals x) (get vals x)
                 (throw (ex-info "dtgen: missing field" {:ctor ctor :field x}))))]
    (if (seq args) (apply list ctor args) ctor)))

(def ^:private all-dt-rules (map second dt/dt-rules))
(defn- dt-node [nm vals]
  (let [rule (rule-of all-dt-rules nm)]
    (build (symbol (str "DT." nm)) (remove hd/f7-side? (hd/f7-fields rule)) vals)))
(defn- sk-node [nm vals]
  (let [rule (rule-of skj/skj-rules nm)]
    (build (symbol (str "SkDT." nm))
           (remove #(or (= 'G (first %)) (hd/f7-side? %)) (hd/f7-fields rule)) vals)))
(defn- hd-node [nm vals]
  (let [rule (rule-of hd/hd-rules nm)]
    (build (symbol (str "HdDT." nm)) (remove hd/f7-side? (hd/f7-fields rule)) vals)))

(defn- fail [msg data] (throw (ex-info (str "dtgen: " msg) data)))

;; ---------------------------------------------------------------------------
;; Skeleton trees (check_skj's SkJ rules).

(declare skj)

(defn skj-ty
  "A tree for SkJ true G A unit."
  [G A]
  (let [tag (first A)]
    (cond
      (base-types tag) (sk-node (symbol (str "w" (subs (name tag) 1))) {})
      (= tag :tT) (sk-node 'wT {'b (sx (second A)) 'hb (skj G (second A) [:bool])})
      (#{:tPi :tSig} tag)
      (let [[_ r X Y] A]
        (sk-node (if (= tag :tPi) 'wPi 'wSig)
                 {'r (usx r) 'A (sx X) 'B (sx Y) 'hA (skj-ty G X) 'hB (skj-ty (into [(skel X)] G) Y)}))
      :else (fail "not a type, for a skeleton" {:A A}))))

(defn- skj-syn
  "[tree skeleton] for SkJ false G e s, s synthesized."
  [G e]
  (let [[tag & xs] e]
    (case tag
      :var (let [i (first xs)] [(sk-node 'sVar {'i i 's (sk-sx (nth G i))}) (nth G i)])
      :star [(sk-node 'sStar {}) [:unit]]
      :tt [(sk-node 'sTT {}) [:bool]]
      :ff [(sk-node 'sFF {}) [:bool]]
      :zero [(sk-node 'sZero {}) [:nat]]
      :succ [(sk-node 'sSucc {'n (sx (first xs)) 'h (skj G (first xs) [:nat])}) [:nat]]
      :ite (let [[b t f] xs [ht s] (skj-syn G t)]
             [(sk-node 'sIte {'b (sx b) 't (sx t) 'e (sx f) 's (sk-sx s)
                              'hb (skj G b [:bool]) 'ht ht 'he (skj G f s)}) s])
      :elimB (let [[P b t f] xs s (skel P)]
               [(sk-node 'sElimB {'P (sx P) 'b (sx b) 't (sx t) 'e (sx f)
                                  'hP (skj-ty (into [[:bool]] G) P) 'hb (skj G b [:bool])
                                  'ht (skj G t s) 'he (skj G f s)}) s])
      :recN (let [[P z st n] xs s (skel P)]
              [(sk-node 'sRecN {'P (sx P) 'z (sx z) 'st (sx st) 'n (sx n)
                                'hP (skj-ty (into [[:nat]] G) P) 'hz (skj G z s)
                                'hs (skj (into [s [:nat]] G) st s) 'hn (skj G n [:nat])}) s])
      :lam (let [[r A t] xs [ht s] (skj-syn (into [(skel A)] G) t)]
             [(sk-node 'sLam {'r (usx r) 'A (sx A) 't (sx t) 's (sk-sx s)
                              'hA (skj-ty G A) 'ht ht}) [:arr (skel A) s]])
      :app (let [[f u] xs [hf sf] (skj-syn G f)]
             (when-not (= :arr (first sf)) (fail "skeleton: applying a non-function" {:e e}))
             (let [[_ s t] sf]
               [(sk-node 'sApp {'f (sx f) 'u (sx u) 's (sk-sx s) 't (sk-sx t) 'hf hf 'hu (skj G u s)}) t]))
      :pair (let [[S x y] xs [_ r A B] S]
              [(sk-node 'sPair {'r (usx r) 'A (sx A) 'B (sx B) 'x (sx x) 'y (sx y)
                                'hS (skj-ty G S) 'hx (skj G x (skel A)) 'hy (skj G y (skel B))})
               [:prod (skel A) (skel B)]])
      :letp (let [[C p t] xs [hp sp] (skj-syn G p)]
              (when-not (= :prod (first sp)) (fail "skeleton: let of a non-pair" {:e e}))
              (let [[_ s1 s2] sp]
                [(sk-node 'sLetp {'C (sx C) 'p (sx p) 't (sx t) 's1 (sk-sx s1) 's2 (sk-sx s2)
                                  'hC (skj-ty G C) 'hp hp 'ht (skj (into [s2 s1] G) t (skel C))})
                 (skel C)]))
      :abort (let [[A t] xs]
               [(sk-node 'sAbort {'A (sx A) 't (sx t) 'hA (skj-ty G A) 'ht (skj G t [:unit])}) (skel A)])
      (fail "no skeleton rule for this term" {:e e}))))

(defn skj
  "A tree for SkJ false G e s."
  [G e s]
  (let [[tree s2] (skj-syn G e)]
    (when-not (= s s2) (fail "skeleton mismatch" {:e e :want s :got s2}))
    tree))

;; ---------------------------------------------------------------------------
;; Reduction (conv.clj's head rules) and conversion chains.

(defn head-step
  "[HdDT contractum] when e is a head redex, else nil."
  [e]
  (let [[tag & xs] e]
    (case tag
      :app (let [[f u] xs]
             (when (= :lam (first f))
               (let [[_ r A t] f]
                 [(hd-node 'beta {'r (usx r) 'A (sx A) 't (sx t) 'u (sx u)}) (subst1 u t)])))
      :letp (let [[C p t] xs]
              (when (= :pair (first p))
                (let [[_ S x y] p]
                  [(hd-node 'betaLet {'C (sx C) 'S (sx S) 'x (sx x) 'y (sx y) 't (sx t)}) (substL [y x] t)])))
      :ite (let [[b t f] xs]
             (case (first b)
               :tt [(hd-node 'iteT {'t (sx t) 'e (sx f)}) t]
               :ff [(hd-node 'iteF {'t (sx t) 'e (sx f)}) f]
               nil))
      :elimB (let [[P b t f] xs]
               (case (first b)
                 :tt [(hd-node 'elimT {'P (sx P) 't (sx t) 'e (sx f)}) t]
                 :ff [(hd-node 'elimF {'P (sx P) 't (sx t) 'e (sx f)}) f]
                 nil))
      :recN (let [[P z s n] xs]
              (case (first n)
                :zero [(hd-node 'recNZ {'P (sx P) 'z (sx z) 's (sx s)}) z]
                :succ (let [m (second n)]
                        [(hd-node 'recNS {'P (sx P) 'z (sx z) 's (sx s) 'n (sx m)})
                         (substL [[:recN P z s m] m] s)])
                nil))
      :tT (case (first (first xs))
            :tt [(hd-node 'tTT {}) [:tUnit]]
            :ff [(hd-node 'tTF {}) [:tEmpty]]
            nil)
      nil)))

;; The position whose weak-head normal form a head redex needs.
(def ^:private scrutinee {:app 0 :letp 1 :ite 0 :elimB 1 :recN 3 :tT 0})

(defn whnf
  "[paths e′]: the head-step paths (relative to e) taking e to e′, its weak
  head normal form."
  [e]
  (loop [e e paths []]
    (if-let [[_ e2] (head-step e)]
      (recur e2 (conj paths []))
      (if-let [pos (scrutinee (first e))]
        (let [[ps c2] (whnf (child e pos))]
          (if (empty? ps) [paths e]
            (recur (set-child e pos c2) (into paths (map #(vec (cons pos %)) ps)))))
        [paths e]))))

(defn conv-steps
  "[pa pb]: paths of head steps with a →* x and b →* x (each relative to the
  term as it stands when the step is taken), or an error."
  [a b]
  (if (= a b) [[] []]
    (let [[pa a2] (whnf a) [pb b2] (whnf b)]
      (if (= a2 b2) [pa pb]
        (do
          (when-not (and (= (first a2) (first b2))
                         (= (remove vector? (rest a2)) (remove vector? (rest b2))))
            (fail "not convertible" {:a a2 :b b2}))
          (reduce (fn [[xa xb] i]
                    (let [[ca cb] (conv-steps (child a2 i) (child b2 i))]
                      [(into xa (map #(vec (cons i %)) ca)) (into xb (map #(vec (cons i %)) cb))]))
                  [pa pb] (range (count (exp-slots a2)))))))))

(defn- chain
  "The steps along paths ps from e: [[path HdDT src tgt] …]."
  [e ps]
  (first (reduce (fn [[acc cur] p]
                   (let [[h c] (or (head-step (get-at cur p)) (fail "no redex at path" {:e cur :p p}))
                         nxt (set-at cur p c)]
                     [(conj acc [p h cur nxt]) nxt]))
                 [[] e] ps)))

(defn- step-dt [[p h src tgt]] (list 'StepDT.at (path-sx p) h (sx src) (sx tgt)))

(defn cv-dt
  "A tree for Cv (skels D) A B: cvRefl at A, cvFwd down A's chain, and cvBwd
  back up B's."
  [D A B]
  (let [Gx (list 'skels (D-sx D)) G (mapv skel D)
        [pa pb] (conv-steps A B)
        start (dt-node 'cvRefl {'G Gx 'A (sx A) 'h (skj-ty G A)})
        fwd (reduce (fn [acc [_ _ src tgt :as st]]
                      (dt-node 'cvFwd {'G Gx 'A (sx A) 'B (sx src) 'C (sx tgt) 'hab acc
                                       'hs (step-dt st) 'hc (skj-ty G tgt)}))
                    start (chain A pa))]
    (reduce (fn [acc [_ _ src tgt :as st]]
              (dt-node 'cvBwd {'G Gx 'A (sx A) 'B (sx tgt) 'C (sx src) 'hab acc
                               'hs (step-dt st) 'hc (skj-ty G src)}))
            fwd (reverse (chain B pb)))))

;; ---------------------------------------------------------------------------
;; Types, without derivations.

(defn type-of [D e]
  (let [[tag & xs] e]
    (case tag
      :var (lift (inc (first xs)) 0 (nth D (first xs)))
      :star [:tUnit] (:tt :ff) [:tBool] :zero [:tNat] :succ [:tNat]
      :lbl [:tLbl]
      :lam (let [[r A t] xs] [:tPi r A (type-of (into [A] D) t)])
      :app (let [[f u] xs Tf (type-of D f)]
             (when-not (= :tPi (first Tf)) (fail "applying a non-function" {:e e :type Tf}))
             (subst1 u (nth Tf 3)))
      :recN (let [[P _ _ n] xs] (subst1 n P))
      :elimB (let [[P b] xs] (subst1 b P))
      :ite (type-of D (second xs))
      :letp (first xs) :pair (first xs) :abort (first xs)
      (fail "no type for this term" {:e e}))))

;; ---------------------------------------------------------------------------
;; Formation and type-level typing (Tl).

(declare tl0-check)

(defn tl-form
  "A tree for Tl true D A 1."
  [D A]
  (let [tag (first A)]
    (cond
      (base-types tag) (dt-node 'fBase {'D (D-sx D) 'X (sx A)})
      (= tag :tT) (dt-node 'fT {'D (D-sx D) 'b (sx (second A)) 'hb (tl0-check D (second A) [:tBool])})
      (#{:tPi :tSig} tag)
      (let [[_ r X Y] A]
        (dt-node (if (= tag :tPi) 'fPi 'fSig)
                 {'D (D-sx D) 'r (usx r) 'A (sx X) 'B (sx Y) 'hA (tl-form D X) 'hB (tl-form (into [X] D) Y)}))
      :else (fail "not a type, for formation" {:A A}))))

(defn- tl0
  "[tree type] for Tl false D e type, the type synthesized."
  [D e]
  (let [[tag & xs] e Dx (D-sx D)]
    (case tag
      :var (let [i (first xs) A (nth D i)]
             [(dt-node 'zVar {'D Dx 'i i 'A (sx A)}) (lift (inc i) 0 A)])
      (:star :tt :ff :zero)
      (let [A (type-of D e)] [(dt-node 'zConst {'D Dx 't (sx e) 'A (sx A)}) A])
      :lam (let [[r A t] xs [ht B] (tl0 (into [A] D) t)]
             [(dt-node 'zLam {'D Dx 'r (usx r) 'A (sx A) 't (sx t) 'B (sx B) 'hA (tl-form D A) 'ht ht})
              [:tPi r A B]])
      :app (let [[f u] xs [hf Tf] (tl0 D f)]
             (when-not (= :tPi (first Tf)) (fail "type level: applying a non-function" {:e e}))
             (let [[_ r A B] Tf]
               [(dt-node 'zApp {'D Dx 'r (usx r) 'f (sx f) 'u (sx u) 'A (sx A) 'B (sx B) 'hf hf
                                'hu (tl0-check D u A) 'hA (tl-form D A) 'hB (tl-form (into [A] D) B)})
                (subst1 u B)]))
      :ite (let [[b t f] xs [ht C] (tl0 D t)]
             [(dt-node 'zIte {'D Dx 'b (sx b) 't (sx t) 'e (sx f) 'C (sx C)
                              'hb (tl0-check D b [:tBool]) 'ht ht 'he (tl0-check D f C)}) C])
      :elimB (let [[P b t f] xs]
               [(dt-node 'zElimB {'D Dx 'P (sx P) 'b (sx b) 't (sx t) 'e (sx f)
                                  'hb (tl0-check D b [:tBool]) 'hP (tl-form (into [[:tBool]] D) P)
                                  'ht (tl0-check D t (subst1 [:tt] P)) 'he (tl0-check D f (subst1 [:ff] P))})
                (subst1 b P)])
      :succ [(dt-node 'zSucc {'D Dx 'n (sx (first xs)) 'h (tl0-check D (first xs) [:tNat])}) [:tNat]]
      :recN (let [[P z s n] xs]
              [(dt-node 'zRecN {'D Dx 'P (sx P) 'z (sx z) 's (sx s) 'n (sx n)
                                'hn (tl0-check D n [:tNat]) 'hP (tl-form (into [[:tNat]] D) P)
                                'hz (tl0-check D z (subst1 [:zero] P))
                                'hs (tl0-check (into [P [:tNat]] D) s (step-ty P))})
               (subst1 n P)])
      (fail "no type-level rule for this term" {:e e}))))

(defn tl0-check
  "A tree for Tl false D e A, converting when the synthesized type differs."
  [D e A]
  (let [[tree T] (tl0 D e)]
    (if (= T A) tree
      (dt-node 'zConv {'D (D-sx D) 't (sx e) 'A (sx T) 'B (sx A) 'ht tree
                       'hB (tl-form D A) 'hc (cv-dt D T A)}))))

;; ---------------------------------------------------------------------------
;; Usage allocation.

(defn- umul [r x]
  (case r :u0 :u0 :u1 x :uw (if (= x :u0) :u0 :uw)))

(defn alloc
  "Split the target vector us among premises, each {:fv set, :scale U}, so
  that the sum of (scale · its vector) is us. A variable at ω goes at ω to
  every premise that uses it (or to the first, which absorbs it); at 1, to
  its one user, which must be unscaled (or to the first unscaled premise);
  at 0, nowhere — and then no premise may use it."
  [us prems]
  (let [n (count prems)
        unscaled (first (filter #(= :u1 (:scale (nth prems %))) (range n)))
        cols (for [j (range (count us))]
               (let [users (filter #(contains? (:fv (nth prems %)) j) (range n))]
                 (case (nth us j)
                   :u0 (if (seq users) (fail "a variable at usage 0 is used" {:j j})
                         (vec (repeat n :u0)))
                   :uw (let [chosen (if (seq users) (set users) #{0})]
                         (vec (for [k (range n)] (if (chosen k) :uw :u0))))
                   :u1 (let [_ (when (> (count users) 1) (fail "a usage-1 variable used twice" {:j j :users users}))
                             k (if (seq users) (first users) unscaled)]
                         (when-not (and k (= :u1 (:scale (nth prems k))))
                           (fail "a usage-1 variable in a scaled premise" {:j j}))
                         (vec (for [m (range n)] (if (= m k) :u1 :u0)))))))]
    (vec (for [k (range n)] (mapv #(nth % k) cols)))))

;; ---------------------------------------------------------------------------
;; Runtime typing (Rt).

(declare rt)

(defn- conv-rt [D us e T A tree]
  (if (= T A) tree
    (dt-node 'rConv {'D (D-sx D) 'us (us-sx us) 't (sx e) 'A (sx T) 'B (sx A) 'ht tree
                     'hB (tl-form D A) 'hc (cv-dt D T A)})))

(defn- rt-syn
  "[tree type] for Rt D us e type, the type synthesized."
  [D us e]
  (let [[tag & xs] e Dx (D-sx D) usx* (us-sx us)]
    (case tag
      :var (let [i (first xs) A (nth D i) r (nth us i)]
             (when (= r :u0) (fail "a variable at usage 0" {:i i}))
             [(dt-node 'rVar {'D Dx 'us usx* 'i i 'A (sx A) 'r (usx r)}) (lift (inc i) 0 A)])
      (:star :tt :ff :zero)
      (let [A (type-of D e)] [(dt-node 'rConst {'D Dx 'us usx* 't (sx e) 'A (sx A)}) A])
      :lam (let [[r A t] xs B (type-of (into [A] D) t)]
             [(dt-node 'rLam {'D Dx 'us usx* 'r (usx r) 'A (sx A) 't (sx t) 'B (sx B)
                              'hA (tl-form D A) 'ht (rt (into [A] D) (into [r] us) t B)})
              [:tPi r A B]])
      :app (let [[f u] xs Tf (type-of D f)]
             (when-not (= :tPi (first Tf)) (fail "applying a non-function" {:e e}))
             (let [[_ r A B] Tf base {'D Dx 'f (sx f) 'u (sx u) 'A (sx A) 'B (sx B)
                                      'hA (tl-form D A) 'hB (tl-form (into [A] D) B)}]
               [(if (= r :u0)
                  (dt-node 'rApp0 (assoc base 'us usx* 'hf (rt D us f Tf) 'hu (tl0-check D u A)))
                  (let [[us1 us2] (alloc us [{:fv (rfv f) :scale :u1} {:fv (rfv u) :scale r}])]
                    (dt-node 'rApp (assoc base 'us1 (us-sx us1) 'us2 (us-sx us2) 'r (usx r)
                                          'hf (rt D us1 f Tf) 'hu (rt D us2 u A)))))
                (subst1 u B)]))
      :succ [(dt-node 'rSucc {'D Dx 'us usx* 'n (sx (first xs)) 'h (rt D us (first xs) [:tNat])}) [:tNat]]
      :recN (let [[P z s n] xs
                  [us1 us2 us3] (alloc us [{:fv (rfv n) :scale :u1} {:fv (rfv z) :scale :u1}
                                           {:fv (shift-down 2 (rfv s)) :scale :uw}])
                  inner (into [:u1 :uw] (map #(umul :uw %) us3))]
              [(dt-node 'rRecN {'D Dx 'us1 (us-sx us1) 'us2 (us-sx us2) 'us3 (us-sx us3)
                                'P (sx P) 'z (sx z) 's (sx s) 'n (sx n)
                                'hn (rt D us1 n [:tNat]) 'hP (tl-form (into [[:tNat]] D) P)
                                'hz (rt D us2 z (subst1 [:zero] P))
                                'hs (rt (into [P [:tNat]] D) inner s (step-ty P))})
               (subst1 n P)])
      :elimB (let [[P b t f] xs
                   [us1 us2] (alloc us [{:fv (rfv b) :scale :u1} {:fv (into (rfv t) (rfv f)) :scale :u1}])]
               [(dt-node 'rElimB {'D Dx 'us1 (us-sx us1) 'us2 (us-sx us2) 'P (sx P) 'b (sx b) 't (sx t) 'e (sx f)
                                  'hb (rt D us1 b [:tBool]) 'hP (tl-form (into [[:tBool]] D) P)
                                  'ht (rt D us2 t (subst1 [:tt] P)) 'he (rt D us2 f (subst1 [:ff] P))})
                (subst1 b P)])
      :pair (let [[S x y] xs [_ r A B] S base {'D Dx 'A (sx A) 'B (sx B) 'x (sx x) 'y (sx y)
                                               'hA (tl-form D A) 'hB (tl-form (into [A] D) B)}]
              [(if (= r :u0)
                 (dt-node 'rPair0 (assoc base 'us usx* 'hx (tl0-check D x A) 'hy (rt D us y (subst1 x B))))
                 (let [[us1 us2] (alloc us [{:fv (rfv x) :scale r} {:fv (rfv y) :scale :u1}])]
                   (dt-node 'rPair (assoc base 'us1 (us-sx us1) 'us2 (us-sx us2) 'r (usx r)
                                          'hx (rt D us1 x A) 'hy (rt D us2 y (subst1 x B))))))
               S])
      (fail "no runtime synthesis rule for this term" {:e e}))))

(defn rt
  "A tree for Rt D us e A."
  [D us e A]
  (let [[tag & xs] e Dx (D-sx D) usx* (us-sx us)]
    (case tag
      :lam (let [[r X t] xs]
             (if (and (= :tPi (first A)) (= r (nth A 1)) (= X (nth A 2)))
               (dt-node 'rLam {'D Dx 'us usx* 'r (usx r) 'A (sx X) 't (sx t) 'B (sx (nth A 3))
                               'hA (tl-form D X) 'ht (rt (into [X] D) (into [r] us) t (nth A 3))})
               (let [[tree T] (rt-syn D us e)] (conv-rt D us e T A tree))))
      :ite (let [[b t f] xs
                 [us1 us2] (alloc us [{:fv (rfv b) :scale :u1} {:fv (into (rfv t) (rfv f)) :scale :u1}])]
             (dt-node 'rIte {'D Dx 'us1 (us-sx us1) 'us2 (us-sx us2) 'b (sx b) 't (sx t) 'e (sx f) 'C (sx A)
                             'hb (rt D us1 b [:tBool]) 'ht (rt D us2 t A) 'he (rt D us2 f A)}))
      :letp (let [[C p t] xs Tp (type-of D p)]
              (when-not (= :tSig (first Tp)) (fail "let of a non-pair" {:e e}))
              (let [[_ r X Y] Tp
                    [us1 us2] (alloc us [{:fv (rfv p) :scale :u1} {:fv (shift-down 2 (rfv t)) :scale :u1}])
                    tree (dt-node 'rLet {'D Dx 'us1 (us-sx us1) 'us2 (us-sx us2) 'r (usx r) 'A (sx X) 'B (sx Y)
                                         'C (sx C) 'p (sx p) 't (sx t)
                                         'hp (rt D us1 p Tp) 'hC (tl-form D C) 'hA (tl-form D X)
                                         'hB (tl-form (into [X] D) Y)
                                         'ht (rt (into [Y X] D) (into [:u1 r] us2) t (lift 2 0 C))})]
                (conv-rt D us e C A tree)))
      :abort (let [[X t] xs
                   tree (dt-node 'rAbort {'D Dx 'us usx* 'A (sx X) 't (sx t)
                                          'ht (rt D us t [:tEmpty]) 'hA (tl-form D X)})]
               (conv-rt D us e X A tree))
      (let [[tree T] (rt-syn D us e)] (conv-rt D us e T A tree)))))

;; ---------------------------------------------------------------------------
;; Surface syntax.

(def ^:private atoms
  {'Nat [:tNat] 'Bool [:tBool] 'Unit [:tUnit] 'Empty [:tEmpty]
   '⋆ [:star] 'tt [:tt] 'ff [:ff] 'zero [:zero]})
(def ^:private usages {:u0 :u0 :u1 :u1 :uw :uw})

(defn compile-term
  "Compile surface syntax to an Exp, in an environment of bound names
  (innermost first) and a map of closed constants."
  ([consts form] (compile-term consts [] form))
  ([consts env form]
   (let [c (fn ([f] (compile-term consts env f)) ([names f] (compile-term consts (into (vec (reverse names)) env) f)))
         ;; (into (vec (reverse names)) env) puts the last name innermost.
         ]
     (cond
       (symbol? form)
       (let [i (.indexOf ^java.util.List env form)]
         (cond (>= i 0) [:var i]
               (contains? atoms form) (atoms form)
               (contains? consts form) (consts form)
               :else (fail "unbound name" {:name form})))
       (seq? form)
       (let [[h & args] form]
         (case h
           λ (let [[r [x A] body] args] [:lam (usages r) (c A) (c [x] body)])
           Π (let [[r [x A] B] args] [:tPi (usages r) (c A) (c [x] B)])
           Σ (let [[r [x A] B] args] [:tSig (usages r) (c A) (c [x] B)])
           ap (reduce (fn [f a] [:app f (c a)]) (c (first args)) (rest args))
           recN (let [[[x P] z [k y s] n] args] [:recN (c [x] P) (c z) (c [k y] s) (c n)])
           elimB (let [[[x P] b t e] args] [:elimB (c [x] P) (c b) (c t) (c e)])
           ite (let [[b t e] args] [:ite (c b) (c t) (c e)])
           letp (let [[C p [a b] t] args] [:letp (c C) (c p) (c [a b] t)])
           pair (let [[S x y] args] [:pair (c S) (c x) (c y)])
           abort (let [[A t] args] [:abort (c A) (c t)])
           succ [:succ (c (first args))]
           T [:tT (c (first args))]
           (fail "unknown surface form" {:form form})))
       :else (fail "unknown surface form" {:form form})))))

;; ---------------------------------------------------------------------------
;; PA formulas and their translation (patr.clj's trT and trF), for the types
;; of closed terms. Formulas are kernel surface terms (PF.peq …).

(defn pa-term [consts t ρ]
  (cond
    (symbol? t) (case t PT.pz [:zero])
    :else (let [[h & xs] t]
            (case h
              PT.pv [:var (ρ (first xs))]
              PT.ps [:succ (pa-term consts (first xs) ρ)]
              PT.padd [:app [:app (consts 'PLUS) (pa-term consts (first xs) ρ)] (pa-term consts (second xs) ρ)]
              PT.pmul [:app [:app (consts 'TIMES) (pa-term consts (first xs) ρ)] (pa-term consts (second xs) ρ)]))))

(defn pa-formula [consts φ ρ]
  (cond
    (= φ 'PF.pbot) [:tEmpty]
    :else (let [[h & xs] φ]
            (case h
              PF.peq [:tT [:app [:app (consts 'EQ) (pa-term consts (first xs) ρ)] (pa-term consts (second xs) ρ)]]
              PF.pimp [:tPi :uw (pa-formula consts (first xs) ρ) (pa-formula consts (second xs) (comp inc ρ))]
              PF.pall [:tPi :uw [:tNat] (pa-formula consts (first xs) (fn [i] (if (zero? i) 0 (inc (ρ (dec i))))))]))))

;; ---------------------------------------------------------------------------
;; Sharing. A printed tree repeats its contexts and types at every node, and
;; every element of a conversion chain repeats most of its type, so a tree's
;; size grows with the square of its depth. share replaces each large
;; subterm that occurs more than once by a constant, defined once: the
;; kernel unfolds a constant only when it must, so evaluation keeps the
;; sharing that printing loses. The definitions are data like the tree; a
;; wrong one makes the check fail, never succeed.

(defn- sort-of
  "The kernel type of a printed subterm, from its head, or nil when it is
  not one share may name."
  [form]
  (when (seq? form)
    (let [h (first form)]
      (cond
        (= h 'List.cons) ({'Exp '(List Exp) 'U '(List U) 'Nat '(List Nat)} (second form))
        (= h 'skels) '(List Sk)
        (symbol? h) (let [ns (namespace h) nm (name h)]
                      (cond
                        (re-find #"^Exp\." (str h)) 'Exp
                        (re-find #"^DT\." (str h)) 'DT
                        (re-find #"^SkDT\." (str h)) 'SkDT
                        (re-find #"^StepDT\." (str h)) 'StepDT
                        (re-find #"^HdDT\." (str h)) 'HdDT
                        :else (do ns nm nil)))))))

(defn share
  "[defs tree′]: defs, in dependency order, are [name type body] for every
  subterm of tree with a sort, at least min-size nodes and two or more
  occurrences; tree′ refers to them by name."
  [prefix min-size tree]
  (let [size (memoize (fn sz [f] (if (seq? f) (reduce + 1 (map sz f)) 1)))
        counts (frequencies (filter #(and (seq? %) (sort-of %)) (tree-seq seq? seq tree)))
        defs (atom []) memo (atom {})
        walk (fn walk [f]
               (if-not (seq? f) f
                 (or (@memo f)
                     (let [f2 (apply list (map walk f))
                           out (if (and (sort-of f) (>= (counts f 0) 2) (>= (size f) min-size))
                                 (let [nm (symbol (str prefix "_" (count @defs)))]
                                   (swap! defs conj [nm (sort-of f) f2]) nm)
                                 f2)]
                       (swap! memo assoc f out) out))))]
    (let [t (walk tree)] [@defs t])))
