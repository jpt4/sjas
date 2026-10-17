(ns lcert.f7-shell
  "Untrusted translation from the EDN parser's AST to F7's extracted data.

  The constructor indices are the declaration order of Exp in
  formal/lcert/formal/syntax.clj.  This bridge cannot grant acceptance:
  production must compare the translated term to the decoded certificate's
  term using extracted equality, then let extracted Check decide the code."
  (:require [lcert.syntax :as s]
            [lcert.typing :as typing]
            [lcert.extracted.runtime :as rt]))

(def ^:private constructors
  ;; Argument kinds: e is an Exp, u is U, n is Nat, l is a label position,
  ;; and bs is the branch list embedded in Exp by bnil/bcons.
  {:Empty [0 []] :Unit [1 []] :Bool [2 []] :Nat [3 []]
   :Lbl [4 []] :Syn [5 []] :Dia [6 []] :R [7 []]
   :T [8 [:e]] :Pi [9 [:u :e :e]] :Sigma [10 [:u :e :e]]
   :var [11 [:n]] :star [12 []] :abort [13 [:e :e]]
   :tt [14 []] :ff [15 []] :if [16 [:e :e :e]]
   :elimBool [17 [:e :e :e :e]] :zero [18 []] :succ [19 [:e]]
   :recN [20 [:e :e :e :e]] :lbl [21 [:l]]
   :caseLbl [22 [:e :e :bs]] :sleaf [25 [:e]]
   :snode [26 [:e :e :e]] :recSyn [27 [:e :e :e :e]]
   :leaf [28 [:e]] :node [29 [:e :e :e :e]]
   :itR [30 [:e :e :e :e]] :print [31 [:e]]
   :lam [32 [:u :e :e]] :app [33 [:e :e]]
   :pair [34 [:e :e :e]] :let [35 [:e :e :e]]
   :chk [36 [:e :e]] :h1 [37 [:e :e :e :e :e]]
   :reflect [38 [:e :e :e]] :inspect [39 [:e :e :e :e :e]]})

(declare exp)

(defn- invalid [kind value]
  (throw (ex-info (str "invalid F7 " (name kind)) {:kind kind :value value})))

(defn- field [kind value]
  (case kind
    :e (exp value)
    :u (case value 0 [0] 1 [1] :w [2] (invalid kind value))
    :n (if (and (integer? value) (<= 0 value)) value (invalid kind value))
    :l (or (s/label-index value) (invalid kind value))
    :bs (if (vector? value)
          (reduce (fn [tail branch] [24 (exp branch) tail])
                  [23] (rseq value))
          (invalid kind value))))

(defn exp
  "Encode a parsed expression as an extracted Exp value, rejecting every
  unknown constructor, wrong arity, invalid usage and invalid label."
  [source]
  (when-not (and (vector? source) (keyword? (first source)))
    (invalid :expression source))
  (let [[tag & args] source
        [index kinds :as definition] (get constructors tag)]
    (when-not (and definition (= (count args) (count kinds)))
      (invalid :expression source))
    (into [index] (map field kinds args))))

(def ^:private base-formations
  #{:EmptyF :UnitF :BoolF :NatF :LblF :SynF :DiaF :RF})

(def ^:private runtime-constants
  #{:Unit :TT :FF :Zero :Lbl})

(defn- context-types [ctx]
  ;; The parser stores contexts outermost first; F7's List Exp is de Bruijn
  ;; order, with the innermost entry at the head.
  (when (seq ctx)
    (apply list (map (comp exp second) (rseq (vec ctx))))))

(defn- context-usages [ctx]
  (when (seq ctx)
    (apply list (map (comp #(field :u %) first) (rseq (vec ctx))))))

;; --- Conversion chains --------------------------------------------------
;;
;; F7 records a conversion A ≡ B as a chain (check_dt.clj's cv rules): cvRefl
;; at A, then one cvFwd or cvBwd node per step. Each node carries the
;; skeleton tree of the type it reaches and a StepDT: the step's path, its
;; head-rule record (HdDT) and both whole types. Everything here is
;; untrusted search; extracted Check decides the resulting tree.

(def ^:private kinds-by-tag
  ;; Field kinds by Exp tag, from `constructors`, plus the branch-list
  ;; constructors bnil (23) and bcons (24), which `field :bs` builds.
  (into {23 [] 24 [:e :e]} (map (fn [[_ [i kinds]]] [i kinds]) constructors)))

(defn- children
  "The Exp children of e in conv.clj's `child` order: its Exp fields (and a
  caseLbl branch list), skipping usage, index and label fields."
  [e]
  (vec (keep-indexed (fn [i kind] (when (#{:e :bs} kind) (nth e (inc i))))
                     (kinds-by-tag (first e)))))

(defn- subterms
  "Every [path subterm] of e, preorder; a path lists child indices."
  [e]
  (cons [() e]
        (mapcat (fn [i c] (map (fn [[p s]] [(cons i p) s]) (subterms c)))
                (range) (children e))))

;; Skeletons (skel.clj's Sk): unit bool nat lbl syn dia cert, arr, prod.
(def ^:private sk-unit [0])
(def ^:private sk-bool [1])
(def ^:private sk-nat [2])
(def ^:private sk-lbl [3])
(def ^:private sk-syn [4])
(def ^:private sk-dia [5])
(def ^:private sk-cert [6])
(defn- sk-arr [s t] [7 s t])
(defn- sk-prod [s t] [8 s t])
(defn- sk2 [s t G] (cons s (cons t G)))

(declare skeleton-term skeleton-at)

(defn skeleton-type
  "The SkDT tree of `SkJ true G A unit`: A is skeleton-well-formed in the
  skeleton context G (innermost first). The SkDT tag is A's Exp tag, and G
  is not stored (check_skj.clj)."
  [G A]
  (let [[tag & xs] A]
    (cond
      (<= 0 tag 7) [tag]
      (= tag 8) (let [[b] xs] [8 b (skeleton-at G b sk-bool)])
      (#{9 10} tag) (let [[r A1 B] xs]
                      [tag r A1 B (skeleton-type G A1)
                       (skeleton-type (cons (rt/skel A1) G) B)])
      :else (invalid :skeleton-type A))))

(defn- skeleton-at
  "The SkDT tree of `SkJ false G t s`. A branch list's skeleton is not
  inferable from bnil, so it is taken from s."
  [G t s]
  (let [[tag & xs] t]
    (cond
      (and (= tag 23) (= 7 (first s)) (= sk-lbl (second s))) [23 (nth s 2)]
      (and (= tag 24) (= 7 (first s)) (= sk-lbl (second s)))
      (let [[h tl] xs, s1 (nth s 2)]
        [24 h tl s1 (skeleton-at G h s1) (skeleton-at G tl s)])
      :else (let [[tree s2] (skeleton-term G t)]
              (when-not (= s s2) (invalid :skeleton {:term t :expected s :found s2}))
              tree))))

(defn- skeleton-term
  "[tree s]: the SkDT tree of `SkJ false G t s` and t's skeleton s, by
  check_skj.clj's term rules (syntax-directed)."
  [G t]
  (let [[tag & xs] t
        at (fn [G' x s] (skeleton-at G' x s))
        ty (fn [G' A] (skeleton-type G' A))]
    (case (int tag)
      11 (let [[i] xs, s (first (rt/nthS G i))]
           (when-not s (invalid :skeleton-var t))
           [[11 i s] s])
      12 [[12] sk-unit]
      13 (let [[A u] xs] [[13 A u (ty G A) (at G u sk-unit)] (rt/skel A)])
      14 [[14] sk-bool]
      15 [[15] sk-bool]
      16 (let [[b u e] xs, [tu s] (skeleton-term G u)]
           [[16 b u e s (at G b sk-bool) tu (at G e s)] s])
      17 (let [[P b u e] xs, sP (rt/skel P)]
           [[17 P b u e (ty (cons sk-bool G) P) (at G b sk-bool) (at G u sP) (at G e sP)] sP])
      18 [[18] sk-nat]
      19 (let [[n] xs] [[19 n (at G n sk-nat)] sk-nat])
      20 (let [[P z st n] xs, sP (rt/skel P)]
           [[20 P z st n (ty (cons sk-nat G) P) (at G z sP) (at (sk2 sP sk-nat G) st sP) (at G n sk-nat)] sP])
      21 [[21 (first xs)] sk-lbl]
      22 (let [[P x bs] xs, sP (rt/skel P)]
           [[22 P x bs (ty (cons sk-lbl G) P) (at G x sk-lbl) (at G bs (sk-arr sk-lbl sP))] sP])
      24 (let [[h tl] xs, [th s] (skeleton-term G h), s* (sk-arr sk-lbl s)]
           [[24 h tl s th (at G tl s*)] s*])
      25 (let [[x] xs] [[25 x (at G x sk-lbl)] sk-syn])
      26 (let [[x c1 c2] xs] [[26 x c1 c2 (at G x sk-lbl) (at G c1 sk-syn) (at G c2 sk-syn)] sk-syn])
      27 (let [[P tl tn c] xs, sP (rt/skel P)]
           [[27 P tl tn c (ty (cons sk-syn G) P) (at (cons sk-lbl G) tl sP)
             (at (sk2 sP sP (sk2 sk-syn sk-syn (cons sk-lbl G))) tn sP) (at G c sk-syn)] sP])
      28 (let [[x] xs] [[28 x (at G x sk-lbl)] sk-cert])
      29 (let [[d x r1 r2] xs]
           [[29 d x r1 r2 (at G d sk-dia) (at G x sk-lbl) (at G r1 sk-cert) (at G r2 sk-cert)] sk-cert])
      30 (let [[X g h r] xs, sX (rt/skel X)]
           [[30 X g h r (ty G X) (at G g (sk-arr sk-lbl sX))
             (at G h (sk-arr sk-dia (sk-arr sk-lbl (sk-arr sX (sk-arr sX sX))))) (at G r sk-cert)] sX])
      31 (let [[r] xs] [[31 r (at G r sk-cert)] sk-syn])
      32 (let [[r A u] xs, sA (rt/skel A), [tu s] (skeleton-term (cons sA G) u)]
           [[32 r A u s (ty G A) tu] (sk-arr sA s)])
      33 (let [[f u] xs, [tf sf] (skeleton-term G f)]
           (when-not (= 7 (first sf)) (invalid :skeleton-app t))
           (let [[_ s s2] sf] [[33 f u s s2 tf (at G u s)] s2]))
      34 (let [[S x y] xs]
           (when-not (= 10 (first S)) (invalid :skeleton-pair t))
           (let [[_ r A B] S, sA (rt/skel A), sB (rt/skel B)]
             [[34 r A B x y (ty G S) (at G x sA) (at G y sB)] (sk-prod sA sB)]))
      35 (let [[Cm p u] xs, [tp sp] (skeleton-term G p)]
           (when-not (= 8 (first sp)) (invalid :skeleton-let t))
           (let [[_ s1 s2] sp, sC (rt/skel Cm)]
             [[35 Cm p u s1 s2 (ty G Cm) tp (at (sk2 s2 s1 G) u sC)] sC]))
      36 (let [[c d] xs] [[36 c d (at G c sk-syn) (at G d sk-syn)] sk-bool])
      37 (let [[r s c e1 e2] xs]
           [[37 r s c e1 e2 (at G r sk-cert) (at G s sk-cert) (at G c sk-syn) (at G e1 sk-unit) (at G e2 sk-unit)] sk-unit])
      38 (let [[D r e] xs] [[38 D r e (ty G D) (at G r sk-cert) (at G e sk-unit)] (rt/skel D)])
      39 (let [[X r c t1 t2] xs, sX (rt/skel X), G2 (sk2 sk-unit sk-cert G)]
           [[39 X r c t1 t2 (ty G X) (at G r sk-cert) (at G c sk-syn) (at G2 t1 sX) (at G2 t2 sX)] sX])
      (invalid :skeleton-term t))))

(defn- nth-branch [bs l]
  (cond (not= 24 (first bs)) nil
        (zero? l) (nth bs 1)
        :else (recur (nth bs 2) (dec l))))

(defn- head-record
  "The HdDT record (check_hd.clj's hd-rules, side conditions dropped) of
  the redex r, chosen by r's shape; nil if r is not a redex. The record is
  only a candidate: stepDTCheck verifies it."
  [r]
  (let [[tag & xs] r, arg (fn [i] (nth r i)), sub-tag (fn [x] (first x))]
    (case (int tag)
      33 (let [[f u] xs] (when (= 32 (sub-tag f)) (let [[_ ru A t] f] [0 ru A t u])))
      35 (let [[Cm p t] xs] (when (= 34 (sub-tag p)) (let [[_ S x y] p] [1 Cm S x y t])))
      16 (let [[b t e] xs] (case (int (sub-tag b)) 14 [2 t e] 15 [3 t e] nil))
      17 (let [[P b t e] xs] (case (int (sub-tag b)) 14 [4 P t e] 15 [5 P t e] nil))
      20 (let [[P z s n] xs] (case (int (sub-tag n)) 18 [6 P z s] 19 [7 P z s (second n)] nil))
      22 (let [[P a bs] xs]
           (when (= 21 (sub-tag a))
             (when-let [b (nth-branch bs (second a))] [8 P (second a) bs b])))
      27 (let [[P tl tn c] xs]
           (case (int (sub-tag c))
             25 [9 P tl tn (second c)]
             26 (let [[_ x c1 c2] c] [10 P tl tn x c1 c2])
             nil))
      30 (let [[X g h c] xs]
           (case (int (sub-tag c))
             28 [11 X g h (second c)]
             29 (let [[_ d x r1 r2] c] [12 X g h d x r1 r2])
             nil))
      31 (let [[c] xs]
           (case (int (sub-tag c))
             28 [13 (second c)]
             29 (let [[_ d x r1 r2] c] [14 d x r1 r2])
             nil))
      36 (let [[c d] xs, cc (first (rt/codeOf c)), dc (first (rt/codeOf d))]
           (when (and cc dc) [15 c d cc dc]))
      8 (case (int (sub-tag (arg 1))) 14 [16] 15 [17] nil)
      nil)))

(defn find-step
  "A StepDT for one step from a to b that extracted stepDTCheck accepts at
  the checker chkf, or nil. Each subterm of a with a head record is tried,
  outermost first."
  [chkf a b]
  (let [chkf* (fn ([c] (fn [d] (chkf c d))) ([c d] (chkf c d)))]
    (some (fn [[p r]]
            (when-let [head (head-record r)]
              (let [step [0 (apply list p) head a b]]
                (when (true? (rt/stepDTCheck chkf* step)) step))))
          (subterms a))))

(defn- full-check
  ;; δ-steps in a chain are contracted by the whole checker. Check consults
  ;; a node-bounded restriction of it (restrC); where the two differ, the
  ;; chain is wrong and Check rejects the certificate.
  [c d] (rt/Check rt/decCert c d))

(defn- conversion
  "The F7 cv tree for a chain of parsed types [A … B] in the skeleton
  context G: cvRefl at A, then cvFwd (a step to the next type) or cvBwd (a
  step from it) per link."
  [G chain]
  (let [[A & more] (map exp chain)]
    (first
      (reduce (fn [[acc B] C]
                (if-let [step (find-step full-check B C)]
                  [[59 G A B C acc step (skeleton-type G C)] C]
                  (if-let [step (find-step full-check C B)]
                    [[60 G A B C acc step (skeleton-type G C)] C]
                    (invalid :conversion-step {:from B :to C}))))
              [[58 G A (skeleton-type G A)] A]
              more))))

(defn dt
  "Translate supported shell derivations to F7 DT data.
  Other rules are rejected until their premise and side-condition data have
  been translated. This builder remains untrusted: extracted Check decides
  whether a certificate containing this tree is accepted."
  [{:keys [rule j ctx term type prems] :as derivation}]
  (let [D (context-types ctx)
        us (context-usages ctx)]
  (cond
    (and (= j :type) (base-formations rule))
    [0 D (exp type)]

    (and (= j :type) (= rule :TF) (= :T (first type)))
    [1 D (exp (second type)) (dt (first prems))]

    (and (= j :type) (= rule :PiF) (= :Pi (first type)))
    (let [[_ u A B] type]
      [2 D (field :u u) (exp A) (exp B)
       (dt (first prems)) (dt (second prems))])

    (and (= j :type) (= rule :SigmaF) (= :Sigma (first type)))
    (let [[_ u A B] type]
      [3 D (field :u u) (exp A) (exp B)
       (dt (first prems)) (dt (second prems))])

    (and (= j :has) (runtime-constants rule))
    [31 D us (exp term) (exp type)]

    (and (= j :has0) (runtime-constants rule))
    [5 D (exp term) (exp type)]

    (and (= j :has0) (= rule :Print) (= :print (first term)))
    [25 D (exp (second term)) (dt (first prems))]

    (and (= j :has0) (= rule :SLeaf) (= :sleaf (first term)))
    [19 D (exp (second term)) (dt (first prems))]

    (and (= j :has0) (= rule :Chk) (= :chk (first term)))
    (let [[_ code expected] term]
      [26 D (exp code) (exp expected)
       (dt (first prems)) (dt (second prems))])

    (and (= j :has0) (= rule :If) (= :if (first term)))
    (let [[_ condition yes no] term]
      [12 D (exp condition) (exp yes) (exp no) (exp type)
       (dt (first prems)) (dt (second prems)) (dt (nth prems 2))])

    (and (= j :has0) (= rule :Var) (= :var (first term)))
    (let [i (second term)
          [_ A] (nth ctx (- (count ctx) i 1))]
      [4 D i (exp A)])

    (and (= j :has) (= rule :Var) (= :var (first term)))
    (let [i (second term)
          [u A] (nth ctx (- (count ctx) i 1))]
      [30 D us i (exp A) (field :u u)])

    (and (= j :has) (= rule :Lam) (= :lam (first term))
         (= :Pi (first type)))
    (let [[_ u A body] term
          B (nth type 3)]
      [32 D us (field :u u) (exp A) (exp body) (exp B)
       (dt (first prems)) (dt (second prems))])

    (and (= j :has) (= rule :App) (= :app (first term))
         (= :Pi (first (:type (first prems)))))
    (let [[hf hu] prems
          [_ r A B] (:type hf)
          [_ f arg] term
          domain-form (dt (typing/tf ctx A))
          codomain-form (dt (typing/tf (conj ctx [r A]) B))]
      (if (= r 0)
        [33 D (context-usages (:ctx hf))
         (exp f) (exp arg) (exp A) (exp B)
         (dt hf) (dt hu) domain-form codomain-form]
        [34 D (context-usages (:ctx hf)) (context-usages (:ctx hu))
         (field :u r) (exp f) (exp arg) (exp A) (exp B)
         (dt hf) (dt hu) domain-form codomain-form]))

    (and (= j :has) (= rule :Pair) (= :pair (first term))
         (= :Sigma (first type)))
    (let [[_ r A B] type
          [_ _ x y] term
          [dS dx dy] prems
          [hA hB] (:prems dS)]
      (if (= r 0)
        [35 D (context-usages (:ctx dy))
         (exp A) (exp B) (exp x) (exp y)
         (dt hA) (dt hB) (dt dx) (dt dy)]
        [36 D (context-usages (:ctx dx)) (context-usages (:ctx dy))
         (field :u r) (exp A) (exp B) (exp x) (exp y)
         (dt hA) (dt hB) (dt dx) (dt dy)]))

    (and (= j :has) (= rule :Let) (= :let (first term))
         (= :Sigma (first (:type (first prems)))))
    (let [[pair-derivation result-form body-derivation] prems
          [_ r A B] (:type pair-derivation)
          [_ C pair-term body-term] term
          outer-body-ctx (subvec (:ctx body-derivation) 0 (count ctx))]
      [37 D (context-usages (:ctx pair-derivation))
       (context-usages outer-body-ctx)
       (field :u r) (exp A) (exp B) (exp C)
       (exp pair-term) (exp body-term)
       (dt pair-derivation) (dt result-form)
       (dt (typing/tf ctx A))
       (dt (typing/tf (conj ctx [r A]) B))
       (dt body-derivation)])

    (and (= j :has) (= rule :Abort) (= :abort (first term)))
    (let [[_ target source] term
          [source-derivation target-formation] prems]
      [38 D us (exp target) (exp source)
       (dt source-derivation) (dt target-formation)])

    (and (= j :has) (= rule :Succ) (= :succ (first term)))
    [42 D us (exp (second term)) (dt (first prems))]

    (and (= j :has) (= rule :RecN) (= :recN (first term)))
    (let [[_ motive base step number] term
          [number-derivation motive-formation base-derivation step-derivation] prems
          step-outer-ctx (subvec (:ctx step-derivation) 0 (count ctx))]
      ;; The step has two bound variables. Its outer usages are scaled by
      ;; omega by rRecN; the two bound usages remain part of its DT premise.
      [43 D (context-usages (:ctx number-derivation))
       (context-usages (:ctx base-derivation))
       (context-usages step-outer-ctx)
       (exp motive) (exp base) (exp step) (exp number)
       (dt number-derivation) (dt motive-formation)
       (dt base-derivation) (dt step-derivation)])

    (and (= j :has) (= rule :CaseLbl) (= :caseLbl (first term)))
    (let [[_ motive label branches] term
          [label-derivation motive-formation & branch-derivations] prems
          branch-usages (context-usages (:ctx (first branch-derivations)))
          motive-tree (dt motive-formation)
          ;; A branch vector is an F7 bcons chain. Every branch has the
          ;; same usage context after the typer joins and raises them.
          [_ branch-tree]
          (reduce (fn [[tail-exp tail-tree] [k branch branch-derivation]]
                    [[24 (exp branch) tail-exp]
                     [46 D branch-usages (exp motive) k (exp branch)
                      tail-exp (dt branch-derivation) tail-tree motive-tree]])
                  [[23] [45 D branch-usages (exp motive)]]
                  (reverse (map vector (range) branches branch-derivations)))]
      [44 D (context-usages (:ctx label-derivation)) branch-usages
       (exp motive) (exp label) (field :bs branches)
       (dt label-derivation) motive-tree branch-tree])

    (and (= j :has) (#{:SLeaf :Leaf :Print} rule))
    [(case rule :SLeaf 47 :Leaf 50 :Print 53)
     D us (exp (second term)) (dt (first prems))]

    (and (= j :has) (= rule :SNode) (= :snode (first term)))
    (let [[_ label left right] term
          [dl da db] prems]
      [48 D (context-usages (:ctx dl)) (context-usages (:ctx da))
       (context-usages (:ctx db))
       (exp label) (exp left) (exp right)
       (dt dl) (dt da) (dt db)])

    (and (= j :has) (= rule :RecSyn) (= :recSyn (first term)))
    (let [[_ motive leaf-method node-method code] term
          [code-derivation motive-formation leaf-derivation node-derivation] prems
          leaf-outer-ctx (subvec (:ctx leaf-derivation) 0 (count ctx))
          node-outer-ctx (subvec (:ctx node-derivation) 0 (count ctx))
          base-ctx (conj ctx [:w [:Lbl]] [:w [:Syn]] [:w [:Syn]])
          first-result-type (s/subst (s/shift motive 3 1) [:var 1])
          second-result-type (s/subst (s/shift motive 4 1) [:var 1])]
      ;; rRecS requires formation of each recursive-result binder as well
      ;; as the four derivations recorded by the shell typer.
      [49 D (context-usages (:ctx code-derivation))
       (context-usages leaf-outer-ctx) (context-usages node-outer-ctx)
       (exp motive) (exp leaf-method) (exp node-method) (exp code)
       (dt code-derivation) (dt motive-formation)
       (dt leaf-derivation) (dt node-derivation)
       (dt (typing/tf base-ctx first-result-type))
       (dt (typing/tf (conj base-ctx [1 first-result-type])
                      second-result-type))])

    (and (= j :has) (= rule :Node) (= :node (first term)))
    (let [[_ token label left right] term
          [dd dl da db] prems]
      [51 D (context-usages (:ctx dd)) (context-usages (:ctx dl))
       (context-usages (:ctx da)) (context-usages (:ctx db))
       (exp token) (exp label) (exp left) (exp right)
       (dt dd) (dt dl) (dt da) (dt db)])

    (and (= j :has) (= rule :ItR) (= :itR (first term)))
    (let [[_ target leaf-method node-method tree] term
          [target-formation leaf-derivation node-derivation tree-derivation] prems]
      [52 D (context-usages (:ctx leaf-derivation))
       (context-usages (:ctx node-derivation))
       (context-usages (:ctx tree-derivation))
       (exp target) (exp leaf-method) (exp node-method) (exp tree)
       (dt target-formation) (dt leaf-derivation)
       (dt node-derivation) (dt tree-derivation)])

    (and (= j :has) (= rule :Chk) (= :chk (first term)))
    (let [[_ code expected] term
          [dc de] prems]
      [54 D (context-usages (:ctx dc)) (context-usages (:ctx de))
       (exp code) (exp expected) (dt dc) (dt de)])

    (and (= j :has) (= rule :H1) (= :h1 (first term)))
    (let [[_ r s code positive negative] term
          [dr ds dc de1 de2] prems]
      [55 D (context-usages (:ctx dr)) (context-usages (:ctx ds))
       (context-usages (:ctx dc)) (context-usages (:ctx de1))
       (context-usages (:ctx de2))
       (exp r) (exp s) (exp code) (exp positive) (exp negative)
       (dt dr) (dt ds) (dt dc) (dt de1) (dt de2)])

    (and (= j :has) (= rule :Reflect) (= :reflect (first term)))
    (let [[_ target certificate evidence] term
          [dr de] prems
          ;; baseCode is extracted from the checked F7 definition. A missing
          ;; base code rejects the shell result before any evaluator sees it.
          encoded-target (first (rt/baseCode (exp target)))]
      (when-not encoded-target (invalid :derivation derivation))
      [56 D (context-usages (:ctx dr)) (context-usages (:ctx de))
       (exp target) encoded-target (exp certificate) (exp evidence)
       (dt dr) (dt de)])

    (and (= j :has) (= rule :Inspect) (= :inspect (first term)))
    (let [[_ target certificate code success failure] term
          [dr dc dX d1 d2] prems
          branch-outer-ctx (subvec (:ctx d1) 0 (count ctx))
          cert-ctx (conj ctx [1 [:R]])
          check-term [:chk [:print [:var 0]] (s/shift code 1)]
          positive-assumption [:T check-term]
          negative-assumption [:T (s/not-term check-term)]]
      ;; The formal rInsp rule has two formation premises absent from the
      ;; shell typer's derivation. Build them here; Check verifies both.
      [57 D (context-usages (:ctx dr)) (context-usages (:ctx dc))
       (context-usages branch-outer-ctx)
       (exp target) (exp certificate) (exp code)
       (exp success) (exp failure)
       (dt dr) (dt dc) (dt dX)
       (dt (typing/tf cert-ctx positive-assumption))
       (dt (typing/tf cert-ctx negative-assumption))
       (dt d1) (dt d2)])

    (and (= j :has) (= rule :If) (= :if (first term)))
    (let [[_ condition yes no] term
          [test-derivation yes-derivation no-derivation] prems]
      [40 D (context-usages (:ctx test-derivation))
       (context-usages (:ctx yes-derivation))
       (exp condition) (exp yes) (exp no) (exp type)
       (dt test-derivation) (dt yes-derivation) (dt no-derivation)])

    (and (= j :has) (= rule :ElimBool) (= :elimBool (first term)))
    (let [[_ motive condition yes no] term
          [condition-derivation motive-formation yes-derivation no-derivation] prems]
      ;; The two branches share a joined usage context. The motive's
      ;; formation premise lives under a type-level Bool binder.
      [41 D (context-usages (:ctx condition-derivation))
       (context-usages (:ctx yes-derivation))
       (exp motive) (exp condition) (exp yes) (exp no)
       (dt condition-derivation) (dt motive-formation)
       (dt yes-derivation) (dt no-derivation)])

    ;; --- Type-level (:has0) rules: F7's z* rules (check_dt.clj's tl-rules),
    ;; which carry no usage vectors. A formation premise that the typer does
    ;; not record is rebuilt with typing/tf, as for the runtime rules.
    (and (= j :has0) (= rule :Lam) (= :lam (first term)) (= :Pi (first type)))
    (let [[_ u A body] term]
      [7 D (field :u u) (exp A) (exp body) (exp (nth type 3))
       (dt (first prems)) (dt (second prems))])

    (and (= j :has0) (= rule :App) (= :app (first term))
         (= :Pi (first (:type (first prems)))))
    (let [[hf hu] prems
          [_ r A B] (:type hf)
          [_ f arg] term]
      [8 D (field :u r) (exp f) (exp arg) (exp A) (exp B) (dt hf) (dt hu)
       (dt (typing/tf ctx A)) (dt (typing/tf (conj ctx [r A]) B))])

    (and (= j :has0) (= rule :Pair) (= :pair (first term)) (= :Sigma (first type)))
    (let [[_ r A B] type
          [_ _ x y] term
          [dS dx dy] prems
          [hA hB] (:prems dS)]
      [9 D (field :u r) (exp A) (exp B) (exp x) (exp y) (dt hA) (dt hB) (dt dx) (dt dy)])

    (and (= j :has0) (= rule :Let) (= :let (first term))
         (= :Sigma (first (:type (first prems)))))
    (let [[dp dC db] prems
          [_ r A B] (:type dp)
          [_ C p body] term]
      [10 D (field :u r) (exp A) (exp B) (exp C) (exp p) (exp body) (dt dp) (dt dC)
       (dt (typing/tf ctx A)) (dt (typing/tf (conj ctx [r A]) B)) (dt db)])

    (and (= j :has0) (= rule :Abort) (= :abort (first term)))
    (let [[_ target source] term
          [source-derivation target-formation] prems]
      [11 D (exp target) (exp source) (dt source-derivation) (dt target-formation)])

    (and (= j :has0) (= rule :ElimBool) (= :elimBool (first term)))
    (let [[_ motive condition yes no] term
          [db dP d1 d2] prems]
      [13 D (exp motive) (exp condition) (exp yes) (exp no) (dt db) (dt dP) (dt d1) (dt d2)])

    (and (= j :has0) (= rule :Succ) (= :succ (first term)))
    [14 D (exp (second term)) (dt (first prems))]

    (and (= j :has0) (= rule :RecN) (= :recN (first term)))
    (let [[_ motive base step number] term
          [dn dP dz ds] prems]
      [15 D (exp motive) (exp base) (exp step) (exp number) (dt dn) (dt dP) (dt dz) (dt ds)])

    (and (= j :has0) (= rule :CaseLbl) (= :caseLbl (first term)))
    (let [[_ motive label branches] term
          [dx dP & branch-derivations] prems
          ;; The branch vector as zBcons over zBnil, as for rCaseL.
          [_ branch-tree]
          (reduce (fn [[tail-exp tail-tree] [k branch branch-derivation]]
                    [[24 (exp branch) tail-exp]
                     [18 D (exp motive) k (exp branch) tail-exp (dt branch-derivation) tail-tree]])
                  [[23] [17 D (exp motive)]]
                  (reverse (map vector (range) branches branch-derivations)))]
      [16 D (exp motive) (exp label) (field :bs branches) (dt dx) (dt dP) branch-tree])

    (and (= j :has0) (= rule :SNode) (= :snode (first term)))
    (let [[_ label left right] term
          [dl da db] prems]
      [20 D (exp label) (exp left) (exp right) (dt dl) (dt da) (dt db)])

    (and (= j :has0) (= rule :RecSyn) (= :recSyn (first term)))
    (let [[_ motive leaf-method node-method code] term
          [dc dP dl dn] prems]
      [21 D (exp motive) (exp leaf-method) (exp node-method) (exp code)
       (dt dc) (dt dP) (dt dl) (dt dn)])

    (and (= j :has0) (= rule :Leaf) (= :leaf (first term)))
    [22 D (exp (second term)) (dt (first prems))]

    (and (= j :has0) (= rule :Node) (= :node (first term)))
    (let [[_ token label left right] term
          [dd dl da db] prems]
      [23 D (exp token) (exp label) (exp left) (exp right) (dt dd) (dt dl) (dt da) (dt db)])

    (and (= j :has0) (= rule :ItR) (= :itR (first term)))
    (let [[_ target leaf-method node-method tree] term
          [dX dl dn dr] prems]
      [24 D (exp target) (exp leaf-method) (exp node-method) (exp tree)
       (dt dX) (dt dl) (dt dn) (dt dr)])

    (and (= j :has0) (= rule :H1) (= :h1 (first term)))
    (let [[_ r s code positive negative] term
          [dr ds dc de1 de2] prems]
      [27 D (exp r) (exp s) (exp code) (exp positive) (exp negative)
       (dt dr) (dt ds) (dt dc) (dt de1) (dt de2)])

    (and (= j :has0) (= rule :Reflect) (= :reflect (first term)))
    (let [[_ target certificate evidence] term
          [dr de] prems
          encoded-target (first (rt/baseCode (exp target)))]
      (when-not encoded-target (invalid :derivation derivation))
      [28 D (exp target) encoded-target (exp certificate) (exp evidence) (dt dr) (dt de)])

    (and (= j :has0) (= rule :Inspect) (= :inspect (first term)))
    (let [[_ target certificate code success failure] term
          [dr dc dX d1 d2] prems]
      [29 D (exp target) (exp certificate) (exp code) (exp success) (exp failure)
       (dt dr) (dt dc) (dt dX) (dt d1) (dt d2)])

    ;; A term retyped along a conversion chain: zConv at type level, rConv
    ;; at runtime. The premises are the term's derivation at its own type
    ;; and the target type's formation (typing/conv).
    (and (#{:has :has0} j) (= rule :Conv))
    (let [[typed formation] prems
          A (exp (:type typed))
          cv (conversion (rt/skels D) (:chain derivation))]
      (if (= j :has)
        [39 D us (exp term) A (exp type) (dt typed) (dt formation) cv]
        [6 D (exp term) A (exp type) (dt typed) (dt formation) cv]))

    :else (invalid :derivation derivation))))

(defn on-large-stack
  "Call f on a thread with a 1 GB stack and return its value, rethrowing
  its exception. Extracted recursions (decoding, cnodes, dtCheck) are as
  deep as a certificate's Code tree, which is deeper than the default
  stack for the tutorial's bounded consistency statement. The stack is
  reserved address space; only the pages used are committed."
  [f]
  (let [result (promise)
        thread (Thread. nil
                        (fn [] (deliver result (try {:value (f)} (catch Throwable e {:error e}))))
                        "f7-large-stack" (* 1024 1024 1024))]
    (.start thread)
    (.join thread)
    (let [{:keys [value error]} @result]
      (if error (throw error) value))))

(defn check
  "Extracted Check of an F7 certificate code at the F7 type `type`."
  [code type]
  (on-large-stack #(true? (rt/Check rt/decCert code (rt/encE type)))))

(defn certificate
  "Encode a supported Θₙ derivation in canonical F7. The production runner
  must check the result at the caller's type and budget before executing."
  [budget derivation]
  (when-not (and (integer? budget) (<= 0 budget)
                 (= :has (:j derivation))
                 (= (:ctx derivation)
                    (vec (repeat budget [1 [:Dia]]))))
    (invalid :derivation derivation))
  (on-large-stack
    (fn []
      (let [term (exp (:term derivation))
            type (exp (:type derivation))
            runtime-tree (dt derivation)
            ;; Check's formation premise is closed even when the runtime root is
            ;; Θₙ: see check_spec.clj's DTJ.tl Bool.true [] A Unit.
            type-tree (dt (typing/tf [] (:type derivation)))]
        (rt/encCert budget term type runtime-tree type-tree)))))

(defn accept-certificate
  "Gate a shell certificate at the caller's exact budget, closed type and
  parsed term. Return only the judgment decoded from the accepted F7 code.
  This function is a shell boundary: all acceptance and judgment equality
  decisions are made by extracted Check, concl, djEq and expEq."
  [budget expected-type parsed-term code]
  (when-not (and (integer? budget) (<= 0 budget))
    (invalid :budget budget))
  (let [expected (exp expected-type)
        parsed (exp parsed-term)
        accepted? (check code expected)
        decoded (when accepted? (first (on-large-stack #(rt/decCert code))))]
    (when-not decoded (invalid :certificate code))
    (let [[m [term [type [runtime-tree type-tree]]]] decoded
          root (first (on-large-stack #(rt/concl runtime-tree)))]
      (when-not (and (= m budget)
                     (true? (rt/expEq type expected))
                     (true? (rt/expEq term parsed))
                     root
                     (true? (rt/djEq root
                                     [1 (rt/thetaD budget) (rt/thetaU budget)
                                      term type])))
        (invalid :checked-judgment decoded))
      {:budget m :term term :type type :runtime-tree runtime-tree
       :type-tree type-tree :code code})))
