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

    :else (invalid :derivation derivation))))

(defn certificate
  "Encode a supported closed derivation in canonical F7. The production runner
  must check the result at the caller's type and budget before executing."
  [budget derivation]
  (when-not (and (integer? budget) (<= 0 budget)
                 (zero? budget) (= :has (:j derivation)))
    (invalid :derivation derivation))
  (let [term (exp (:term derivation))
        type (exp (:type derivation))
        runtime-tree (dt derivation)
        type-tree (dt (typing/tf [] (:type derivation)))]
    (rt/encCert budget term type runtime-tree type-tree)))
