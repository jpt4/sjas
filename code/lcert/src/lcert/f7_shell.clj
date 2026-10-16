(ns lcert.f7-shell
  "Untrusted translation from the EDN parser's AST to F7's extracted data.

  The constructor indices are the declaration order of Exp in
  formal/lcert/formal/syntax.clj.  This bridge cannot grant acceptance:
  production must compare the translated term to the decoded certificate's
  term using extracted equality, then let extracted Check decide the code."
  (:require [lcert.syntax :as s]))

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
    :u (case value 0 0 1 1 :w 2 (invalid kind value))
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
