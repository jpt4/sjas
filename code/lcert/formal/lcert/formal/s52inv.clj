(ns lcert.formal.s52inv
  "Theorem 5.2, universal form (ADR-0006): inversion of the evaluators.

  Ev and EvE are inductive families indexed by (n, src, v).  Ansatz has no
  usable `cases` on such a family (the tactic needs the indices to unify), so
  the generation lemmas are obtained by a function, not by case analysis:
  InvAll n src v is a Prop defined by recursion on the SOURCE (and, for
  applications, on the applied value), which says exactly what a derivation
  of (n, src, v) must consist of -- a disjunction over the rules that can
  conclude that source, each an existential over the rule's own variables
  with the rule's premises -- and ev_inv / evE_inv prove, by one induction
  on the derivation, that every derivation satisfies it.

  The clause for each term constructor is GENERATED from the constructor
  table of the evaluator (the same declarations the kernel checks, read
  from eval.clj / erase.clj as safety52.clj reads the Ok tables), so the
  inversion cannot drift from the rules.  The sources that are not terms
  (application, recN iteration, recSyn, itR) are four small hand-written
  functions.  The uses are in s52univ.clj.

  Nothing here depends on the typing of the program: the lemmas hold for
  every derivation.  Every rule that can conclude a source appears in its
  disjunction, in particular eAbort, eH1 and the three reflect rules."
  (:require [ansatz.core :as a]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef! lv]]
            [lcert.formal.erase]
            [lcert.formal.s52sim :refer [rv-rec]]))

;; --- the rule tables -------------------------------------------------------------

(defn ctor-forms
  "The constructor declarations (name binder... :where [n src v]) of the
  inductive `pred`, read from the source file `resource`."
  [resource pred]
  (with-open [r (java.io.PushbackReader. (io/reader (io/resource resource)))]
    (loop []
      (let [f (read {:eof nil} r)]
        (cond (nil? f) (throw (ex-info "inductive not found" {:pred pred}))
              (and (seq? f) (= 'a/inductive (first f)) (= pred (second f)))
              (filter seq? (drop 3 f))
              :else (recur))))))

(defn parse-ctor
  "{:name :fields [[f ty]...] :where [n src v]} of one constructor form."
  [[nm & more]]
  {:name nm
   :fields (vec (take-while vector? more))
   :where (second (drop-while #(not= :where %) more))})

(def exp-fields @#'lcert.formal.syntactic/exp-fields)

(defn- premise? [pred ty]
  (and (seq? ty) (or (= pred (first ty)) (= 'Eq (first ty)))))

(defn- conj-of [props]
  (cond (empty? props) 'True
        (= 1 (count props)) (first props)
        :else (list 'And (first props) (conj-of (rest props)))))

(defn- exists-of [binders body]
  (reduce (fn [acc [x ty]] (list 'Exists (list 'fn [x :- ty] acc))) body (reverse binders)))

(defn- or-of [alts]
  (cond (empty? alts) 'False
        (= 1 (count alts)) (first alts)
        :else (list 'Or (first alts) (or-of (rest alts)))))

;; --- term sources: one clause per rule, grouped by the term constructor ----------------

(defn tm-rule
  "Analysis of a rule whose source is (EvSrc.tm rho (Exp.C args...)):
  {:ctor C, :exists [[f ty]...], :props [...], :sub {...}, :premises [[f ty]...],
   :value-eq? bool}.  The clause variables are ZN (budget), ZRHO (environment),
  ZV (value) and q_i (the i-th argument of the Exp constructor)."
  [pred {:keys [fields where]}]
  (let [[wn src wv] where
        [_ wrho e] src
        ctor (symbol (subs (str (if (seq? e) (first e) e)) (count "Exp.")))
        args (if (seq? e) (rest e) [])
        _ (assert (every? symbol? args) (str "non-variable pattern in " where))
        qs (map #(symbol (str "q_" %)) (range (count args)))
        sub (merge {wn 'ZN wrho 'ZRHO} (zipmap args qs))
        data (remove (fn [[_ ty]] (premise? pred ty)) fields)
        premises (filter (fn [[_ ty]] (premise? pred ty)) fields)
        bare? (and (symbol? wv) (some (fn [[f _]] (= f wv)) data))
        sub (if bare? (assoc sub wv 'ZV) sub)
        ex (remove (fn [[f _]] (contains? sub f)) data)
        sb (fn [x] (walk/postwalk-replace sub x))
        props (concat (when-not bare? [(list 'Eq 'RV 'ZV (sb wv))])
                      (map (fn [[_ ty]] (sb ty)) premises))]
    {:ctor ctor :exists (vec ex) :props (vec props) :sub sub
     :premises (vec premises) :value-eq? (not bare?) :data (vec data)}))

(defn- ST [] '(=> Nat (List RV) RV Prop))

(defn tm-minor
  "The Exp.rec minor premise for Exp constructor `ctor`, from its rules."
  [pred ctor fs rules]
  (let [bs (vec (concat (mapcat (fn [i [_ ty _]] [(symbol (str "q_" i)) :- ty]) (range) fs)
                        (mapcat (fn [[f ty _]] (when (= ty 'Exp) [(symbol (str "i_" f)) :- (ST)])) fs)))
        alts (for [r rules] (exists-of (:exists r) (conj-of (:props r))))
        body (list 'fn '[ZN :- Nat, ZRHO :- (List RV), ZV :- RV] (or-of alts))]
    (if (seq bs) (list 'fn bs body) body)))

;; --- the non-term sources -----------------------------------------------------------

(def ^:private PARAMS
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])
(def ^:private PTYPES
  '[[chkf (=> Code Code Bool)] [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] [encTy (=> Exp Code)]])

(defn- def-over-params!
  "Define nm : forall chkf dec encTy, ty := fn chkf dec encTy, body (kernel checked)."
  [nm ty body]
  (kdef! nm
    (reduce (fn [acc [x t]] (list 'forall [x t] acc)) ty (reverse PTYPES))
    (list 'fn PARAMS body)))

(defn- cons* [v rho] (list 'List.cons 'RV v rho))

(defn- ap-def
  "InvAp n f a v: what a derivation of (n, ap f a, v) consists of.  By the
  shape of f: a closure runs its body; the empty list defaults; a branch list
  answers label 0 with its head and label k+1 by the tail."
  [P]
  (let [ev (fn [n src v] (list P 'chkf 'dec 'encTy n src v))
        T '(=> Nat RV RV Prop)
        T4 '(=> Nat RV RV RV Prop)
        no3 '(fn [n :- Nat, a :- RV, v :- RV] False)
        no4 '(fn [n :- Nat, h :- RV, t :- RV, v :- RV] False)
        ;; the label case, on the argument a of a branch list h::t
        lbl-case (list 'fn '[l :- Nat]
                   (list 'Nat.rec$1 '(fn [_ :- Nat] (=> Nat RV RV RV Prop))
                     '(fn [n :- Nat, h :- RV, t :- RV, v :- RV] (Eq RV v h))
                     (list 'fn '[k :- Nat, ih :- (=> Nat RV RV RV Prop)]
                       (list 'fn '[n :- Nat, h :- RV, t :- RV, v :- RV]
                         (ev 'n '(EvSrc.ap t (RV.lbl k)) 'v)))
                     'l))
        on-arg (rv-rec T4 T4 no4 no4 {:lbl lbl-case} 'a false)
        bcons-case (list 'fn ['h :- 'RV, 't :- 'RV, 'hi :- T, 'ti :- T]
                     (list 'fn '[n :- Nat, a :- RV, v :- RV] (list on-arg 'n 'h 't 'v)))
        clos-case (list 'fn ['rho :- '(List RV), 'body :- 'Exp, 'ri :- T]
                    (list 'fn '[n :- Nat, a :- RV, v :- RV]
                      (ev 'n (list 'EvSrc.tm (cons* 'a 'rho) 'body) 'v)))
        bnil-case '(fn [s :- Sk] (fn [n :- Nat, a :- RV, v :- RV] (Eq RV v (rdflt s))))]
    (list 'fn '[n :- Nat, f :- RV, a :- RV, v :- RV]
      (list (rv-rec T T no3 no3 {:clos clos-case :bnil bnil-case :bcons bcons-case} 'f false)
            'n 'a 'v))))

(defn- iter-def [P]
  (let [ev (fn [n src v] (list P 'chkf 'dec 'encTy n src v))
        T '(=> Nat (List RV) Exp RV RV Prop)]
    (list 'fn '[n :- Nat, rho :- (List RV), st :- Exp, k :- Nat, acc :- RV, v :- RV]
      (list (list 'Nat.rec$1 '(fn [_ :- Nat] (=> Nat (List RV) Exp RV RV Prop))
              '(fn [n :- Nat, rho :- (List RV), st :- Exp, acc :- RV, v :- RV] (Eq RV v acc))
              (list 'fn ['k :- 'Nat, 'ih :- T]
                (list 'fn '[n :- Nat, rho :- (List RV), st :- Exp, acc :- RV, v :- RV]
                  (list 'Exists (list 'fn '[mid :- RV]
                    (list 'And (ev 'n '(EvSrc.iter rho st k acc) 'mid)
                      (ev 'n (list 'EvSrc.tm (cons* 'mid (cons* '(RV.nat k) 'rho)) 'st) 'v))))))
              'k)
            'n 'rho 'st 'acc 'v))))

(defn- recs-def [P]
  (let [ev (fn [n src v] (list P 'chkf 'dec 'encTy n src v))
        T '(=> Nat (List RV) Exp Exp RV Prop)]
    (list 'fn '[n :- Nat, rho :- (List RV), tl :- Exp, tn :- Exp, c :- Code, v :- RV]
      (list (list 'Code.rec$1 '(fn [_ :- Code] (=> Nat (List RV) Exp Exp RV Prop))
              (list 'fn '[l :- Nat]
                (list 'fn '[n :- Nat, rho :- (List RV), tl :- Exp, tn :- Exp, v :- RV]
                  (ev 'n (list 'EvSrc.tm (cons* '(RV.lbl l) 'rho) 'tl) 'v)))
              (list 'fn ['l :- 'Nat, 'a :- 'Code, 'b :- 'Code, 'ai :- T, 'bi :- T]
                (list 'fn '[n :- Nat, rho :- (List RV), tl :- Exp, tn :- Exp, v :- RV]
                  (list 'Exists (list 'fn '[ya :- RV]
                    (list 'Exists (list 'fn '[yb :- RV]
                      (list 'And (ev 'n '(EvSrc.recs rho tl tn a) 'ya)
                        (list 'And (ev 'n '(EvSrc.recs rho tl tn b) 'yb)
                          (ev 'n (list 'EvSrc.tm
                                  (cons* 'yb (cons* 'ya (cons* '(RV.code b) (cons* '(RV.code a)
                                    (cons* '(RV.lbl l) 'rho))))) 'tn) 'v)))))))))
              'c)
            'n 'rho 'tl 'tn 'v))))

(defn- itr-def [P]
  (let [ev (fn [n src v] (list P 'chkf 'dec 'encTy n src v))
        T '(=> Nat RV RV RV Prop)]
    (list 'fn '[n :- Nat, g :- RV, h :- RV, c :- Code, v :- RV]
      (list (list 'Code.rec$1 '(fn [_ :- Code] (=> Nat RV RV RV Prop))
              (list 'fn '[l :- Nat]
                (list 'fn '[n :- Nat, g :- RV, h :- RV, v :- RV]
                  (ev 'n '(EvSrc.ap g (RV.lbl l)) 'v)))
              (list 'fn ['l :- 'Nat, 'a :- 'Code, 'b :- 'Code, 'ai :- T, 'bi :- T]
                (list 'fn '[n :- Nat, g :- RV, h :- RV, v :- RV]
                  (exists-of '[[ya RV] [yb RV] [v1 RV] [v2 RV] [v3 RV]]
                    (conj-of [(ev 'n '(EvSrc.itr g h a) 'ya)
                              (ev 'n '(EvSrc.itr g h b) 'yb)
                              (ev 'n '(EvSrc.ap h RV.token) 'v1)
                              (ev 'n '(EvSrc.ap v1 (RV.lbl l)) 'v2)
                              (ev 'n '(EvSrc.ap v2 ya) 'v3)
                              (ev 'n '(EvSrc.ap v3 yb) 'v)]))))
              'c)
            'n 'g 'h 'v))))

;; --- assembling InvAll and proving the inversion theorem --------------------------------

;; Rules whose source is not a term: proofs by hand (their instance of the inversion).
(def ^:private hand-proofs
  '{apClos [(exact h)] apBnil [(rfl)] apBconsZ [(rfl)] apBconsS [(exact hp)]
    eIterZ [(rfl)]
    eIterS [(constructor) (exact mid) (constructor) (exact hp) (exact hs)]
    eRecSL [(exact h)]
    eRecSN [(constructor) (exact ya) (constructor) (exact yb)
            (constructor) (exact ha) (constructor) (exact hb) (exact hs)]
    eItL [(exact ha)]
    eItN [(constructor) (exact ya) (constructor) (exact yb) (constructor) (exact v1)
          (constructor) (exact v2) (constructor) (exact v3)
          (constructor) (exact ha) (constructor) (exact hb) (constructor) (exact h1)
          (constructor) (exact h2) (constructor) (exact h3) (exact h4)]})

(defn- tm-rule? [{:keys [where]}] (= 'EvSrc.tm (first (second where))))

(defn- tm-proof
  "Tactics proving the clause of rule r, the idx-th of n alternatives."
  [r idx n]
  (concat
    (repeat idx '(right))
    (when (< idx (dec n)) ['(left)])
    (mapcat (fn [[f _]] [(list 'constructor) (list 'exact f)]) (:exists r))
    (let [ps (concat (when (:value-eq? r) ['(rfl)])
                     (map (fn [[f _]] (list 'exact f)) (:premises r)))]
      (cond (empty? ps) ['(exact True.intro)]
            :else (concat (mapcat (fn [p] ['(constructor) p]) (butlast ps)) [(last ps)])))))

(defn generate!
  "Declare InvT/InvAp/InvIter/InvRecs/InvItr/InvAll for evaluator `P` (read from
  `resource`) and prove the inversion theorem `thm-name`."
  [P resource thm-name]
  (let [sfx (str "_" P)
        rules (map parse-ctor (ctor-forms resource P))
        tms (map #(tm-rule P %) (filter tm-rule? rules))
        by-ctor (group-by :ctor tms)
        nm (fn [base] (symbol (str base sfx)))]
    ;; InvT: Exp.rec over the constructor table
    (def-over-params! (nm "InvT") '(=> Exp Nat (List RV) RV Prop)
      (list 'fn '[e :- Exp]
        (concat (list 'Exp.rec$1 (list 'fn '[_ :- Exp] (ST)))
                (for [[c fs] exp-fields] (tm-minor P c fs (get by-ctor c [])))
                ['e])))
    (def-over-params! (nm "InvAp") '(=> Nat RV RV RV Prop) (ap-def P))
    (def-over-params! (nm "InvIter") '(=> Nat (List RV) Exp Nat RV RV Prop) (iter-def P))
    (def-over-params! (nm "InvRecs") '(=> Nat (List RV) Exp Exp Code RV Prop) (recs-def P))
    (def-over-params! (nm "InvItr") '(=> Nat RV RV Code RV Prop) (itr-def P))
    (def-over-params! (nm "InvAll") '(=> Nat EvSrc RV Prop)
      (list 'fn '[n :- Nat, src :- EvSrc, v :- RV]
        (list (list 'EvSrc.rec$1 '(fn [_ :- EvSrc] (=> Nat RV Prop))
                (list 'fn '[rho :- (List RV), e :- Exp]
                  (list 'fn '[n :- Nat, v :- RV] (list (nm "InvT") 'chkf 'dec 'encTy 'e 'n 'rho 'v)))
                (list 'fn '[f :- RV, a :- RV]
                  (list 'fn '[n :- Nat, v :- RV] (list (nm "InvAp") 'chkf 'dec 'encTy 'n 'f 'a 'v)))
                (list 'fn '[rho :- (List RV), st :- Exp, k :- Nat, acc :- RV]
                  (list 'fn '[n :- Nat, v :- RV] (list (nm "InvIter") 'chkf 'dec 'encTy 'n 'rho 'st 'k 'acc 'v)))
                (list 'fn '[rho :- (List RV), tl :- Exp, tn :- Exp, c :- Code]
                  (list 'fn '[n :- Nat, v :- RV] (list (nm "InvRecs") 'chkf 'dec 'encTy 'n 'rho 'tl 'tn 'c 'v)))
                (list 'fn '[g :- RV, h :- RV, c :- Code]
                  (list 'fn '[n :- Nat, v :- RV] (list (nm "InvItr") 'chkf 'dec 'encTy 'n 'g 'h 'c 'v)))
                'src)
              'n 'v)))
    ;; the theorem: every derivation satisfies its inversion
    (let [tm-idx (into {} (for [[_ rs] by-ctor, [i r] (map-indexed vector rs)] [r [i (count rs)]]))
          tm-by-name (zipmap (map :name (filter tm-rule? rules)) tms)]
      (a/prove-theorem thm-name
        (lv (vec (concat PARAMS ['n0 :- 'Nat 'src0 :- 'EvSrc 'v0 :- 'RV
                                 'der :- (list P 'chkf 'dec 'encTy 'n0 'src0 'v0)])))
        (lv (list (nm "InvAll") 'chkf 'dec 'encTy 'n0 'src0 'v0))
        (lv (into ['(induction der)]
              (mapcat (fn [r]
                        (if (tm-rule? r)
                          (let [tr (tm-by-name (:name r))
                                [i n] (tm-idx tr)]
                            (tm-proof tr i n))
                          (hand-proofs (:name r))))
                      rules)))))))

(generate! 'Ev "lcert/formal/eval.clj" 'ev_inv)
(generate! 'EvE "lcert/formal/erase.clj" 'evE_inv)
