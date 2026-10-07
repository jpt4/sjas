(ns lcert.formal.s52det
  "Theorem 5.2, universal form (ADR-0006): Ok is determined up to bnil tags.

  Main lemma (detOk / detOkE).  If Ok n src v is a safe trace, then EVERY
  evaluation Ev n src' v' of a source src' that equals src up to bnil tags
  (flL / flV, s52sim.clj) is itself a safe trace Ok n src' v' of a value v'
  equal to v up to tags.  The same for OkE and EvE under the budget
  hypothesis for reflect.

  Why this is true: the rules of Ev are syntax- and value-directed, so
  two evaluations of the same program can differ only where a rule has a
  free choice: eBnil's tag, and (for EvE) the erasure of a reflected program.
  Ok has no rule for apBnil, so a safe trace never lets a tag decide a
  branch; Ok has no abort, H1 or H node, so an Ev node of those kinds has
  no Ok counterpart, and the induction on the Ok trace never meets one.

  The proof is an induction on the Ok trace (an explicit recursor with one
  lemma per Ok rule) that inverts the Ev derivation at each node
  (s52inv.clj) and applies the induction hypotheses to its premises.  The
  case lemmas are generated from the rule tables of Ok and Ev by the
  generator below; the rules whose Ev counterpart has several alternatives
  (ite, elimB, inspect, reflect) have a short list of extra tactics.

  MP is the induction motive, defined by recursion on the source: it states
  the lemma for the form of the source (term, application, iteration,
  recSyn, itR)."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef! lv]]
            [lcert.formal.erase]
            [lcert.formal.s52sim]
            [lcert.formal.s52inv :as inv]))

(def ^:private PARAMS
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code)])
(def ^:private PTYPES
  '[[chkf (=> Code Code Bool)] [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] [encTy (=> Exp Code)]])
(def ^:private PS '[chkf dec encTy])

;; --- the motive ---------------------------------------------------------------------

(defn- app* [f & args] (apply list f args))
(defn- ev [P n src v] (list P 'chkf 'dec 'encTy n src v))
(defn- fl= [x y] (list 'Eq 'RV (list 'flV x) (list 'flV y)))
(defn- fll= [x y] (list 'Eq '(List RV) (list 'flL x) (list 'flL y)))

(defn mp-form
  "The statement of the main lemma for the node (n, src, v) of Ok (the trace
  predicate O, whose evaluator is P): every Ev run of a source equal to src
  up to tags is a safe trace of a value equal to v up to tags.  The unprimed
  names are the node's; the bound variables pR pV pF pA ... are the other
  run's."
  [P O src n v]
  (let [[kind & as] src
        res (fn [src2] (list 'And (ev O n src2 'pV) (fl= v 'pV)))
        imp (fn [src2 & hyps] (apply list '=> (ev P n src2 'pV) (concat hyps [(res src2)])))]
    (case (str kind)
      "EvSrc.tm" (let [[rho e] as src2 (list 'EvSrc.tm 'pR e)]
                   (list 'forall '[pR (List RV)] (list 'forall '[pV RV] (imp src2 (fll= rho 'pR)))))
      "EvSrc.ap" (let [[f x] as src2 (list 'EvSrc.ap 'pF 'pA)]
                   (list 'forall '[pF RV] (list 'forall '[pA RV] (list 'forall '[pV RV]
                     (imp src2 (fl= f 'pF) (fl= x 'pA))))))
      "EvSrc.iter" (let [[rho st k acc] as src2 (list 'EvSrc.iter 'pR st k 'pA)]
                     (list 'forall '[pR (List RV)] (list 'forall '[pA RV] (list 'forall '[pV RV]
                       (imp src2 (fll= rho 'pR) (fl= acc 'pA))))))
      "EvSrc.recs" (let [[rho tl tn c] as src2 (list 'EvSrc.recs 'pR tl tn c)]
                     (list 'forall '[pR (List RV)] (list 'forall '[pV RV]
                       (imp src2 (fll= rho 'pR)))))
      "EvSrc.itr" (let [[g h c] as src2 (list 'EvSrc.itr 'pF 'pA c)]
                    (list 'forall '[pF RV] (list 'forall '[pA RV] (list 'forall '[pV RV]
                      (imp src2 (fl= g 'pF) (fl= h 'pA)))))))))

(defn- over-params! [nm ty body]
  (kdef! nm (reduce (fn [acc [x t]] (list 'forall [x t] acc)) ty (reverse PTYPES))
            (list 'fn PARAMS body)))

(defn motive! [P O nm]
  (over-params! nm '(=> Nat EvSrc RV Prop)
    (list 'fn '[n :- Nat, src :- EvSrc, v :- RV]
      (list (list 'EvSrc.rec$1 '(fn [_ :- EvSrc] (=> Nat RV Prop))
              (list 'fn '[rho :- (List RV), e :- Exp]
                (list 'fn '[n :- Nat, v :- RV] (mp-form P O '(EvSrc.tm rho e) 'n 'v)))
              (list 'fn '[f :- RV, x :- RV]
                (list 'fn '[n :- Nat, v :- RV] (mp-form P O '(EvSrc.ap f x) 'n 'v)))
              (list 'fn '[rho :- (List RV), st :- Exp, k :- Nat, acc :- RV]
                (list 'fn '[n :- Nat, v :- RV] (mp-form P O '(EvSrc.iter rho st k acc) 'n 'v)))
              (list 'fn '[rho :- (List RV), tl :- Exp, tn :- Exp, c :- Code]
                (list 'fn '[n :- Nat, v :- RV] (mp-form P O '(EvSrc.recs rho tl tn c) 'n 'v)))
              (list 'fn '[g :- RV, h :- RV, c :- Code]
                (list 'fn '[n :- Nat, v :- RV] (mp-form P O '(EvSrc.itr g h c) 'n 'v)))
              'src)
            'n 'v))))

(motive! 'Ev 'Ok 'MP_Ok)
(motive! 'EvE 'OkE 'MP_OkE)

;; --- the rule tables -----------------------------------------------------------------

(defn- table [resource pred] (map inv/parse-ctor (inv/ctor-forms resource pred)))
(def ^:private OK-TABLE {'Ok (table "lcert/formal/erase.clj" 'Ok) 'OkE (table "lcert/formal/erase.clj" 'OkE)})
(def ^:private EV-TABLE {'Ev (table "lcert/formal/eval.clj" 'Ev) 'EvE (table "lcert/formal/erase.clj" 'EvE)})
(def ^:private EVAL-OF {'Ok 'Ev 'OkE 'EvE})

(defn- find-rule [tbl nm] (first (filter #(= nm (:name %)) tbl)))
(defn- ok-premise? [O ty] (and (seq? ty) (= O (first ty))))
(defn- eq-premise? [ty] (and (seq? ty) (= 'Eq (first ty))))
(defn- premise-ty? [O ty] (or (ok-premise? O ty) (eq-premise? ty)))

(def ^:private DATA-CTORS
  {'RV.bool 'sm_bool_inj 'RV.nat 'sm_nat_inj 'RV.lbl 'sm_lbl_inj 'RV.code 'sm_code_inj 'RV.cert 'sm_cert_inj})
(def ^:private DATA-TYPES {'RV.bool 'Bool 'RV.nat 'Nat 'RV.lbl 'Nat 'RV.code 'Code 'RV.cert 'Code})

(defn- chain-get
  "The i-th conjunct of a conjunction chain of k conjuncts held in `h`, as a term."
  [h i k]
  (cond (= k 1) h
        (= i (dec k)) (reduce (fn [t _] (list 'And.right t)) h (range i))
        :else (list 'And.left (reduce (fn [t _] (list 'And.right t)) h (range i)))))

(defn- sub-expr [m x] (walk/postwalk-replace m x))

(defn case-lemma!
  "Prove the main-lemma case for the Ok rule `rule` of the safe-trace
  predicate O (Ok or OkE): lemma mc_<O>_<rule>.  `extra-params` are extra
  hypotheses (budget induction for reflect in OkE).  The rule's source must
  be a term (EvSrc.tm); its Ev counterpart has a single alternative."
  [O rule extra-params & [final-sim]]
  (let [P (EVAL-OF O)
        o (find-rule (OK-TABLE O) rule)
        pe-rule (find-rule (EV-TABLE P) rule)
        pe (inv/tm-rule P pe-rule)
        [wn src wv] (:where o)
        [_ wrho E] src
        pat-args (if (seq? E) (rest E) [])
        bare? (not (:value-eq? pe))
        exs (:exists pe)
        ex-names (map first exs)
        ex-ty (into {} exs)
        primed (fn [x] (symbol (str x "_p")))
        unified (atom #{})
        simv (atom {})                       ; RV variable -> proof of flV x = flV x_p
        sigma-map (fn [] (merge {wrho 'pR}
                                (when bare? {wv 'pV})
                                (into {} (for [x ex-names] [x (if (@unified x) x (primed x))]))))
        sigma (fn [x] (sub-expr (sigma-map) x))
        tacs (atom [])
        emit! (fn [& ts] (swap! tacs into ts))
        ;; sim proofs for values and environments
        sim-list-ref (atom nil)
        sim-list (fn [l] (@sim-list-ref l))
        sim-val (fn sim-val [x]
                  (cond (and (symbol? x) (@simv x)) (@simv x)
                        (and (seq? x) (= 'RV.pair (first x)))
                        (list 'sm_pair_cong (nth x 1) (sigma (nth x 1)) (nth x 2) (sigma (nth x 2))
                              (sim-val (nth x 1)) (sim-val (nth x 2)))
                        (and (seq? x) (= 'RV.bcons (first x)))
                        (list 'sm_bcons_cong (nth x 1) (sigma (nth x 1)) (nth x 2) (sigma (nth x 2))
                              (sim-val (nth x 1)) (sim-val (nth x 2)))
                        (and (seq? x) (= 'RV.clos (first x)))
                        (list 'sm_clos_cong (nth x 1) (sigma (nth x 1)) (nth x 2) (nth x 2)
                              (sim-list (nth x 1)) (list 'Eq.refl (nth x 2)))
                        :else (list 'Eq.refl (list 'flV x))))
        _ (reset! sim-list-ref (fn sim-list* [l]
                   (cond (= l wrho) 'pS
                         (and (seq? l) (= 'List.cons (first l)))
                         (list 'sm_cons_cong (nth l 2) (sigma (nth l 2)) (nth l 3) (sigma (nth l 3))
                               (sim-val (nth l 2)) (sim-list* (nth l 3)))
                         :else (list 'Eq.refl (list 'flL l)))))
        ;; learn from proof: flV VAL = flV (sigma VAL)
        learn! (fn learn! [val proof]
                 (cond
                   (symbol? val)
                   (when (or (some #{val} ex-names) (= val wv))
                     (let [nm (symbol (str "Zs_" val))]
                       (emit! (list 'have nm := proof))
                       (swap! simv assoc val nm)))
                   (and (seq? val) (DATA-CTORS (first val)))
                   (let [x (second val)]
                     (when (and (symbol? x) (some #{x} ex-names) (not (@unified x)))
                       (let [q (symbol (str "Zq_" x))]
                         (emit! (list 'have q (list 'Eq (DATA-TYPES (first val)) (primed x) x)
                                      (list 'Eq.symm (list (DATA-CTORS (first val)) x (primed x) proof)))
                                (list 'subst q))
                         (swap! unified conj x))))
                   (and (seq? val) (= 'RV.pair (first val)))
                   (let [a (nth val 1) b (nth val 2)
                         pr (symbol (str "Zpr_" (gensym)))]
                     (emit! (list 'have pr := (list 'sm_pair_inj a (sigma a) b (sigma b) proof)))
                     (learn! a (list 'And.left pr))
                     (learn! b (list 'And.right pr)))
                   :else nil))
        pnames (vec (:premises pe))
        k (count (:props pe))
        off (if (:value-eq? pe) 1 0)
        pidx (fn [nm] (+ off (.indexOf ^java.util.List (mapv first pnames) nm)))
        pprop (fn [nm] (symbol (str "Zp_" (pidx nm))))]
    ;; --- intro, invert, destructure ---------------------------------------------
    (emit! '(intro pR pV pEv pS)
           (list 'have 'Zinv := (list (if (= O 'Ok) 'ev_inv 'evE_inv) 'chkf 'dec 'encTy wn
                                  (list 'EvSrc.tm 'pR E) 'pV 'pEv)))
    (let [last-h (reduce (fn [prev x]
                           (let [h (symbol (str "Zh_" x))]
                             (emit! (list 'refine' (list 'exT (ex-ty x) '_ '_ prev '_))
                                    (list 'intro (primed x) h))
                             h))
                         'Zinv ex-names)]
      ;; split the conjunction into Zp_0 ...
      (doseq [i (range k)]
        (emit! (list 'have (symbol (str "Zp_" i)) := (chain-get last-h i k))))
      (when (:value-eq? pe) (emit! '(subst Zp_0))))
    ;; --- the premises ---------------------------------------------------------------
    (let [results (atom {})]
      (doseq [[f ty] (:fields o) :when (premise-ty? O ty)
              :let [nm f]]
        (when (ok-premise? O ty)
          (let [[_ _ _ _ _ psrc pval] ty
                ih (symbol (str "ih_" nm))
                call (case (str (first psrc))
                       "EvSrc.tm" (list ih (sigma (nth psrc 1)) (sigma pval) (pprop nm) (sim-list (nth psrc 1)))
                       "EvSrc.ap" (list ih (sigma (nth psrc 1)) (sigma (nth psrc 2)) (sigma pval) (pprop nm)
                                        (sim-val (nth psrc 1)) (sim-val (nth psrc 2)))
                       "EvSrc.iter" (list ih (sigma (nth psrc 1)) (sigma (nth psrc 4)) (sigma pval) (pprop nm)
                                          (sim-list (nth psrc 1)) (sim-val (nth psrc 4)))
                       "EvSrc.recs" (list ih (sigma (nth psrc 1)) (sigma pval) (pprop nm) (sim-list (nth psrc 1)))
                       "EvSrc.itr" (list ih (sigma (nth psrc 1)) (sigma (nth psrc 2)) (sigma pval) (pprop nm)
                                         (sim-val (nth psrc 1)) (sim-val (nth psrc 2))))
                r (symbol (str "Zr_" nm))]
            (emit! (list 'have r := call))
            (swap! results assoc nm r)
            (learn! pval (list 'And.right r)))))
      ;; --- conclusion ---------------------------------------------------------------
      (let [args (for [[f ty] (:fields o)]
                   (cond (= f wn) wn
                         (= f wrho) 'pR
                         (some #{f} pat-args) f
                         (and bare? (= f wv)) 'pV
                         (premise-ty? O ty)
                         (cond (ok-premise? O ty) (list 'And.left (@results f))
                               (some #{f} (map first pnames)) (pprop f)
                               :else f)         ; hH, kept from the node itself
                         :else (sigma f)))]
        (emit! '(constructor)
               (list 'exact (apply list (symbol (str O "." rule)) 'chkf 'dec 'encTy args))
               (list 'exact (or final-sim
                                (if (symbol? wv)
                                  (if bare? (@simv wv) (sim-val wv))
                                  (sim-val wv)))))))
    ;; --- the lemma ------------------------------------------------------------------
    (let [ih-binders (mapcat (fn [[f ty]]
                               (when (ok-premise? O ty)
                                 (let [[_ _ _ _ _ psrc pval] ty]
                                   [(symbol (str "ih_" f)) :- (mp-form P O psrc (nth ty 4) pval)])))
                             (:fields o))
          params (vec (concat PARAMS extra-params
                              (mapcat (fn [[f ty]] [f :- ty]) (:fields o))
                              ih-binders))]
      (a/prove-theorem (symbol (str "mc_" O "_" rule))
        (lv params)
        (lv (mp-form P O src wn wv))
        (lv @tacs)))))

(def generic-tm-rules
  '[eStar eTT eFF eZero eLbl eSucc eRecN eCaseL eBnil eBcons eSleaf eSnode eRecS
    eLeaf eNode eItR ePrn eLam eApp ePair eLet eChk])

(doseq [O '[Ok OkE], r generic-tm-rules] (case-lemma! O r []))
;; Variable lookup: the two environments agree up to tags (s52sim).
(doseq [O '[Ok OkE]]
  (case-lemma! O 'eVar [] '(sm_lookup_aux rho pR i v pV pS h Zp_0)))
