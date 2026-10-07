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
            [lcert.formal.s52erased]
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

;; --- helper lemmas ---------------------------------------------------------------------

;; Existential elimination at a Prop witness (exT is for Type witnesses): the erasure
;; derivation `her` of OkE's reflect rule is such a witness.
(thm exP [α :- Prop, P :- (=> α Prop), Q :- Prop, h :- (Exists P), f :- (forall [x α] (=> (P x) Q))] Q
  (exact (AT_Exists.rec α P (fn [_ :- (Exists P)] Q) f h)))

;; The decoded triple (m, t2, A) is a function of the certificate.
(thm dec_det [m :- Nat, m2 :- Nat, t :- Exp, t2 :- Exp, A :- Exp, A2 :- Exp,
              h :- (Eq (Option (Prod Nat (Prod Exp Exp)))
                     (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t A)))
                     (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m2 (Prod.mk Exp Exp t2 A2))))]
  (And (Eq Nat m m2) (And (Eq Exp t t2) (Eq Exp A A2)))
  (have e (Eq (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t A))
              (Prod.mk Nat (Prod Exp Exp) m2 (Prod.mk Exp Exp t2 A2))) (Option.some.inj h))
  (constructor)
  (exact (congrArg (fn [p :- (Prod Nat (Prod Exp Exp))] (Prod.fst p)) e))
  (constructor)
  (exact (congrArg (fn [p :- (Prod Nat (Prod Exp Exp))] (Prod.fst (Prod.snd p))) e))
  (exact (congrArg (fn [p :- (Prod Nat (Prod Exp Exp))] (Prod.snd (Prod.snd p))) e)))

(thm none_ne_some_dec [x :- (Prod Nat (Prod Exp Exp)),
                       h :- (Eq (Option (Prod Nat (Prod Exp Exp)))
                              (Option.none (Prod Nat (Prod Exp Exp)))
                              (Option.some (Prod Nat (Prod Exp Exp)) x))]
  False
  (cases h))

;; A successful reflect (guard true, decoded triple) runs at a budget strictly below
;; the current one, and at the reflected base type D itself (CheckSpec; as in
;; s52_refl_yes, s52reflect.clj).
(thm refl_data [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy),
                n :- Nat, c :- Code, D :- Exp, m :- Nat, t2 :- Exp, A :- Exp,
                hb :- (Eq Bool (Bool.and (Nat.ble (cnodes c) n) (chkf c (encTy D))) Bool.true),
                hd :- (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
                        (Option.some (Prod Nat (Prod Exp Exp))
                          (Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A)))),
                hB :- (Eq Bool (isBaseTy D) Bool.true)]
  (And (LT.lt m n) (Eq Exp A D))
  (have hchk (Eq Bool (chkf c (encTy D)) Bool.true)
    (andb_right (Nat.ble (cnodes c) n) (chkf c (encTy D)) hb))
  (refine' (exT Nat _ _ ((And.left hcs) c (encTy D) hchk) _)) (intro m1 hm)
  (refine' (exT Exp _ _ hm _)) (intro t1 ht1)
  (refine' (exT Exp _ _ ht1 _)) (intro A1 hA)
  (have hd1 (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c)
              (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk Nat (Prod Exp Exp) m1 (Prod.mk Exp Exp t1 A1))))
    (And.left hA))
  (have hclA (Eq Bool (closedTy A1) Bool.true) (And.left (And.right (And.right (And.right hA)))))
  (have henc (Eq Code (encTy A1) (encTy D)) (And.left (And.right (And.right (And.right (And.right hA))))))
  (have hlt0 (LT.lt m1 (cnodes c)) (And.right (And.right (And.right (And.right (And.right hA))))))
  (have hle (Nat.le (cnodes c) n)
    (Nat.le_of_ble_eq_true (andb_left (Nat.ble (cnodes c) n) (chkf c (encTy D)) hb)))
  (have hlt1 (LT.lt m1 n) (lt_le_omega m1 (cnodes c) n hlt0 hle))
  (have eAD (Eq Exp A1 D)
    ((And.right (And.right (And.right hcs))) A1 D hclA (closed_of_base D hB) henc))
  (have hdd := (dec_det m1 m t1 t2 A1 A (Eq.trans (Eq.symm hd1) hd)))
  (constructor)
  (exact (Eq.mp (congrArg (fn [z :- Nat] (LT.lt z n)) (And.left hdd)) hlt1))
  (exact (Eq.trans (Eq.symm (And.right (And.right hdd))) eAD)))

;; Inversion of flV at the base-value constructors (the helper is s52sim's).
(def ^:private rv-inversion! @#'lcert.formal.s52sim/rv-inversion!)
(rv-inversion! 'sm_star_inv [] :star '(flV RV.star) '(Eq RV v RV.star)
  '(fn [hh :- (Eq RV (flV RV.star) (flV RV.star))] (Eq.refl RV.star)))
(rv-inversion! 'sm_bool_inv '[bb :- Bool] :bool '(flV (RV.bool bb)) '(Eq RV v (RV.bool bb))
  '(fn [hh :- (Eq RV (flV (RV.bool bb)) (flV (RV.bool b)))]
     (congrArg RV.bool (Eq.symm (sm_bool_inj bb b hh)))))
(rv-inversion! 'sm_nat_inv '[kk :- Nat] :nat '(flV (RV.nat kk)) '(Eq RV v (RV.nat kk))
  '(fn [hh :- (Eq RV (flV (RV.nat kk)) (flV (RV.nat k)))]
     (congrArg RV.nat (Eq.symm (sm_nat_inj kk k hh)))))
(rv-inversion! 'sm_code_inv '[cc :- Code] :code '(flV (RV.code cc)) '(Eq RV v (RV.code cc))
  '(fn [hh :- (Eq RV (flV (RV.code cc)) (flV (RV.code c)))]
     (congrArg RV.code (Eq.symm (sm_code_inj cc c hh)))))
(rv-inversion! 'sm_cert_inv '[cc :- Code] :cert '(flV (RV.cert cc)) '(Eq RV v (RV.cert cc))
  '(fn [hh :- (Eq RV (flV (RV.cert cc)) (flV (RV.cert c)))]
     (congrArg RV.cert (Eq.symm (sm_cert_inj cc c hh)))))

;; At a base data type S fixes the value: two S-related values of one carrier
;; element are equal; and S is invariant under forgetting tags.
(def ^:private S-ARGS '[chkf dec encTy m erasing])
(def ^:private BASE-CTORS '#{tEmpty tUnit tBool tNat tLbl tSyn tR})
(defn- s52-at [v] (list 'S52 'chkf 'dec 'encTy 'm 'erasing 'A 'G 'en '(skel A) v 'a))

(let [fields @#'lcert.formal.syntactic/exp-fields
      eq1 (fn [c] (if (#{'tLbl 'tSyn 'tR} c) '(And.left h1) 'h1))
      eq2 (fn [c] (if (#{'tLbl 'tSyn 'tR} c) '(And.left h2) 'h2))]
  (a/prove-theorem 's52_base_unique
    (lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
          encTy :- (=> Exp Code), m :- Nat, erasing :- Bool, G :- (List Sk), en :- (HEnv G), A :- Exp])
    (lv (list '=> '(Eq Bool (isBaseTy A) Bool.true)
          (list 'forall '[v1 RV] (list 'forall '[v2 RV] (list 'forall '[a (Car (skel A))]
            (list '=> (s52-at 'v1) (s52-at 'v2) '(Eq RV v1 v2)))))))
    (lv (into ['(cases A)]
          (mapcat (fn [[c _]]
            (cond (= c 'tEmpty) '[(intro hb v1 v2 a h1 h2) (exact (False.elim h1))]
                  (BASE-CTORS c) ['(intro hb v1 v2 a h1 h2)
                                  (list 'exact (list 'Eq.trans (eq1 c) (list 'Eq.symm (eq2 c))))]
                  :else '[(intro hb) (exact (Bool.noConfusion hb))]))
            fields)))))

(let [fields @#'lcert.formal.syntactic/exp-fields
      inv-of {'tUnit 'sm_star_inv 'tBool 'sm_bool_inv 'tNat 'sm_nat_inv 'tLbl 'sm_lbl_inv
              'tSyn 'sm_code_inv 'tR 'sm_cert_inv}
      conj? #{'tLbl 'tSyn 'tR}]
  (a/prove-theorem 's52_base_sim
    (lv '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
          encTy :- (=> Exp Code), m :- Nat, erasing :- Bool, G :- (List Sk), en :- (HEnv G), A :- Exp])
    (lv (list '=> '(Eq Bool (isBaseTy A) Bool.true)
          (list 'forall '[v1 RV] (list 'forall '[v2 RV] (list 'forall '[a (Car (skel A))]
            (list '=> (s52-at 'v1) '(Eq RV (flV v1) (flV v2)) (s52-at 'v2)))))))
    (lv (into ['(cases A)]
          (mapcat (fn [[c _]]
            (cond
              (= c 'tEmpty) '[(intro hb v1 v2 a h1 hf) (exact (False.elim h1))]
              (= c 'tUnit)
              '[(intro hb v1 v2 a h1 hf)
                (have hf2 (Eq RV (flV RV.star) (flV v2))
                  (Eq.mp (congrArg (fn [z :- RV] (Eq RV (flV z) (flV v2))) h1) hf))
                (exact (sm_star_inv v2 hf2))]
              (BASE-CTORS c)
              (let [ctor ({'tBool 'RV.bool 'tNat 'RV.nat 'tLbl 'RV.lbl 'tSyn 'RV.code 'tR 'RV.cert} c)
                    e1 (if (conj? c) '(And.left h1) 'h1)]
                ['(intro hb v1 v2 a h1 hf)
                 (list 'have 'hf2 (list 'Eq 'RV (list 'flV (list ctor 'a)) '(flV v2))
                   (list 'Eq.mp (list 'congrArg (list 'fn '[z :- RV] (list 'Eq 'RV '(flV z) '(flV v2))) e1) 'hf))
                 (if (conj? c)
                   (list 'exact (list 'And.intro (list (inv-of c) 'a 'v2 'hf2) '(And.right h1)))
                   (list 'exact (list (inv-of c) 'a 'v2 'hf2)))])
              :else '[(intro hb) (exact (Bool.noConfusion hb))]))
            fields)))))

(defn- tm-alt-tactics
  "The tactics that prove the node of the Ok rule `o` (safe-trace predicate O)
  from one alternative `pe` (a tm-rule analysis of an Ev rule of the same
  term constructor) of the inversion of the other run.

  mode :mine   - pe is the Ev counterpart of o: rebuild the Ok node.
  mode :wrong  - pe is another alternative: premises before `upto` are
                 processed as in :mine; then `contra` (tactics) refutes it.
  `start` names the hypothesis holding the alternative's clause.  `hooks`
  maps a premise name to a function (state -> tactics) run just after that
  premise is processed (used for the decoded triple of reflect)."
  [O P o pe {:keys [start mode upto contra hooks sim overrides] :or {hooks {} overrides {}}}]
  (let [final-sim sim
        mkcall (fn [ih & args] (if (= O 'OkE) (apply list ih 'Zbw args) (apply list ih args)))
        [wn src wv] (:where o)
        [_ wrho E] src
        pat-args (if (seq? E) (rest E) [])
        bare? (not (:value-eq? pe))
        exs (:exists pe)
        ex-names (map first exs)
        ex-ty (into {} exs)
        primed (fn [x] (symbol (str x "_p")))
        unified (atom #{})
        simv (atom {})
        sigma-map (fn [] (merge {wrho 'pR}
                                (when bare? {wv 'pV})
                                (into {} (for [x ex-names] [x (if (@unified x) x (primed x))]))))
        sigma (fn [x] (sub-expr (sigma-map) x))
        tacs (atom [])
        emit! (fn [& ts] (swap! tacs into ts))
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
        _ (reset! sim-list-ref
            (fn sim-list* [l]
              (cond (= l wrho) 'pS
                    (and (seq? l) (= 'List.cons (first l)))
                    (list 'sm_cons_cong (nth l 2) (sigma (nth l 2)) (nth l 3) (sigma (nth l 3))
                          (sim-val (nth l 2)) (sim-list* (nth l 3)))
                    :else (list 'Eq.refl (list 'flL l)))))
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
        pnames (mapv first (:premises pe))
        k (count (:props pe))
        off (if (:value-eq? pe) 1 0)
        pprop (fn [nm] (symbol (str "Zp_" (+ off (.indexOf ^java.util.List pnames nm)))))
        ;; the alternative's own premise type, in this node's variables
        qmap (zipmap (map #(symbol (str "q_" %)) (range)) pat-args)
        inst (fn [x] (sigma (sub-expr (merge qmap {'ZN wn 'ZRHO wrho 'ZV (if bare? 'pV wv)}) x)))
        alt-prop (fn [nm] (nth (:props pe) (+ off (.indexOf ^java.util.List pnames nm))))
        results (atom {})
        done? (atom false)]
    (emit! (list 'have 'Zalt := start))
    (let [last-h (reduce (fn [prev x]
                           (let [h (symbol (str "Zh_" x))
                                 earlier (take-while #(not= % x) ex-names)
                                 ty (sub-expr (into {} (map (fn [y] [y (primed y)]) earlier)) (ex-ty x))]
                             (emit! (list 'refine' (list (if (and (seq? ty) (= 'Er (first ty))) 'exP 'exT) ty '_ '_ prev '_))
                                    (list 'intro (primed x) h))
                             h))
                         'Zalt ex-names)]
      (doseq [i (range k)]
        (emit! (list 'have (symbol (str "Zp_" i)) := (chain-get last-h i k))))
      (when (:value-eq? pe) (emit! '(subst Zp_0))))
    (doseq [[f ty] (:fields o)
            :when (and (not @done?) (premise-ty? O ty))]
      (let [here? (some #{f} pnames)]
        (when (and (= mode :wrong) (= f upto))
          (reset! done? true)
          (when (ok-premise? O ty)
            (let [[_ _ _ _ _ psrc pval] ty
                  ih (symbol (str "ih_" f))
                  ap (alt-prop f)                    ; the alternative's (P ... src v')
                  pv' (nth ap 6)
                  call (case (str (first psrc))
                         "EvSrc.tm" (mkcall ih (sigma (nth psrc 1)) (inst pv') (pprop f) (sim-list (nth psrc 1)))
                         "EvSrc.ap" (mkcall ih (sigma (nth psrc 1)) (sigma (nth psrc 2)) (inst pv') (pprop f)
                                          (sim-val (nth psrc 1)) (sim-val (nth psrc 2))))]
              (emit! (list 'have (symbol (str "Zr_" f)) := call))))
          (apply emit! (if (fn? contra) (contra {:pprop pprop}) contra)))
        (when (and (not @done?) here?)
          (when (ok-premise? O ty)
            (let [[_ _ _ _ _ psrc pval] ty
                  override (overrides f)
                  ih (symbol (str "ih_" f))
                  call (case (str (first psrc))
                         "EvSrc.tm" (mkcall ih (sigma (nth psrc 1)) (sigma pval) (pprop f) (sim-list (nth psrc 1)))
                         "EvSrc.ap" (mkcall ih (sigma (nth psrc 1)) (sigma (nth psrc 2)) (sigma pval) (pprop f)
                                          (sim-val (nth psrc 1)) (sim-val (nth psrc 2)))
                         "EvSrc.iter" (mkcall ih (sigma (nth psrc 1)) (sigma (nth psrc 4)) (sigma pval) (pprop f)
                                            (sim-list (nth psrc 1)) (sim-val (nth psrc 4)))
                         "EvSrc.recs" (mkcall ih (sigma (nth psrc 1)) (sigma pval) (pprop f) (sim-list (nth psrc 1)))
                         "EvSrc.itr" (mkcall ih (sigma (nth psrc 1)) (sigma (nth psrc 2)) (sigma pval) (pprop f)
                                           (sim-val (nth psrc 1)) (sim-val (nth psrc 2))))
                  r (symbol (str "Zr_" f))]
              (if override
                (apply emit! (override {:pprop pprop :unified unified :primed primed :sigma sigma}))
                (emit! (list 'have r := call)))
              (swap! results assoc f r)
              (learn! pval (list 'And.right r))))
          (when-let [hk (hooks f)]
            (apply emit! (hk {:unified unified :primed primed :emit emit! :sigma sigma :pprop pprop}))))))
    (when (= mode :mine)
      (let [args (for [[f ty] (:fields o)]
                   (cond (= f wn) wn
                         (= f wrho) 'pR
                         (some #{f} pat-args) f
                         (and bare? (= f wv)) 'pV
                         (premise-ty? O ty)
                         (cond (ok-premise? O ty) (list 'And.left (@results f))
                               (some #{f} pnames) (pprop f)
                               :else f)
                         :else (sigma f)))]
        (emit! '(constructor)
               (list 'exact (apply list (symbol (str O "." (:name o))) 'chkf 'dec 'encTy args))
               (list 'exact (or final-sim
                                (if (symbol? wv)
                                  (if bare? (@simv wv) (sim-val wv))
                                  (sim-val wv)))))))
    @tacs))

(defn- or-split
  "Tactics that destructure the n-way inversion `Zinv` (an Or chain) and run the
  alternatives' bodies in order; alts is a seq of tactic vectors, each starting
  from the hypothesis named in its :start (Zinv itself for one alternative,
  else the h that `cases` introduces)."
  [alts]
  (let [n (count alts)]
    (cond (= n 1) (first alts)
          :else (into ['(cases Zinv)]
                  (concat (first alts)
                          (if (= n 2)
                            (second alts)
                            (into ['(cases h)] (mapcat identity (rest alts)))))))))

;; --- the budget hypothesis for evalE ----------------------------------------------------
;; UE m: the universal statement at budget m (the induction hypothesis of the
;; budget induction): every run of every erasure of a typed program in the
;; token context is a safe trace and, at a base type, S-related to the denotation.
(over-params! 'UE_E '(=> Nat Prop)
  '(fn [m :- Nat]
     (forall [t Exp] (forall [A Exp]
       (=> (Rt chkf (thetaD m) (thetaU m) t A)
         (forall [e Exp] (=> (Er chkf (thetaD m) (thetaU m) t A e)
           (forall [v RV] (=> (EvE chkf dec encTy m (EvSrc.tm (rtokens m) e) v)
             (And (OkE chkf dec encTy m (EvSrc.tm (rtokens m) e) v)
               (=> (Eq Bool (isBaseTy A) Bool.true)
                 (S52 chkf dec encTy m Bool.true A (skels (thetaD m)) (tokEnvD m) (skel A) v
                   (den chkf dec encTy m t (skels (thetaD m)) (skel A) (tokEnvD m))))))))))))))
;; BELOW n: UE at every smaller budget (the budget a reflected program runs at).
(over-params! 'BELOW_E '(=> Nat Prop)
  '(fn [n :- Nat] (forall [m Nat] (=> (LT.lt m n) (UE_E chkf dec encTy m)))))
;; The evalE motive: under the budget hypothesis for its own budget.
(over-params! 'MPB_OkE '(=> Nat EvSrc RV Prop)
  '(fn [n :- Nat, src :- EvSrc, v :- RV]
     (=> (BELOW_E chkf dec encTy n) (MP_OkE chkf dec encTy n src v))))

(defn- bw [O n mp] (if (= O 'OkE) (list '=> (list 'BELOW_E 'chkf 'dec 'encTy n) mp) mp))

(defn- lemma-parts
  "[params goal] of the case lemma for Ok rule o: the node's fields, then one
  induction hypothesis per safe-trace premise."
  [O P o extra]
  (let [[wn src wv] (:where o)
        ih-binders (mapcat (fn [[f ty]]
                             (when (ok-premise? O ty)
                               (let [[_ _ _ _ _ psrc pval] ty]
                                 [(symbol (str "ih_" f)) :- (bw O (nth ty 4) (mp-form P O psrc (nth ty 4) pval))])))
                           (:fields o))]
    [(vec (concat PARAMS extra
                  (mapcat (fn [[f ty]] [f :- ty]) (:fields o))
                  ih-binders))
     (bw O wn (mp-form P O src wn wv))]))

(defn case-lemma!
  "Prove the main-lemma case for the term rule `rule` of O (Ok or OkE): lemma
  mc_<O>_<rule>.  opts: :extra (extra lemma parameters), :sim (final similarity
  proof), :wrong ({alternative-rule {:upto premise :contra tactics-or-fn}}),
  :hooks and :overrides (premise -> fn of the state -> tactics)."
  [O rule & [{:keys [extra sim wrong hooks overrides] :or {extra [] wrong {} hooks {} overrides {}}}]]
  (let [P (EVAL-OF O)
        o (find-rule (OK-TABLE O) rule)
        ev-rules (EV-TABLE P)
        [wn src wv] (:where o)
        [_ wrho E] src
        ctor-sym (if (seq? E) (first E) E)
        alts (filter (fn [r] (let [[_ s _] (:where r)]
                               (and (= 'EvSrc.tm (first s))
                                    (= ctor-sym (let [e (nth s 2)] (if (seq? e) (first e) e))))))
                     ev-rules)
        alt-tactics (for [r alts]
                      (let [pe (inv/tm-rule P r)]
                        (if (= (:name r) rule)
                          (tm-alt-tactics O P o pe {:start (if (= 1 (count alts)) 'Zinv 'h) :mode :mine
                                                    :hooks hooks :sim sim :overrides overrides})
                          (let [w (wrong (:name r))]
                            (tm-alt-tactics O P o pe {:start 'h :mode :wrong :upto (:upto w)
                                                      :contra (:contra w) :hooks hooks})))))
        tacs (into [(if (= O 'OkE) '(intro Zbw pR pV pEv pS) '(intro pR pV pEv pS))
                    (list 'have 'Zinv := (list (if (= O 'Ok) 'ev_inv 'evE_inv) 'chkf 'dec 'encTy wn
                                               (list 'EvSrc.tm 'pR E) 'pV 'pEv))]
                   (or-split (vec alt-tactics)))
        [params goal] (lemma-parts O P o extra)]
    (a/prove-theorem (symbol (str "mc_" O "_" rule)) (lv params) (lv goal) (lv tacs))))

;; --- rules whose source is not a term ----------------------------------------------------

(defn- nontm-lemma!
  "Prove the case for one of the rules whose source is an application, an
  iteration, a recSyn or an itR, from the tactics built by tacs-fn.  tacs-fn
  receives ih*, which applies an induction hypothesis (adding the budget
  hypothesis for OkE), and the evaluator-specific names."
  [O rule tacs-fn]
  (let [P (EVAL-OF O)
        o (find-rule (OK-TABLE O) rule)
        inv-name (if (= O 'Ok) 'ev_inv 'evE_inv)
        ih* (fn [ih & args] (if (= O 'OkE) (apply list ih 'Zbw args) (apply list ih args)))
        okc (fn [& args] (apply list (symbol (str O "." rule)) 'chkf 'dec 'encTy args))
        evt (fn [n src v] (list P 'chkf 'dec 'encTy n src v))
        invc (fn [n src v h] (list inv-name 'chkf 'dec 'encTy n src v h))
        intro (fn [& names] (cons 'intro (concat (when (= O 'OkE) ['Zbw]) names)))
        [params goal] (lemma-parts O P o [])]
    (a/prove-theorem (symbol (str "mc_" O "_" rule)) (lv params) (lv goal)
      (lv (tacs-fn {:ih ih* :ok okc :ev evt :inv invc :intro intro})))))

(defn- apply-nontm! [O]
  (nontm-lemma! O 'apClos
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pF 'pA 'pV 'pEv 'pSF 'pSA)
       '(refine' (exT (List RV) _ _ (sm_clos_inv rho body pF pSF) _)) '(intro rho_p Zc)
       '(have Zeq (Eq RV pF (RV.clos rho_p body)) (And.left Zc))
       '(subst Zeq)
       (list 'have 'Zev (ev 'n '(EvSrc.tm (List.cons RV pA rho_p) body) 'pV)
             (inv 'n '(EvSrc.ap (RV.clos rho_p body) pA) 'pV 'pEv))
       (list 'have 'Zr := (ih 'ih_h '(List.cons RV pA rho_p) 'pV 'Zev
                              '(sm_cons_cong arg pA rho rho_p pSA (And.right Zc))))
       '(constructor)
       (list 'exact (ok 'n 'rho_p 'body 'pA 'pV '(And.left Zr)))
       '(exact (And.right Zr))]))
  (nontm-lemma! O 'apBconsZ
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pF 'pA 'pV 'pEv 'pSF 'pSA)
       '(refine' (exT RV _ _ (sm_bcons_inv h t pF pSF) _)) '(intro h_p Zb)
       '(refine' (exT RV _ _ Zb _)) '(intro t_p Zb2)
       '(have Zeq (Eq RV pF (RV.bcons h_p t_p)) (And.left Zb2))
       '(subst Zeq)
       '(have Zl (Eq RV pA (RV.lbl 0)) (sm_lbl_inv 0 pA pSA))
       '(subst Zl)
       (list 'have 'Zv '(Eq RV pV h_p) (inv 'n '(EvSrc.ap (RV.bcons h_p t_p) (RV.lbl 0)) 'pV 'pEv))
       '(subst Zv)
       '(constructor)
       (list 'exact (ok 'n 'h_p 't_p))
       '(exact (And.left (And.right Zb2)))]))
  (nontm-lemma! O 'apBconsS
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pF 'pA 'pV 'pEv 'pSF 'pSA)
       '(refine' (exT RV _ _ (sm_bcons_inv h t pF pSF) _)) '(intro h_p Zb)
       '(refine' (exT RV _ _ Zb _)) '(intro t_p Zb2)
       '(have Zeq (Eq RV pF (RV.bcons h_p t_p)) (And.left Zb2))
       '(subst Zeq)
       '(have Zl (Eq RV pA (RV.lbl (Nat.succ k))) (sm_lbl_inv (Nat.succ k) pA pSA))
       '(subst Zl)
       (list 'have 'Zev (ev 'n '(EvSrc.ap t_p (RV.lbl k)) 'pV)
             (inv 'n '(EvSrc.ap (RV.bcons h_p t_p) (RV.lbl (Nat.succ k))) 'pV 'pEv))
       (list 'have 'Zr := (ih 'ih_hp 't_p '(RV.lbl k) 'pV 'Zev
                              '(And.right (And.right Zb2)) '(Eq.refl (flV (RV.lbl k)))))
       '(constructor)
       (list 'exact (ok 'n 'h_p 't_p 'k 'pV '(And.left Zr)))
       '(exact (And.right Zr))]))
  (nontm-lemma! O 'eIterZ
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pR 'pA 'pV 'pEv 'pSR 'pSA)
       (list 'have 'Zv '(Eq RV pV pA) (inv 'n '(EvSrc.iter pR step 0 pA) 'pV 'pEv))
       '(subst Zv)
       '(constructor)
       (list 'exact (ok 'n 'pR 'step 'pA))
       '(exact pSA)]))
  (nontm-lemma! O 'eIterS
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pR 'pA 'pV 'pEv 'pSR 'pSA)
       (list 'have 'Zinv := (inv 'n '(EvSrc.iter pR step (Nat.succ k) pA) 'pV 'pEv))
       '(refine' (exT RV _ _ Zinv _)) '(intro mid_p Zh)
       (list 'have 'Zr1 := (ih 'ih_hp 'pR 'pA 'mid_p '(And.left Zh) 'pSR 'pSA))
       (list 'have 'Zr2 := (ih 'ih_hs '(List.cons RV mid_p (List.cons RV (RV.nat k) pR)) 'pV '(And.right Zh)
                              '(sm_cons_cong mid mid_p (List.cons RV (RV.nat k) rho)
                                 (List.cons RV (RV.nat k) pR) (And.right Zr1)
                                 (sm_cons_cong (RV.nat k) (RV.nat k) rho pR (Eq.refl (flV (RV.nat k))) pSR))))
       '(constructor)
       (list 'exact (ok 'n 'pR 'step 'k 'pA 'mid_p 'pV '(And.left Zr1) '(And.left Zr2)))
       '(exact (And.right Zr2))]))
  (nontm-lemma! O 'eRecSL
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pR 'pV 'pEv 'pSR)
       (list 'have 'Zev (ev 'n '(EvSrc.tm (List.cons RV (RV.lbl l) pR) tl) 'pV)
             (inv 'n '(EvSrc.recs pR tl tn (Code.sl l)) 'pV 'pEv))
       (list 'have 'Zr := (ih 'ih_h '(List.cons RV (RV.lbl l) pR) 'pV 'Zev
                              '(sm_cons_cong (RV.lbl l) (RV.lbl l) rho pR (Eq.refl (flV (RV.lbl l))) pSR)))
       '(constructor)
       (list 'exact (ok 'n 'pR 'tl 'tn 'l 'pV '(And.left Zr)))
       '(exact (And.right Zr))]))
  (nontm-lemma! O 'eRecSN
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pR 'pV 'pEv 'pSR)
       (list 'have 'Zinv := (inv 'n '(EvSrc.recs pR tl tn (Code.sn l a b)) 'pV 'pEv))
       '(refine' (exT RV _ _ Zinv _)) '(intro ya_p Zh1)
       '(refine' (exT RV _ _ Zh1 _)) '(intro yb_p Zh2)
       (list 'have 'Zr1 := (ih 'ih_ha 'pR 'ya_p '(And.left Zh2) 'pSR))
       (list 'have 'Zr2 := (ih 'ih_hb 'pR 'yb_p '(And.left (And.right Zh2)) 'pSR))
       (list 'have 'Zr3 := (ih 'ih_hs
         '(List.cons RV yb_p (List.cons RV ya_p (List.cons RV (RV.code b) (List.cons RV (RV.code a)
            (List.cons RV (RV.lbl l) pR)))))
         'pV '(And.right (And.right Zh2))
         '(sm_cons_cong yb yb_p
            (List.cons RV ya (List.cons RV (RV.code b) (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))))
            (List.cons RV ya_p (List.cons RV (RV.code b) (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) pR))))
            (And.right Zr2)
            (sm_cons_cong ya ya_p
               (List.cons RV (RV.code b) (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho)))
               (List.cons RV (RV.code b) (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) pR)))
               (And.right Zr1)
               (sm_cons_cong (RV.code b) (RV.code b)
                  (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))
                  (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) pR))
                  (Eq.refl (flV (RV.code b)))
                  (sm_cons_cong (RV.code a) (RV.code a)
                     (List.cons RV (RV.lbl l) rho) (List.cons RV (RV.lbl l) pR)
                     (Eq.refl (flV (RV.code a)))
                     (sm_cons_cong (RV.lbl l) (RV.lbl l) rho pR (Eq.refl (flV (RV.lbl l))) pSR)))))))
       '(constructor)
       (list 'exact (ok 'n 'pR 'tl 'tn 'l 'a 'b 'ya_p 'yb_p 'pV '(And.left Zr1) '(And.left Zr2) '(And.left Zr3)))
       '(exact (And.right Zr3))]))
  (nontm-lemma! O 'eItL
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pF 'pA 'pV 'pEv 'pSF 'pSA)
       (list 'have 'Zev (ev 'n '(EvSrc.ap pF (RV.lbl l)) 'pV)
             (inv 'n '(EvSrc.itr pF pA (Code.sl l)) 'pV 'pEv))
       (list 'have 'Zr := (ih 'ih_ha 'pF '(RV.lbl l) 'pV 'Zev 'pSF '(Eq.refl (flV (RV.lbl l)))))
       '(constructor)
       (list 'exact (ok 'n 'pF 'pA 'l 'pV '(And.left Zr)))
       '(exact (And.right Zr))]))
  (nontm-lemma! O 'eItN
    (fn [{:keys [ih ok ev inv intro]}]
      [(intro 'pF 'pA 'pV 'pEv 'pSF 'pSA)
       (list 'have 'Zinv := (inv 'n '(EvSrc.itr pF pA (Code.sn l a b)) 'pV 'pEv))
       '(refine' (exT RV _ _ Zinv _)) '(intro ya_p Zh1)
       '(refine' (exT RV _ _ Zh1 _)) '(intro yb_p Zh2)
       '(refine' (exT RV _ _ Zh2 _)) '(intro v1_p Zh3)
       '(refine' (exT RV _ _ Zh3 _)) '(intro v2_p Zh4)
       '(refine' (exT RV _ _ Zh4 _)) '(intro v3_p Zh5)
       (list 'have 'Zr1 := (ih 'ih_ha 'pF 'pA 'ya_p '(And.left Zh5) 'pSF 'pSA))
       (list 'have 'Zr2 := (ih 'ih_hb 'pF 'pA 'yb_p '(And.left (And.right Zh5)) 'pSF 'pSA))
       (list 'have 'Zr3 := (ih 'ih_h1 'pA 'RV.token 'v1_p '(And.left (And.right (And.right Zh5)))
                               'pSA '(Eq.refl (flV RV.token))))
       (list 'have 'Zr4 := (ih 'ih_h2 'v1_p '(RV.lbl l) 'v2_p
                               '(And.left (And.right (And.right (And.right Zh5))))
                               '(And.right Zr3) '(Eq.refl (flV (RV.lbl l)))))
       (list 'have 'Zr5 := (ih 'ih_h3 'v2_p 'ya_p 'v3_p
                               '(And.left (And.right (And.right (And.right (And.right Zh5)))))
                               '(And.right Zr4) '(And.right Zr1)))
       (list 'have 'Zr6 := (ih 'ih_h4 'v3_p 'yb_p 'pV
                               '(And.right (And.right (And.right (And.right (And.right Zh5)))))
                               '(And.right Zr5) '(And.right Zr2)))
       '(constructor)
       (list 'exact (ok 'n 'pF 'pA 'l 'a 'b 'ya_p 'yb_p 'v1_p 'v2_p 'v3_p 'pV
                        '(And.left Zr1) '(And.left Zr2) '(And.left Zr3) '(And.left Zr4) '(And.left Zr5) '(And.left Zr6)))
       '(exact (And.right Zr6))])))


;; --- the instantiations ---------------------------------------------------------------------

(def generic-tm-rules
  '[eStar eTT eFF eZero eLbl eSucc eRecN eCaseL eBnil eBcons eSleaf eSnode eRecS
    eLeaf eNode eItR ePrn eLam eApp ePair eLet eChk])

(defn- try-case!
  "A failing case lemma aborts the load (kept as a named wrapper for readability)."
  [f & args] (apply f args))

(doseq [O '[Ok OkE], r generic-tm-rules] (try-case! case-lemma! O r))
;; Variable lookup: the two environments agree up to tags (s52sim).
(doseq [O '[Ok OkE]]
  (try-case! case-lemma! O 'eVar {:sim '(sm_lookup_aux rho pR i v pV pS h Zp_0)}))

;; The branching eliminators: the other run's other branch is refuted by the scrutinee.
(def ^:private bool-clash
  (fn [x y] [(list 'exact (list 'Bool.noConfusion (list 'sm_bool_inj x y '(And.right Zr_hb))))]))
(doseq [O '[Ok OkE]]
  (doseq [[rule other x y] '[[eIteT eIteF Bool.true Bool.false] [eIteF eIteT Bool.false Bool.true]
                             [eElimT eElimF Bool.true Bool.false] [eElimF eElimT Bool.false Bool.true]]]
    (try-case! case-lemma! O rule {:wrong {other {:upto 'hb :contra (bool-clash x y)}}})))

;; inspect: the other run's other branch is refuted by the checker's answer.
(def ^:private chk-clash
  (fn [st] [(list 'exact (list 'Bool.noConfusion
                          (list 'Eq.trans '(Eq.symm hb) ((:pprop st) 'hb))))]))
(doseq [O '[Ok OkE]]
  (try-case! case-lemma! O 'eInspT {:wrong {'eInspF {:upto 'hb :contra chk-clash}}})
  (try-case! case-lemma! O 'eInspF {:wrong {'eInspT {:upto 'hb :contra chk-clash}}}))

;; reflect.
(def ^:private guard-clash          ; the other run's guard disagrees: hb' vs hb
  (fn [st] [(list 'exact (list 'Bool.noConfusion
                          (list 'Eq.trans '(Eq.symm hb) ((:pprop st) 'hb))))]))
(def ^:private guard-clash-rev
  (fn [st] [(list 'exact (list 'Bool.noConfusion
                          (list 'Eq.trans (list 'Eq.symm ((:pprop st) 'hb)) 'hb)))]))
(def ^:private dec-clash-some       ; mine: dec c = some, other: dec c = none
  (fn [st] [(list 'exact (list 'False.elim (list 'none_ne_some_dec
                          '(Prod.mk Nat (Prod Exp Exp) m (Prod.mk Exp Exp t2 A))
                          (list 'Eq.trans (list 'Eq.symm ((:pprop st) 'hd)) 'hd))))]))
(def ^:private dec-clash-none       ; mine: dec c = none, other: dec c = some
  (fn [st] [(list 'exact (list 'False.elim (list 'none_ne_some_dec
                          '(Prod.mk Nat (Prod Exp Exp) m_p (Prod.mk Exp Exp t2_p A_p))
                          (list 'Eq.trans (list 'Eq.symm 'hd) ((:pprop st) 'hd)))))]))
;; ReflOk: unify the decoded triple of the two runs
(def ^:private dec-hook
  (fn [st]
    (swap! (:unified st) into '#{m t2 A})
    [(list 'have 'Zdd := (list 'dec_det 'm 'm_p 't2 't2_p 'A 'A_p
                          (list 'Eq.trans '(Eq.symm hd) ((:pprop st) 'hd))))
     '(have Zm (Eq Nat m_p m) (Eq.symm (And.left Zdd))) '(subst Zm)
     '(have Zt (Eq Exp t2_p t2) (Eq.symm (And.left (And.right Zdd)))) '(subst Zt)
     '(have ZA (Eq Exp A_p A) (Eq.symm (And.right (And.right Zdd)))) '(subst ZA)]))
;; OkE: the reflected program runs under ANY erasure; the budget induction compares the runs.
(def ^:private ht-override
  (fn [st]
    [(list 'have 'Zdat := '(refl_data chkf dec encTy hcs n c D m t2 A hb hd hB))
     '(have Zlt (LT.lt m n) (And.left Zdat))
     '(have Zrt (Rt chkf (thetaD m) (thetaU m) t2 A) (er_rt chkf (thetaD m) (thetaU m) t2 A e2 her))
     '(have ZbaseA (Eq Bool (isBaseTy A) Bool.true)
        (Eq.mpr (congrArg (fn [z :- Exp] (Eq Bool (isBaseTy z) Bool.true)) (And.right Zdat)) hB))
     (list 'have 'Zu1 := (list 'Zbw 'm 'Zlt 't2 'A 'Zrt 'e2_p 'her_p 'pV ((:pprop st) 'ht)))
     '(have Zu0 := (Zbw m Zlt t2 A Zrt e2 her w (ok52_evalE chkf dec encTy m (EvSrc.tm (rtokens m) e2) w ht)))
     '(have Zeqw (Eq RV w pV)
        (s52_base_unique chkf dec encTy m Bool.true (skels (thetaD m)) (tokEnvD m) A ZbaseA w pV
          (den chkf dec encTy m t2 (skels (thetaD m)) (skel A) (tokEnvD m))
          ((And.right Zu0) ZbaseA) ((And.right Zu1) ZbaseA)))
     '(have Zr_ht := (And.intro (And.left Zu1) (congrArg flV Zeqw)))]))

(doseq [O '[Ok OkE]]
  (try-case! case-lemma! O 'eReflOk
    (merge {:hooks {'hd dec-hook}
            :wrong {'eReflNo {:upto 'hb :contra guard-clash}
                    'eReflNone {:upto 'hd :contra dec-clash-some}}}
           (when (= O 'OkE)
             {:extra '[hcs :- (CheckSpec chkf dec encTy)] :overrides {'ht ht-override}})))
  (try-case! case-lemma! O 'eReflNo
    {:wrong {'eReflOk {:upto 'hb :contra guard-clash-rev}
             'eReflNone {:upto 'hb :contra guard-clash-rev}}})
  (try-case! case-lemma! O 'eReflNone
    {:wrong {'eReflOk {:upto 'hd :contra dec-clash-none}
             'eReflNo {:upto 'hb :contra guard-clash}}}))

;; the rules whose source is not a term
(doseq [O '[Ok OkE]] (try-case! apply-nontm! O))

;; --- assembly: the main lemma for every safe trace ------------------------------------------

(defn- assemble!
  "detOk / detOkE: induction on the safe trace with one case lemma per rule."
  [O thm-name motive extra minor-extra]
  (a/prove-theorem thm-name
    (lv (vec (concat PARAMS extra ['n0 :- 'Nat 'src0 :- 'EvSrc 'v0 :- 'RV
                                   'der :- (list O 'chkf 'dec 'encTy 'n0 'src0 'v0)])))
    (lv (list motive 'chkf 'dec 'encTy 'n0 'src0 'v0))
    (lv [(list 'exact
           (apply list (symbol (str O ".rec")) 'chkf 'dec 'encTy
             (list 'fn ['n :- 'Nat, 'src :- 'EvSrc, 'v :- 'RV, 'h :- (list O 'chkf 'dec 'encTy 'n 'src 'v)]
               (list motive 'chkf 'dec 'encTy 'n 'src 'v))
             (concat (for [r (OK-TABLE O)]
                       (apply list (symbol (str "mc_" O "_" (:name r))) 'chkf 'dec 'encTy (minor-extra (:name r))))
                     ['n0 'src0 'v0 'der])))])))

(assemble! 'Ok 'detOk 'MP_Ok [] (constantly []))
(assemble! 'OkE 'detOkE 'MPB_OkE '[hcs :- (CheckSpec chkf dec encTy)]
           (fn [c] (if (= c 'eReflOk) '[hcs] [])))

;; The main lemma at a term source, in the form the theorems use.
(thm detOk_tm
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, rho :- (List RV), e :- Exp, v :- RV,
   der :- (Ok chkf dec encTy n (EvSrc.tm rho e) v)]
  (forall [pR (List RV)] (forall [pV RV]
    (=> (Ev chkf dec encTy n (EvSrc.tm pR e) pV)
        (Eq (List RV) (flL rho) (flL pR))
        (And (Ok chkf dec encTy n (EvSrc.tm pR e) pV) (Eq RV (flV v) (flV pV))))))
  (exact (detOk chkf dec encTy n (EvSrc.tm rho e) v der)))

(thm detOkE_tm
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), n :- Nat, rho :- (List RV), e :- Exp, v :- RV,
   der :- (OkE chkf dec encTy n (EvSrc.tm rho e) v), below :- (BELOW_E chkf dec encTy n)]
  (forall [pR (List RV)] (forall [pV RV]
    (=> (EvE chkf dec encTy n (EvSrc.tm pR e) pV)
        (Eq (List RV) (flL rho) (flL pR))
        (And (OkE chkf dec encTy n (EvSrc.tm pR e) pV) (Eq RV (flV v) (flV pV))))))
  (exact (detOkE chkf dec encTy hcs n (EvSrc.tm rho e) v der below)))
