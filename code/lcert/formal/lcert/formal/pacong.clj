(ns lcert.formal.pacong
  "F6.3 — schematic term congruence and equality substitution (R4 §6.2, E2)."
  (:require [ansatz.core :as a]
            [clojure.walk :as w]
            [lcert.formal.base :as b :refer [thm kdef]]
            [lcert.formal.paequality :as eq]
            [lcert.formal.paclosed :as pc]
            [lcert.formal.paprop :as pp]))
;; Symbolic PA substitution proof emitter. Every equation is checked by
;; the kernel; opaque PT parameters are never evaluated by the host.
(defn eq-chain [p q] (list 'Eq.trans p q))
(defn cg [f p] (list 'congrArg f p))
(defn sh [t] (list 'paSbT 'paShT t))
(defn ctor-eq [ctor types old new proofs]
  (reduce eq-chain 'rfl
    (map-indexed
      (fn [i pf]
        (cg (list 'fn ['v ':- (nth types i)]
              (apply list ctor (concat (take i new) ['v] (drop (inc i) old)))) pf)) proofs)))
(declare sb-term)
(defn sb-var [sigma i]
  (cond
    (= sigma 'paShT) (list 'PT.pv (inc i))
    (= 'paInst (first sigma)) (if (zero? i) (second sigma) (list 'PT.pv (dec i)))
    (= 'paUp (first sigma)) (if (zero? i) '(PT.pv 0) (sh (sb-var (second sigma) (dec i))))))
(defn sb-term [sigma t]
  (cond
    (= t 'PT.pz) [t 'rfl]
    (and (seq? t) (= 'PT.pv (first t))) [(sb-var sigma (second t)) 'rfl]
    (and (seq? t) (#{'PT.ps 'PT.padd 'PT.pmul} (first t)))
    (let [args (rest t), results (mapv #(sb-term sigma %) args)]
      [(apply list (first t) (map first results))
       (ctor-eq (first t) (repeat (count args) 'PT)
         (map #(list 'paSbT sigma %) args) (map first results) (map second results))])
    (and (seq? t) (= 'paSbT (first t)) (= 'paShT (second t)))
    (let [a (nth t 2)]
      (if (= 'paInst (first sigma)) [a (list 'paSbT_inst_shift a (second sigma))]
        (let [[v p] (sb-term (second sigma) a)]
          [(sh v) (eq-chain (list 'paSbT_up_shift (second sigma) a)
                    (cg '(paSbT paShT) p))])))
    :else [(list 'paSbT sigma t) 'rfl]))
(defn sb-form [sigma p]
  (if (= p 'PF.pbot) [p 'rfl]
    (let [tag (first p), args (rest p)
          results (mapv (if (= tag 'PF.peq) #(sb-term sigma %)
                         #(sb-form (if (= tag 'PF.pall) (list 'paUp sigma) sigma) %)) args)]
      [(apply list tag (map first results))
       (ctor-eq tag (repeat (count args) (if (= tag 'PF.peq) 'PT 'PF))
         (map #(list (if (= tag 'PF.peq) 'paSbT 'paSbF)
                 (if (= tag 'PF.pall) (list 'paUp sigma) sigma) %) args)
         (map first results) (map second results))])))

(defn instantiate-closed! [nm formula n]
  (let [args (mapv #(symbol (str "a" %)) (range n))
        ps (vec (mapcat (fn [x] [x ':- 'PT]) args))
        tr #(list 'trF % 'ρ)
        term (symbol (str "paC_" nm))
        fname (symbol (str "paF_" nm))
        e0 (list 'paPad (tr fname) term)
        h0 (list (symbol (str term "_at")) 'chkf 'D 'ρ)
        [F e h]
        (reduce (fn [[F e h] [i arg]]
          (let [body (second F), [next-form eq] (sb-form (list 'paInst arg) body)
                raw (list 'paSbF (list 'paInst arg) body)
                en (list 'Exp.app e (list 'trT arg 'ρ))
                eqname (symbol (str "paEq_open_" nm "_" i))]
            (a/prove-theorem eqname ps (list 'Eq 'PF raw next-form)
              [(list 'have 'heq (list 'Eq 'PF raw next-form) eq) '(exact heq)])
            [next-form en
             (list 'rt_cast 'chkf 'D '(allW D) '(allW D) en (tr raw) (tr next-form)
               (list 'pa_instantiate 'chkf body arg 'D 'ρ 'hg e h) 'rfl
               (cg '(fn [p :- PF] (trF p ρ)) (apply list eqname args)))]))
          [formula e0 h0] (map-indexed vector args))]
    (a/prove-theorem (symbol (str "pa_eq_" (.toLowerCase (str nm))))
      (into (into '[chkf :- (=> Code Code Bool)] ps)
        '[D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)])
      (list 'Exists (list 'fn '[e :- Exp] (list 'Rt 'chkf 'D '(allW D) 'e (tr F))))
      ['(constructor) (list 'exact e) (list 'exact h)])))
(doseq [[nm formula _] (concat (filter #(#{'REFL 'CONGS} (first %)) pc/combinators)
                              eq/combinators)]
  (instantiate-closed! nm formula
    (loop [p formula, n 0] (if (= 'PF.pall (first p)) (recur (second p) (inc n)) n))))

;; Modus ponens for existentially supplied proof terms, all at allW.
(thm pa_exists_mp [chkf :- (=> Code Code Bool), p :- PF, q :- PF,
  D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ),
  hf :- (Exists (fn [f :- Exp] (Rt chkf D (allW D) f (trF (PF.pimp p q) ρ)))),
  hu :- (Exists (fn [u :- Exp] (Rt chkf D (allW D) u (trF p ρ))))]
  (Exists (fn [e :- Exp] (Rt chkf D (allW D) e (trF q ρ))))
  (refine' (exists_elimE _ _ hf _)) (intro f h_f)
  (refine' (exists_elimE _ _ hu _)) (intro u h_u)
  (constructor) (exact (Exp.app f u)) (exact (pa_mp chkf p q D ρ hg f u h_f h_u)))

(thm pa_cong_var_bool [chkf :- (=> Code Code Bool), i :- Nat, j :- Nat, x :- Nat,
  D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ), e :- Exp,
  he :- (Rt chkf D (allW D) e (trF (PF.peq (PT.pv i) (PT.pv j)) ρ)), bb :- Bool]
  (Exists (fn [u :- Exp] (Rt chkf D (allW D) u
    (trF (PF.peq (Bool.rec$1 (fn [_ :- Bool] PT) (PT.pv x) (PT.pv i) bb)
                 (Bool.rec$1 (fn [_ :- Bool] PT) (PT.pv x) (PT.pv j) bb)) ρ))))
  (cases bb)
  (exact (pa_eq_refl chkf (PT.pv x) D ρ hg))
  (constructor) (exact e) (exact he))

(defn peq [s t] (list 'PF.peq s t))
(defn pimp [p q] (list 'PF.pimp p q))
(defn rn [v s] (list 'paSbT (list 'paRn 'z v) s))
(defn ec [s] (peq (rn 'i s) (rn 'j s)))
(defn emp [p q f u] (list 'pa_exists_mp 'chkf p q 'D 'ρ 'hg f u))
(let [goal (list 'Exists (list 'fn '[u :- Exp]
             (list 'Rt 'chkf 'D '(allW D) 'u (list 'trF (ec 's0) 'ρ))))
      succ (emp (ec 't) (peq (list 'PT.ps (rn 'i 't)) (list 'PT.ps (rn 'j 't)))
             (list 'pa_eq_congs 'chkf (rn 'i 't) (rn 'j 't) 'D 'ρ 'hg) 'ih_t)
      binary (fn [ctor lemma]
               (let [R (peq (list ctor (rn 'i 't) (rn 'i 'u))
                             (list ctor (rn 'j 't) (rn 'j 'u)))]
                 (emp (ec 'u) R
                   (emp (ec 't) (pimp (ec 'u) R)
                     (list lemma 'chkf (rn 'i 't) (rn 'j 't) (rn 'i 'u) (rn 'j 'u) 'D 'ρ 'hg)
                     'ih_t) 'ih_u)))]
  (a/prove-theorem 'pa_cong
    '[chkf :- (=> Code Code Bool), ix :- Nat, jx :- Nat, z :- Nat,
      D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ), e :- Exp,
      he :- (Rt chkf D (allW D) e (trF (PF.peq (PT.pv ix) (PT.pv jx)) ρ)), s0 :- PT]
    (w/postwalk-replace {'i 'ix 'j 'jx} goal)
    ['(induction s0)
     '(exact (pa_cong_var_bool chkf ix jx i D ρ hg e he (Nat.beq i z)))
     '(exact (pa_eq_refl chkf PT.pz D ρ hg))
     (list 'exact (w/postwalk-replace {'i 'ix 'j 'jx} succ))
     (list 'exact (w/postwalk-replace {'i 'ix 'j 'jx} (binary 'PT.padd 'pa_eq_congp)))
     (list 'exact (w/postwalk-replace {'i 'ix 'j 'jx} (binary 'PT.pmul 'pa_eq_congt)))]))

(thm pa_exists_given [chkf :- (=> Code Code Bool), p :- PF,
  D :- (List Exp), ρ :- (=> Nat Nat), e :- Exp,
  he :- (Rt chkf D (allW D) e (trF p ρ))]
  (Exists (fn [u :- Exp] (Rt chkf D (allW D) u (trF p ρ))))
  (constructor) (exact e) (exact he))

;; E2: under e:i=j and h:s[i/z]=t[i/z], term congruence gives
;; s[i/z]=s[j/z] and t[i/z]=t[j/z]. Symmetry and two uses of
;; transitivity give s[j/z]=t[j/z]. No beta conversion on an opaque
;; translated formula is needed.
(let [E (peq '(PT.pv i) '(PT.pv j)), S (peq (rn 'i 's) (rn 'i 't))
      T (peq (rn 'j 's) (rn 'j 't))
      init {:D 'D :rho 'ρ :good 'hg :stack []}
      ce (#'pp/extend-proof init E), ch (#'pp/extend-proof ce S)
      {D :D r :rho hg :good} ch
      he (#'pp/hyp ch 1), hh (#'pp/hyp ch 0)
      si (rn 'i 's), sj (rn 'j 's), ti (rn 'i 't), tj (rn 'j 't)
      cong (fn [term] (list 'pa_cong 'chkf 'i 'j 'z D r hg (:e he) (:proof he) term))
      mp (fn [p q f u] (list 'pa_exists_mp 'chkf p q D r hg f u))
      sym (mp (peq si sj) (peq sj si) (list 'pa_eq_sym 'chkf si sj D r hg) (cong 's))
      ;; The given h is injected into Exists using a tiny helper below.
      h-ex (list 'pa_exists_given 'chkf S D r (:e hh) (:proof hh))
      sti (mp S (peq sj ti)
            (mp (peq sj si) (pimp S (peq sj ti))
              (list 'pa_eq_trans 'chkf sj si ti D r hg) sym) h-ex)
      result (mp (peq ti tj) T
               (mp (peq sj ti) (pimp (peq ti tj) T)
                 (list 'pa_eq_trans 'chkf sj ti tj D r hg) sti) (cong 't))
      lams (#'pp/lam init E (fn [_] (#'pp/lam ce S
                    (fn [_] {:p T :e 'body :proof 'hbody}))))]
  (a/prove-theorem 'pa_ax_e2
    '[chkf :- (=> Code Code Bool), i :- Nat, j :- Nat, z :- Nat, s :- PT, t :- PT,
      D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)]
    (list 'Exists (list 'fn '[e :- Exp] (list 'Rt 'chkf 'D '(allW D) 'e
      (list 'trF (pimp E (pimp S T)) 'ρ))))
    [(list 'refine' (list 'exists_elimE '_ '_ result '_)) '(intro body hbody)
     '(constructor) (list 'exact (:e lams)) (list 'exact (:proof lams))]))
