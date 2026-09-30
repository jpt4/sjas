(ns lcert.formal.conversion
  "F3m — conversion invariance (R4-metatheory.md §1.5, §3.2 Lemma 3.2,
  §3.3 Lemma 3.3's conversion clause) and the Conv case of the fundamental
  lemma (Lemma 3.6).

  The paper: if t ≡ t′ (both skeleton-typed) then ⟦t⟧ⁿη = ⟦t′⟧ⁿη
  (Lemma 3.2), and if A ≡ B then Vⁿₖ(A)η = Vⁿₖ(B)η (Lemma 3.3).  Conversion
  (conv.clj, Cv) is a chain of single steps (Step: a head step Hd at a
  path), taken in either direction, every element of which is a
  skeleton-well-formed type (SkJ Bool.true).

  DEVIATION (forced; §0 below has the kernel-checked counterexample).  Over
  skeleton typing alone the conversion clause is FALSE for this
  denotation, for the reason substitution.clj §5 found for Lemma 3.1: a
  branch list (bnil / bcons) is skeleton-typed at Lbl → s, but skOf gives it
  no skeleton, so ⟦f u⟧ defaults when u is a branch list.  A β-step that
  moves a branch list out of argument position changes the denotation:
      b  = (λ(y : Π(l:Lbl).Bool). caseL Bool 0 y) (bcons tt bnil)
      b′ = caseL Bool 0 (bcons tt bnil)   (b ⇝ b′ by β)
  ⟦b⟧ = ff (the application defaults) but ⟦b′⟧ = tt, so T(b′) ≡ T(b), both
  skeleton-well-formed, but V(T b′) = {⋆} and V(T b) = ∅.  The paper's
  syntax has no branch lists as terms (caseLbl's branches are part of its
  syntax); the encoding's bnil / bcons are first-class Exp terms only for
  structural recursion (syntax.clj).  So the lemmas below assume every
  element of the chain keeps branch lists in branch-list position: nbrF
  (below) — a branch list may occur only as the branch list of a caseL (or
  the tail of a branch list).  CvN is Cv with that condition on every
  element; the Conv rule of the fundamental lemma then needs a CvN chain
  (F_conv).  Real (Rt/Tl) terms type branch lists only at the pseudo-type
  tBrs, which no binder or argument has.

  Contents.
  §0  The counterexample (kernel-checked): conv_skj_counterexample.
  §1  nbrF, and skOf_complete: on a skeleton-typed term with no stray
      branch list, skOf returns the typed skeleton.
  §2  Inversion of skeleton typing (InvSkJ / skj_inv / inv_<ctor>).
  §3  Head steps preserve the denotation, skOf, the skeleton and V
      (Lemma 3.2 for Hd: β, ι, δ and T steps): hd_equiv.
  §4  Steps at a position (congruence): step_equiv, by induction on the
      skeleton typing of the stepped expression, one case per rule.
  §5  Chains: Lemma 3.2 for types' V (Lemma 3.3's conversion clause),
      cvN_V, and F_conv (the Conv case of Lemma 3.6).

  Equivalence at a position.  EquivAt w G s a b says a may replace b at a
  position of mode w, context G and skeleton s:
    w = false (a term):  ⟦a⟧ G s η = ⟦b⟧ G s η for every η, and
                         skOf G a = skOf G b (application and let read the
                         skeleton of their argument through skOf);
    w = true (a type):   skel a = skel b, and
                         Vⁿₖ(a) G η = Vⁿₖ(b) G η at the skeleton skel b.
  Only the source of a step need be typed (and nbrF): the target's
  denotation, skOf and V are computed from the source's."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.subst :refer :all]
            [lcert.formal.unfold :refer :all]
            [lcert.formal.substitution :refer :all]
            [lcert.formal.skeletons :refer :all]
            [lcert.formal.skof :refer :all]
            [lcert.formal.fundamental :refer :all]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private skj-rules @#'lcert.formal.substitution/skj-rules)
(def ^:private cong @#'lcert.formal.substitution/cong)

;; ===========================================================================
;; §1  Branch lists in branch-list position; skOf is complete there
;; ===========================================================================

;; nbrF e fl: e has no branch list (bnil / bcons) outside branch-list
;; position.  The flag fl says whether e itself stands in branch-list
;; position: the third field of caseL, and the tail of a bcons.  A branch
;; list is allowed exactly there; every other field is read with fl = false.
;; Recursion on e, returning a function of the flag (structural, as liftF).
;; The conjunctions are right-nested in field order, so a field's condition
;; is reached by band_left / band_right (skof.clj).
(defn- and-chain [xs]
  (if (seq xs) (reduce (fn [acc x] (list 'Bool.and x acc)) (last xs) (reverse (butlast xs))) 'true))

(defn- nbr-clause [[ctor fields]]
  (let [pat (if (seq fields) (apply list ctor (map first fields)) ctor)
        exps (for [[f ty] fields :when (= ty 'Exp)] f)
        flag (fn [f] (if (or (and (= ctor 'caseL) (= f 'bs)) (and (= ctor 'bcons) (= f 't))) 'true 'false))
        body (case ctor
               bnil 'fl
               bcons (list 'Bool.and 'fl (and-chain (for [f exps] (list (list 'nbrF f) (flag f)))))
               (and-chain (for [f exps] (list (list 'nbrF f) (flag f)))))]
    [pat (list 'fn '[fl :- Bool] body)]))

(eval (list 'a/defn 'nbrF '[e :- Exp] '(=> Bool Bool)
            (apply list 'match 'e (map nbr-clause exp-fields))))

;; nbr e: e stands in ordinary (term or type) position.
(a/defn nbr [e :- Exp] Bool ((nbrF e) false))

;; The rules whose conclusion is a formation judgment (w = true).
(def ^:private formation-rules '#{wEmpty wUnit wBool wNat wLbl wSyn wDia wR wT wPi wSig})

;; skOf_complete: G ⊢ e : s (a term) with no stray branch list ⟹
;; skOf G e = some s.  (skOf_typed, substitution.clj, shows skOf is none or
;; s on any typed term; none arises only from a branch list at the end of
;; skOf's spine, which nbrF excludes.)  Induction on the derivation, one
;; explicit tactic list per rule (in SkJ's constructor order, skj-rules):
;; formation rules are refuted by w = false; bnil / bcons by nbrF.
(defn- skc-case [rule]
  (cons '(intro hw hn)
        (cond
          (formation-rules rule) '[(exact (Bool.noConfusion hw))]
          :else
          (case rule
            sVar '[(exact h)]
            sIte '[(exact (ih_ht rfl (band_left ((nbrF t) false) ((nbrF e) false)
                                        (band_right ((nbrF b) false) (Bool.and ((nbrF t) false) ((nbrF e) false)) hn))))]
            sLam '[(exact (skOf_lam G r A t s (ih_ht rfl (band_right ((nbrF A) false) ((nbrF t) false) hn))))]
            sApp '[(exact (skOf_app G f u (Sk.arr s t) (ih_hf rfl (band_left ((nbrF f) false) ((nbrF u) false) hn))))]
            sBnil '[(exact (Bool.noConfusion hn))]
            sBcons '[(exact (Bool.noConfusion hn))]
            '[(rfl)]))))

(a/prove-theorem 'skOf_complete '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  '(=> (Eq Bool w0 Bool.false) (Eq Bool ((nbrF e0) false) Bool.true) (Eq (Option Sk) (skOf G0 e0) (Option.some Sk s0)))
  (into ['(induction der)] (mapcat skc-case skj-rules)))

;; The term form: a typed term with no stray branch list.
(thm skOf_ok [G :- (List Sk), e :- Exp, s :- Sk, h :- (SkJ Bool.false G e s), hn :- (Eq Bool ((nbrF e) false) Bool.true)]
  (Eq (Option Sk) (skOf G e) (Option.some Sk s))
  (exact (skOf_complete Bool.false G e s h rfl hn)))

;; ===========================================================================
;; §2  Inversion of skeleton typing
;; ===========================================================================
;; InvSkJ w G e s: the premises of the (unique) SkJ rule for e's
;; constructor, with the conclusion's indices as equations.  skj_inv proves
;; SkJ w G e s → InvSkJ w G e s by induction on the derivation, and
;; inv_<ctor> restates it at each constructor with the clause spelled out.
;; (Ansatz has no inversion tactic; `cases` on an indexed family with
;; non-variable indices is unreliable.)

(defn- ands [xs] (reduce (fn [acc x] (list 'And x acc)) (last xs) (reverse (butlast xs))))
(defn- sj [w G e s] (list 'SkJ w G e s))
(def ^:private WF '(Eq Bool w0 Bool.false))
(def ^:private WT '(Eq Bool w0 Bool.true))
(defn- s= [x] (list 'Eq 'Sk 's0 x))
(defn- ex [v ty body] (list 'Exists (list 'fn [v ':- ty] body)))
(defn- cons-sk [s G] (list 'List.cons 'Sk s G))
(defn- HT [q] (list 'Sk.arr 'Sk.dia (list 'Sk.arr 'Sk.lbl (list 'Sk.arr q (list 'Sk.arr q q)))))

(def ^:private inv-clause
  (merge
   (zipmap '[tEmpty tUnit tBool tNat tLbl tSyn tDia tR] (repeat (ands [WT (s= 'Sk.unit)])))
   {'tT (ands [WT (s= 'Sk.unit) (sj 'Bool.false 'G0 'b 'Sk.bool)])
    'tPi (ands [WT (s= 'Sk.unit) (sj 'Bool.true 'G0 'A 'Sk.unit) (sj 'Bool.true (cons-sk '(skel A) 'G0) 'B 'Sk.unit)])
    'tSig (ands [WT (s= 'Sk.unit) (sj 'Bool.true 'G0 'A 'Sk.unit) (sj 'Bool.true (cons-sk '(skel A) 'G0) 'B 'Sk.unit)])
    'var (ands [WF '(Eq (Option Sk) (nthS G0 i) (Option.some Sk s0))])
    'star (ands [WF (s= 'Sk.unit)])
    'abort (ands [WF (s= '(skel A)) (sj 'Bool.true 'G0 'A 'Sk.unit) (sj 'Bool.false 'G0 't 'Sk.unit)])
    'tt (ands [WF (s= 'Sk.bool)])
    'ff (ands [WF (s= 'Sk.bool)])
    'ite (ands [WF (sj 'Bool.false 'G0 'b 'Sk.bool) (sj 'Bool.false 'G0 't 's0) (sj 'Bool.false 'G0 'e 's0)])
    'elimB (ands [WF (s= '(skel P)) (sj 'Bool.true (cons-sk 'Sk.bool 'G0) 'P 'Sk.unit) (sj 'Bool.false 'G0 'b 'Sk.bool)
                  (sj 'Bool.false 'G0 't '(skel P)) (sj 'Bool.false 'G0 'e '(skel P))])
    'zero (ands [WF (s= 'Sk.nat)])
    'succ (ands [WF (s= 'Sk.nat) (sj 'Bool.false 'G0 'n 'Sk.nat)])
    'recN (ands [WF (s= '(skel P)) (sj 'Bool.true (cons-sk 'Sk.nat 'G0) 'P 'Sk.unit) (sj 'Bool.false 'G0 'z '(skel P))
                 (sj 'Bool.false '(sk2 (skel P) Sk.nat G0) 's '(skel P)) (sj 'Bool.false 'G0 'n 'Sk.nat)])
    'lbl (ands [WF (s= 'Sk.lbl)])
    'caseL (ands [WF (s= '(skel P)) (sj 'Bool.true (cons-sk 'Sk.lbl 'G0) 'P 'Sk.unit) (sj 'Bool.false 'G0 'a 'Sk.lbl)
                  (sj 'Bool.false 'G0 'bs '(Sk.arr Sk.lbl (skel P)))])
    'bnil (ands [WF (ex 's1 'Sk (s= '(Sk.arr Sk.lbl s1)))])
    'bcons (ands [WF (ex 's1 'Sk (ands [(s= '(Sk.arr Sk.lbl s1)) (sj 'Bool.false 'G0 'h 's1) (sj 'Bool.false 'G0 't '(Sk.arr Sk.lbl s1))]))])
    'sleaf (ands [WF (s= 'Sk.syn) (sj 'Bool.false 'G0 'a 'Sk.lbl)])
    'snode (ands [WF (s= 'Sk.syn) (sj 'Bool.false 'G0 'a 'Sk.lbl) (sj 'Bool.false 'G0 'c1 'Sk.syn) (sj 'Bool.false 'G0 'c2 'Sk.syn)])
    'recS (ands [WF (s= '(skel P)) (sj 'Bool.true (cons-sk 'Sk.syn 'G0) 'P 'Sk.unit)
                 (sj 'Bool.false (cons-sk 'Sk.lbl 'G0) 'tl '(skel P))
                 (sj 'Bool.false '(sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G0))) 'tn '(skel P))
                 (sj 'Bool.false 'G0 'c 'Sk.syn)])
    'leaf (ands [WF (s= 'Sk.cert) (sj 'Bool.false 'G0 'a 'Sk.lbl)])
    'node (ands [WF (s= 'Sk.cert) (sj 'Bool.false 'G0 'd 'Sk.dia) (sj 'Bool.false 'G0 'a 'Sk.lbl)
                 (sj 'Bool.false 'G0 'r1 'Sk.cert) (sj 'Bool.false 'G0 'r2 'Sk.cert)])
    'itR (ands [WF (s= '(skel X)) (sj 'Bool.true 'G0 'X 'Sk.unit) (sj 'Bool.false 'G0 'g '(Sk.arr Sk.lbl (skel X)))
                (sj 'Bool.false 'G0 'h (HT '(skel X))) (sj 'Bool.false 'G0 'r 'Sk.cert)])
    'prn (ands [WF (s= 'Sk.syn) (sj 'Bool.false 'G0 'r 'Sk.cert)])
    'lam (ands [WF (ex 's1 'Sk (ands [(s= '(Sk.arr (skel A) s1)) (sj 'Bool.true 'G0 'A 'Sk.unit)
                                      (sj 'Bool.false (cons-sk '(skel A) 'G0) 't 's1)]))])
    'app (ands [WF (ex 's1 'Sk (ands [(sj 'Bool.false 'G0 'f '(Sk.arr s1 s0)) (sj 'Bool.false 'G0 'u 's1)]))])
    'pair (ands [WF (ex 'rr 'U (ex 'AA 'Exp (ex 'BB 'Exp
                  (ands ['(Eq Exp S (Exp.tSig rr AA BB)) (s= '(Sk.prod (skel AA) (skel BB)))
                         (sj 'Bool.true 'G0 'S 'Sk.unit) (sj 'Bool.false 'G0 'a '(skel AA)) (sj 'Bool.false 'G0 'b '(skel BB))]))))])
    'letp (ands [WF (s= '(skel C)) (ex 's1 'Sk (ex 's2 'Sk
                  (ands [(sj 'Bool.true 'G0 'C 'Sk.unit) (sj 'Bool.false 'G0 'p '(Sk.prod s1 s2))
                         (sj 'Bool.false '(sk2 s2 s1 G0) 't '(skel C))])))])
    'chk (ands [WF (s= 'Sk.bool) (sj 'Bool.false 'G0 'c 'Sk.syn) (sj 'Bool.false 'G0 'd 'Sk.syn)])
    'h1 (ands [WF (s= 'Sk.unit) (sj 'Bool.false 'G0 'r 'Sk.cert) (sj 'Bool.false 'G0 's 'Sk.cert) (sj 'Bool.false 'G0 'c 'Sk.syn)
               (sj 'Bool.false 'G0 'e1 'Sk.unit) (sj 'Bool.false 'G0 'e2 'Sk.unit)])
    'refl (ands [WF (s= '(skel D)) (sj 'Bool.true 'G0 'D 'Sk.unit) '(Eq Bool (isBaseTy D) Bool.true)
                 (sj 'Bool.false 'G0 'r 'Sk.cert) (sj 'Bool.false 'G0 'e 'Sk.unit)])
    'insp (ands [WF (s= '(skel X)) (sj 'Bool.true 'G0 'X 'Sk.unit) (sj 'Bool.false 'G0 'r 'Sk.cert) (sj 'Bool.false 'G0 'c 'Sk.syn)
                 (sj 'Bool.false '(sk2 Sk.unit Sk.cert G0) 't1 '(skel X)) (sj 'Bool.false '(sk2 Sk.unit Sk.cert G0) 't2 '(skel X))])
    'tBrs 'False}))

;; InvSkJ by Exp.rec into Prop-valued clauses (the recursive results are
;; unused).
(eval
 (list 'kdef 'InvSkJ '(=> Bool (List Sk) Exp Sk Prop)
       (list 'fn '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk]
             (concat (list 'Exp.rec$1 '(fn [_ :- Exp] Prop))
                     (for [[ctor fields] exp-fields]
                       (let [bs (vec (concat (mapcat (fn [[f ty]] [f :- ty]) fields)
                                             (mapcat (fn [[f ty]] (when (= ty 'Exp) [(symbol (str "i_" f)) :- 'Prop])) fields)))]
                         (if (seq bs) (list 'fn bs (inv-clause ctor)) (inv-clause ctor))))
                     ['e0]))))

;; Each SkJ rule: its constructor, conclusion (w, s), the conclusion's
;; constructor fields, and the proof of the clause from the rule's premises
;; (field names as `induction` binds them; A = And.intro, E = Exists.intro).
(def ^:private skj-concl
  (merge
   (zipmap '[wEmpty wUnit wBool wNat wLbl wSyn wDia wR]
           (map (fn [c] [c 'Bool.true 'Sk.unit [] '(A rfl rfl)]) '[tEmpty tUnit tBool tNat tLbl tSyn tDia tR]))
   '{wT [tT Bool.true Sk.unit [b] (A rfl (A rfl hb))]
     wPi [tPi Bool.true Sk.unit [r A B] (A rfl (A rfl (A hA hB)))]
     wSig [tSig Bool.true Sk.unit [r A B] (A rfl (A rfl (A hA hB)))]
     sVar [var Bool.false s [i] (A rfl h)]
     sStar [star Bool.false Sk.unit [] (A rfl rfl)]
     sAbort [abort Bool.false (skel A) [A t] (A rfl (A rfl (A hA ht)))]
     sTT [tt Bool.false Sk.bool [] (A rfl rfl)]
     sFF [ff Bool.false Sk.bool [] (A rfl rfl)]
     sIte [ite Bool.false s [b t e] (A rfl (A hb (A ht he)))]
     sElimB [elimB Bool.false (skel P) [P b t e] (A rfl (A rfl (A hP (A hb (A ht he)))))]
     sZero [zero Bool.false Sk.nat [] (A rfl rfl)]
     sSucc [succ Bool.false Sk.nat [n] (A rfl (A rfl h))]
     sRecN [recN Bool.false (skel P) [P z st n] (A rfl (A rfl (A hP (A hz (A hs hn)))))]
     sLbl [lbl Bool.false Sk.lbl [l] (A rfl rfl)]
     sCaseL [caseL Bool.false (skel P) [P x bs] (A rfl (A rfl (A hP (A hx hb))))]
     sBnil [bnil Bool.false (Sk.arr Sk.lbl s) [] (A rfl (E s rfl))]
     sBcons [bcons Bool.false (Sk.arr Sk.lbl s) [h t] (A rfl (E s (A rfl (A hh ht))))]
     sSleaf [sleaf Bool.false Sk.syn [x] (A rfl (A rfl h))]
     sSnode [snode Bool.false Sk.syn [x c1 c2] (A rfl (A rfl (A hx (A h1 h2))))]
     sRecS [recS Bool.false (skel P) [P tl tn c] (A rfl (A rfl (A hP (A hl (A hn hc)))))]
     sLeaf [leaf Bool.false Sk.cert [x] (A rfl (A rfl h))]
     sNode [node Bool.false Sk.cert [d x r1 r2] (A rfl (A rfl (A hd (A hx (A h1 h2)))))]
     sItR [itR Bool.false (skel X) [X g h r] (A rfl (A rfl (A hX (A hg (A hh hr)))))]
     sPrn [prn Bool.false Sk.syn [r] (A rfl (A rfl h))]
     sLam [lam Bool.false (Sk.arr (skel A) s) [r A t] (A rfl (E s (A rfl (A hA ht))))]
     sApp [app Bool.false t [f u] (A rfl (E s (A hf hu)))]
     sPair [pair Bool.false (Sk.prod (skel A) (skel B)) [(Exp.tSig r A B) x y] (A rfl (E r (E A (E B (A rfl (A rfl (A hS (A hx hy))))))))]
     sLetp [letp Bool.false (skel C) [C p t] (A rfl (A rfl (E s1 (E s2 (A hC (A hp ht))))))]
     sChk [chk Bool.false Sk.bool [c d] (A rfl (A rfl (A hc hd)))]
     sH1 [h1 Bool.false Sk.unit [r s c e1 e2] (A rfl (A rfl (A hr (A hs (A hc (A h1 h2))))))]
     sRefl [refl Bool.false (skel D) [D r e] (A rfl (A rfl (A hD (A hb (A hr he)))))]
     sInsp [insp Bool.false (skel X) [X r c t1 t2] (A rfl (A rfl (A hX (A hr (A hc (A h1 h2))))))]}))

(def ^:private field-names (into {} (for [[c fs] exp-fields] [c (mapv first fs)])))

;; The clause of `ctor` at w, G, s and the given field values.
(defn- clause-at [ctor w G s vals]
  (walk/postwalk-replace (merge {'w0 w 'G0 G 's0 s} (zipmap (field-names ctor) vals)) (inv-clause ctor)))

;; A proof term: A/E expanded along the clause's shape (Exists.intro is
;; given its predicate explicitly, read off the clause).
(defn- build [pf clause]
  (cond
    (and (seq? pf) (= 'A (first pf)))
    (list 'And.intro (build (second pf) (second clause)) (build (nth pf 2) (nth clause 2)))
    (and (seq? pf) (= 'E (first pf)))
    (let [[_ [_ [v _ ty] body]] clause
          w (second pf)]
      (list 'AT_Exists.intro ty (list 'fn [v ':- ty] body) w
            (build (nth pf 2) (walk/postwalk-replace {v w} body))))
    :else pf))

(defn- inv-case [rule]
  (let [[ctor w s vals pf] (skj-concl rule)
        cl (clause-at ctor w 'G s vals)]
    [(list 'change cl) (list 'exact (build pf cl))]))

(a/prove-theorem 'skj_inv '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  '(InvSkJ w0 G0 e0 s0)
  (lv (into ['(induction der)] (mapcat inv-case skj-rules))))
