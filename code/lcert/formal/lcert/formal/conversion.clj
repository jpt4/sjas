(ns lcert.formal.conversion
  "F3m — conversion invariance (R4-metatheory.md §1.5, §3.2 Lemma 3.2,
  §3.3 Lemma 3.3's conversion clause) and the Conv case of the fundamental
  lemma (Lemma 3.6).

  The paper: if t ≡ t′ (both skeleton-typed) then ⟦t⟧ⁿη = ⟦t′⟧ⁿη
  (Lemma 3.2), and if A ≡ B then Vⁿₖ(A)η = Vⁿₖ(B)η (Lemma 3.3).  Conversion
  (conv.clj, Cv) is a chain of single steps (Step: a head step Hd at a
  path), taken in either direction, every element of which is a
  skeleton-well-formed type (SkJ Bool.true).

  DEVIATION (forced; conv_skj_counterexample is the kernel-checked witness).  Over
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
  structural recursion (syntax.clj).  Steps that read an argument through
  skOf (β, and the ι-steps that substitute or apply) therefore assume nbr
  (§1): a branch list may occur only as the branch list of a caseL, or as
  the tail of a branch list.  Cv itself only requires SkJ of each element,
  so a chain lemma cannot induct on plain Cv; it will need nbr on every
  element.  Real (Rt/Tl) terms type branch lists only at the pseudo-type
  tBrs, which no binder or argument has.

  Contents.
  §1  nbrF, and skOf_complete: on a skeleton-typed term with no stray
      branch list, skOf returns the typed skeleton.
  §2  Inversion of skeleton typing (InvSkJ / skj_inv / inv_<ctor>).
  §3  EquivAt, and the head steps proved so far (Lemma 3.2 for Hd).
      ι that only selects a branch (ite, elimB, recN at zero) and print
      preserve the denotation with no nbr hypothesis.  β preserves it
      when the redex is nbr (den_beta, via den_subst1 and skOf_complete).
      conv_skj_counterexample is the kernel-checked failure of β over
      SkJ alone.  itR on a leaf (den_itRL_nbr) needs nbr so the label has an skOf.  T(tt) and T(ff) preserve V (hd_tTT, hd_tTF).  The substituting ι-steps, itR on a node, caseLbl, and δ are open.
  β for let (den_betaLet) is the same pattern.  Position congruence, the chain for V, and F_conv are not in this file yet.

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

;; inv_<ctor> w G fields… sk hj : the clause, spelled out (by skj_inv; the
;; kernel unfolds InvSkJ at the constructor).
(defn- ctor-term [ctor vals] (if (seq vals) (apply list (symbol (str "Exp." ctor)) vals) (symbol (str "Exp." ctor))))

(doseq [[ctor fields] exp-fields :when (not= ctor 'tBrs)]
  (let [fs (map first fields)
        e (ctor-term ctor fs)]
    (a/prove-theorem (symbol (str "inv_" ctor))
      (lv (vec (concat '[w :- Bool, G :- (List Sk)] (mapcat (fn [[f ty]] [f :- ty]) fields)
                       ['sk :- 'Sk 'hj :- (list 'SkJ 'w 'G e 'sk)])))
      (lv (clause-at ctor 'w 'G 'sk fs))
      (lv [(list 'exact (list 'skj_inv 'w 'G e 'sk 'hj))]))))

;; ===========================================================================
;; §3  Equivalence at a position; head steps
;; ===========================================================================

(kdef EquivAt
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> Nat Bool (List Sk) Sk Exp Exp Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
       n :- Nat, w :- Bool, G :- (List Sk), s :- Sk, a :- Exp, b :- Exp]
    (And (=> (Eq Bool w Bool.false)
             (And (forall [en (HEnv G)] (Eq (Car s) (den chkf dec encTy n a G s en) (den chkf dec encTy n b G s en)))
                  (Eq (Option Sk) (skOf G a) (skOf G b))))
         (=> (Eq Bool w Bool.true)
             (And (Eq Sk (skel a) (skel b))
                  (forall [en (HEnv G)] (forall [k Nat] (forall [v (Car (skel b))]
                    (Eq Prop (V chkf dec encTy n a G en k (skel b) v) (V chkf dec encTy n b G en k (skel b) v))))))))))

(def ^:private P4 '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat])
(defn- dn [e G s en] (list 'den 'chkf 'dec 'encTy 'n e G s en))
(defn- Vn [A G en k s v] (list 'V 'chkf 'dec 'encTy 'n A G en k s v))
(defn- EQ [w G s a b] (list 'EquivAt 'chkf 'dec 'encTy 'n w G s a b))
(defmacro ^:private pthm [nm params prop & tactics]
  `(a/prove-theorem '~nm (lv (into P4 '~params)) (lv '~prop) (lv '~(vec tactics))))

;; Building and reading EquivAt.
(pthm mk_eqv_t [w :- Bool, G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, hw :- (Eq Bool w Bool.false),
                hd :- (forall [en (HEnv G)] (Eq (Car s) (den chkf dec encTy n a G s en) (den chkf dec encTy n b G s en))),
                hk :- (Eq (Option Sk) (skOf G a) (skOf G b))]
  (EquivAt chkf dec encTy n w G s a b)
  (subst hw)
  (unfold EquivAt)
  (apply And.intro)
  (intro hf)
  (exact (And.intro hd hk))
  (intro ht)
  (exact (Bool.noConfusion ht)))

(pthm mk_eqv_ty [w :- Bool, G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, hw :- (Eq Bool w Bool.true),
                 hs :- (Eq Sk (skel a) (skel b)),
                 hv :- (forall [en (HEnv G)] (forall [k Nat] (forall [v (Car (skel b))]
                         (Eq Prop (V chkf dec encTy n a G en k (skel b) v) (V chkf dec encTy n b G en k (skel b) v)))))]
  (EquivAt chkf dec encTy n w G s a b)
  (subst hw)
  (unfold EquivAt)
  (apply And.intro)
  (intro hf)
  (exact (Bool.noConfusion hf))
  (intro ht)
  (exact (And.intro hs hv)))

(pthm eqv_den [G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, h :- (EquivAt chkf dec encTy n Bool.false G s a b), en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n a G s en) (den chkf dec encTy n b G s en))
  (unfold EquivAt at h)
  (exact (And.left ((And.left h) rfl) en)))

(pthm eqv_sko [G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, h :- (EquivAt chkf dec encTy n Bool.false G s a b)]
  (Eq (Option Sk) (skOf G a) (skOf G b))
  (unfold EquivAt at h)
  (exact (And.right ((And.left h) rfl))))

(pthm eqv_skel [G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, h :- (EquivAt chkf dec encTy n Bool.true G s a b)]
  (Eq Sk (skel a) (skel b))
  (unfold EquivAt at h)
  (exact (And.left ((And.right h) rfl))))

(pthm eqv_V [G :- (List Sk), s :- Sk, a :- Exp, b :- Exp, h :- (EquivAt chkf dec encTy n Bool.true G s a b),
             en :- (HEnv G), k :- Nat, v :- (Car (skel b))]
  (Eq Prop (V chkf dec encTy n a G en k (skel b) v) (V chkf dec encTy n b G en k (skel b) v))
  (unfold EquivAt at h)
  (exact (And.right ((And.right h) rfl) en k v)))

;; --- ι steps that select a branch (Lemma 3.2; no nbr hypothesis) -----------
;;
;; skOf of ite follows the then-branch, ignoring the scrutinee (carrier.clj).
;; So ite tt t e and t agree on skOf by computation.  ite ff agrees with the
;; else-branch only once both branches have the same skOf (both typed at s
;; and nbr, via skOf_ok); that packaging is not here yet.  elimB and recN
;; report skel of the motive, which need not be skOf of the selected branch.

;; Lemma 3.2 for Hd.iteT, packaged as EquivAt.  inv_ite gives w = false, so
;; the type side of EquivAt is vacuous.  The last rewrite closes both the
;; denotation (Bool.rec at tt) and skOf (definitional).  Do not add a trailing
;; rfl: rw already closes by reflexivity, and a further rfl aborts the proof.
(pthm hd_iteT [t :- Exp, e :- Exp, w :- Bool, G :- (List Sk), s :- Sk, hj :- (SkJ w G (Exp.ite Exp.tt t e) s)]
  (EquivAt chkf dec encTy n w G s t (Exp.ite Exp.tt t e))
  (apply mk_eqv_t)
  (exact (And.left (inv_ite w G Exp.tt t e s hj)))
  (intro en)
  (rw [(den_ite_at chkf dec encTy n Exp.tt t e G s en)])
  (change (Eq (Car s) (den chkf dec encTy n t G s en)
              (Bool.rec$1 (fn [_ :- Bool] (Car s)) (den chkf dec encTy n e G s en) (den chkf dec encTy n t G s en)
                          (den chkf dec encTy n Exp.tt G Sk.bool en))))
  (rw [(den_tt_bool chkf dec encTy n G en)]))

(thm skof_ite [b :- Exp, t :- Exp, e :- Exp, G :- (List Sk)]
  (Eq (Option Sk) (skOf G t) (skOf G (Exp.ite b t e)))
  (rfl))

;; ⟦ite tt t e⟧ = ⟦t⟧.  Bool.rec's arguments are else, then, scrutinee.
(thm den_iteT [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               t :- Exp, e :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n t G s en) (den chkf dec encTy n (Exp.ite Exp.tt t e) G s en))
  (rw [(den_ite_at chkf dec encTy n Exp.tt t e G s en)])
  (change (Eq (Car s) (den chkf dec encTy n t G s en)
              (Bool.rec$1 (fn [_ :- Bool] (Car s)) (den chkf dec encTy n e G s en) (den chkf dec encTy n t G s en)
                          (den chkf dec encTy n Exp.tt G Sk.bool en))))
  (rw [(den_tt_bool chkf dec encTy n G en)]))

(thm den_iteF [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               t :- Exp, e :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n e G s en) (den chkf dec encTy n (Exp.ite Exp.ff t e) G s en))
  (rw [(den_ite_at chkf dec encTy n Exp.ff t e G s en)])
  (change (Eq (Car s) (den chkf dec encTy n e G s en)
              (Bool.rec$1 (fn [_ :- Bool] (Car s)) (den chkf dec encTy n e G s en) (den chkf dec encTy n t G s en)
                          (den chkf dec encTy n Exp.ff G Sk.bool en))))
  (rw [(den_ff_bool chkf dec encTy n G en)]))

(thm den_elimT [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                P :- Exp, t :- Exp, e :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n t G s en) (den chkf dec encTy n (Exp.elimB P Exp.tt t e) G s en))
  (rw [(den_elimB_at chkf dec encTy n P Exp.tt t e G s en)])
  (change (Eq (Car s) (den chkf dec encTy n t G s en)
              (Bool.rec$1 (fn [_ :- Bool] (Car s)) (den chkf dec encTy n e G s en) (den chkf dec encTy n t G s en)
                          (den chkf dec encTy n Exp.tt G Sk.bool en))))
  (rw [(den_tt_bool chkf dec encTy n G en)]))

(thm den_elimF [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                P :- Exp, t :- Exp, e :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n e G s en) (den chkf dec encTy n (Exp.elimB P Exp.ff t e) G s en))
  (rw [(den_elimB_at chkf dec encTy n P Exp.ff t e G s en)])
  (change (Eq (Car s) (den chkf dec encTy n e G s en)
              (Bool.rec$1 (fn [_ :- Bool] (Car s)) (den chkf dec encTy n e G s en) (den chkf dec encTy n t G s en)
                          (den chkf dec encTy n Exp.ff G Sk.bool en))))
  (rw [(den_ff_bool chkf dec encTy n G en)]))

;; ⟦recN P z st zero⟧ = ⟦z⟧.  Nat.rec at zero is the base; den_zero_at
;; reduces the scrutinee and closes the goal by computation.
(thm den_recNZ [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                P :- Exp, z :- Exp, st :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n z G s en) (den chkf dec encTy n (Exp.recN P z st Exp.zero) G s en))
  (rw [(den_recN_at chkf dec encTy n P z st Exp.zero G s en)])
  (change (Eq (Car s) (den chkf dec encTy n z G s en)
              (Nat.rec$1 (fn [_ :- Nat] (Car s)) (den chkf dec encTy n z G s en)
                (fn [k :- Nat, acc :- (Car s)] (den chkf dec encTy n st (sk2 s Sk.nat G) s (Prod.mk acc (Prod.mk k en))))
                (den chkf dec encTy n Exp.zero G Sk.nat en))))
  (rw [(den_zero_at chkf dec encTy n G Sk.nat en)]))

;; Print.  ⟦prn (leaf x)⟧ = ⟦sleaf x⟧ and ⟦prn (node d x r1 r2)⟧ =
;; ⟦snode x (prn r1) (prn r2)⟧.  The node token d is not in the code.
(thm den_prnL [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               x :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (Exp.sleaf x) G s en) (den chkf dec encTy n (Exp.prn (Exp.leaf x)) G s en))
  (rw [(den_prn_at chkf dec encTy n (Exp.leaf x) G s en)])
  (rw [(den_sleaf_at chkf dec encTy n x G s en)])
  (change (Eq (Car s)
              (coe Sk.syn s (Code.sl (den chkf dec encTy n x G Sk.lbl en)))
              (coe Sk.syn s (den chkf dec encTy n (Exp.leaf x) G Sk.cert en))))
  (rw [(den_leaf_at chkf dec encTy n x G Sk.cert en)]))

(thm den_prnN [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s)
      (den chkf dec encTy n (Exp.snode x (Exp.prn r1) (Exp.prn r2)) G s en)
      (den chkf dec encTy n (Exp.prn (Exp.node d x r1 r2)) G s en))
  (rw [(den_prn_at chkf dec encTy n (Exp.node d x r1 r2) G s en)])
  (rw [(den_snode_at chkf dec encTy n x (Exp.prn r1) (Exp.prn r2) G s en)])
  (change (Eq (Car s)
              (coe Sk.syn s (Code.sn (den chkf dec encTy n x G Sk.lbl en)
                                     (den chkf dec encTy n (Exp.prn r1) G Sk.syn en)
                                     (den chkf dec encTy n (Exp.prn r2) G Sk.syn en)))
              (coe Sk.syn s (den chkf dec encTy n (Exp.node d x r1 r2) G Sk.cert en))))
  (rw [(den_node_at chkf dec encTy n d x r1 r2 G Sk.cert en)])
  (rw [(den_prn_at chkf dec encTy n r1 G Sk.syn en)])
  (rw [(den_prn_at chkf dec encTy n r2 G Sk.syn en)]))

;; boolExp b denotes the Boolean b, coerced into the skeleton s.  cases on
;; Bool focuses the false goal first (do not trust (peek)'s numbering).
(thm den_boolExp_ff [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                     G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (boolExp Bool.false) G s en) (coe Sk.bool s Bool.false))
  (rw [(den_ff_at chkf dec encTy n G s en)]))

(thm den_boolExp_tt [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                     G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (boolExp Bool.true) G s en) (coe Sk.bool s Bool.true))
  (rw [(den_tt_at chkf dec encTy n G s en)]))

(thm den_boolExp [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                  b :- Bool, G :- (List Sk), s :- Sk, en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (boolExp b) G s en) (coe Sk.bool s b))
  (cases b)
  (exact (den_boolExp_ff chkf dec encTy n G s en))
  (exact (den_boolExp_tt chkf dec encTy n G s en)))

;; --- δ's coding of canonical syntax (partial) --------------------------------
;; codeOf succeeds only for sleaf (lbl l) and snode of the same (skel.clj).
;; none ≠ some for Code; cases on that equation is accepted by the kernel
;; (unlike Option in general, which needs some_inj / none_ne_someE).

(thm none_ne_someC [r :- Code, h :- (Eq (Option Code) (Option.none Code) (Option.some Code r))]
  False
  (cases h))

;; ⟦sleaf (lbl l)⟧ at Syn is the leaf code Code.sl l.
(thm den_sleaf_code [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                     l :- Nat, G :- (List Sk), en :- (HEnv G)]
  (Eq Code (den chkf dec encTy n (Exp.sleaf (Exp.lbl l)) G Sk.syn en) (Code.sl l))
  (rw [(den_sleaf_at chkf dec encTy n (Exp.lbl l) G Sk.syn en)])
  (change (Eq Code (coe Sk.syn Sk.syn (Code.sl (den chkf dec encTy n (Exp.lbl l) G Sk.lbl en))) (Code.sl l)))
  (rw [(den_lbl_at chkf dec encTy n l G Sk.lbl en)]))

;; codeOf (sleaf x) = some c0 ⇒ x is a label and c0 is its leaf code.
;; One False.elim per non-label constructor: first/exact would accept the
;; eliminator on the label goal and the kernel would then reject the theorem.
(let [ctors (mapv first exp-fields)
      idx (.indexOf ctors 'lbl)
      n (count ctors)
      refute '(exact (False.elim$0 (none_ne_someC c0 hc)))
      lbl ['(have he (Eq Code (Code.sl l) c0) (Option.some.inj hc))
           '(apply Exists.intro)
           '(exact l)
           '(exact (And.intro (Eq.refl$1 (Exp.lbl l)) (Eq.symm he)))]
      tacs (vec (concat ['(cases x)] (repeat idx refute) lbl (repeat (- n idx 1) refute)))]
  (when (neg? idx) (throw (ex-info "lbl is not an Exp constructor" {})))
  (a/prove-theorem 'codeOf_sleaf_inv
    (lv '[x :- Exp, c0 :- Code, hc :- (Eq (Option Code) (codeOf (Exp.sleaf x)) (Option.some Code c0))])
    (lv '(Exists (fn [l :- Nat] (And (Eq Exp x (Exp.lbl l)) (Eq Code c0 (Code.sl l))))))
    (lv tacs)))

;; --- β (Lemma 3.2), under nbr ------------------------------------------------
;; Arrow injectivity.  cases on an equality of arrows is accepted (the
;; some/none pitfall is special to Option).
(thm arr_inj [a :- Sk, b :- Sk, c :- Sk, d :- Sk, h :- (Eq Sk (Sk.arr a b) (Sk.arr c d))]
  (And (Eq Sk a c) (Eq Sk b d))
  (cases h)
  (exact (And.intro rfl rfl)))

;; Exists-elimination for a skeleton witness.  The motive is the predicate
;; P; the witness and its proof are the fields cases binds (w and h).
(thm exSk [P :- (=> Sk Prop), Q :- Prop, hx :- (Exists P), f :- (forall [s Sk] (=> (P s) Q))]
  Q
  (cases hx)
  (exact (f w h)))

;; ⟦λ(r:A). t⟧ at Arr (skel A) sb, applied to v, is ⟦t⟧ at (v, η).
;; One unfolding of den_lam; arrCase at an arrow reduces to the body.
(thm den_lam_arr [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                  r :- U, A :- Exp, t :- Exp, G :- (List Sk), sb :- Sk, en :- (HEnv G), v :- (Car (skel A))]
  (Eq (Car sb) ((den chkf dec encTy n (Exp.lam r A t) G (Sk.arr (skel A) sb) en) v)
               (den chkf dec encTy n t (List.cons Sk (skel A) G) sb (Prod.mk v en)))
  (rw [(den_lam_at chkf dec encTy n r A t G (Sk.arr (skel A) sb) en)]))

;; The unpacked β equation.  harr identifies the application skeleton with
;; the λ skeleton; ht types the body in (skel A :: G); hu types the argument
;; at the domain; hn is nbr of the redex, which gives skOf of the argument
;; via skOf_ok (a branch list in argument position is exactly what hn rules
;; out — see conv_skj_counterexample).
(thm den_beta_core [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    r :- U, A :- Exp, t :- Exp, u :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, sb :- Sk,
                    harr :- (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb)),
                    ht :- (SkJ Bool.false (List.cons Sk (skel A) G) t sb),
                    hu :- (SkJ Bool.false G u s1),
                    hn :- (Eq Bool (nbr (Exp.app (Exp.lam r A t) u)) Bool.true),
                    en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (subst1 u t) G s en) (den chkf dec encTy n (Exp.app (Exp.lam r A t) u) G s en))
  (have hai (And (Eq Sk s1 (skel A)) (Eq Sk s sb)) (arr_inj s1 s (skel A) sb harr))
  (have h1 (Eq Sk s1 (skel A)) (And.left hai))
  (have h2 (Eq Sk s sb) (And.right hai))
  (have hnu (Eq Bool ((nbrF u) false) Bool.true)
        (band_right ((nbrF (Exp.lam r A t)) false) ((nbrF u) false) hn))
  (have hku (Eq (Option Sk) (skOf G u) (Option.some Sk s1)) (skOf_ok G u s1 hu hnu))
  (subst h1)
  (subst h2)
  (rw [(den_app_some chkf dec encTy n (Exp.lam r A t) u G (skel A) sb en hku)])
  (rw [(den_lam_arr chkf dec encTy n r A t G sb en (den chkf dec encTy n u G (skel A) en))])
  (exact (den_subst1 chkf dec encTy n Bool.false G (skel A) t sb ht u hu hku en)))

;; Lemma 3.2 for Hd.beta, under nbr of the redex.  Inversion of the
;; application and of the λ supply the skeletons den_beta_core needs.
(thm den_beta [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
               r :- U, A :- Exp, t :- Exp, u :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.app (Exp.lam r A t) u) s),
               hn :- (Eq Bool (nbr (Exp.app (Exp.lam r A t) u)) Bool.true),
               en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (subst1 u t) G s en) (den chkf dec encTy n (Exp.app (Exp.lam r A t) u) G s en))
  (have happ (And (Eq Bool Bool.false Bool.false)
                  (Exists (fn [s1 :- Sk]
                    (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s))
                         (SkJ Bool.false G u s1)))))
        (inv_app Bool.false G (Exp.lam r A t) u s hj))
  (exact (exSk (fn [s1 :- Sk] (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s)) (SkJ Bool.false G u s1)))
               (Eq (Car s) (den chkf dec encTy n (subst1 u t) G s en) (den chkf dec encTy n (Exp.app (Exp.lam r A t) u) G s en))
               (And.right happ)
               (fn [s1 :- Sk, hs1 :- (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s)) (SkJ Bool.false G u s1))]
                 (exSk (fn [sb :- Sk] (And (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb))
                                           (And (SkJ Bool.true G A Sk.unit)
                                                (SkJ Bool.false (List.cons Sk (skel A) G) t sb))))
                       (Eq (Car s) (den chkf dec encTy n (subst1 u t) G s en) (den chkf dec encTy n (Exp.app (Exp.lam r A t) u) G s en))
                       (And.right (inv_lam Bool.false G r A t (Sk.arr s1 s) (And.left hs1)))
                       (fn [sb :- Sk, hsb :- (And (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb))
                                                  (And (SkJ Bool.true G A Sk.unit)
                                                       (SkJ Bool.false (List.cons Sk (skel A) G) t sb)))]
                         (den_beta_core chkf dec encTy n r A t u G s s1 sb
                           (And.left hsb) (And.right (And.right hsb)) (And.right hs1) hn en)))))))

;; --- the failure of β over SkJ alone -----------------------------------------
;;
;; redex     = (λ(y : Π(l:Lbl).Bool). caseL Bool 0 y) (bcons tt bnil)
;; contractum = caseL Bool 0 (bcons tt bnil)
;;             = subst1 (bcons tt bnil) (caseL Bool 0 (var 0))
;;
;; Both the redex and the step are well-formed (SkJ at Bool, Hd.beta), but
;; nbr of the redex is false: the branch list stands in argument position.
;; ⟦redex⟧ at Bool is ff (den_app defaults when skOf of the argument is none)
;; and ⟦contractum⟧ is tt (caseL computes).  This is why den_beta assumes nbr.
;; Wrapping either side in T makes V differ (V(T tt) is inhabited, V(T ff)
;; is empty), which is why a plain Cv chain does not preserve V.

(thm cex_red_eq []
  (Eq Exp (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
          (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil)))
  (rfl))

(thm cex_den_redex [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (Eq Bool (den chkf dec encTy 0
                (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                         (Exp.bcons Exp.tt Exp.bnil))
                (List.nil Sk) Sk.bool Unit.unit)
          Bool.false)
  (rfl))

(thm cex_den_contr [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (Eq Bool (den chkf dec encTy 0
                (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.bcons Exp.tt Exp.bnil))
                (List.nil Sk) Sk.bool Unit.unit)
          Bool.true)
  (rfl))

(thm cex_nbr_ff []
  (Eq Bool (nbr (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                         (Exp.bcons Exp.tt Exp.bnil)))
          Bool.false)
  (rfl))

(thm cex_redex_typed []
  (SkJ Bool.false (List.nil Sk)
       (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                (Exp.bcons Exp.tt Exp.bnil))
       Sk.bool)
  (exact (SkJ.sApp (List.nil Sk)
           (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
           (Exp.bcons Exp.tt Exp.bnil)
           (Sk.arr Sk.lbl Sk.bool) Sk.bool
           (SkJ.sLam (List.nil Sk) U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool)
             (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)) Sk.bool
             (SkJ.wPi (List.nil Sk) U.uw Exp.tLbl Exp.tBool
               (SkJ.wLbl (List.nil Sk))
               (SkJ.wBool (List.cons Sk Sk.lbl (List.nil Sk))))
             (SkJ.sCaseL (List.cons Sk (Sk.arr Sk.lbl Sk.bool) (List.nil Sk))
               Exp.tBool (Exp.lbl 0) (Exp.var 0)
               (SkJ.wBool (List.cons Sk Sk.lbl (List.cons Sk (Sk.arr Sk.lbl Sk.bool) (List.nil Sk))))
               (SkJ.sLbl (List.cons Sk (Sk.arr Sk.lbl Sk.bool) (List.nil Sk)) 0)
               (SkJ.sVar (List.cons Sk (Sk.arr Sk.lbl Sk.bool) (List.nil Sk)) 0 (Sk.arr Sk.lbl Sk.bool)
                 (nthS.eq_2 (Sk.arr Sk.lbl Sk.bool) (List.nil Sk)))))
           (SkJ.sBcons (List.nil Sk) Exp.tt Exp.bnil Sk.bool
             (SkJ.sTT (List.nil Sk))
             (SkJ.sBnil (List.nil Sk) Sk.bool)))))

(thm cex_den_ne [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (Not (Eq Bool
           (den chkf dec encTy 0
                (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                         (Exp.bcons Exp.tt Exp.bnil))
                (List.nil Sk) Sk.bool Unit.unit)
           (den chkf dec encTy 0
                (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                (List.nil Sk) Sk.bool Unit.unit)))
  (intro heq)
  (exact (Bool.noConfusion
           (Eq.trans (Eq.symm (cex_den_redex chkf dec encTy))
             (Eq.trans heq
               (Eq.trans
                 (congrArg (fn [e :- Exp] (den chkf dec encTy 0 e (List.nil Sk) Sk.bool Unit.unit)) cex_red_eq)
                 (cex_den_contr chkf dec encTy)))))))

;; The counterexample, as one statement: a head β step, the redex
;; skeleton-typed at Bool, nbr false, and the two denotations unequal.
(thm conv_skj_counterexample [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (And (Hd chkf
           (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                    (Exp.bcons Exp.tt Exp.bnil))
           (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0))))
       (And (SkJ Bool.false (List.nil Sk)
                 (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                          (Exp.bcons Exp.tt Exp.bnil))
                 Sk.bool)
            (And (Eq Bool (nbr (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                                        (Exp.bcons Exp.tt Exp.bnil)))
                     Bool.false)
                 (Not (Eq Bool
                          (den chkf dec encTy 0
                               (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                                        (Exp.bcons Exp.tt Exp.bnil))
                               (List.nil Sk) Sk.bool Unit.unit)
                          (den chkf dec encTy 0
                               (subst1 (Exp.bcons Exp.tt Exp.bnil) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)))
                               (List.nil Sk) Sk.bool Unit.unit))))))
  (exact (And.intro (Hd.beta chkf U.uw (Exp.tPi U.uw Exp.tLbl Exp.tBool) (Exp.caseL Exp.tBool (Exp.lbl 0) (Exp.var 0)) (Exp.bcons Exp.tt Exp.bnil))
                    (And.intro cex_redex_typed (And.intro cex_nbr_ff (cex_den_ne chkf dec encTy))))))

;; --- β for let (Lemma 3.2), under nbr ---------------------------------------
;; Hd.betaLet sends letp C (pair S x y) t to substL [y, x] t.  skOf of a pair
;; is some (skel S), read off the type annotation, so the let's denotation
;; splits the pair rather than defaulting — provided the pair is not a branch
;; list, which nbr of the redex gives for x and y (den_substL2 needs their
;; skOf).  The environment order is the let's: (⟦y⟧, ⟦x⟧, η).

(thm prod_inj [a :- Sk, b :- Sk, c :- Sk, d :- Sk, h :- (Eq Sk (Sk.prod a b) (Sk.prod c d))]
  (And (Eq Sk a c) (Eq Sk b d))
  (cases h)
  (exact (And.intro rfl rfl)))

(thm exU [P :- (=> U Prop), Q :- Prop, hx :- (Exists P), f :- (forall [r U] (=> (P r) Q))]
  Q
  (cases hx)
  (exact (f w h)))

(thm exExp [P :- (=> Exp Prop), Q :- Prop, hx :- (Exists P), f :- (forall [e Exp] (=> (P e) Q))]
  Q
  (cases hx)
  (exact (f w h)))

(thm skof_pair [S :- Exp, x :- Exp, y :- Exp, G :- (List Sk)]
  (Eq (Option Sk) (skOf G (Exp.pair S x y)) (Option.some Sk (skel S)))
  (rfl))

;; ⟦pair S x y⟧ at a product skeleton is the pair of the components' denotations.
(thm den_pair_prod [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                    S :- Exp, x :- Exp, y :- Exp, G :- (List Sk), sa :- Sk, sb :- Sk, en :- (HEnv G)]
  (Eq (Prod (Car sa) (Car sb))
      (den chkf dec encTy n (Exp.pair S x y) G (Sk.prod sa sb) en)
      (Prod.mk (den chkf dec encTy n x G sa en) (den chkf dec encTy n y G sb en)))
  (rw [(den_pair_at chkf dec encTy n S x y G (Sk.prod sa sb) en)]))

(lcert.formal.base/thm den_betaLet_at [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, s2 :- Sk,
  rr :- U, AA :- Exp, BB :- Exp,
  hs :- (Eq Sk s (skel C)),
  hp :- (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2)),
  ht :- (SkJ Bool.false (sk2 s2 s1 G) t (skel C)),
  hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true),
  en :- (HEnv G),
  hBB :- (And (Eq Exp S (Exp.tSig rr AA BB))
           (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
             (And (SkJ Bool.true G S Sk.unit)
               (And (SkJ Bool.false G x (skel AA))
                 (SkJ Bool.false G y (skel BB))))))]
  (Eq (Car s)
      (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
      (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
  (have hS (Eq Exp S (Exp.tSig rr AA BB)) (And.left hBB))
  (have hsk (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB))) (And.left (And.right hBB)))
  (have hx (SkJ Bool.false G x (skel AA)) (And.left (And.right (And.right (And.right hBB)))))
  (have hy (SkJ Bool.false G y (skel BB)) (And.right (And.right (And.right (And.right hBB)))))
  (have hpi (And (Eq Sk s1 (skel AA)) (Eq Sk s2 (skel BB))) (prod_inj s1 s2 (skel AA) (skel BB) hsk))
  (have h1 (Eq Sk s1 (skel AA)) (And.left hpi))
  (have h2 (Eq Sk s2 (skel BB)) (And.right hpi))
  (have hrest (Eq Bool (Bool.and ((nbrF (Exp.pair S x y)) false) ((nbrF t) false)) Bool.true)
        (band_right ((nbrF C) false) (Bool.and ((nbrF (Exp.pair S x y)) false) ((nbrF t) false)) hn))
  (have hpn (Eq Bool ((nbrF (Exp.pair S x y)) false) Bool.true)
        (band_left ((nbrF (Exp.pair S x y)) false) ((nbrF t) false) hrest))
  (have hxy (Eq Bool (Bool.and ((nbrF x) false) ((nbrF y) false)) Bool.true)
        (band_right ((nbrF S) false) (Bool.and ((nbrF x) false) ((nbrF y) false)) hpn))
  (have hnx (Eq Bool ((nbrF x) false) Bool.true) (band_left ((nbrF x) false) ((nbrF y) false) hxy))
  (have hny (Eq Bool ((nbrF y) false) Bool.true) (band_right ((nbrF x) false) ((nbrF y) false) hxy))
  (have hkx (Eq (Option Sk) (skOf G x) (Option.some Sk (skel AA))) (skOf_ok G x (skel AA) hx hnx))
  (have hky (Eq (Option Sk) (skOf G y) (Option.some Sk (skel BB))) (skOf_ok G y (skel BB) hy hny))
  (subst h1)
  (subst h2)
  (subst hs)
  (have hkp (Eq (Option Sk) (skOf G (Exp.pair S x y)) (Option.some Sk (Sk.prod (skel AA) (skel BB))))
        (Eq.trans (skof_pair S x y G) (congrArg (fn [E :- Exp] (Option.some Sk (skel E))) hS)))
  (rw [(den_letp_some chkf dec encTy n C (Exp.pair S x y) t G (skel AA) (skel BB) (skel C) en hkp)])
  (rw [(den_pair_prod chkf dec encTy n S x y G (skel AA) (skel BB) en)])
  (exact (den_substL2 chkf dec encTy n G x (skel AA) y (skel BB) hx hkx hy hky Bool.false t (skel C) ht en)))

(lcert.formal.base/thm den_betaLet_core [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, s2 :- Sk,
  hs :- (Eq Sk s (skel C)),
  hp :- (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2)),
  ht :- (SkJ Bool.false (sk2 s2 s1 G) t (skel C)),
  hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true),
  en :- (HEnv G)]
  (Eq (Car s)
      (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
      (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
  (have hpair (And (Eq Bool Bool.false Bool.false)
                   (Exists (fn [rr :- U]
                     (Exists (fn [AA :- Exp]
                       (Exists (fn [BB :- Exp]
                         (And (Eq Exp S (Exp.tSig rr AA BB))
                           (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                             (And (SkJ Bool.true G S Sk.unit)
                               (And (SkJ Bool.false G x (skel AA))
                                 (SkJ Bool.false G y (skel BB)))))))))))))
        (inv_pair Bool.false G S x y (Sk.prod s1 s2) hp))
  (exact (exU (fn [rr :- U] (Exists (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                   (And (Eq Exp S (Exp.tSig rr AA BB))
                     (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                       (And (SkJ Bool.true G S Sk.unit)
                         (And (SkJ Bool.false G x (skel AA))
                           (SkJ Bool.false G y (skel BB)))))))))))
               (Eq (Car s)
                   (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
                   (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
               (And.right hpair)
               (fn [rr :- U, hrr :- (Exists (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                      (And (Eq Exp S (Exp.tSig rr AA BB))
                        (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                          (And (SkJ Bool.true G S Sk.unit)
                            (And (SkJ Bool.false G x (skel AA))
                              (SkJ Bool.false G y (skel BB))))))))))]
                 (exExp (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                          (And (Eq Exp S (Exp.tSig rr AA BB))
                            (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                              (And (SkJ Bool.true G S Sk.unit)
                                (And (SkJ Bool.false G x (skel AA))
                                  (SkJ Bool.false G y (skel BB)))))))))
                       (Eq (Car s)
                           (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
                           (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
                       hrr
                       (fn [AA :- Exp, hAA :- (Exists (fn [BB :- Exp]
                              (And (Eq Exp S (Exp.tSig rr AA BB))
                                (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                  (And (SkJ Bool.true G S Sk.unit)
                                    (And (SkJ Bool.false G x (skel AA))
                                      (SkJ Bool.false G y (skel BB))))))))]
                         (exExp (fn [BB :- Exp]
                                 (And (Eq Exp S (Exp.tSig rr AA BB))
                                   (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                     (And (SkJ Bool.true G S Sk.unit)
                                       (And (SkJ Bool.false G x (skel AA))
                                         (SkJ Bool.false G y (skel BB)))))))
                               (Eq (Car s)
                                   (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
                                   (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
                               hAA
                               (fn [BB :- Exp, hBB :- (And (Eq Exp S (Exp.tSig rr AA BB))
                                                      (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                                        (And (SkJ Bool.true G S Sk.unit)
                                                          (And (SkJ Bool.false G x (skel AA))
                                                            (SkJ Bool.false G y (skel BB))))))]
                                 (den_betaLet_at chkf dec encTy n C S x y t G s s1 s2 rr AA BB hs hp ht hn en hBB)))))))))
(lcert.formal.base/thm den_betaLet [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk,
  hj :- (SkJ Bool.false G (Exp.letp C (Exp.pair S x y) t) s),
  hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true),
  en :- (HEnv G)]
  (Eq (Car s)
      (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
      (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
  (have hlet (And (Eq Bool Bool.false Bool.false)
                  (And (Eq Sk s (skel C))
                    (Exists (fn [s1 :- Sk]
                      (Exists (fn [s2 :- Sk]
                        (And (SkJ Bool.true G C Sk.unit)
                          (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                            (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))))))))
        (inv_letp Bool.false G C (Exp.pair S x y) t s hj))
  (have hs (Eq Sk s (skel C)) (And.left (And.right hlet)))
  (exact (exSk (fn [s1 :- Sk] (Exists (fn [s2 :- Sk]
                   (And (SkJ Bool.true G C Sk.unit)
                     (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                       (SkJ Bool.false (sk2 s2 s1 G) t (skel C)))))))
               (Eq (Car s)
                   (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
                   (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
               (And.right (And.right hlet))
               (fn [s1 :- Sk, hs1 :- (Exists (fn [s2 :- Sk]
                      (And (SkJ Bool.true G C Sk.unit)
                        (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                          (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))))]
                 (exSk (fn [s2 :- Sk]
                         (And (SkJ Bool.true G C Sk.unit)
                           (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                             (SkJ Bool.false (sk2 s2 s1 G) t (skel C)))))
                       (Eq (Car s)
                           (den chkf dec encTy n (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) G s en)
                           (den chkf dec encTy n (Exp.letp C (Exp.pair S x y) t) G s en))
                       hs1
                       (fn [s2 :- Sk, hs2 :- (And (SkJ Bool.true G C Sk.unit)
                                             (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                                               (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))]
                         (den_betaLet_core chkf dec encTy n C S x y t G s s1 s2 hs
                           (And.left (And.right hs2)) (And.right (And.right hs2)) hn en)))))))

;; --- itR on a leaf (Lemma 3.2) ----------------------------------------------
;; Hd.itRL sends itR X g h (leaf x) to app g x.  The certificate denotes a
;; leaf code, so the iterator applies g; the application does the same once
;; skOf x = some Lbl (otherwise it defaults).  den_itRL_nbr gets that skOf
;; from skeleton typing of the redex together with nbr, via skOf_ok.

(lcert.formal.base/thm den_itRL [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  X :- Exp, g :- Exp, h :- Exp, x :- Exp, G :- (List Sk), s :- Sk, en :- (HEnv G),
  hk :- (Eq (Option Sk) (skOf G x) (Option.some Sk Sk.lbl))]
  (Eq (Car s) (den chkf dec encTy n (Exp.app g x) G s en) (den chkf dec encTy n (Exp.itR X g h (Exp.leaf x)) G s en))
  (rw [(den_app_some chkf dec encTy n g x G Sk.lbl s en hk)])
  (rw [(den_itR_at chkf dec encTy n X g h (Exp.leaf x) G s en)])
  (change (Eq (Car s) ((den chkf dec encTy n g G (Sk.arr Sk.lbl s) en) (den chkf dec encTy n x G Sk.lbl en))
              (Code.rec$1 (fn [_ :- Code] (Car s))
                (fn [l :- Nat] ((den chkf dec encTy n g G (Sk.arr Sk.lbl s) en) l))
                (fn [l :- Nat, a :- Code, b :- Code, ya :- (Car s), yb :- (Car s)]
                  ((den chkf dec encTy n h G (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr s (Sk.arr s s)))) en) Unit.unit l ya yb))
                (den chkf dec encTy n (Exp.leaf x) G Sk.cert en))))
  (rw [(den_leaf_at chkf dec encTy n x G Sk.cert en)]))

(lcert.formal.base/thm den_itRL_nbr [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  X :- Exp, g :- Exp, h :- Exp, x :- Exp, G :- (List Sk), s :- Sk,
  hj :- (SkJ Bool.false G (Exp.itR X g h (Exp.leaf x)) s),
  hn :- (Eq Bool (nbr (Exp.itR X g h (Exp.leaf x))) Bool.true),
  en :- (HEnv G)]
  (Eq (Car s) (den chkf dec encTy n (Exp.app g x) G s en) (den chkf dec encTy n (Exp.itR X g h (Exp.leaf x)) G s en))
  (have hit (And (Eq Bool Bool.false Bool.false)
                 (And (Eq Sk s (skel X))
                   (And (SkJ Bool.true G X Sk.unit)
                     (And (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))
                       (And (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
                         (SkJ Bool.false G (Exp.leaf x) Sk.cert))))))
        (inv_itR Bool.false G X g h (Exp.leaf x) s hj))
  (have hleaf (And (Eq Bool Bool.false Bool.false)
                   (And (Eq Sk Sk.cert Sk.cert)
                     (SkJ Bool.false G x Sk.lbl)))
        (inv_leaf Bool.false G x Sk.cert (And.right (And.right (And.right (And.right (And.right hit)))))))
  (have h1 (Eq Bool (Bool.and ((nbrF g) false) (Bool.and ((nbrF h) false) ((nbrF (Exp.leaf x)) false))) Bool.true)
        (band_right ((nbrF X) false)
                    (Bool.and ((nbrF g) false) (Bool.and ((nbrF h) false) ((nbrF (Exp.leaf x)) false))) hn))
  (have h2 (Eq Bool (Bool.and ((nbrF h) false) ((nbrF (Exp.leaf x)) false)) Bool.true)
        (band_right ((nbrF g) false) (Bool.and ((nbrF h) false) ((nbrF (Exp.leaf x)) false)) h1))
  (have h3 (Eq Bool ((nbrF (Exp.leaf x)) false) Bool.true)
        (band_right ((nbrF h) false) ((nbrF (Exp.leaf x)) false) h2))
  (have hk (Eq (Option Sk) (skOf G x) (Option.some Sk Sk.lbl))
        (skOf_ok G x Sk.lbl (And.right (And.right hleaf)) h3))
  (exact (den_itRL chkf dec encTy n X g h x G s en hk)))

;; --- T steps (Lemma 3.3's conversion, at a head step) -----------------------
;; V(T(tt)) = V(1) and V(T(ff)) = V(0).  Both types have skeleton Unit, so
;; EquivAt's type side is the V equation (hd_tTT, hd_tTF).  unfold V exposes
;; the clause; propext identifies (tt = tt) with True and (ff = tt) with False.

(lcert.formal.base/thm V_tTT [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  G :- (List Sk), en :- (HEnv G), k :- Nat, v :- (Car Sk.unit)]
  (Eq Prop (V chkf dec encTy n (Exp.tT Exp.tt) G en k Sk.unit v)
          (V chkf dec encTy n Exp.tUnit G en k Sk.unit v))
  (unfold V)
  (rw [(den_tt_bool chkf dec encTy n G en)])
  (exact (propext (Iff.intro (fn [_ :- (Eq Bool Bool.true Bool.true)] True.intro) (fn [_ :- True] (Eq.refl$1 Bool.true))))))

;; The same equation at skel(T(tt)), which is the skeleton EquivAt quantifies over.
(lcert.formal.base/thm V_tTT_at [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  G :- (List Sk), en :- (HEnv G), k :- Nat, v :- (Car (skel (Exp.tT Exp.tt)))]
  (Eq Prop (V chkf dec encTy n Exp.tUnit G en k (skel (Exp.tT Exp.tt)) v)
          (V chkf dec encTy n (Exp.tT Exp.tt) G en k (skel (Exp.tT Exp.tt)) v))
  (unfold V)
  (rw [(den_tt_bool chkf dec encTy n G en)])
  (exact (propext (Iff.intro (fn [_ :- True] (Eq.refl$1 Bool.true)) (fn [_ :- (Eq Bool Bool.true Bool.true)] True.intro)))))

(lcert.formal.base/thm V_tTF [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  G :- (List Sk), en :- (HEnv G), k :- Nat, v :- (Car Sk.unit)]
  (Eq Prop (V chkf dec encTy n (Exp.tT Exp.ff) G en k Sk.unit v)
          (V chkf dec encTy n Exp.tEmpty G en k Sk.unit v))
  (unfold V)
  (rw [(den_ff_bool chkf dec encTy n G en)])
  (exact (propext (Iff.intro (fn [h :- (Eq Bool Bool.false Bool.true)] (Bool.noConfusion h))
                             (fn [h :- False] (False.elim$0 h))))))

(lcert.formal.base/thm V_tTF_at [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  G :- (List Sk), en :- (HEnv G), k :- Nat, v :- (Car (skel (Exp.tT Exp.ff)))]
  (Eq Prop (V chkf dec encTy n Exp.tEmpty G en k (skel (Exp.tT Exp.ff)) v)
          (V chkf dec encTy n (Exp.tT Exp.ff) G en k (skel (Exp.tT Exp.ff)) v))
  (unfold V)
  (rw [(den_ff_bool chkf dec encTy n G en)])
  (exact (propext (Iff.intro (fn [h :- False] (False.elim$0 h))
                             (fn [h :- (Eq Bool Bool.false Bool.true)] (Bool.noConfusion h))))))
(lcert.formal.base/thm hd_tTT [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  w :- Bool, G :- (List Sk), s :- Sk, hj :- (SkJ w G (Exp.tT Exp.tt) s)]
  (EquivAt chkf dec encTy n w G s Exp.tUnit (Exp.tT Exp.tt))
  (exact (mk_eqv_ty chkf dec encTy n w G s Exp.tUnit (Exp.tT Exp.tt)
            (And.left (inv_tT w G Exp.tt s hj))
            (Eq.refl$1 Sk.unit)
            (fn [en :- (HEnv G), k :- Nat, v :- (Car (skel (Exp.tT Exp.tt)))]
              (V_tTT_at chkf dec encTy n G en k v)))))
(lcert.formal.base/thm hd_tTF [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
  w :- Bool, G :- (List Sk), s :- Sk, hj :- (SkJ w G (Exp.tT Exp.ff) s)]
  (EquivAt chkf dec encTy n w G s Exp.tEmpty (Exp.tT Exp.ff))
  (exact (mk_eqv_ty chkf dec encTy n w G s Exp.tEmpty (Exp.tT Exp.ff)
            (And.left (inv_tT w G Exp.ff s hj))
            (Eq.refl$1 Sk.unit)
            (fn [en :- (HEnv G), k :- Nat, v :- (Car (skel (Exp.tT Exp.ff)))]
              (V_tTF_at chkf dec encTy n G en k v)))))
