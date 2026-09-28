(ns lcert.formal.substitution
  "F3h — weakening and substitution for the denotation
  (R4-metatheory.md §3.2 Lemma 3.1, §3.3 Lemma 3.3 substitution clause).

  Every statement is about the denotation of lcert.formal.den, for arbitrary
  parameters chkf (the checker), dec (the decoder of certificate trees) and
  encTy (the type encoding), at every budget n (den n := denT n n).  Most
  theorems are first proved for one level of the table, `denAt chkf dec
  encTy pv cp` with an arbitrary lower table pv : DenFn and cap cp, and then
  transferred to `den n` by `den_eq_denAt` (den n is denAt at its own lower
  table, prevOf n).  This is possible because renaming and substitution
  never reach the lower table: reflect runs a CLOSED decoded program in
  tokenEnv (den_gen.clj, den_refl), and reflect's type D is a base type,
  hence closed (SkJ's sRefl premise isBaseTy D).

  Contents.
  §1  Insertion into a skeleton context and an environment: insS c x G
      inserts skeleton x at position c of G; insE c x G η v inserts the
      value v at the same position of η.  Both are definitional on cons:
      insE (c+1) x (s :: G) (a, η) v ≡ (a, insE c x G η v).
  §2  Lookup (and nthS) in an extended context: below c unchanged, at or
      above c shifted by one (lookup_ins_below, lookup_ins_above,
      nthS_ins_below, nthS_ins_above).
  §3  Skeleton inference commutes with lifting (skOf_lift), and on a
      skeleton-typed term returns either none or the typed skeleton
      (skOf_typed).  Application and let read skOf of a subterm, so these
      two facts are what the app and letp cases need.
  §4  Item 1, weakening (the renaming half of Lemma 3.1): for any SkJ
      derivation G ⊢ u : s,
        ⟦lift 1 c u⟧ (insS c x G) (insE c x G η v) = ⟦u⟧ G η,
      `denAt_weaken` (one level) and `den_weaken` (every budget n).  One
      case lemma per SkJ rule (wk_<rule>), dispatched by induction over the
      derivation.
  §5  Lemma 3.1 as stated over skeleton typing is FALSE for this
      denotation: `lemma31_skj_counterexample` (kernel-checked).  A branch
      list (bnil / bcons) is skeleton-typed at Lbl → s, but skOf returns
      none on it, so the application clause of ⟦·⟧ defaults when a branch
      list is substituted for a variable in argument position:
        t = (λ(y : Π(l:Lbl).Nat). y) x   in x : Lbl → Nat,
        u = bcons (succ zero) bnil        (the branch list 0 ↦ 1),
        ⟦t[u/x]⟧ 0 = 0  but  ⟦t⟧(x ↦ ⟦u⟧) 0 = 1.
      The real typing judgment never places a branch list in argument
      position (its type is the pseudo-type tBrs, which no binder has), so
      the substitution lemma holds with the additional hypothesis that
      skOf of every substituted term is its skeleton (SubOK, §6).

  Proof technique.  Each case lemma is one explicit proof term: a chain of
  Eq.trans/congrArg steps (`cong`), one step per recursive call of the
  denotation clause, with funext under the clause's own binders.  The
  terms are generated from short templates (`expand`); the generator is not
  trusted, since every term is checked by the kernel against the goal,
  which the kernel unfolds (denAt, the den_<ctor> clause, liftF, insS,
  insE) by definitional reduction."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.subst :refer :all]
            [lcert.formal.skeletons :refer :all]))

;; The constructor table of lcert.formal.syntactic ([ctor [[field type
;; binder-depth] ...]], in Exp's constructor order), for inductions over Exp.
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)

;; SkJ's rules in constructor order: `induction` over a derivation leaves
;; one goal per rule, in this order (checked with peek).
(def ^:private skj-rules
  '[wEmpty wUnit wBool wNat wLbl wSyn wDia wR wT wPi wSig sVar sStar sAbort sTT sFF sIte sElimB sZero sSucc
    sRecN sLbl sCaseL sBnil sBcons sSleaf sSnode sRecS sLeaf sNode sItR sPrn sLam sApp sPair sLetp sChk sH1 sRefl sInsp])

;; ===========================================================================
;; §1  Insertion into skeleton contexts and environments
;; ===========================================================================

;; insS c x G: G with x inserted at position c (innermost first), i.e. the
;; context of `lift 1 c`.  Recursion on c, returning a function of G (so the
;; kernel sees a plain Nat.rec); past the end of G nothing is inserted, which
;; keeps lookups beyond the context equal (both are defaults / none).
(kdef insSF (=> Nat Sk (List Sk) (List Sk))
  (fn [c :- Nat, x :- Sk]
    (Nat.rec$1 (fn [_ :- Nat] (=> (List Sk) (List Sk)))
      (fn [G :- (List Sk)] (List.cons Sk x G))
      (fn [k :- Nat, ih :- (=> (List Sk) (List Sk))]
        (fn [G :- (List Sk)]
          (List.rec$1$0 Sk (fn [_ :- (List Sk)] (List Sk))
            (List.nil Sk)
            (fn [s :- Sk, rest :- (List Sk), _ :- (List Sk)] (List.cons Sk s (ih rest)))
            G)))
      c)))

(kdef insS (=> Nat Sk (List Sk) (List Sk))
  (fn [c :- Nat, x :- Sk, G :- (List Sk)] (insSF c x G)))

(thm insS_zero [x :- Sk, G :- (List Sk)] (= (insS 0 x G) (List.cons Sk x G)) (rfl))
(thm insS_succ_cons [c :- Nat, x :- Sk, s :- Sk, G :- (List Sk)]
  (= (insS (+ c 1) x (List.cons Sk s G)) (List.cons Sk s (insS c x G))) (rfl))
(thm insS_succ_nil [c :- Nat, x :- Sk] (= (insS (+ c 1) x (List.nil Sk)) (List.nil Sk)) (rfl))

;; insE c x G η v : HEnv (insS c x G): η with v inserted at position c.
;; Nat.rec on c, then List.rec on G, so that on a cons the equation
;; insE (c+1) x (s :: G) (a, η) v = (a, insE c x G η v) is definitional.
(kdef InsET (=> Nat Sk (Sort 1))
  (fn [c :- Nat, x :- Sk] (forall [G (List Sk)] (=> (HEnv G) (Car x) (HEnv (insS c x G))))))

(kdef insEF (forall [c Nat] (forall [x Sk] (InsET c x)))
  (fn [c :- Nat, x :- Sk]
    (Nat.rec$1 (fn [k :- Nat] (InsET k x))
      (fn [G :- (List Sk), en :- (HEnv G), v :- (Car x)] (Prod.mk v en))
      (fn [k :- Nat, ih :- (InsET k x)]
        (fn [G :- (List Sk)]
          (List.rec$1$0 Sk (fn [G2 :- (List Sk)] (=> (HEnv G2) (Car x) (HEnv (insS (Nat.succ k) x G2))))
            (fn [en :- Unit, v :- (Car x)] Unit.unit)
            (fn [s :- Sk, rest :- (List Sk), _ :- (=> (HEnv rest) (Car x) (HEnv (insS (Nat.succ k) x rest)))]
              (fn [en :- (HEnv (List.cons Sk s rest)), v :- (Car x)]
                (Prod.mk (Prod.fst en) (ih rest (Prod.snd en) v))))
            G)))
      c)))

(kdef insE (forall [c Nat] (forall [x Sk] (forall [G (List Sk)] (=> (HEnv G) (Car x) (HEnv (insS c x G))))))
  (fn [c :- Nat, x :- Sk, G :- (List Sk), en :- (HEnv G), v :- (Car x)] (insEF c x G en v)))

(thm insE_zero [x :- Sk, G :- (List Sk), en :- (HEnv G), v :- (Car x)]
  (= (insE 0 x G en v) (Prod.mk v en)) (rfl))
(thm insE_succ_cons [c :- Nat, x :- Sk, s :- Sk, G :- (List Sk), a0 :- (Car s), en :- (HEnv G), v :- (Car x)]
  (= (insE (+ c 1) x (List.cons Sk s G) (Prod.mk a0 en) v) (Prod.mk a0 (insE c x G en v))) (rfl))

;; ===========================================================================
;; §2  Lookup in an extended context
;; ===========================================================================

;; The core statements are by induction with the index written so that it
;; reduces against insS: above the insertion point, index j + c + 1 of the
;; extended context is index j + c of the original; below it, index i of a
;; context extended at d + i + 1 is unchanged.
(thm lookup_ins_above_core [x :- Sk, v :- (Car x), j :- Nat, c :- Nat]
  (forall [G (List Sk)] (forall [s Sk] (forall [en (HEnv G)]
    (= (lookup (insS c x G) (+ (+ j c) 1) s (insE c x G en v)) (lookup G (+ j c) s en)))))
  (induction c)
  (intro G s en)
  (rfl)
  (intro G)
  (cases G)
  (intro s en)
  (rfl)
  (intro s en)
  (exact (ih_n tail s (Prod.snd en))))

(thm lookup_ins_below_core [x :- Sk, v :- (Car x), d :- Nat, i :- Nat]
  (forall [G (List Sk)] (forall [s Sk] (forall [en (HEnv G)]
    (= (lookup (insS (+ (+ d i) 1) x G) i s (insE (+ (+ d i) 1) x G en v)) (lookup G i s en)))))
  (induction i)
  (intro G)
  (cases G)
  (intro s en)
  (rfl)
  (intro s en)
  (rfl)
  (intro G)
  (cases G)
  (intro s en)
  (rfl)
  (intro s en)
  (exact (ih_n tail s (Prod.snd en))))

;; The forms the variable case uses (lift_var_above / lift_var_below): the
;; index is rewritten into the core form with Nat.sub_add_cancel.
(thm lookup_ins_above [x :- Sk, v :- (Car x), c :- Nat, i :- Nat, G :- (List Sk), s :- Sk, en :- (HEnv G), h :- (LE.le c i)]
  (= (lookup (insS c x G) (+ i 1) s (insE c x G en v)) (lookup G i s en))
  (have hi (= (+ (- i c) c) i) (Nat.sub_add_cancel h))
  (have core (= (lookup (insS c x G) (+ (+ (- i c) c) 1) s (insE c x G en v)) (lookup G (+ (- i c) c) s en))
    (lookup_ins_above_core x v (- i c) c G s en))
  (rw [(Eq.symm hi)])
  (exact core))

(thm lookup_ins_below [x :- Sk, v :- (Car x), c :- Nat, i :- Nat, G :- (List Sk), s :- Sk, en :- (HEnv G), h :- (LT.lt i c)]
  (= (lookup (insS c x G) i s (insE c x G en v)) (lookup G i s en))
  (have hc (= (+ (+ (- c (+ i 1)) i) 1) c) (Nat.sub_add_cancel h))
  (rw [(Eq.symm hc)])
  (exact (lookup_ins_below_core x v (- c (+ i 1)) i G s en)))

;; The same for nthS.  nthS recurses on two arguments, so Ansatz defines it
;; by well-founded recursion; it is unfolded through its equation lemmas
;; nthS.eq_1 (nil), nthS.eq_2 (cons, 0), nthS.eq_3 (cons, i+1).
(thm nthS_ins_above_core [xs :- Sk, j :- Nat, c :- Nat]
  (forall [G (List Sk)] (= (nthS (insS c xs G) (+ (+ j c) 1)) (nthS G (+ j c))))
  (induction c)
  (intro G)
  (exact (nthS.eq_3 xs G (+ j 0)))
  (intro G)
  (cases G)
  (exact (Eq.trans (nthS.eq_1 (+ (+ j (+ n 1)) 1)) (Eq.symm (nthS.eq_1 (+ j (+ n 1))))))
  (exact (Eq.trans (nthS.eq_3 head (insS n xs tail) (+ (+ j n) 1))
           (Eq.trans (ih_n tail) (Eq.symm (nthS.eq_3 head tail (+ j n)))))))

(thm nthS_ins_below_core [xs :- Sk, d :- Nat, i :- Nat]
  (forall [G (List Sk)] (= (nthS (insS (+ (+ d i) 1) xs G) i) (nthS G i)))
  (induction i)
  (intro G)
  (cases G)
  (rfl)
  (exact (Eq.trans (nthS.eq_2 head (insS (+ d 0) xs tail)) (Eq.symm (nthS.eq_2 head tail))))
  (intro G)
  (cases G)
  (rfl)
  (exact (Eq.trans (nthS.eq_3 head (insS (+ (+ d n) 1) xs tail) n)
           (Eq.trans (ih_n tail) (Eq.symm (nthS.eq_3 head tail n))))))

(thm nthS_ins_above [xs :- Sk, c :- Nat, i :- Nat, G :- (List Sk), h :- (LE.le c i)]
  (= (nthS (insS c xs G) (+ i 1)) (nthS G i))
  (have hi (= (+ (- i c) c) i) (Nat.sub_add_cancel h))
  (have core (= (nthS (insS c xs G) (+ (+ (- i c) c) 1)) (nthS G (+ (- i c) c)))
    (nthS_ins_above_core xs (- i c) c G))
  (rw [(Eq.symm hi)])
  (exact core))

(thm nthS_ins_below [xs :- Sk, c :- Nat, i :- Nat, G :- (List Sk), h :- (LT.lt i c)]
  (= (nthS (insS c xs G) i) (nthS G i))
  (have hc (= (+ (+ (- c (+ i 1)) i) 1) c) (Nat.sub_add_cancel h))
  (rw [(Eq.symm hc)])
  (exact (nthS_ins_below_core xs (- c (+ i 1)) i G)))

;; ===========================================================================
;; §3  Skeleton inference: lifting, and typed terms
;; ===========================================================================

;; skOf's λ and application clauses, as named functions of the recursive
;; result (both sides of each equation are the same match, so rfl).
(a/defn lamSk [a0 :- Sk, o :- (Option Sk)] (Option Sk)
  (match o [none (Option.none Sk)] [(some b) (Option.some Sk (Sk.arr a0 b))]))
(a/defn appSk [o :- (Option Sk)] (Option Sk)
  (match o [none (Option.none Sk)] [(some sf) (arrCod sf)]))

(thm skOf_lam_eq [G :- (List Sk), r :- U, A :- Exp, t :- Exp]
  (= (skOf G (Exp.lam r A t)) (lamSk (skel A) (skOf (List.cons Sk (skel A) G) t)))
  (rfl))
(thm skOf_app_eq [G :- (List Sk), f :- Exp, u :- Exp]
  (= (skOf G (Exp.app f u)) (appSk (skOf G f)))
  (rfl))

(thm skOf_lift_var [xs :- Sk, cu :- Nat, G :- (List Sk), i :- Nat]
  (= (skOf (insS cu xs G) (lift 1 cu (Exp.var i))) (skOf G (Exp.var i)))
  (have hc (Decidable (Nat.lt i cu)) (Nat.decLt i cu))
  (cases hc)
  ;; (rw parks the rewritten goal last: rewrite both, then close in order)
  (rw [(lift_var_above 1 cu i (Nat.le_of_not_lt h))])
  (rw [(lift_var_below 1 cu i h)])
  (exact (nthS_ins_above xs cu i G (Nat.le_of_not_lt h)))
  (exact (nthS_ins_below xs cu i G h)))

;; Constructors whose skOf is `some (skel F)` for the field F, lifted under
;; k binders; skel commutes with lifting (skel_lift, lcert.formal.syntactic).
(def ^:private skel-field '{abort [A 0] elimB [P 1] recN [P 1] caseL [P 1] recS [P 1] itR [X 0]
                            pair [S 0] letp [C 0] refl [D 0] insp [X 0]})

(defn- skof-lift-case [ctor]
  (cond
    (= ctor 'var) '[(exact (skOf_lift_var xs cu G i))]
    (= ctor 'ite) '[(exact (ih_t cu xs G))]
    (= ctor 'lam)
    '[(exact (Eq.trans (skOf_lam_eq (insS cu xs G) r (lift 1 cu A) (lift 1 (+ cu 1) t))
               (Eq.trans (congrArg (fn [q :- Sk] (lamSk q (skOf (List.cons Sk q (insS cu xs G)) (lift 1 (+ cu 1) t)))) (skel_lift A 1 cu))
                 (Eq.trans (congrArg (fn [o :- (Option Sk)] (lamSk (skel A) o)) (ih_t (+ cu 1) xs (List.cons Sk (skel A) G)))
                   (Eq.symm (skOf_lam_eq G r A t))))))]
    (= ctor 'app)
    '[(exact (Eq.trans (skOf_app_eq (insS cu xs G) (lift 1 cu f) (lift 1 cu u))
               (Eq.trans (congrArg appSk (ih_f cu xs G)) (Eq.symm (skOf_app_eq G f u)))))]
    (skel-field ctor)
    (let [[F k] (skel-field ctor)]
      [(list 'exact (list 'congrArg '(fn [q :- Sk] (Option.some Sk q))
                          (list 'skel_lift F 1 (if (zero? k) 'cu (list '+ 'cu k)))))])
    ;; every other clause is a constant
    :else '[(rfl)]))

;; skOf commutes with weakening, for every expression (no typing needed).
(a/prove-theorem 'skOf_lift '[e :- Exp]
  '(forall [cu Nat] (forall [xs Sk] (forall [G (List Sk)] (= (skOf (insS cu xs G) (lift 1 cu e)) (skOf G e)))))
  (into ['(induction e)]
        (mapcat (fn [[ctor _]] (cons '(intro cu xs G) (skof-lift-case ctor))) exp-fields)))

(thm skOK_lam [G :- (List Sk), r :- U, A :- Exp, t :- Exp, s :- Sk,
               h :- (Or (= (skOf (List.cons Sk (skel A) G) t) (Option.none Sk)) (= (skOf (List.cons Sk (skel A) G) t) (Option.some Sk s)))]
  (Or (= (skOf G (Exp.lam r A t)) (Option.none Sk)) (= (skOf G (Exp.lam r A t)) (Option.some Sk (Sk.arr (skel A) s))))
  (cases h)
  (exact (Or.inl (Eq.trans (skOf_lam_eq G r A t) (congrArg (lamSk (skel A)) h))))
  (exact (Or.inr (Eq.trans (skOf_lam_eq G r A t) (congrArg (lamSk (skel A)) h)))))

(thm skOK_app [G :- (List Sk), f :- Exp, u :- Exp, s :- Sk, t :- Sk,
               h :- (Or (= (skOf G f) (Option.none Sk)) (= (skOf G f) (Option.some Sk (Sk.arr s t))))]
  (Or (= (skOf G (Exp.app f u)) (Option.none Sk)) (= (skOf G (Exp.app f u)) (Option.some Sk t)))
  (cases h)
  (exact (Or.inl (Eq.trans (skOf_app_eq G f u) (congrArg appSk h))))
  (exact (Or.inr (Eq.trans (skOf_app_eq G f u) (congrArg appSk h)))))

;; On a skeleton-typed expression skOf never returns a WRONG skeleton: it is
;; none (types, branch lists, and terms whose skOf-spine ends in one) or the
;; typed skeleton.  So the app and letp clauses of ⟦·⟧ either default on
;; both sides of an equation or use the typed skeleton on both.
(defn- skok-case [rule]
  (case rule
    (wEmpty wUnit wBool wNat wLbl wSyn wDia wR wT wPi wSig sBnil sBcons) '[(apply Or.inl) (rfl)]
    sVar '[(exact (Or.inr h))]
    sIte '[(exact ih_ht)]
    sLam '[(exact (skOK_lam G r A t s ih_ht))]
    sApp '[(exact (skOK_app G f u s t ih_hf))]
    '[(apply Or.inr) (rfl)]))

(a/prove-theorem 'skOf_typed '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  '(Or (= (skOf G0 e0) (Option.none Sk)) (= (skOf G0 e0) (Option.some Sk s0)))
  (into ['(induction der)] (mapcat skok-case skj-rules)))

;; Congruence for the Option.rec of the app and letp clauses: equal
;; scrutinees that are none or some s, and equal branches at s.
(thm optrec_congr [B :- (Sort 1), d :- B, F1 :- (=> Sk B), F2 :- (=> Sk B), o1 :- (Option Sk), o2 :- (Option Sk), s :- Sk,
                   ho :- (= o1 o2), hok :- (Or (= o2 (Option.none Sk)) (= o2 (Option.some Sk s))), hF :- (= (F1 s) (F2 s))]
  (= (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] B) d F1 o1) (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] B) d F2 o2))
  (subst ho)
  (cases hok)
  (subst h)
  (rfl)
  (subst h)
  (exact hF))

;; reflect's type is a base type, which lifting (and substitution) fixes.
(def ^:private base-ctors '#{tEmpty tUnit tBool tNat tLbl tSyn tR})

(a/prove-theorem 'lift_base '[D :- Exp]
  '(=> (= (isBaseTy D) true) (forall [k Nat] (forall [c Nat] (= (lift k c D) D))))
  (into ['(cases D)]
        (mapcat (fn [[ctor _]] (if (base-ctors ctor) '[(intro hb k c) (rfl)] '[(intro hb) (cases hb)])) exp-fields)))

;; ===========================================================================
;; §4  Item 1: weakening (renaming by one insertion)
;; ===========================================================================

;; --- proof-term generation ---------------------------------------------------
;; Names used in every case lemma: the denotation parameters chkf dec encTy,
;; the lower table pv and cap cp; the cut cu, the inserted skeleton xs, the
;; environment en and the inserted value vx.  (Rule fields named c, x, s, h,
;; v exist, so the motive's own names avoid them.)

(def ^:private dparams
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), pv :- DenFn, cp :- Nat])

(defn- D [e G s en] (list 'denAt 'chkf 'dec 'encTy 'pv 'cp e G s en))
(defn- cut [k] (if (= k 0) 'cu (list '+ 'cu k)))

;; The motive of item 1 for G ⊢ e : s.
(defn- WK [G e s]
  (list 'forall '[cu Nat] (list 'forall '[xs Sk] (list 'forall ['en (list 'HEnv G)] (list 'forall '[vx (Car xs)]
    (list '= (D (list 'lift 1 'cu e) (list 'insS 'cu 'xs G) s (list 'insE 'cu 'xs G 'en 'vx)) (D e G s 'en)))))))

(defn- cong
  "A chain of congrArg steps rewriting, left to right, each hole of `tmpl`
  (the symbols q1, q2, …) from its left to its right value.  A hole is
  [type left right proof-of-left=right]."
  [tmpl holes]
  (let [holes (vec holes) n (count holes) lhs (mapv second holes) rhs (mapv #(nth % 2) holes)
        qs (mapv #(symbol (str "q" (inc %))) (range n))
        inst (fn [vals] (walk/postwalk-replace (zipmap qs vals) tmpl))
        steps (for [i (range n)]
                (let [[ty _ _ pf] (holes i)]
                  (list 'congrArg (list 'fn ['qq ':- ty] (inst (vec (concat (take i rhs) ['qq] (drop (inc i) lhs))))) pf)))]
    (reduce (fn [acc st] (list 'Eq.trans acc st)) steps)))

(defn- expand
  "Expand the abbreviations of the case terms:
    (%WK G e s)     the motive;           (%D e G s en)  denAt at one level;
    (%H f G s ih)   hole: subterm f in the same context and environment;
    (%HB [binders] f k G2 s en2 ih)
                    hole: subterm f under the clause's binders, at cut cu+k,
                    in context G2 with environment en2 (funext per binder);
    (%CONG tmpl hole…)  the congruence chain."
  [form]
  (walk/postwalk
   (fn [x]
     (if (seq? x)
       (case (first x)
         %WK (apply WK (rest x))
         %D (apply D (rest x))
         %H (let [[_ f G s ih] x]
              [(list 'Car s) (D (list 'lift 1 'cu f) (list 'insS 'cu 'xs G) s (list 'insE 'cu 'xs G 'en 'vx)) (D f G s 'en)
               (list ih 'cu 'xs 'en 'vx)])
         %HB (let [[_ binders f k G2 s en2 ih] x
                   vars (take-nth 3 binders)
                   types (take-nth 3 (drop 2 binders))
                   ty (concat ['=>] types [(list 'Car s)])
                   body-l (D (list 'lift 1 (cut k) f) (list 'insS (cut k) 'xs G2) s (list 'insE (cut k) 'xs G2 en2 'vx))
                   body-r (D f G2 s en2)
                   pf (reduce (fn [acc [v t]] (list 'funext (list 'fn [v ':- t] acc)))
                              (list ih (cut k) 'xs en2 'vx)
                              (reverse (map vector vars types)))]
               (if (empty? binders)
                 [(list 'Car s) body-l body-r pf]
                 [ty (list 'fn binders body-l) (list 'fn binders body-r) pf]))
         %CONG (cong (second x) (drop 2 x))
         x)
       x))
   form))

(defn- case-thm! [nm params prop tactics]
  (a/prove-theorem nm (lv (expand (into dparams params))) (lv (expand prop)) (lv (expand tactics))))

;; A case whose clause is `tmpl` over the recursive calls `holes`.
(defn- simple-case! [nm params concl-e concl-s tmpl holes]
  (case-thm! nm params (list '%WK 'G concl-e concl-s)
             ['(intro cu xs en vx) (list 'exact (list* '%CONG tmpl holes))]))

;; --- the cases, one per SkJ rule with recursive calls ---------------------------
;; (Rules whose clause is a constant or a default — types, ⋆, tt, ff, zero,
;; labels, bnil, abort, H₁ — are closed by rfl in the induction below.)

;; sVar: lookup at the lifted index; the stored skeleton is read by lookup on
;; both sides, so no typing is used.
(case-thm! 'wk_var '[G :- (List Sk), i :- Nat, s :- Sk]
  '(%WK G (Exp.var i) s)
  '[(intro cu xs en vx)
    (have hc (Decidable (Nat.lt i cu)) (Nat.decLt i cu))
    (cases hc)
    (rw [(lift_var_above 1 cu i (Nat.le_of_not_lt h))])
    (rw [(lift_var_below 1 cu i h)])
    (exact (lookup_ins_above xs vx cu i G s en (Nat.le_of_not_lt h)))
    (exact (lookup_ins_below xs vx cu i G s en h))])

;; sIte, sElimB: Bool.rec over the scrutinee (elimB's motive P is unused).
(simple-case! 'wk_ite '[G :- (List Sk), b :- Exp, t :- Exp, e :- Exp, s :- Sk,
                        ih_hb :- (%WK G b Sk.bool), ih_ht :- (%WK G t s), ih_he :- (%WK G e s)]
  '(Exp.ite b t e) 's
  '(Bool.rec$1 (fn [_ :- Bool] (Car s)) q1 q2 q3)
  '[(%H e G s ih_he) (%H t G s ih_ht) (%H b G Sk.bool ih_hb)])

(simple-case! 'wk_elimB '[G :- (List Sk), P :- Exp, b :- Exp, t :- Exp, e :- Exp,
                          ih_hb :- (%WK G b Sk.bool), ih_ht :- (%WK G t (skel P)), ih_he :- (%WK G e (skel P))]
  '(Exp.elimB P b t e) '(skel P)
  '(Bool.rec$1 (fn [_ :- Bool] (Car (skel P))) q1 q2 q3)
  '[(%H e G (skel P) ih_he) (%H t G (skel P) ih_ht) (%H b G Sk.bool ih_hb)])

(simple-case! 'wk_succ '[G :- (List Sk), n :- Exp, ih_h :- (%WK G n Sk.nat)]
  '(Exp.succ n) 'Sk.nat
  '(coe Sk.nat Sk.nat (Nat.succ q1)) '[(%H n G Sk.nat ih_h)])

;; sRecN: Nat.rec; the step is under two binders (y, x), at cut cu+2.
(simple-case! 'wk_recN '[G :- (List Sk), P :- Exp, z :- Exp, st :- Exp, n :- Exp,
                         ih_hz :- (%WK G z (skel P)), ih_hs :- (%WK (sk2 (skel P) Sk.nat G) st (skel P)), ih_hn :- (%WK G n Sk.nat)]
  '(Exp.recN P z st n) '(skel P)
  '(Nat.rec$1 (fn [_ :- Nat] (Car (skel P))) q1 q2 q3)
  '[(%H z G (skel P) ih_hz)
    (%HB [k :- Nat, acc :- (Car (skel P))] st 2 (sk2 (skel P) Sk.nat G) (skel P) (Prod.mk acc (Prod.mk k en)) ih_hs)
    (%H n G Sk.nat ih_hn)])

(simple-case! 'wk_caseL '[G :- (List Sk), P :- Exp, x :- Exp, bs :- Exp,
                          ih_hx :- (%WK G x Sk.lbl), ih_hb :- (%WK G bs (Sk.arr Sk.lbl (skel P)))]
  '(Exp.caseL P x bs) '(skel P)
  '(q1 q2) '[(%H bs G (Sk.arr Sk.lbl (skel P)) ih_hb) (%H x G Sk.lbl ih_hx)])

(simple-case! 'wk_bcons '[G :- (List Sk), h :- Exp, t :- Exp, s :- Sk,
                          ih_hh :- (%WK G h s), ih_ht :- (%WK G t (Sk.arr Sk.lbl s))]
  '(Exp.bcons h t) '(Sk.arr Sk.lbl s)
  '(fn [v :- Nat] (Nat.rec$1 (fn [_ :- Nat] (Car s)) q1 (fn [k :- Nat, w :- (Car s)] (q2 (coe Sk.lbl Sk.lbl k))) (coe Sk.lbl Sk.lbl v)))
  '[(%H h G s ih_hh) (%H t G (Sk.arr Sk.lbl s) ih_ht)])

(simple-case! 'wk_sleaf '[G :- (List Sk), x :- Exp, ih_h :- (%WK G x Sk.lbl)]
  '(Exp.sleaf x) 'Sk.syn
  '(coe Sk.syn Sk.syn (Code.sl q1)) '[(%H x G Sk.lbl ih_h)])

(simple-case! 'wk_snode '[G :- (List Sk), x :- Exp, c1 :- Exp, c2 :- Exp,
                          ih_hx :- (%WK G x Sk.lbl), ih_h1 :- (%WK G c1 Sk.syn), ih_h2 :- (%WK G c2 Sk.syn)]
  '(Exp.snode x c1 c2) 'Sk.syn
  '(coe Sk.syn Sk.syn (Code.sn q1 q2 q3)) '[(%H x G Sk.lbl ih_hx) (%H c1 G Sk.syn ih_h1) (%H c2 G Sk.syn ih_h2)])

;; sRecS: Code.rec; the leaf step under one binder (a), the node step under
;; five (a, c₁, c₂, y₁, y₂), at cuts cu+1 and cu+5.
(simple-case! 'wk_recS '[G :- (List Sk), P :- Exp, tl :- Exp, tn :- Exp, c :- Exp,
                         ih_hl :- (%WK (List.cons Sk Sk.lbl G) tl (skel P)),
                         ih_hn :- (%WK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn (skel P)),
                         ih_hc :- (%WK G c Sk.syn)]
  '(Exp.recS P tl tn c) '(skel P)
  '(Code.rec$1 (fn [_ :- Code] (Car (skel P))) q1 q2 q3)
  '[(%HB [l :- Nat] tl 1 (List.cons Sk Sk.lbl G) (skel P) (Prod.mk l en) ih_hl)
    (%HB [l :- Nat, a0 :- Code, b0 :- Code, ya :- (Car (skel P)), yb :- (Car (skel P))] tn 5
         (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) (skel P)
         (Prod.mk yb (Prod.mk ya (Prod.mk b0 (Prod.mk a0 (Prod.mk l en))))) ih_hn)
    (%H c G Sk.syn ih_hc)])

(simple-case! 'wk_leaf '[G :- (List Sk), x :- Exp, ih_h :- (%WK G x Sk.lbl)]
  '(Exp.leaf x) 'Sk.cert
  '(coe Sk.cert Sk.cert (Code.sl q1)) '[(%H x G Sk.lbl ih_h)])

(simple-case! 'wk_node '[G :- (List Sk), d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp,
                         ih_hx :- (%WK G x Sk.lbl), ih_h1 :- (%WK G r1 Sk.cert), ih_h2 :- (%WK G r2 Sk.cert)]
  '(Exp.node d x r1 r2) 'Sk.cert
  '(coe Sk.cert Sk.cert (Code.sn q1 q2 q3)) '[(%H x G Sk.lbl ih_hx) (%H r1 G Sk.cert ih_h1) (%H r2 G Sk.cert ih_h2)])

(simple-case! 'wk_itR '[G :- (List Sk), X :- Exp, g :- Exp, h :- Exp, r :- Exp,
                        ih_hg :- (%WK G g (Sk.arr Sk.lbl (skel X))),
                        ih_hh :- (%WK G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))),
                        ih_hr :- (%WK G r Sk.cert)]
  '(Exp.itR X g h r) '(skel X)
  '(Code.rec$1 (fn [_ :- Code] (Car (skel X))) (fn [l :- Nat] (q1 l))
      (fn [l :- Nat, a0 :- Code, b0 :- Code, ya :- (Car (skel X)), yb :- (Car (skel X))] (q2 Unit.unit l ya yb)) q3)
  '[(%H g G (Sk.arr Sk.lbl (skel X)) ih_hg)
    (%H h G (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))) ih_hh)
    (%H r G Sk.cert ih_hr)])

(simple-case! 'wk_prn '[G :- (List Sk), r :- Exp, ih_h :- (%WK G r Sk.cert)]
  '(Exp.prn r) 'Sk.syn
  '(coe Sk.syn Sk.syn q1) '[(%H r G Sk.cert ih_h)])

;; sLam: arrCase at the arrow skeleton reduces to the body under one binder.
(simple-case! 'wk_lam '[G :- (List Sk), r :- U, A :- Exp, t :- Exp, s :- Sk,
                        ih_ht :- (%WK (List.cons Sk (skel A) G) t s)]
  '(Exp.lam r A t) '(Sk.arr (skel A) s)
  'q1 '[(%HB [a0 :- (Car (skel A))] t 1 (List.cons Sk (skel A) G) s (Prod.mk a0 en) ih_ht)])

;; sApp: the clause is Option.rec over skOf of the argument; skOf_lift makes
;; the scrutinees equal, and skOf_typed leaves only none (default on both
;; sides) or the typed skeleton s (the two recursive calls).
(case-thm! 'wk_app '[G :- (List Sk), f :- Exp, u :- Exp, s :- Sk, t :- Sk,
                     ih_hf :- (%WK G f (Sk.arr s t)), ih_hu :- (%WK G u s), hu :- (SkJ Bool.false G u s)]
  '(%WK G (Exp.app f u) t)
  '[(intro cu xs en vx)
    (exact (optrec_congr (Car t) (dflt t)
             (fn [su :- Sk] ((%D (lift 1 cu f) (insS cu xs G) (Sk.arr su t) (insE cu xs G en vx))
                             (%D (lift 1 cu u) (insS cu xs G) su (insE cu xs G en vx))))
             (fn [su :- Sk] ((%D f G (Sk.arr su t) en) (%D u G su en)))
             (skOf (insS cu xs G) (lift 1 cu u)) (skOf G u) s
             (skOf_lift u cu xs G) (skOf_typed Bool.false G u s hu)
             (%CONG (q1 q2) (%H f G (Sk.arr s t) ih_hf) (%H u G s ih_hu))))])

(simple-case! 'wk_pair '[G :- (List Sk), r :- U, A :- Exp, B :- Exp, x :- Exp, y :- Exp,
                         ih_hx :- (%WK G x (skel A)), ih_hy :- (%WK G y (skel B))]
  '(Exp.pair (Exp.tSig r A B) x y) '(Sk.prod (skel A) (skel B))
  '(Prod.mk q1 q2) '[(%H x G (skel A) ih_hx) (%H y G (skel B) ih_hy)])

;; sLetp: as application, over skOf of the pair; at the product skeleton
;; splitProd reduces to the body under two binders (cut cu+2), whose
;; environment holds the pair's components: first the pair's value is
;; rewritten, then the body's induction hypothesis applies.
(case-thm! 'wk_letp '[G :- (List Sk), C :- Exp, p :- Exp, t :- Exp, s1 :- Sk, s2 :- Sk,
                      ih_hp :- (%WK G p (Sk.prod s1 s2)), ih_ht :- (%WK (sk2 s2 s1 G) t (skel C)),
                      hp :- (SkJ Bool.false G p (Sk.prod s1 s2))]
  '(%WK G (Exp.letp C p t) (skel C))
  '[(intro cu xs en vx)
    (exact (optrec_congr (Car (skel C)) (dflt (skel C))
             (fn [sp :- Sk] (splitProd (skel C) sp (%D (lift 1 cu p) (insS cu xs G) sp (insE cu xs G en vx))
                              (fn [a0 :- Sk, b0 :- Sk, va :- (Car a0), vb :- (Car b0)]
                                (%D (lift 1 (+ cu 2) t) (sk2 b0 a0 (insS cu xs G)) (skel C) (Prod.mk vb (Prod.mk va (insE cu xs G en vx)))))))
             (fn [sp :- Sk] (splitProd (skel C) sp (%D p G sp en)
                              (fn [a0 :- Sk, b0 :- Sk, va :- (Car a0), vb :- (Car b0)]
                                (%D t (sk2 b0 a0 G) (skel C) (Prod.mk vb (Prod.mk va en))))))
             (skOf (insS cu xs G) (lift 1 cu p)) (skOf G p) (Sk.prod s1 s2)
             (skOf_lift p cu xs G) (skOf_typed Bool.false G p (Sk.prod s1 s2) hp)
             (Eq.trans
               (%CONG (%D (lift 1 (+ cu 2) t) (insS (+ cu 2) xs (sk2 s2 s1 G)) (skel C)
                          (insE (+ cu 2) xs (sk2 s2 s1 G) (Prod.mk (Prod.snd q1) (Prod.mk (Prod.fst q1) en)) vx))
                      (%H p G (Sk.prod s1 s2) ih_hp))
               (ih_ht (+ cu 2) xs (Prod.mk (Prod.snd (%D p G (Sk.prod s1 s2) en)) (Prod.mk (Prod.fst (%D p G (Sk.prod s1 s2) en)) en)) vx))))])

(simple-case! 'wk_chk '[G :- (List Sk), c :- Exp, d :- Exp, ih_hc :- (%WK G c Sk.syn), ih_hd :- (%WK G d Sk.syn)]
  '(Exp.chk c d) 'Sk.bool
  '(coe Sk.bool Sk.bool (chkf q1 q2)) '[(%H c G Sk.syn ih_hc) (%H d G Sk.syn ih_hd)])

;; sRefl: the base type D is closed (lift_base), so the check against
;; encTy D and the decoded program's run at the lower table pv are the same
;; on both sides; only the certificate r's value is rewritten.
(def ^:private refl-tmpl
  '(Bool.rec$1 (fn [_ :- Bool] (Car (skel D))) (dflt (skel D))
     (Option.rec$1$0 (Prod Nat (Prod Exp Exp)) (fn [_ :- (Option (Prod Nat (Prod Exp Exp)))] (Car (skel D))) (dflt (skel D))
       (fn [tr :- (Prod Nat (Prod Exp Exp))]
         (coe (skel D) (skel D) (pv (Prod.fst tr) (Prod.fst (Prod.snd tr)) (thetaSk (Prod.fst tr)) (skel D) (tokenEnv (Prod.fst tr)))))
       (dec q1))
     (Bool.and (Nat.ble (cnodes q1) cp) (chkf q1 (encTy D)))))

(case-thm! 'wk_refl '[G :- (List Sk), D :- Exp, r :- Exp, e :- Exp, hb :- (= (isBaseTy D) true), ih_hr :- (%WK G r Sk.cert)]
  '(%WK G (Exp.refl D r e) (skel D))
  ['(intro cu xs en vx)
   '(have hD (= (lift 1 cu D) D) (lift_base D hb 1 cu))
   '(change (= (%D (Exp.refl (lift 1 cu D) (lift 1 cu r) (lift 1 cu e)) (insS cu xs G) (skel D) (insE cu xs G en vx))
               (%D (Exp.refl D r e) G (skel D) en)))
   '(rw [hD])
   (list 'exact (list '%CONG refl-tmpl '(%H r G Sk.cert ih_hr)))])

;; sInsp: the branches are under two binders (x ↦ ⟦r⟧, e ↦ ⋆): first the
;; branches' hypotheses at the lifted r's value, then r and c themselves.
(case-thm! 'wk_insp '[G :- (List Sk), X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp,
                      ih_hr :- (%WK G r Sk.cert), ih_hc :- (%WK G c Sk.syn),
                      ih_h1 :- (%WK (sk2 Sk.unit Sk.cert G) t1 (skel X)), ih_h2 :- (%WK (sk2 Sk.unit Sk.cert G) t2 (skel X))]
  '(%WK G (Exp.insp X r c t1 t2) (skel X))
  '[(intro cu xs en vx)
    (exact (Eq.trans
      (%CONG (Bool.rec$1 (fn [_ :- Bool] (Car (skel X))) q1 q2
               (chkf (%D (lift 1 cu r) (insS cu xs G) Sk.cert (insE cu xs G en vx)) (%D (lift 1 cu c) (insS cu xs G) Sk.syn (insE cu xs G en vx))))
        (%HB [] t2 2 (sk2 Sk.unit Sk.cert G) (skel X) (Prod.mk Unit.unit (Prod.mk (%D (lift 1 cu r) (insS cu xs G) Sk.cert (insE cu xs G en vx)) en)) ih_h2)
        (%HB [] t1 2 (sk2 Sk.unit Sk.cert G) (skel X) (Prod.mk Unit.unit (Prod.mk (%D (lift 1 cu r) (insS cu xs G) Sk.cert (insE cu xs G en vx)) en)) ih_h1))
      (%CONG (Bool.rec$1 (fn [_ :- Bool] (Car (skel X)))
               (%D t2 (sk2 Sk.unit Sk.cert G) (skel X) (Prod.mk Unit.unit (Prod.mk q1 en)))
               (%D t1 (sk2 Sk.unit Sk.cert G) (skel X) (Prod.mk Unit.unit (Prod.mk q1 en)))
               (chkf q1 q2))
        (%H r G Sk.cert ih_hr) (%H c G Sk.syn ih_hc))))])

;; --- the induction ----------------------------------------------------------------

(def ^:private wk-args
  '{sVar [wk_var G i s]
    sIte [wk_ite G b t e s ih_hb ih_ht ih_he]
    sElimB [wk_elimB G P b t e ih_hb ih_ht ih_he]
    sSucc [wk_succ G n ih_h]
    sRecN [wk_recN G P z st n ih_hz ih_hs ih_hn]
    sCaseL [wk_caseL G P x bs ih_hx ih_hb]
    sBcons [wk_bcons G h t s ih_hh ih_ht]
    sSleaf [wk_sleaf G x ih_h]
    sSnode [wk_snode G x c1 c2 ih_hx ih_h1 ih_h2]
    sRecS [wk_recS G P tl tn c ih_hl ih_hn ih_hc]
    sLeaf [wk_leaf G x ih_h]
    sNode [wk_node G d x r1 r2 ih_hx ih_h1 ih_h2]
    sItR [wk_itR G X g h r ih_hg ih_hh ih_hr]
    sPrn [wk_prn G r ih_h]
    sLam [wk_lam G r A t s ih_ht]
    sApp [wk_app G f u s t ih_hf ih_hu hu]
    sPair [wk_pair G r A B x y ih_hx ih_hy]
    sLetp [wk_letp G C p t s1 s2 ih_hp ih_ht hp]
    sChk [wk_chk G c d ih_hc ih_hd]
    sRefl [wk_refl G D r e hb ih_hr]
    sInsp [wk_insp G X r c t1 t2 ih_hr ih_hc ih_h1 ih_h2]})

(defn- wk-case [rule]
  (if-let [[lem & args] (wk-args rule)]
    [(list 'exact (list* lem 'chkf 'dec 'encTy 'pv 'cp args))]
    '[(intro cu xs en vx) (rfl)]))

;; Item 1 at one level of the table: for every derivation G ⊢ e : s (types
;; included; their denotation is a default), inserting a variable of any
;; skeleton at any position c leaves the denotation unchanged.
(a/prove-theorem 'denAt_weaken
  (lv (into dparams '[w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]))
  (lv (WK 'G0 'e0 's0))
  (into ['(induction der)] (mapcat wk-case skj-rules)))

;; den n is denAt at its own lower table: den0 at n = 0, denT k at n = k+1
;; (denT_top).
(kdef prevOf (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
               (=> Nat DenFn))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat]
    (Nat.rec$1 (fn [_ :- Nat] DenFn) den0 (fn [k :- Nat, _ :- DenFn] (denT chkf dec encTy k)) n)))

(thm den_eq_denAt [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, t :- Exp]
  (= (den chkf dec encTy n t) (denAt chkf dec encTy (prevOf chkf dec encTy n) n t))
  (cases n)
  (rfl)
  (exact (denT_top chkf dec encTy n t)))

;; Item 1 (weakening), at every budget n.
(thm den_weaken [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat,
                 w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (forall [cu Nat] (forall [xs Sk] (forall [en (HEnv G0)] (forall [vx (Car xs)]
    (= (den chkf dec encTy n (lift 1 cu e0) (insS cu xs G0) s0 (insE cu xs G0 en vx)) (den chkf dec encTy n e0 G0 s0 en))))))
  (intro cu xs en vx)
  (rw [(den_eq_denAt chkf dec encTy n (lift 1 cu e0)) (den_eq_denAt chkf dec encTy n e0)])
  (exact (denAt_weaken chkf dec encTy (prevOf chkf dec encTy n) n w0 G0 e0 s0 der cu xs en vx)))

;; ===========================================================================
;; §5  Lemma 3.1 over skeleton typing alone is false (kernel-checked)
;; ===========================================================================

;; t = (λ(y : Π(l:Lbl).Nat). y) x in the context x : Lbl → Nat, and the
;; branch list u = bcons (succ zero) bnil : Lbl → Nat (label 0 ↦ 1).
(thm cex31_t_typed []
  (SkJ Bool.false (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))
       (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0))
       (Sk.arr Sk.lbl Sk.nat))
  (exact (SkJ.sApp (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))
           (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0)
           (Sk.arr Sk.lbl Sk.nat) (Sk.arr Sk.lbl Sk.nat)
           (SkJ.sLam (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0) (Sk.arr Sk.lbl Sk.nat)
             (SkJ.wPi (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) U.uw Exp.tLbl Exp.tNat
               (SkJ.wLbl (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)))
               (SkJ.wNat (List.cons Sk Sk.lbl (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)))))
             (SkJ.sVar (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))) 0 (Sk.arr Sk.lbl Sk.nat)
               (nthS.eq_2 (Sk.arr Sk.lbl Sk.nat) (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)))))
           (SkJ.sVar (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) 0 (Sk.arr Sk.lbl Sk.nat)
             (nthS.eq_2 (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))))))

(thm cex31_u_typed []
  (SkJ Bool.false (List.nil Sk) (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) (Sk.arr Sk.lbl Sk.nat))
  (exact (SkJ.sBcons (List.nil Sk) (Exp.succ Exp.zero) Exp.bnil Sk.nat
           (SkJ.sSucc (List.nil Sk) Exp.zero (SkJ.sZero (List.nil Sk)))
           (SkJ.sBnil (List.nil Sk) Sk.nat))))

;; none ≠ some, for skeletons.
(thm none_ne_someS [r :- Sk, h :- (= (Option.none Sk) (Option.some Sk r))] False
  (cases h))

;; The substitution u/x is skeleton-typed (the only variable of the context
;; is sent to u, typed at its skeleton).
(thm cex31_sigma_typed []
  (forall [i Nat] (forall [s2 Sk]
    (=> (= (nthS (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) i) (Option.some Sk s2))
        (SkJ Bool.false (List.nil Sk) (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i) s2))))
  (intro i s2)
  (exact (Nat.rec$0
    (fn [j :- Nat] (=> (Eq (Option Sk) (nthS (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) j) (Option.some Sk s2))
                       (SkJ Bool.false (List.nil Sk) (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) j) s2)))
    (fn [h0 :- (Eq (Option Sk) (nthS (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) Nat.zero) (Option.some Sk s2))]
      (skj_cast Bool.false (List.nil Sk) (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) (Sk.arr Sk.lbl Sk.nat) s2 cex31_u_typed
        (Option.some.inj (Eq.trans (Eq.symm (nthS.eq_2 (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))) h0))))
    (fn [j :- Nat,
         _ :- (=> (Eq (Option Sk) (nthS (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) j) (Option.some Sk s2))
                  (SkJ Bool.false (List.nil Sk) (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) j) s2)),
         hj :- (Eq (Option Sk) (nthS (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) (Nat.succ j)) (Option.some Sk s2))]
      (False.rec$0 (fn [_ :- False] (SkJ Bool.false (List.nil Sk) (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) (Nat.succ j)) s2))
       (none_ne_someS s2
        (Eq.trans (Eq.symm (Eq.trans (nthS.eq_3 (Sk.arr Sk.lbl Sk.nat) (List.nil Sk) j) (nthS.eq_1 j))) hj))))
    i)))

;; The two sides at label 0, computed by the kernel (any checker and
;; decoder; budget 0).
(thm cex31_lhs [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (= (den chkf dec encTy 0
          (subst (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i))
                 (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0)))
          (List.nil Sk) (Sk.arr Sk.lbl Sk.nat) Unit.unit 0)
     0)
  (rfl))

(thm cex31_rhs [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
  (= (den chkf dec encTy 0
          (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0))
          (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) (Sk.arr Sk.lbl Sk.nat)
          (envOf chkf dec encTy 0 (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))
                 (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i)) (List.nil Sk) Unit.unit)
          0)
     1)
  (rfl))

;; Lemma 3.1 with only skeleton typing of t and of the substitution fails.
(thm lemma31_skj_counterexample []
  (Not (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [Gp (List Sk)] (forall [t Exp] (forall [s Sk] (forall [G (List Sk)] (forall [sg (=> Nat Exp)] (forall [eta (HEnv G)]
          (=> (SkJ Bool.false Gp t s)
              (forall [i Nat] (forall [s2 Sk] (=> (= (nthS Gp i) (Option.some Sk s2)) (SkJ Bool.false G (sg i) s2))))
              (= (den chkf dec encTy n (subst sg t) G s eta)
                 (den chkf dec encTy n t Gp s (envOf chkf dec encTy n Gp sg G eta)))))))))))))))
  (intro h)
  (have heq (= (den (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) 0
                    (subst (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i))
                           (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0)))
                    (List.nil Sk) (Sk.arr Sk.lbl Sk.nat) Unit.unit)
               (den (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) 0
                    (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0))
                    (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)) (Sk.arr Sk.lbl Sk.nat)
                    (envOf (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) 0
                           (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))
                           (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i)) (List.nil Sk) Unit.unit)))
    (h (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0)) 0
       (List.cons Sk (Sk.arr Sk.lbl Sk.nat) (List.nil Sk))
       (Exp.app (Exp.lam U.uw (Exp.tPi U.uw Exp.tLbl Exp.tNat) (Exp.var 0)) (Exp.var 0))
       (Sk.arr Sk.lbl Sk.nat) (List.nil Sk)
       (fn [i :- Nat] (inst1 (Exp.bcons (Exp.succ Exp.zero) Exp.bnil) i)) Unit.unit
       cex31_t_typed cex31_sigma_typed))
  (have h01 (= 0 1)
    (Eq.trans (Eq.symm (cex31_lhs (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0))))
      (Eq.trans (congrArg (fn [fv :- (Car (Sk.arr Sk.lbl Sk.nat))] (fv 0)) heq)
        (cex31_rhs (fn [a :- Code, b :- Code] Bool.true) (fn [c :- Code] (Option.none (Prod Nat (Prod Exp Exp)))) (fn [e :- Exp] (Code.sl 0))))))
  (exact (absurd (Eq.symm h01) (Nat.succ_ne_zero 0))))
