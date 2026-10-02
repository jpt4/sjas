(ns lcert.formal.uskel
  "Usage-skeleton congruence for the erasing evaluator (R4-metatheory.md §5,
  Theorem 4′, \"What E does not depend on\").

  Erel (erase.clj) is defined by recursion on a usage skeleton, and usk
  forgets the terms inside a type.  Two facts make that the paper's
  invariance:

  - Substitution.  usk_subst says a substitution whose values all have
    usage skeleton `base unit` leaves usk unchanged.  Every term has that
    skeleton precisely when it has ordinary skeleton Unit (usk_of_unit):
    the type formers usk reads are the type formers skel reads.  So
    Lemma 3.1's hypothesis is this one.  usk_subst1 / usk_substL are the
    single and list forms.  erel_subst1 transports an E-witness along the
    resulting equation: E(B[u/x]) and E(B) are the same relation.
    Substituting the type Nat for a variable changes usk (usk_subst_needs_unit),
    the same counterexample as skel_subst_needs_unit.

  - Conversion.  usk_hd (erase.clj) is the head step.  A step under a path
    is usk_step_path, modelled on skeletons.clj's step_skel_path: UChild
    is ChildOK for usk.  Inside T(·) the child is ignored.  At a Π or Σ
    component the child is a type, and usk of the written type depends on
    that child only through usk of the child — and, unlike skel, through
    the usage, which a step never writes (usk_pi_dom and the three
    siblings case on it).  usk_step is one Step; usk_cv_wf / usk_cv are a
    Cv chain.  The isTy hypothesis is necessary (usk_step_needs_wf, the
    ill-typed β of step_skel_needs_wf).  erel_cv transports E along the
    chain, which is the Conv case of the fundamental property.

  tBrs is not of shape isTy, so a conversion chain does not reach it.
  usk still defines it, as an arrow from labels, and substitution covers
  it by the same congruence as Π at usage 1."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.syntactic]
            [lcert.formal.skeletons]
            ;; a/defn compiles its body as Clojure, so usk must be a var here.
            ;; thm quotes its body and only needs the kernel constant.
            [lcert.formal.erase :refer [usk]]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private congruence @#'lcert.formal.syntactic/congruence)

(defn- under [cut depth] (if (zero? depth) cut (list '+ cut depth)))
(defn- ih [field] (symbol (str "ih_" field)))

;; --- substitution -----------------------------------------------------------

;; Lifting does not change a usage skeleton: it changes variable indices,
;; and usk does not read them.  The variable case is the only one that
;; does not reduce until the index comparison is split, as in skel_lift_var.
(thm usk_lift_var [i :- Nat, k :- Nat, cut :- Nat]
  (Eq USk (usk (lift k cut (Exp.var i))) (usk (Exp.var i)))
  (have hc (Decidable (Nat.lt i cut)) (Nat.decLt i cut))
  (cases hc)
  (exact (Eq.trans (congrArg usk (lift_var_above k cut i (Nat.le_of_not_lt h))) rfl))
  (exact (Eq.trans (congrArg usk (lift_var_below k cut i h)) rfl)))

;; The Exp fields usk actually reads are the domain and codomain of Π and
;; Σ, and the motive of a branch list.  Every other constructor is a
;; constant usage skeleton, so the lifted term agrees by rfl.
(defn- usk-lift-args [fields]
  (for [[f ty depth] fields :when (= ty 'Exp)]
    ['USk (list 'usk (list 'lift 'amt (under 'cut depth) f))
     (list 'usk f) (list (ih f) 'amt (under 'cut depth))]))

(a/prove-theorem 'usk_lift (lv '[e :- Exp])
  (lv '(forall [amt Nat] (forall [cut Nat]
         (Eq USk (usk (lift amt cut e)) (usk e)))))
  (lv (into ['(induction e)]
            (mapcat
             (fn [[ctor fields]]
               (cons '(intro amt cut)
                     (cond
                       (= ctor 'var) '[(exact (usk_lift_var i amt cut))]
                       (= ctor 'tPi)
                       (into ['(cases r)]
                             (for [k '[USk.arr0 USk.arrN USk.arrN]]
                               (list 'exact (congruence k (usk-lift-args fields)))))
                       (= ctor 'tSig)
                       (into ['(cases r)]
                             (for [k '[USk.prod0 USk.prodN USk.prodN]]
                               (list 'exact (congruence k (usk-lift-args fields)))))
                       (= ctor 'tBrs)
                       [(list 'exact
                              (congruence 'USk.arrN
                                (into [['USk '(USk.base Sk.lbl) '(USk.base Sk.lbl) nil]]
                                      (usk-lift-args fields))))]
                       :else '[(rfl)])))
             exp-fields))))

;; up shifts a substitution under one binder.  Variable 0 is a variable,
;; usage skeleton base unit; every later value is a lift of a value the
;; hypothesis already gives.
(thm up_usk [sg :- (=> Nat Exp),
             hs :- (forall [i Nat] (Eq USk (usk (sg i)) (USk.base Sk.unit))),
             i :- Nat]
  (Eq USk (usk (up sg i)) (USk.base Sk.unit))
  (cases i)
  (rfl)
  (exact (Eq.trans (usk_lift (sg n) 1 0) (hs n))))

(thm upn_usk [n :- Nat]
  (forall [sg (=> Nat Exp)]
    (=> (forall [i Nat] (Eq USk (usk (sg i)) (USk.base Sk.unit)))
        (forall [i Nat] (Eq USk (usk (upn n sg i)) (USk.base Sk.unit)))))
  (induction n)
  (intro sg hs)
  (exact hs)
  (intro sg hs i)
  (exact (up_usk (upn n sg) (ih_n sg hs) i)))

;; The type formers on which usk and skel disagree with Unit are the same
;; ones.  A term of skeleton Unit — Lemma 3.1's hypothesis — therefore has
;; usage skeleton base unit, and may be substituted into a type without
;; changing E.
(thm usk_of_unit [e :- Exp]
  (=> (Eq Sk (skel e) Sk.unit) (Eq USk (usk e) (USk.base Sk.unit)))
  (induction e)
  (all_goals (intro h))
  (all_goals (try rfl))
  (all_goals (cases h)))

(defn- usk-subst-args [fields]
  (for [[f ty depth] fields :when (= ty 'Exp)]
    (let [sub (if (zero? depth) 'sg (list 'upn depth 'sg))
          hyp (if (zero? depth) 'hs (list 'upn_usk depth 'sg 'hs))]
      ['USk (list 'usk (list 'subst sub f)) (list 'usk f) (list (ih f) sub hyp)])))

;; usk_subst — Theorem 4′, invariance of E under substitution, at the
;; skeleton E reads.  The variable case is the hypothesis.  Π, Σ and tBrs
;; are congruences (Π and Σ case on the usage, which substitution does not
;; change).  Every other constructor ignores its fields.
(a/prove-theorem 'usk_subst (lv '[e :- Exp])
  (lv '(forall [sg (=> Nat Exp)]
         (=> (forall [i Nat] (Eq USk (usk (sg i)) (USk.base Sk.unit)))
             (Eq USk (usk (subst sg e)) (usk e)))))
  (lv (into ['(induction e)]
            (mapcat
             (fn [[ctor fields]]
               (cons '(intro sg hs)
                     (cond
                       (= ctor 'var) '[(exact (hs i))]
                       (= ctor 'tPi)
                       (into ['(cases r)]
                             (for [k '[USk.arr0 USk.arrN USk.arrN]]
                               (list 'exact (congruence k (usk-subst-args fields)))))
                       (= ctor 'tSig)
                       (into ['(cases r)]
                             (for [k '[USk.prod0 USk.prodN USk.prodN]]
                               (list 'exact (congruence k (usk-subst-args fields)))))
                       (= ctor 'tBrs)
                       [(list 'exact
                              (congruence 'USk.arrN
                                (into [['USk '(USk.base Sk.lbl) '(USk.base Sk.lbl) nil]]
                                      (usk-subst-args fields))))]
                       :else '[(rfl)])))
             exp-fields))))

(thm usk_subst1 [u :- Exp, e :- Exp, hu :- (Eq USk (usk u) (USk.base Sk.unit))]
  (Eq USk (usk (subst1 u e)) (usk e))
  (exact (usk_subst e (fn [i :- Nat] (inst1 u i))
           (fn [i :- Nat]
             (Nat.rec (fn [j :- Nat] (Eq USk (usk (inst1 u j)) (USk.base Sk.unit)))
                      hu
                      (fn [j :- Nat, _ :- (Eq USk (usk (inst1 u j)) (USk.base Sk.unit))]
                        (Eq.refl$1 (USk.base Sk.unit)))
                      i)))))

(thm usk_substL [us :- (List Exp), e :- Exp,
                 hs :- (forall [i Nat] (Eq USk (usk (instL us i)) (USk.base Sk.unit)))]
  (Eq USk (usk (substL us e)) (usk e))
  (exact (usk_subst e (fn [i :- Nat] (instL us i)) hs)))

;; The hypothesis is necessary.  Nat is not usage-skeleton unit, and
;; substituting it for a variable takes the wildcard unit clause to Bool's
;; — the same redex as skel_subst_needs_unit, read by usk.
(thm usk_subst_needs_unit []
  (Not (forall [e Exp] (forall [sg (=> Nat Exp)] (Eq USk (usk (subst sg e)) (usk e)))))
  (intro h)
  (have hn (Eq USk (USk.base Sk.nat) (USk.base Sk.unit))
    (h (Exp.var 0) (fn [i :- Nat] (inst1 Exp.tNat i))))
  (cases hn))

;; A type context read by usk, innermost first, in the order skels uses.
;; usks forgets the usage bit; usk_ctx says that agrees with skel entrywise,
;; so a carrier environment for E is a carrier environment for ⟦·⟧.
(a/defn uskCtx [D :- (List Exp)] (List USk)
  (match D
    [nil (List.nil USk)]
    [(cons A rest) (List.cons USk (usk A) (uskCtx rest))]))

(thm uskCtx_nil []
  (Eq (List USk) (uskCtx (List.nil Exp)) (List.nil USk))
  (rfl))

(thm uskCtx_cons [A :- Exp, rest :- (List Exp)]
  (Eq (List USk) (uskCtx (List.cons Exp A rest))
      (List.cons USk (usk A) (uskCtx rest)))
  (rfl))

(thm usk_ctx [D :- (List Exp)]
  (Eq (List Sk) (usks (uskCtx D)) (skels D))
  (induction D)
  (rfl)
  (exact (Eq.trans
           (congrArg (fn [s :- Sk] (List.cons Sk s (usks (uskCtx tail)))) (usk_skel head))
           (congrArg (fn [g :- (List Sk)] (List.cons Sk (skel head) g)) ih_tail))))

;; The carrier environment E quantifies over, transported to the context
;; ⟦·⟧ quantifies over.  This is a function, not a theorem: HEnv lives in
;; Type, and a theorem's type must be a proposition.  Eq.mp along usk_ctx.
(kdef henv_of_usk
  (forall [D (List Exp)] (=> (HEnv (usks (uskCtx D))) (HEnv (skels D))))
  (fn [D :- (List Exp), eta :- (HEnv (usks (uskCtx D)))]
    (Eq.mp (congrArg HEnv (usk_ctx D)) eta)))

;; --- path congruence --------------------------------------------------------

;; Writing one component of a Π.  The usage is not a child, so the clause
;; (arr0 or arrN) is the same on both sides; only the written component's
;; usage skeleton changes.
(thm usk_pi_dom [r :- U, X :- Exp, Z :- Exp, Y :- Exp,
                 hx :- (Eq USk (usk X) (usk Z))]
  (Eq USk (usk (Exp.tPi r X Y)) (usk (Exp.tPi r Z Y)))
  (cases r)
  (exact (congrArg (fn [v :- USk] (USk.arr0 v (usk Y))) hx))
  (exact (congrArg (fn [v :- USk] (USk.arrN v (usk Y))) hx))
  (exact (congrArg (fn [v :- USk] (USk.arrN v (usk Y))) hx)))

(thm usk_pi_cod [r :- U, X :- Exp, Y :- Exp, Z :- Exp,
                 hy :- (Eq USk (usk Y) (usk Z))]
  (Eq USk (usk (Exp.tPi r X Y)) (usk (Exp.tPi r X Z)))
  (cases r)
  (exact (congrArg (fn [v :- USk] (USk.arr0 (usk X) v)) hy))
  (exact (congrArg (fn [v :- USk] (USk.arrN (usk X) v)) hy))
  (exact (congrArg (fn [v :- USk] (USk.arrN (usk X) v)) hy)))

(thm usk_sig_dom [r :- U, X :- Exp, Z :- Exp, Y :- Exp,
                  hx :- (Eq USk (usk X) (usk Z))]
  (Eq USk (usk (Exp.tSig r X Y)) (usk (Exp.tSig r Z Y)))
  (cases r)
  (exact (congrArg (fn [v :- USk] (USk.prod0 v (usk Y))) hx))
  (exact (congrArg (fn [v :- USk] (USk.prodN v (usk Y))) hx))
  (exact (congrArg (fn [v :- USk] (USk.prodN v (usk Y))) hx)))

(thm usk_sig_cod [r :- U, X :- Exp, Y :- Exp, Z :- Exp,
                  hy :- (Eq USk (usk Y) (usk Z))]
  (Eq USk (usk (Exp.tSig r X Y)) (usk (Exp.tSig r X Z)))
  (cases r)
  (exact (congrArg (fn [v :- USk] (USk.prod0 (usk X) v)) hy))
  (exact (congrArg (fn [v :- USk] (USk.prodN (usk X) v)) hy))
  (exact (congrArg (fn [v :- USk] (USk.prodN (usk X) v)) hy)))

;; UChild A i c: writing child i (currently c) of a type A either never
;; changes usk (the term inside T(·)), or c is of shape isTy and usk
;; depends on that child only through usk c (a Π or Σ component).
(kdef UChild (=> Exp Nat Exp Prop)
  (fn [A :- Exp, i :- Nat, c :- Exp]
    (Or (forall [x Exp] (Eq USk (usk (setKid A i x)) (usk A)))
        (And (Eq Bool (isTy c) Bool.true)
             (forall [x Exp]
               (=> (Eq USk (usk x) (usk c))
                   (Eq USk (usk (setKid A i x)) (usk A))))))))

;; Child 0 is the domain, child 1 the codomain.  some_inj names the child;
;; subst puts that name in the type.  cases on the index is not used here:
;; it leaves a context the elaborator cannot read back (skeletons.clj).
(thm uchild_tPi0 [r :- U, A :- Exp, B :- Exp, c :- Exp,
                  hA :- (Eq Bool (isTy (Exp.tPi r A B)) Bool.true),
                  hc :- (Eq (Option Exp) (child (Exp.tPi r A B) 0) (Option.some Exp c))]
  (UChild (Exp.tPi r A B) 0 c)
  (have heq (Eq Exp A c) (some_inj A c hc))
  (subst heq)
  (unfold UChild)
  (exact (Or.inr (And.intro (andb_left (isTy c) (isTy B) hA)
           (fn [x :- Exp, hx :- (Eq USk (usk x) (usk c))]
             (usk_pi_dom r x c B hx))))))

(thm uchild_tPi1 [r :- U, A :- Exp, B :- Exp, c :- Exp,
                  hA :- (Eq Bool (isTy (Exp.tPi r A B)) Bool.true),
                  hc :- (Eq (Option Exp) (child (Exp.tPi r A B) 1) (Option.some Exp c))]
  (UChild (Exp.tPi r A B) 1 c)
  (have heq (Eq Exp B c) (some_inj B c hc))
  (subst heq)
  (unfold UChild)
  (exact (Or.inr (And.intro (andb_right (isTy A) (isTy c) hA)
           (fn [x :- Exp, hx :- (Eq USk (usk x) (usk c))]
             (usk_pi_cod r A x c hx))))))

(thm uchild_tSig0 [r :- U, A :- Exp, B :- Exp, c :- Exp,
                   hA :- (Eq Bool (isTy (Exp.tSig r A B)) Bool.true),
                   hc :- (Eq (Option Exp) (child (Exp.tSig r A B) 0) (Option.some Exp c))]
  (UChild (Exp.tSig r A B) 0 c)
  (have heq (Eq Exp A c) (some_inj A c hc))
  (subst heq)
  (unfold UChild)
  (exact (Or.inr (And.intro (andb_left (isTy c) (isTy B) hA)
           (fn [x :- Exp, hx :- (Eq USk (usk x) (usk c))]
             (usk_sig_dom r x c B hx))))))

(thm uchild_tSig1 [r :- U, A :- Exp, B :- Exp, c :- Exp,
                   hA :- (Eq Bool (isTy (Exp.tSig r A B)) Bool.true),
                   hc :- (Eq (Option Exp) (child (Exp.tSig r A B) 1) (Option.some Exp c))]
  (UChild (Exp.tSig r A B) 1 c)
  (have heq (Eq Exp B c) (some_inj B c hc))
  (subst heq)
  (unfold UChild)
  (exact (Or.inr (And.intro (andb_right (isTy A) (isTy c) hA)
           (fn [x :- Exp, hx :- (Eq USk (usk x) (usk c))]
             (usk_sig_cod r A x c hx))))))

;; 0, then 1, then every larger index (no child: child returns none).
(defn- u-index-split [ctor]
  (let [T (list (symbol (str "Exp." ctor)) 'r 'A 'B)
        hyp (fn [j] (list 'Eq '(Option Exp) (list 'child T j) '(Option.some Exp c)))
        motive (fn [j] (list '=> (hyp j) (list 'UChild T j 'c)))
        lem (fn [k] (symbol (str "uchild_" ctor k)))]
    (list 'Nat.rec$0 (list 'fn '[j :- Nat] (motive 'j))
          (list 'fn ['h0 :- (hyp 0)] (list (lem 0) 'r 'A 'B 'c 'hA 'h0))
          (list 'fn ['j :- 'Nat, 'ihj :- (motive 'j)]
                (list 'Nat.rec$0 (list 'fn '[k :- Nat] (motive '(Nat.succ k)))
                      (list 'fn ['h1 :- (hyp 1)] (list (lem 1) 'r 'A 'B 'c 'hA 'h1))
                      (list 'fn ['k :- 'Nat, 'ihk :- (motive '(Nat.succ k)),
                                 'h2 :- (hyp '(Nat.succ (Nat.succ k)))]
                            (list 'False.rec$0
                                  (list 'fn '[_ :- False] (list 'UChild T '(Nat.succ (Nat.succ k)) 'c))
                                  '(none_ne_someE c h2)))
                      'j))
          'i)))

(doseq [ctor '[tPi tSig]]
  (let [T (list (symbol (str "Exp." ctor)) 'r 'A 'B)]
    (a/prove-theorem (symbol (str "uchild_" ctor))
      (lv ['r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'c :- 'Exp,
           'hA :- (list 'Eq 'Bool (list 'isTy T) 'Bool.true), 'i :- 'Nat])
      (lv (list '=> (list 'Eq '(Option Exp) (list 'child T 'i) '(Option.some Exp c))
                (list 'UChild T 'i 'c)))
      (lv [(list 'exact (u-index-split ctor))]))))

;; Every existing child of a type of shape isTy satisfies UChild.  Base
;; types have no child.  Term constructors and tBrs are not of shape isTy.
(a/prove-theorem 'uchild (lv '[A :- Exp])
  (lv '(forall [i Nat] (forall [c Exp]
         (=> (Eq Bool (isTy A) Bool.true)
             (Eq (Option Exp) (child A i) (Option.some Exp c))
             (UChild A i c)))))
  (lv (into ['(cases A)]
            (mapcat
             (fn [[ctor _]]
               (cons '(intro i c hA hc)
                     (cond
                       ('#{tEmpty tUnit tBool tNat tLbl tSyn tDia tR} ctor) '[(cases hc)]
                       (= ctor 'tT) '[(unfold UChild)
                                      (exact (Or.inl (fn [x :- Exp] (Eq.refl$1 (USk.base Sk.unit)))))]
                       (= ctor 'tPi) '[(exact (uchild_tPi r A B c hA i hc))]
                       (= ctor 'tSig) '[(exact (uchild_tSig r A B c hA i hc))]
                       :else '[(exact (Bool.noConfusion hA))])))
             exp-fields))))

;; A head redex at path p inside a type of shape isTy preserves usk.
;; The empty path is usk_hd.  A longer path descends into the child.
(thm usk_step_path [chkf :- (=> Code Code Bool), p :- (List Nat)]
  (forall [A Exp] (forall [r Exp] (forall [r2 Exp]
    (=> (Eq Bool (isTy A) Bool.true)
        (Eq (Option Exp) (getP p A) (Option.some Exp r))
        (Hd chkf r r2)
        (Eq USk (usk (setP p A r2)) (usk A))))))
  (induction p)
  (intro A r r2 hA hg hd)
  (have heq (Eq Exp A r) (some_inj A r hg))
  (subst heq)
  (exact (usk_hd chkf r r2 hd hA))
  (intro A r r2 hA hg hd)
  (exact (exists_elimE
           (fn [c :- Exp]
             (And (Eq (Option Exp) (child A head) (Option.some Exp c))
                  (And (Eq (Option Exp) (getP tail c) (Option.some Exp r))
                       (Eq Exp (setP (List.cons Nat head tail) A r2)
                           (setKid A head (setP tail c r2))))))
           (Eq USk (usk (setP (List.cons Nat head tail) A r2)) (usk A))
           (getP_cons head tail A r2 r hg)
           (fn [c :- Exp,
                hc :- (And (Eq (Option Exp) (child A head) (Option.some Exp c))
                           (And (Eq (Option Exp) (getP tail c) (Option.some Exp r))
                                (Eq Exp (setP (List.cons Nat head tail) A r2)
                                    (setKid A head (setP tail c r2)))))]
             (Or.elim (uchild A head c hA (And.left hc))
               (fn [h1 :- (forall [x Exp] (Eq USk (usk (setKid A head x)) (usk A)))]
                 (Eq.trans (congrArg usk (And.right (And.right hc))) (h1 (setP tail c r2))))
               (fn [h2 :- (And (Eq Bool (isTy c) Bool.true)
                               (forall [x Exp]
                                 (=> (Eq USk (usk x) (usk c))
                                     (Eq USk (usk (setKid A head x)) (usk A)))))]
                 (Eq.trans (congrArg usk (And.right (And.right hc)))
                           ((And.right h2) (setP tail c r2)
                            (ih_tail c r r2 (And.left h2) (And.left (And.right hc)) hd)))))))))

(thm usk_step [chkf :- (=> Code Code Bool), A :- Exp, B :- Exp,
               hA :- (Eq Bool (isTy A) Bool.true), hs :- (Step chkf A B)]
  (Eq USk (usk B) (usk A))
  (exact (exists_elimL
           (fn [p :- (List Nat)]
             (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
               (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                    (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))))))
           (Eq USk (usk B) (usk A))
           hs
           (fn [p :- (List Nat),
                hp :- (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
                        (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                             (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))))))]
             (exists_elimE
               (fn [r :- Exp]
                 (Exists (fn [r2 :- Exp]
                   (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                        (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))))
               (Eq USk (usk B) (usk A))
               hp
               (fn [r :- Exp,
                    hr :- (Exists (fn [r2 :- Exp]
                            (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                                 (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))))]
                 (exists_elimE
                   (fn [r2 :- Exp]
                     (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                          (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))
                   (Eq USk (usk B) (usk A))
                   hr
                   (fn [r2 :- Exp,
                        h3 :- (And (Eq (Option Exp) (getP p A) (Option.some Exp r))
                                   (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))]
                     (Eq.trans (congrArg usk (And.right (And.right h3)))
                               (usk_step_path chkf p A r r2 hA (And.left h3)
                                 (And.left (And.right h3))))))))))))

;; A conversion chain relates types of equal usage skeleton.  Each forward
;; or backward step needs isTy of its source, which the chain's
;; skeleton-well-formedness supplies (skj_isTy), as in cv_skel_wf.
(thm usk_cv_wf [chkf :- (=> Code Code Bool), G0 :- (List Sk), A0 :- Exp, B0 :- Exp,
                der :- (Cv chkf G0 A0 B0)]
  (And (Eq USk (usk A0) (usk B0)) (SkJ Bool.true G0 B0 Sk.unit))
  (induction der)
  (exact (And.intro (Eq.refl$1 (usk A)) h))
  (exact (And.intro
           (Eq.trans (And.left ih_hab)
                     (Eq.symm (usk_step chkf B C
                                (skj_isTy Bool.true G0 B Sk.unit (And.right ih_hab) (Eq.refl$1 Bool.true))
                                hs)))
           hc))
  (exact (And.intro
           (Eq.trans (And.left ih_hab)
                     (usk_step chkf C B
                       (skj_isTy Bool.true G0 C Sk.unit hc (Eq.refl$1 Bool.true))
                       hs))
           hc)))

(thm usk_cv [chkf :- (=> Code Code Bool), G :- (List Sk), A :- Exp, B :- Exp,
             h :- (Cv chkf G A B)]
  (Eq USk (usk A) (usk B))
  (exact (And.left (usk_cv_wf chkf G A B h))))

;; The same ill-typed β that changes skel changes usk: the wildcard unit
;; clause of an application against Bool.  usk_step's isTy hypothesis is
;; not dispensable.
(thm usk_step_needs_wf [chkf :- (=> Code Code Bool)]
  (Not (forall [A Exp] (forall [B Exp] (=> (Step chkf A B) (Eq USk (usk A) (usk B))))))
  (intro h)
  (have hn (Eq USk (USk.base Sk.unit) (USk.base Sk.bool))
    (h (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star) Exp.tBool (beta_step_example chkf)))
  (cases hn))

;; --- E itself ---------------------------------------------------------------

;; An equality of usage skeletons transports an E-witness.  The carrier
;; value is cast along uskSk, which is how ⟦·⟧'s carrier and E's carrier
;; meet when the skeletons agree only propositionally.  cases, not subst:
;; subst drops the equation before Eq.mp can reduce (dflt_cast).
(thm erel_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u1 :- USk, u2 :- USk, v :- RV, a :- (Car (uskSk u1)),
   hv :- (Erel chkf dec encTy n u1 v a),
   e :- (Eq USk u1 u2)]
  (Erel chkf dec encTy n u2 v (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) e) a))
  (cases e)
  (exact hv))

;; Theorem 4′, invariance of E under substitution.  Membership at e is
;; membership at e[u/x], at the carrier value cast along usk_subst1.
(thm erel_subst1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- Exp, e :- Exp, v :- RV, a :- (Car (uskSk (usk e))),
   hu :- (Eq USk (usk u) (USk.base Sk.unit)),
   hv :- (Erel chkf dec encTy n (usk e) v a)]
  (Erel chkf dec encTy n (usk (subst1 u e)) v
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm (usk_subst1 u e hu))) a))
  (exact (erel_at chkf dec encTy n (usk e) (usk (subst1 u e)) v a hv
           (Eq.symm (usk_subst1 u e hu)))))

;; The same, from Lemma 3.1's hypothesis (skeleton Unit) rather than from
;; a usage skeleton.  This is the form the App and Pair cases apply.
(thm erel_subst_unit
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, u :- Exp, e :- Exp, v :- RV, a :- (Car (uskSk (usk e))),
   hu :- (Eq Sk (skel u) Sk.unit),
   hv :- (Erel chkf dec encTy n (usk e) v a)]
  (Erel chkf dec encTy n (usk (subst1 u e)) v
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 u e (usk_of_unit u hu)))) a))
  (exact (erel_subst1 chkf dec encTy n u e v a (usk_of_unit u hu) hv)))

;; Theorem 4′, invariance of E under ≡.  A conversion chain equates the
;; usage skeletons (usk_cv); E transports.
(thm erel_cv
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, G :- (List Sk), A :- Exp, B :- Exp, v :- RV,
   a :- (Car (uskSk (usk A))),
   hcv :- (Cv chkf G A B),
   hv :- (Erel chkf dec encTy n (usk A) v a)]
  (Erel chkf dec encTy n (usk B) v
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (usk_cv chkf G A B hcv)) a))
  (exact (erel_at chkf dec encTy n (usk A) (usk B) v a hv (usk_cv chkf G A B hcv))))
