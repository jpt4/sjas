(ns lcert.formal.skeletons
  "F2e — Lemma 2.5 (skeletons) of R4-metatheory.md §2, and the facts about
  skeletons the model (§3) relies on (ADR-0006).

  The paper's Lemma 2.5 has two facts:
    (i)  if A ≡ B then skel(A) = skel(B);
    (ii) if Γ ⊢ t :σ A then t is simply typed at skel(A) in skel(Γ).
  Here they are, with the auxiliary facts their proofs need:

  1. Substitution (§A).  `skel_subst`: substituting a substitution whose
     every value has skeleton Unit changes no skeleton.  The hypothesis is
     necessary and is not a weakening of the paper: skel reads the outer
     type structure, and a variable there has skeleton Unit, so replacing it
     by a type changes the skeleton (`skel_subst_needs_unit`, kernel-checked:
     skel((x)[Nat/x]) = Nat ≠ Unit = skel(x)).  Every substitution the rules
     perform (subst1 u for a typed term u, stepTy, leafTy, nodeTy, y1Ty,
     y2Ty) substitutes terms, and a simply typed term has skeleton Unit
     (`skj_term_unit`), so the hypothesis always holds where it is used.

  2. Conversion (§B), Lemma 2.5 (i).  A step preserves the skeleton of a
     skeleton-well-formed type (`step_skel`), so a conversion chain — every
     element of which is skeleton-well-formed (§1.5, review R4-01) —
     preserves it (`cv_skel`).  The well-formedness hypothesis is necessary:
     a β-step at the head of the ill-formed type (λx:Nat. Bool) ⋆ changes its
     skeleton from Unit to Bool (`step_skel_needs_wf`, kernel-checked).  Cv
     supplies the hypothesis, so Lemma 2.5 (i) holds as stated for ≡.

  3. Lemma 2.5 (ii) (§C).  `lemma25_tl` (type level: formation and :⁰, one
     family) and `lemma25_rt` (runtime :¹), by induction on derivations; the
     Tl theorem is standalone and the Rt theorem uses it for type-level
     premises.  Every rule of the table is covered: no case is weakened.

  4. Skeleton inference (§D): pending.  Completeness is stated for terms
     whose skOf-spine (then-branch of if, body of λ, function of an
     application) does not end in a branch list, since a branch list has
     every skeleton Lbl → s and skOf returns none on it; that restriction is
     harmless, as no branch list is an application's argument.

  Written by a Claude subagent (2026-09-27), which stopped at a rate limit;
  integrated, with item 4 set aside, by the coordinating session.

  Proof technique.  Inductions over the 41 constructors of Exp are generated
  as explicit Eq.trans/congrArg terms by the table-driven generator of
  lcert.formal.syntactic (reused here, not duplicated).  Inductions over the
  derivation families use Ansatz's `induction` tactic, with one explicit
  proof term per rule, written out below so that each case can be read
  against the rule table (judgment.clj) and SkJ's rules (conv.clj).  Nothing
  here is trusted beyond the kernel: a wrong case fails kernel checking."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.syntactic]))

;; The constructor table and congruence builder of lcert.formal.syntactic
;; (private there; referenced, not copied, so there is one table).
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private congruence @#'lcert.formal.syntactic/congruence)
(def ^:private under @#'lcert.formal.syntactic/under)
(def ^:private ih @#'lcert.formal.syntactic/ih)

;; ===========================================================================
;; §A  Substitution preserves skeletons (item 1)
;; ===========================================================================

;; A substitution "of skeleton Unit": every value it substitutes has
;; skeleton Unit.  up (the shift under one binder) keeps the property: its
;; values are variable 0 and lifted values, and lifting preserves skeletons
;; (skel_lift, lcert.formal.syntactic).
(thm up_unit [sg :- (=> Nat Exp), hs :- (forall [i Nat] (= (skel (sg i)) Sk.unit)), i :- Nat]
  (= (skel (up sg i)) Sk.unit)
  (cases i)
  (rfl)
  (exact (Eq.trans (skel_lift (sg n) 1 0) (hs n))))

;; ... and so does upn n, the shift under n binders.
(thm upn_unit [n :- Nat]
  (forall [sg (=> Nat Exp)]
    (=> (forall [i Nat] (= (skel (sg i)) Sk.unit))
        (forall [i Nat] (= (skel (upn n sg i)) Sk.unit))))
  (induction n)
  (intro sg hs)
  (exact hs)
  (intro sg hs i)
  (exact (up_unit (upn n sg) (ih_n sg hs) i)))

;; skel_subst — Lemma 2.5's substitution fact.  By induction on e, the
;; substitution quantified so that it can be shifted under binders.  Only
;; the variable case uses the hypothesis; Π, Σ and the branch-list
;; pseudo-type tBrs are congruences; every other constructor's skeleton is
;; a constant.
(a/prove-theorem 'skel_subst '[e :- Exp]
  '(forall [sg (=> Nat Exp)]
     (=> (forall [i Nat] (= (skel (sg i)) Sk.unit))
         (= (skel (subst sg e)) (skel e))))
  (into ['(induction e)]
        (mapcat
         (fn [[ctor fields]]
           (cons '(intro sg hs)
                 (cond
                   (= ctor 'var) '[(exact (hs i))]
                   (#{'tPi 'tSig 'tBrs} ctor)
                   [(list 'exact
                          (congruence (if (= ctor 'tSig) 'Sk.prod 'Sk.arr)
                                      (into (if (= ctor 'tBrs) [['Sk 'Sk.lbl 'Sk.lbl nil]] [])
                                            (for [[f ty depth] fields :when (= ty 'Exp)]
                                              (let [sub (if (zero? depth) 'sg (list 'upn depth 'sg))
                                                    hyp (if (zero? depth) 'hs (list 'upn_unit depth 'sg 'hs))]
                                                ['Sk (list 'skel (list 'subst sub f)) (list 'skel f)
                                                 (list (ih f) sub hyp)])))))]
                   :else '[(rfl)])))
         exp-fields)))

;; The single substitution t[u/x] of β and B[u/x], for u of skeleton Unit.
(thm skel_subst1 [u :- Exp, e :- Exp, hu :- (= (skel u) Sk.unit)]
  (= (skel (subst1 u e)) (skel e))
  (exact (skel_subst e (fn [i :- Nat] (inst1 u i))
           (fn [i :- Nat] (Nat.rec (fn [j :- Nat] (Eq Sk (skel (inst1 u j)) Sk.unit))
                                   hu (fn [j :- Nat, _ :- (Eq Sk (skel (inst1 u j)) Sk.unit)] (Eq.refl$1 Sk.unit)) i)))))

;; The list substitution of β for let, recN and recSyn, for a list of
;; values of skeleton Unit (and the variables past its end, which stay
;; variables).
(thm skel_substL [us :- (List Exp), e :- Exp, hs :- (forall [i Nat] (= (skel (instL us i)) Sk.unit))]
  (= (skel (substL us e)) (skel e))
  (exact (skel_subst e (fn [i :- Nat] (instL us i)) hs)))

;; The hypothesis is necessary: substituting the type Nat for a variable
;; changes the skeleton Unit to Nat.  (Kernel-checked counterexample.)
(thm skel_subst_needs_unit []
  (Not (forall [e Exp] (forall [sg (=> Nat Exp)] (= (skel (subst sg e)) (skel e)))))
  (intro h)
  (have hn (= Sk.nat Sk.unit) (h (Exp.var 0) (fn [i :- Nat] (inst1 Exp.tNat i))))
  (cases hn))

;; ===========================================================================
;; §B  Conversion preserves skeletons: Lemma 2.5 (i) (item 2)
;; ===========================================================================

;; isTy A: A is built, along the spine skel reads, from the type formers that
;; SkJ Bool.true accepts: base types, ◇, T(b) (whose b skel does not read), Π
;; and Σ.  Every skeleton-well-formed type has this shape (skj_isTy); the
;; step lemma needs only the shape, not the typing of the terms inside T(·).
(a/defn isTy [A :- Exp] Bool
  (match A
    [tEmpty true] [tUnit true] [tBool true] [tNat true] [tLbl true] [tSyn true] [tDia true] [tR true]
    [(tT b) true]
    [(tPi r X Y) (Bool.and (isTy X) (isTy Y))]
    [(tSig r X Y) (Bool.and (isTy X) (isTy Y))]
    [_ false]))

(thm andb_intro [x :- Bool, y :- Bool, hx :- (= x true), hy :- (= y true)] (= (Bool.and x y) true)
  (subst hx) (subst hy) (rfl))
(thm andb_left [x :- Bool, y :- Bool, h :- (= (Bool.and x y) true)] (= x true)
  (cases x) (cases h) (rfl))
(thm andb_right [x :- Bool, y :- Bool, h :- (= (Bool.and x y) true)] (= y true)
  (cases y) (cases x) (cases h) (cases h) (rfl))

;; A skeleton-well-formed type has the shape isTy.  The motive is vacuous
;; (w = true is refuted) at the term rules.
(thm skj_isTy [w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (=> (= w0 true) (= (isTy e0) true))
  (induction der)
  (all_goals (intro hw))
  ;; (cases hw on true = true would succeed without closing, so it comes last.)
  (all_goals (first (rfl)
                    (exact (andb_intro (isTy A) (isTy B) (ih_hA (Eq.refl$1 Bool.true)) (ih_hB (Eq.refl$1 Bool.true))))
                    (cases hw))))

;; A head step never starts from a type of shape isTy, except T(tt) ⇝ 1 and
;; T(ff) ⇝ 0, and those relate types of skeleton Unit.  Every other head
;; redex is a term constructor, refuted by isTy.
(thm hd_skel [chkf :- (=> Code Code Bool), r :- Exp, r2 :- Exp, der :- (Hd chkf r r2)]
  (=> (= (isTy r) true) (= (skel r2) (skel r)))
  (induction der)
  (all_goals (intro hc))
  (all_goals (first (rfl) (cases hc))))

;; Reading a path i :: q: the child at i exists, the rest of the path reads
;; inside it, and writing at i :: q writes inside that child.  Stated first
;; for an arbitrary option (the child), then instantiated: getP and setP
;; unfold to Option.rec over `child e i`.
(thm none_ne_someE [r :- Exp, h :- (= (Option.none Exp) (Option.some Exp r))] False
  (cases h))

(thm getP_cons_gen [i :- Nat, q :- (List Nat), e :- Exp, y :- Exp, r :- Exp, o :- (Option Exp)]
  (=> (= (Option.rec$1$0 Exp (fn [_ :- (Option Exp)] (Option Exp)) (Option.none Exp) (fn [c :- Exp] (getP q c)) o)
         (Option.some Exp r))
      (Exists (fn [c :- Exp] (And (= o (Option.some Exp c))
        (And (= (getP q c) (Option.some Exp r))
             (= (Option.rec$1$0 Exp (fn [_ :- (Option Exp)] Exp) e (fn [c :- Exp] (setKid e i (setP q c y))) o)
                (setKid e i (setP q c y))))))))
  (cases o)
  (intro hg)
  (exact (False.elim$0 (none_ne_someE r hg)))
  (intro hg)
  (exact (Exists.intro val (And.intro (Eq.refl$1 (Option.some Exp val))
                              (And.intro hg (Eq.refl$1 (setKid e i (setP q val y))))))))

(thm getP_cons [i :- Nat, q :- (List Nat), e :- Exp, y :- Exp, r :- Exp,
                hg :- (= (getP (List.cons Nat i q) e) (Option.some Exp r))]
  (Exists (fn [c :- Exp] (And (= (child e i) (Option.some Exp c))
    (And (= (getP q c) (Option.some Exp r)) (= (setP (List.cons Nat i q) e y) (setKid e i (setP q c y)))))))
  (exact (getP_cons_gen i q e y r (child e i) hg)))

(thm some_inj [x :- Exp, y :- Exp, h :- (= (Option.some Exp x) (Option.some Exp y))] (= x y)
  (cases h) (rfl))

;; ChildOK A i c: writing child i (currently c) of A either never changes
;; A's skeleton (inside T(·)), or c is itself of shape isTy and the
;; skeleton depends on c only through skel c (Π and Σ components).
(kdef ChildOK (=> Exp Nat Exp Prop)
  (fn [A :- Exp, i :- Nat, c :- Exp]
    (Or (forall [x Exp] (Eq Sk (skel (setKid A i x)) (skel A)))
        (And (Eq Bool (isTy c) Bool.true)
             (forall [x Exp] (=> (Eq Sk (skel x) (skel c)) (Eq Sk (skel (setKid A i x)) (skel A))))))))

;; The Π and Σ cases at child 0 (the domain) and child 1 (the codomain).
;; some_inj then subst replaces the component by c.  (Ansatz detail:
;; `cases` on the equation, or on the index i, leaves a context the
;; elaborator cannot read back; the index split is done by Nat.rec below.)
(doseq [[ctor k] '[[tPi Sk.arr] [tSig Sk.prod]]]
  (let [T (list (symbol (str "Exp." ctor)) 'r 'A 'B)]
    (a/prove-theorem (symbol (str "childOK_" ctor "0"))
      (lv ['r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'c :- 'Exp, 'hA :- (list '= (list 'isTy T) 'true),
           'hc :- (list '= (list 'child T 0) '(Option.some Exp c))])
      (list 'ChildOK T 0 'c)
      (lv ['(have heq (= A c) (some_inj A c hc)) '(subst heq) '(unfold ChildOK)
           (list 'exact (list 'Or.inr (list 'And.intro '(andb_left (isTy c) (isTy B) hA)
                                           (list 'fn '[x :- Exp, hx :- (Eq Sk (skel x) (skel c))]
                                                 (list 'congrArg (list 'fn '[v :- Sk] (list k 'v '(skel B))) 'hx)))))]))
    (a/prove-theorem (symbol (str "childOK_" ctor "1"))
      (lv ['r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'c :- 'Exp, 'hA :- (list '= (list 'isTy T) 'true),
           'hc :- (list '= (list 'child T 1) '(Option.some Exp c))])
      (list 'ChildOK T 1 'c)
      (lv ['(have heq (= B c) (some_inj B c hc)) '(subst heq) '(unfold ChildOK)
           (list 'exact (list 'Or.inr (list 'And.intro '(andb_right (isTy A) (isTy c) hA)
                                           (list 'fn '[x :- Exp, hx :- (Eq Sk (skel x) (skel c))]
                                                 (list 'congrArg (list 'fn '[v :- Sk] (list k '(skel A) 'v)) 'hx)))))]))))

;; child_skel: ChildOK holds at every existing child of a type of shape
;; isTy.  By cases on A (one generated case per Exp constructor; term
;; constructors and tBrs are refuted by isTy, base types have no child) and,
;; for Π and Σ, on the index: 0, 1, or ≥ 2 (no child).
(defn- child-index-split [ctor]
  (let [T (list (symbol (str "Exp." ctor)) 'r 'A 'B)
        hyp (fn [j] (list 'Eq '(Option Exp) (list 'child T j) '(Option.some Exp c)))
        motive (fn [j] (list '=> (hyp j) (list 'ChildOK T j 'c)))]
    (list 'Nat.rec$0 (list 'fn '[j :- Nat] (motive 'j))
          (list 'fn ['h0 :- (hyp 0)] (list (symbol (str "childOK_" ctor "0")) 'r 'A 'B 'c 'hA 'h0))
          (list 'fn ['j :- 'Nat, 'ihj :- (motive 'j)]
                (list 'Nat.rec$0 (list 'fn '[k :- Nat] (motive '(Nat.succ k)))
                      (list 'fn ['h1 :- (hyp 1)] (list (symbol (str "childOK_" ctor "1")) 'r 'A 'B 'c 'hA 'h1))
                      (list 'fn ['k :- 'Nat, 'ihk :- (motive '(Nat.succ k)), 'h2 :- (hyp '(Nat.succ (Nat.succ k)))]
                            (list 'False.rec$0 (list 'fn '[_ :- False] (list 'ChildOK T '(Nat.succ (Nat.succ k)) 'c))
                                  '(none_ne_someE c h2)))
                      'j))
          'i)))

;; The index split as one theorem per constructor (Nat.rec is applied to
;; exactly its arity here; the equation hypothesis is the motive's premise).
(doseq [ctor '[tPi tSig]]
  (let [T (list (symbol (str "Exp." ctor)) 'r 'A 'B)]
    (a/prove-theorem (symbol (str "childOK_" ctor))
      (lv ['r :- 'U, 'A :- 'Exp, 'B :- 'Exp, 'c :- 'Exp, 'hA :- (list '= (list 'isTy T) 'true), 'i :- 'Nat])
      (list '=> (list '= (list 'child T 'i) '(Option.some Exp c)) (list 'ChildOK T 'i 'c))
      (lv [(list 'exact (child-index-split ctor))]))))

(a/prove-theorem 'child_skel '[A :- Exp]
  '(forall [i Nat] (forall [c Exp]
     (=> (= (isTy A) true) (= (child A i) (Option.some Exp c)) (ChildOK A i c))))
  (lv (into ['(cases A)]
        (mapcat
         (fn [[ctor _]]
           (cons '(intro i c hA hc)
                 (cond
                   ('#{tEmpty tUnit tBool tNat tLbl tSyn tDia tR} ctor) '[(cases hc)]
                   (= ctor 'tT) '[(unfold ChildOK) (exact (Or.inl (fn [x :- Exp] (Eq.refl$1 Sk.unit))))]
                   ('#{tPi tSig} ctor) [(list 'exact (list (symbol (str "childOK_" ctor)) 'r 'A 'B 'c 'hA 'i 'hc))]
                   :else '[(cases hA)])))
         exp-fields))))

;; Exists elimination into a Prop, for witnesses in Exp (Init has no
;; Exists.elim; the `cases` tactic on the hypothesis names them w and h).
(thm exists_elimE [P :- (=> Exp Prop), Q :- Prop, hx :- (Exists P), f :- (forall [c Exp] (=> (P c) Q))] Q
  (cases hx)
  (exact (f w h)))

;; Lemma 2.5 (i) at one position: rewriting a head redex at path p inside a
;; type of shape isTy preserves its skeleton.  By induction on the path.
(thm step_skel_path [chkf :- (=> Code Code Bool), p :- (List Nat)]
  (forall [A Exp] (forall [r Exp] (forall [r2 Exp]
    (=> (= (isTy A) true) (= (getP p A) (Option.some Exp r)) (Hd chkf r r2)
        (= (skel (setP p A r2)) (skel A))))))
  (induction p)
  ;; the empty path: the redex is A itself
  (intro A r r2 hA hg hd)
  (have heq (= A r) (some_inj A r hg))
  (subst heq)
  (exact (hd_skel chkf r r2 hd hA))
  ;; i :: q: descend into the child at i
  (intro A r r2 hA hg hd)
  (exact (exists_elimE
    (fn [c :- Exp] (And (Eq (Option Exp) (child A head) (Option.some Exp c))
                        (And (Eq (Option Exp) (getP tail c) (Option.some Exp r))
                             (Eq Exp (setP (List.cons Nat head tail) A r2) (setKid A head (setP tail c r2))))))
    (Eq Sk (skel (setP (List.cons Nat head tail) A r2)) (skel A))
    (getP_cons head tail A r2 r hg)
    (fn [c :- Exp,
         hc :- (And (Eq (Option Exp) (child A head) (Option.some Exp c))
                    (And (Eq (Option Exp) (getP tail c) (Option.some Exp r))
                         (Eq Exp (setP (List.cons Nat head tail) A r2) (setKid A head (setP tail c r2)))))]
      (Or.elim (child_skel A head c hA (And.left hc))  ; ChildOK unfolds to this Or
        (fn [h1 :- (forall [x Exp] (Eq Sk (skel (setKid A head x)) (skel A)))]
          (Eq.trans (congrArg skel (And.right (And.right hc))) (h1 (setP tail c r2))))
        (fn [h2 :- (And (Eq Bool (isTy c) Bool.true)
                        (forall [x Exp] (=> (Eq Sk (skel x) (skel c)) (Eq Sk (skel (setKid A head x)) (skel A)))))]
          (Eq.trans (congrArg skel (And.right (And.right hc)))
                    ((And.right h2) (setP tail c r2)
                     (ih_tail c r r2 (And.left h2) (And.left (And.right hc)) hd)))))))))

(thm exists_elimL [P :- (=> (List Nat) Prop), Q :- Prop, hx :- (Exists P), f :- (forall [p (List Nat)] (=> (P p) Q))] Q
  (cases hx)
  (exact (f w h)))

;; One step (a head step at some position, conv.clj's Step) from a type of
;; shape isTy preserves the skeleton.
(thm step_skel [chkf :- (=> Code Code Bool), A :- Exp, B :- Exp, hA :- (= (isTy A) true), hs :- (Step chkf A B)]
  (= (skel B) (skel A))
  (exact (exists_elimL
    (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
      (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))))))
    (Eq Sk (skel B) (skel A))
    hs
    (fn [p :- (List Nat),
         hp :- (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
                 (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))))))]
      (exists_elimE
        (fn [r :- Exp] (Exists (fn [r2 :- Exp]
          (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))))
        (Eq Sk (skel B) (skel A))
        hp
        (fn [r :- Exp,
             hr :- (Exists (fn [r2 :- Exp]
                     (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))))]
          (exists_elimE
            (fn [r2 :- Exp] (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2)))))
            (Eq Sk (skel B) (skel A))
            hr
            (fn [r2 :- Exp,
                 h3 :- (And (Eq (Option Exp) (getP p A) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp B (setP p A r2))))]
              (Eq.trans (congrArg skel (And.right (And.right h3)))
                        (step_skel_path chkf p A r r2 hA (And.left h3) (And.left (And.right h3))))))))))))

;; Lemma 2.5 (i): a conversion chain relates types of equal skeleton.  Cv
;; requires every element of the chain to be skeleton-well-formed (§1.5,
;; review R4-01); the induction carries the right endpoint's
;; well-formedness, which each further step needs.
(thm cv_skel_wf [chkf :- (=> Code Code Bool), G0 :- (List Sk), A0 :- Exp, B0 :- Exp, der :- (Cv chkf G0 A0 B0)]
  (And (= (skel A0) (skel B0)) (SkJ Bool.true G0 B0 Sk.unit))
  (induction der)
  ;; cvRefl
  (exact (And.intro (Eq.refl$1 (skel A)) h))
  ;; cvFwd: A ≡ B, B ⇝ C
  (exact (And.intro
           (Eq.trans (And.left ih_hab)
                     (Eq.symm (step_skel chkf B C (skj_isTy Bool.true G0 B Sk.unit (And.right ih_hab) (Eq.refl$1 Bool.true)) hs)))
           hc))
  ;; cvBwd: A ≡ B, C ⇝ B
  (exact (And.intro
           (Eq.trans (And.left ih_hab)
                     (step_skel chkf C B (skj_isTy Bool.true G0 C Sk.unit hc (Eq.refl$1 Bool.true)) hs))
           hc)))

(thm cv_skel [chkf :- (=> Code Code Bool), G :- (List Sk), A :- Exp, B :- Exp, h :- (Cv chkf G A B)]
  (= (skel A) (skel B))
  (exact (And.left (cv_skel_wf chkf G A B h))))

;; The well-formedness hypothesis of step_skel is necessary: a β-step at the
;; head of the ill-formed "type" (λx:Nat. Bool) ⋆ yields Bool, changing the
;; skeleton from Unit to Bool.  (Kernel-checked counterexample.)
(thm beta_step_example [chkf :- (=> Code Code Bool)]
  (Step chkf (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star) Exp.tBool)
  (unfold Step)
  ;; (the witnesses are supplied by apply: Exists.intro's predicate is not
  ;; inferred from the expected type inside a nested term)
  (apply (Exists.intro (List.nil Nat)))
  (apply (Exists.intro (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star)))
  (apply (Exists.intro (subst1 Exp.star Exp.tBool)))
  (exact (And.intro (Eq.refl$1 (Option.some Exp (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star)))
                    (And.intro (Hd.beta chkf U.uw Exp.tNat Exp.tBool Exp.star) (Eq.refl$1 Exp.tBool)))))

(thm step_skel_needs_wf [chkf :- (=> Code Code Bool)]
  (Not (forall [A Exp] (forall [B Exp] (=> (Step chkf A B) (= (skel A) (skel B))))))
  (intro h)
  (have hn (= Sk.unit Sk.bool)
    (h (Exp.app (Exp.lam U.uw Exp.tNat Exp.tBool) Exp.star) Exp.tBool (beta_step_example chkf)))
  (cases hn))

;; ===========================================================================
;; §C  Lemma 2.5 (ii): derivations are simply typed at skeletons (item 3)
;; ===========================================================================

;; --- casts along skeleton equations -----------------------------------------

(thm skj_cast [w :- Bool, G :- (List Sk), t :- Exp, s1 :- Sk, s2 :- Sk, h :- (SkJ w G t s1), e :- (= s1 s2)]
  (SkJ w G t s2)
  (subst e)
  (exact h))

;; The two innermost context entries (recSyn's recursive results y₁, y₂).
(thm skj_cast2 [w :- Bool, a0 :- Sk, b0 :- Sk, a1 :- Sk, b1 :- Sk, G :- (List Sk), t :- Exp, s :- Sk,
                h :- (SkJ w (List.cons Sk a0 (List.cons Sk b0 G)) t s), ea :- (= a0 a1), eb :- (= b0 b1)]
  (SkJ w (List.cons Sk a1 (List.cons Sk b1 G)) t s)
  (subst ea)
  (subst eb)
  (exact h))

;; --- variables, constants, base types ------------------------------------------

;; The skeleton context lists the skeletons of the telescope's entries.
;; nthE and nthS recurse on two arguments, so Ansatz defines them by
;; well-founded recursion and they do not unfold definitionally: the proof
;; goes through their equation lemmas nthE.eq_1..3, nthS.eq_1..3.
(thm skels_nth [D :- (List Exp)]
  (forall [i Nat] (forall [A Exp]
    (=> (= (nthE D i) (Option.some Exp A)) (= (nthS (skels D) i) (Option.some Sk (skel A))))))
  (induction D)
  (intro i A h)
  (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  (intro i A)
  (exact (Nat.rec$0
    (fn [j :- Nat] (=> (Eq (Option Exp) (nthE (List.cons Exp head tail) j) (Option.some Exp A))
                       (Eq (Option Sk) (nthS (skels (List.cons Exp head tail)) j) (Option.some Sk (skel A)))))
    (fn [h0 :- (Eq (Option Exp) (nthE (List.cons Exp head tail) Nat.zero) (Option.some Exp A))]
      (Eq.trans (nthS.eq_2 (skel head) (skels tail))
        (congrArg (fn [x :- Exp] (Option.some Sk (skel x)))
                  (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h0)))))
    (fn [j :- Nat,
         _ :- (=> (Eq (Option Exp) (nthE (List.cons Exp head tail) j) (Option.some Exp A))
                  (Eq (Option Sk) (nthS (skels (List.cons Exp head tail)) j) (Option.some Sk (skel A)))),
         hj :- (Eq (Option Exp) (nthE (List.cons Exp head tail) (Nat.succ j)) (Option.some Exp A))]
      (Eq.trans (nthS.eq_3 (skel head) (skels tail) j)
                (ih_tail j A (Eq.trans (Eq.symm (nthE.eq_3 head tail j)) hj))))
    i)))

(def ^:private base-rule
  '{tEmpty SkJ.wEmpty tUnit SkJ.wUnit tBool SkJ.wBool tNat SkJ.wNat tLbl SkJ.wLbl
    tSyn SkJ.wSyn tDia SkJ.wDia tR SkJ.wR})

;; Formation of a base type or ◇ (rule fBase).
(a/prove-theorem 'base_skj '[X :- Exp]
  '(forall [G (List Sk)] (=> (= (isBaseOrDia X) true) (SkJ Bool.true G X Sk.unit)))
  (lv (into ['(cases X)]
            (mapcat (fn [[ctor _]]
                      (if-let [rule (base-rule ctor)]
                        ['(intro G h) (list 'exact (list rule 'G))]
                        ['(intro G h) '(cases h)]))
                    exp-fields))))

;; The constants typed by the axiom rules (zConst, rConst): ⋆ : 1, tt ff :
;; Bool, zero : Nat, ℓ : Lbl.
(def ^:private const-rule
  '{star (SkJ.sStar G) tt (SkJ.sTT G) ff (SkJ.sFF G) zero (SkJ.sZero G) lbl (SkJ.sLbl G l)})

(a/prove-theorem 'const_skj '[t :- Exp]
  '(forall [A Exp] (forall [G (List Sk)] (=> (= (constTyped t A) true) (SkJ Bool.false G t (skel A)))))
  (lv (into ['(cases t)]
            (mapcat (fn [[ctor _]]
                      (if-let [proof (const-rule ctor)]
                        ;; split the type; the matching one is the axiom, the
                        ;; others refute constTyped (tried second: `cases` on a
                        ;; true = true hypothesis would not close the goal)
                        ;; (one tactic block per constructor of A: all_goals
                        ;; would also reach the outer split's pending goals)
                        (into ['(intro A) '(cases A)]
                              (mapcat (fn [_] ['(intro G h) (list 'first (list 'exact proof) '(cases h))])
                                      exp-fields))
                        ['(intro A G h) '(cases h)]))
                    exp-fields))))

;; reflect's base type (rule zRefl/rRefl): baseCode X is defined exactly on
;; the base data types, which are formed and isBaseTy (SkJ's sRefl premise).
(def ^:private base-data (dissoc base-rule 'tDia))

(a/prove-theorem 'baseCode_skj '[X :- Exp]
  '(forall [cd Exp] (forall [G (List Sk)]
     (=> (= (baseCode X) (Option.some Exp cd))
         (And (= (isBaseTy X) true) (SkJ Bool.true G X Sk.unit)))))
  (lv (into ['(cases X)]
            (mapcat (fn [[ctor _]]
                      (if-let [rule (base-data ctor)]
                        ['(intro cd G h) (list 'exact (list 'And.intro '(Eq.refl$1 Bool.true) (list rule 'G)))]
                        ['(intro cd G h) '(cases h)]))
                    exp-fields))))

;; A simply typed term has skeleton Unit: skel reads only type formers, and
;; no term rule concludes at a type former.  This is what makes every
;; substitution of the rule table skeleton-preserving (skel_subst).
(thm skj_term_unit [w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (=> (= w0 false) (= (skel e0) Sk.unit))
  (induction der)
  (all_goals (intro hw))
  (all_goals (first (rfl) (cases hw))))

;; --- the motive instances of the rule table -------------------------------------
;; Each substitutes variables or term constructors, of skeleton Unit.

(thm sSucc_unit [i :- Nat] (= (skel (sSucc i)) Sk.unit) (cases i) (rfl) (rfl))
(thm sLeafI_unit [i :- Nat] (= (skel (sLeafI i)) Sk.unit) (cases i) (rfl) (rfl))
(thm sNodeI_unit [i :- Nat] (= (skel (sNodeI i)) Sk.unit) (cases i) (rfl) (rfl))
(thm sAt_unit [k :- Nat, sh :- Nat, i :- Nat] (= (skel (sAt k sh i)) Sk.unit) (cases i) (rfl) (rfl))

(thm skel_stepTy [P :- Exp] (= (skel (stepTy P)) (skel P))
  (exact (Eq.trans (skel_lift (subst (fn [i :- Nat] (sSucc i)) P) 1 0)
                   (skel_subst P (fn [i :- Nat] (sSucc i)) sSucc_unit))))
(thm skel_leafTy [P :- Exp] (= (skel (leafTy P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sLeafI i)) sLeafI_unit)))
(thm skel_nodeTy [P :- Exp] (= (skel (nodeTy P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sNodeI i)) sNodeI_unit)))
(thm skel_y1Ty [P :- Exp] (= (skel (y1Ty P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sAt 1 3 i)) (sAt_unit 1 3))))
(thm skel_y2Ty [P :- Exp] (= (skel (y2Ty P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sAt 1 4 i)) (sAt_unit 1 4))))

;; itR's step types g : Π(a:Lbl). X and h : ◇ ⊸ Π(a:Lbl). X ⊸ X ⊸ X (X lifted
;; under their binders).
(thm skel_gTy [X :- Exp] (= (skel (gTy X)) (Sk.arr Sk.lbl (skel X)))
  (exact (congrArg (fn [v :- Sk] (Sk.arr Sk.lbl v)) (skel_lift X 1 0))))
(thm skel_hTy [X :- Exp]
  (= (skel (hTy X)) (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
  (exact (Eq.trans
    (congrArg (fn [v :- Sk] (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr v (Sk.arr (skel (lift 3 0 X)) (skel (lift 4 0 X)))))))
              (skel_lift X 2 0))
    (Eq.trans
      (congrArg (fn [v :- Sk] (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr v (skel (lift 4 0 X))))))) (skel_lift X 3 0))
      (congrArg (fn [v :- Sk] (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) v))))) (skel_lift X 4 0))))))

;; --- the case terms ---------------------------------------------------------------
;; Each case of the two inductions below is one explicit proof term, written
;; with four abbreviations, expanded before elaboration:
;;   G                 the skeleton context  (skels D)
;;   (CAST t s1 s2 h e) h : SkJ false G t s1 and e : s1 = s2 give SkJ false G t s2
;;   (TU x s h)         h : SkJ false G x s gives skel x = Unit (skj_term_unit)
;;   RU                 skel c = Unit for a constant c (by computation)
(defn- expand-case [form]
  (clojure.walk/postwalk
   (fn [x]
     (cond
       (= x 'G) '(skels D)
       (= x 'RU) '(Eq.refl$1 Sk.unit)
       (and (seq? x) (= (first x) 'CAST))
       (let [[_ t s1 s2 h e] x] (list 'skj_cast 'Bool.false '(skels D) t s1 s2 h e))
       (and (seq? x) (= (first x) 'TU))
       (let [[_ t s h] x] (list 'skj_term_unit 'Bool.false '(skels D) t s h '(Eq.refl$1 Bool.false)))
       :else x))
   form))

;; Lemma 2.5 (ii) at type level, one case per rule of Tl (judgment.clj), in
;; the order of the rule table.  The premises' induction hypotheses are
;; ih_<premise>.  Substituted motives (subst1, stepTy, leafTy, nodeTy, y1Ty,
;; y2Ty) are handled by §A, lifted types by skel_lift, conversion by §B.
(def ^:private tl-cases
  '[;; fBase: base types and ◇
    (base_skj X G h)
    ;; fT, fPi, fSig: formation
    (SkJ.wT G b ih_hb)
    (SkJ.wPi G r A B ih_hA ih_hB)
    (SkJ.wSig G r A B ih_hA ih_hB)
    ;; zVar: the variable's type, lifted out of the telescope
    (SkJ.sVar G i (skel (lift (+ i 1) 0 A))
      (Eq.trans (skels_nth D i A h) (congrArg (fn [x :- Sk] (Option.some Sk x)) (Eq.symm (skel_lift A (+ i 1) 0)))))
    ;; zConst
    (const_skj t A G h)
    ;; zConv: Lemma 2.5 (i)
    (CAST t (skel A) (skel B) ih_ht (cv_skel chkf G A B hc))
    ;; zLam
    (SkJ.sLam G r A t (skel B) ih_hA ih_ht)
    ;; zApp: B[u/x] has B's skeleton, u being a term
    (CAST (Exp.app f u) (skel B) (skel (subst1 u B))
      (SkJ.sApp G f u (skel A) (skel B) ih_hf ih_hu)
      (Eq.symm (skel_subst1 u B (TU u (skel A) ih_hu))))
    ;; zPair
    (SkJ.sPair G r A B x y ih_hS ih_hx
      (CAST y (skel (subst1 x B)) (skel B) ih_hy (skel_subst1 x B (TU x (skel A) ih_hx))))
    ;; zLet: the body's type C is lifted past the two pattern variables
    (SkJ.sLetp G C p t (skel A) (skel B) ih_hC ih_hp
      (skj_cast Bool.false (sk2 (skel B) (skel A) G) t (skel (lift 2 0 C)) (skel C) ih_ht (skel_lift C 2 0)))
    ;; zAbort
    (SkJ.sAbort G A t ih_hA ih_ht)
    ;; zIte
    (SkJ.sIte G b t e (skel C) ih_hb ih_ht ih_he)
    ;; zElimB
    (CAST (Exp.elimB P b t e) (skel P) (skel (subst1 b P))
      (SkJ.sElimB G P b t e ih_hP ih_hb
        (CAST t (skel (subst1 Exp.tt P)) (skel P) ih_ht (skel_subst1 Exp.tt P RU))
        (CAST e (skel (subst1 Exp.ff P)) (skel P) ih_he (skel_subst1 Exp.ff P RU)))
      (Eq.symm (skel_subst1 b P (TU b (skel Exp.tBool) ih_hb))))
    ;; zSucc
    (SkJ.sSucc G n ih_h)
    ;; zRecN: the step's type is P[succ x/x] under y
    (CAST (Exp.recN P z s n) (skel P) (skel (subst1 n P))
      (SkJ.sRecN G P z s n ih_hP
        (CAST z (skel (subst1 Exp.zero P)) (skel P) ih_hz (skel_subst1 Exp.zero P RU))
        (skj_cast Bool.false (sk2 (skel P) Sk.nat G) s (skel (stepTy P)) (skel P) ih_hs (skel_stepTy P))
        ih_hn)
      (Eq.symm (skel_subst1 n P (TU n (skel Exp.tNat) ih_hn))))
    ;; zCaseL: the branch list's pseudo-type tBrs P 0 has skeleton Lbl → skel P
    (CAST (Exp.caseL P x bs) (skel P) (skel (subst1 x P))
      (SkJ.sCaseL G P x bs ih_hP ih_hx ih_hb)
      (Eq.symm (skel_subst1 x P (TU x (skel Exp.tLbl) ih_hx))))
    ;; zBnil, zBcons
    (SkJ.sBnil G (skel P))
    (SkJ.sBcons G h t (skel P)
      (CAST h (skel (subst1 (Exp.lbl k) P)) (skel P) ih_hh (skel_subst1 (Exp.lbl k) P RU))
      ih_ht)
    ;; zSleaf, zSnode
    (SkJ.sSleaf G x ih_h)
    (SkJ.sSnode G x c1 c2 ih_hx ih_h1 ih_h2)
    ;; zRecS: the node step lives under a c1 c2 y1 y2, y1 y2 at P[c1/x], P[c2/x]
    (CAST (Exp.recS P tl tn c) (skel P) (skel (subst1 c P))
      (SkJ.sRecS G P tl tn c ih_hP
        (skj_cast Bool.false (List.cons Sk Sk.lbl G) tl (skel (leafTy P)) (skel P) ih_hl (skel_leafTy P))
        (skj_cast2 Bool.false (skel (y2Ty P)) (skel (y1Ty P)) (skel P) (skel P)
          (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G)) tn (skel P)
          (skj_cast Bool.false
            (List.cons Sk (skel (y2Ty P)) (List.cons Sk (skel (y1Ty P)) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))))
            tn (skel (nodeTy P)) (skel P) ih_hn (skel_nodeTy P))
          (skel_y2Ty P) (skel_y1Ty P))
        ih_hc)
      (Eq.symm (skel_subst1 c P (TU c (skel Exp.tSyn) ih_hc))))
    ;; zLeaf, zNode
    (SkJ.sLeaf G x ih_h)
    (SkJ.sNode G d x r1 r2 ih_hd ih_hx ih_h1 ih_h2)
    ;; zItR: g and h's types are X lifted under their binders
    (SkJ.sItR G X g h r ih_hX
      (CAST g (skel (gTy X)) (Sk.arr Sk.lbl (skel X)) ih_hg (skel_gTy X))
      (CAST h (skel (hTy X)) (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))) ih_hh (skel_hTy X))
      ih_hr)
    ;; zPrn, zChk
    (SkJ.sPrn G r ih_h)
    (SkJ.sChk G c d ih_hc ih_hd)
    ;; zH1: the premises' types T(…) have skeleton Unit
    (SkJ.sH1 G r s c e1 e2 ih_hr ih_hs ih_hc ih_h1 ih_h2)
    ;; zRefl: X is a base data type (its code exists)
    (SkJ.sRefl G X r e (And.right (baseCode_skj X cd G hb)) (And.left (baseCode_skj X cd G hb)) ih_hr ih_he)
    ;; zInsp: the branches live under the certificate and a T(…) proof
    (SkJ.sInsp G X r c t1 t2 ih_hX ih_hr ih_hc
      (skj_cast Bool.false (sk2 Sk.unit Sk.cert G) t1 (skel (lift 2 0 X)) (skel X) ih_h1 (skel_lift X 2 0))
      (skj_cast Bool.false (sk2 Sk.unit Sk.cert G) t2 (skel (lift 2 0 X)) (skel X) ih_h2 (skel_lift X 2 0)))])

;; Lemma 2.5 (ii), type level: if Δ ⊢ t :⁰ A then t is simply typed at
;; skel(A) in skel(Δ); if Δ ⊢ A type then A is skeleton-well-formed (the
;; w = true instance: A's index is then tUnit, of skeleton Unit).
(a/prove-theorem 'lemma25_tl
  '[chkf :- (=> Code Code Bool), w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp, der :- (Tl chkf w0 D0 t0 A0)]
  '(SkJ w0 (skels D0) t0 (skel A0))
  (lv (into ['(induction der)] (map (fn [c] (list 'exact (expand-case c))) tl-cases))))

(thm lemma25_tl_term [chkf :- (=> Code Code Bool), D :- (List Exp), t :- Exp, A :- Exp, h :- (Tl chkf Bool.false D t A)]
  (SkJ Bool.false (skels D) t (skel A))
  (exact (lemma25_tl chkf Bool.false D t A h)))

(thm lemma25_tl_type [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, h :- (Tl chkf Bool.true D A Exp.tUnit)]
  (SkJ Bool.true (skels D) A Sk.unit)
  (exact (lemma25_tl chkf Bool.true D A Exp.tUnit h)))

;; Lemma 2.5 (ii) at runtime, one case per rule of Rt.  Type-level premises
;; (formation, :⁰ arguments) go through lemma25_tl, abbreviated
;;   (LT Δ A h)  for a formation premise  Δ ⊢ A type
;;   (LF Δ t A h) for a :⁰ premise        Δ ⊢ t :⁰ A.
;; Usages play no part: skel(Γ) is the telescope's skeletons.
(defn- expand-rt [form]
  (expand-case
   (clojure.walk/postwalk
    (fn [x]
      (cond
        (and (seq? x) (= (first x) 'LT)) (let [[_ d a h] x] (list 'lemma25_tl 'chkf 'Bool.true d a 'Exp.tUnit h))
        (and (seq? x) (= (first x) 'LF)) (let [[_ d t a h] x] (list 'lemma25_tl 'chkf 'Bool.false d t a h))
        :else x))
    form)))

(def ^:private rt-cases
  '[;; rVar, rConst
    (SkJ.sVar G i (skel (lift (+ i 1) 0 A))
      (Eq.trans (skels_nth D i A hA) (congrArg (fn [x :- Sk] (Option.some Sk x)) (Eq.symm (skel_lift A (+ i 1) 0)))))
    (const_skj t A G h)
    ;; rLam
    (SkJ.sLam G r A t (skel B) (LT D A hA) ih_ht)
    ;; rApp0: the argument is a type-level premise
    (CAST (Exp.app f u) (skel B) (skel (subst1 u B))
      (SkJ.sApp G f u (skel A) (skel B) ih_hf (LF D u A hu))
      (Eq.symm (skel_subst1 u B (TU u (skel A) (LF D u A hu)))))
    ;; rApp
    (CAST (Exp.app f u) (skel B) (skel (subst1 u B))
      (SkJ.sApp G f u (skel A) (skel B) ih_hf ih_hu)
      (Eq.symm (skel_subst1 u B (TU u (skel A) ih_hu))))
    ;; rPair0: the first component is a type-level premise
    (SkJ.sPair G U.u0 A B x y (SkJ.wSig G U.u0 A B (LT D A hA) (LT (List.cons Exp A D) B hB)) (LF D x A hx)
      (CAST y (skel (subst1 x B)) (skel B) ih_hy (skel_subst1 x B (TU x (skel A) (LF D x A hx)))))
    ;; rPair
    (SkJ.sPair G r A B x y (SkJ.wSig G r A B (LT D A hA) (LT (List.cons Exp A D) B hB)) ih_hx
      (CAST y (skel (subst1 x B)) (skel B) ih_hy (skel_subst1 x B (TU x (skel A) ih_hx))))
    ;; rLet
    (SkJ.sLetp G C p t (skel A) (skel B) (LT D C hC) ih_hp
      (skj_cast Bool.false (sk2 (skel B) (skel A) G) t (skel (lift 2 0 C)) (skel C) ih_ht (skel_lift C 2 0)))
    ;; rAbort
    (SkJ.sAbort G A t (LT D A hA) ih_ht)
    ;; rConv: Lemma 2.5 (i)
    (CAST t (skel A) (skel B) ih_ht (cv_skel chkf G A B hc))
    ;; rIte
    (SkJ.sIte G b t e (skel C) ih_hb ih_ht ih_he)
    ;; rElimB
    (CAST (Exp.elimB P b t e) (skel P) (skel (subst1 b P))
      (SkJ.sElimB G P b t e (LT (consE Exp.tBool D) P hP) ih_hb
        (CAST t (skel (subst1 Exp.tt P)) (skel P) ih_ht (skel_subst1 Exp.tt P RU))
        (CAST e (skel (subst1 Exp.ff P)) (skel P) ih_he (skel_subst1 Exp.ff P RU)))
      (Eq.symm (skel_subst1 b P (TU b (skel Exp.tBool) ih_hb))))
    ;; rSucc
    (SkJ.sSucc G n ih_h)
    ;; rRecN
    (CAST (Exp.recN P z s n) (skel P) (skel (subst1 n P))
      (SkJ.sRecN G P z s n (LT (consE Exp.tNat D) P hP)
        (CAST z (skel (subst1 Exp.zero P)) (skel P) ih_hz (skel_subst1 Exp.zero P RU))
        (skj_cast Bool.false (sk2 (skel P) Sk.nat G) s (skel (stepTy P)) (skel P) ih_hs (skel_stepTy P))
        ih_hn)
      (Eq.symm (skel_subst1 n P (TU n (skel Exp.tNat) ih_hn))))
    ;; rCaseL
    (CAST (Exp.caseL P x bs) (skel P) (skel (subst1 x P))
      (SkJ.sCaseL G P x bs (LT (consE Exp.tLbl D) P hP) ih_hx ih_hb)
      (Eq.symm (skel_subst1 x P (TU x (skel Exp.tLbl) ih_hx))))
    ;; rBnil, rBcons
    (SkJ.sBnil G (skel P))
    (SkJ.sBcons G h t (skel P)
      (CAST h (skel (subst1 (Exp.lbl k) P)) (skel P) ih_hh (skel_subst1 (Exp.lbl k) P RU))
      ih_ht)
    ;; rSleaf, rSnode
    (SkJ.sSleaf G x ih_h)
    (SkJ.sSnode G x c1 c2 ih_hx ih_h1 ih_h2)
    ;; rRecS
    (CAST (Exp.recS P tl tn c) (skel P) (skel (subst1 c P))
      (SkJ.sRecS G P tl tn c (LT (consE Exp.tSyn D) P hP)
        (skj_cast Bool.false (List.cons Sk Sk.lbl G) tl (skel (leafTy P)) (skel P) ih_hl (skel_leafTy P))
        (skj_cast2 Bool.false (skel (y2Ty P)) (skel (y1Ty P)) (skel P) (skel P)
          (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G)) tn (skel P)
          (skj_cast Bool.false
            (List.cons Sk (skel (y2Ty P)) (List.cons Sk (skel (y1Ty P)) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))))
            tn (skel (nodeTy P)) (skel P) ih_hn (skel_nodeTy P))
          (skel_y2Ty P) (skel_y1Ty P))
        ih_hc)
      (Eq.symm (skel_subst1 c P (TU c (skel Exp.tSyn) ih_hc))))
    ;; rLeaf, rNode
    (SkJ.sLeaf G x ih_h)
    (SkJ.sNode G d x r1 r2 ih_hd ih_hx ih_h1 ih_h2)
    ;; rItR
    (SkJ.sItR G X g h r (LT D X hX)
      (CAST g (skel (gTy X)) (Sk.arr Sk.lbl (skel X)) ih_hg (skel_gTy X))
      (CAST h (skel (hTy X)) (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))) ih_hh (skel_hTy X))
      ih_hr)
    ;; rPrn, rChk, rH1
    (SkJ.sPrn G r ih_h)
    (SkJ.sChk G c d ih_hc ih_hd)
    (SkJ.sH1 G r s c e1 e2 ih_hr ih_hs ih_hc ih_h1 ih_h2)
    ;; rRefl
    (SkJ.sRefl G X r e (And.right (baseCode_skj X cd G hb)) (And.left (baseCode_skj X cd G hb)) ih_hr ih_he)
    ;; rInsp
    (SkJ.sInsp G X r c t1 t2 (LT D X hX) ih_hr ih_hc
      (skj_cast Bool.false (sk2 Sk.unit Sk.cert G) t1 (skel (lift 2 0 X)) (skel X) ih_h1 (skel_lift X 2 0))
      (skj_cast Bool.false (sk2 Sk.unit Sk.cert G) t2 (skel (lift 2 0 X)) (skel X) ih_h2 (skel_lift X 2 0)))])

;; Lemma 2.5 (ii), runtime: if Γ ⊢ t :¹ A then t is simply typed at skel(A)
;; in skel(Γ).
(a/prove-theorem 'lemma25_rt
  '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)]
  '(SkJ Bool.false (skels D0) t0 (skel A0))
  (lv (into ['(induction der)] (map (fn [c] (list 'exact (expand-rt c))) rt-cases))))

;; ===========================================================================
;; §D  Skeleton inference skOf (carrier.clj) is correct (item 4)
;; ===========================================================================

;; The two clauses of skOf that inspect a recursive result: λ (an arrow
;; from the annotation's skeleton to the body's) and application (the
;; function's codomain).  Stated over an arbitrary option, then read back at
;; skOf by computation.
(thm lamOpt_some [a :- Sk, o :- (Option Sk), b :- Sk, h :- (= o (Option.some Sk b))]
  (= (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Option Sk)) (Option.none Sk) (fn [x :- Sk] (Option.some Sk (Sk.arr a x))) o)
     (Option.some Sk (Sk.arr a b)))
  (subst h)
  (rfl))

(thm appOpt_some [o :- (Option Sk), sf :- Sk, h :- (= o (Option.some Sk sf))]
  (= (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Option Sk)) (Option.none Sk) (fn [x :- Sk] (arrCod x)) o) (arrCod sf))
  (subst h)
  (rfl))

(thm skOf_lam [G :- (List Sk), r :- U, A :- Exp, t :- Exp, b :- Sk,
               h :- (= (skOf (List.cons Sk (skel A) G) t) (Option.some Sk b))]
  (= (skOf G (Exp.lam r A t)) (Option.some Sk (Sk.arr (skel A) b)))
  (exact (lamOpt_some (skel A) (skOf (List.cons Sk (skel A) G) t) b h)))

(thm skOf_app [G :- (List Sk), f :- Exp, u :- Exp, sf :- Sk, h :- (= (skOf G f) (Option.some Sk sf))]
  (= (skOf G (Exp.app f u)) (arrCod sf))
  (exact (appOpt_some (skOf G f) sf h)))

;; brHead e: following skOf's recursion (the then-branch of if, the body of
;; λ, the function of an application), e ends in a branch list.  A branch
;; list has every skeleton Lbl → s (rules sBnil, sBcons), so skOf, which
;; must return one skeleton, returns none there.
(a/defn brHead [e :- Exp] Bool
  (match e
    [bnil true]
    [(bcons h t) true]
    [(ite b t x) (brHead t)]
    [(lam r A t) (brHead t)]
    [(app f u) (brHead f)]
    [_ false]))

;; skOf_complete (item 4) is pending: induction over SkJ with the index
;; w0 fixed by a hypothesis needs the formation cases refuted, and Ansatz's
;; exact accepted an ill-typed refutation there (the kernel then rejected the
;; whole proof).  See the formalization log.
