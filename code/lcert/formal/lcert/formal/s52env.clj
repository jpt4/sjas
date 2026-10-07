(ns lcert.formal.s52env
  "F5 — Theorem 5.2: related environments, restriction, and the variable
  case (R4-metatheory.md §5, the fundamental property's hypotheses).

  Env52 … erasing D us ρ η: the runtime environment ρ is related to the
  carrier environment η : HEnv (skels D) entry by entry.  Entry i, of type
  A over the entries after it, is related at S(A) read in the tail of η —
  the paper's (ρ(x), η(x)) ∈ S(A_x)η.  Which entries are constrained
  depends on the evaluator (Entry52):
  - evalₙ (erasing = false): every entry, whatever its usage (the paper:
    \"for every entry, whatever its usage\");
  - evalᴱₙ (erasing = true): entries of usage 1 or ω.  An entry of usage 0
    may hold any runtime value and any carrier value (the paper's relaxed
    environments).
  The usage vector us has the context's length.  (Env52 is Codex's draft
  of 2026-10-05, test-runs/s52env-draft.clj, kept as written.)

  Restriction (env52_sub): an environment related at a vector is related
  at any vector inside it (SubU, theorem4e.clj): an entry nonzero in the
  premise is nonzero in the conclusion, where it is already related.  This
  is the paper's \"Restriction\" bullet, which the erasing assembly uses at
  every premise; for evalₙ, Entry52 ignores the usage and the restriction
  is not needed.

  WFS D: the context is well-formed at the level of skeletons — each entry
  is SkJ-formed over the entries after it.  It is what the variable case
  needs (S of a lifted type is S of the type, s52_lift), and it follows from
  WFCtx (formation by Tl, Lemma 3.6's hypothesis) by Lemma 2.5
  (wfs_of_wfctx).  The skeleton level is used, not WFCtx, because two
  type-level rules (zRecS, zInsp) extend the context by types whose
  formation the rule does not record; their skeleton formation follows
  from the rule's other premises (s52fund.clj), while their Tl formation
  would need a substitution lemma for Tl, which the formalization does not
  have.

  var52: in a related environment of a well-formed context, the value of
  variable i is ρ(i), and it is related at lift (i+1) 0 A — the type Var
  gives it — to ⟦var i⟧η.  By induction on the context: the head entry is
  related at A over the tail, which is S(lift 1 0 A) over the whole
  environment (s52_lift); a deeper entry is the tail's, lifted once more
  (lift_comp)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52facts]
            [lcert.formal.s52trace]
            [lcert.formal.theorem4e]))

;; --- one entry ------------------------------------------------------------------

;; Entry52 erasing r P: what an entry of usage r must satisfy, P being its
;; relation.  P for evalₙ; for evalᴱₙ, True at usage 0 and P otherwise.
(kdef Entry52 (=> Bool U Prop Prop)
  (fn [erasing :- Bool, r :- U, P :- Prop]
    (Bool.rec$1 (fn [_ :- Bool] Prop) P
      (U.rec$1 (fn [_ :- U] Prop) True P P r) erasing)))

(thm entry52_all [r :- U, P :- Prop]
  (Eq Prop (Entry52 Bool.false r P) P) (rfl))

(thm entry52_zero [P :- Prop]
  (Entry52 Bool.true U.u0 P) (exact True.intro))

(thm entry52_nonzero [erasing :- Bool, r :- U, P :- Prop,
                      hr :- (Eq Bool (nonzero r) Bool.true)]
  (Eq Prop (Entry52 erasing r P) P)
  (cases r)
  (exact (Bool.noConfusion hr))
  (cases erasing) (rfl) (rfl)
  (cases erasing) (rfl) (rfl))

;; The entry is its relation whenever the evaluator is evalₙ or the usage is
;; nonzero: the side condition of every case that reads an entry.
(thm entry52_or [erasing :- Bool, r :- U, P :- Prop,
                 h :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))]
  (Eq Prop (Entry52 erasing r P) P)
  (cases h)
  (subst h) (rfl)
  (exact (entry52_nonzero erasing r P h)))

;; Raising the usage keeps an entry related (zero only where it was zero).
(thm entry52_mono [erasing :- Bool, r :- U, r2 :- U, P :- Prop]
  ;; Keep the usage premise in the motive: cases does not specialize a
  ;; hypothesis that was already introduced before splitting r2.
  (=> (=> (Eq Bool (nonzero r2) Bool.true) (Eq Bool (nonzero r) Bool.true))
      (Entry52 erasing r P) (Entry52 erasing r2 P))
  (cases erasing)
  (intro hs h) (exact h)
  (cases r2)
  (intro hs h) (exact True.intro)
  (intro hs h) (exact (Eq.mp$0 (entry52_nonzero Bool.true r P (hs (Eq.refl Bool.true))) h))
  (intro hs h) (exact (Eq.mp$0 (entry52_nonzero Bool.true r P (hs (Eq.refl Bool.true))) h)))

;; --- related environments -----------------------------------------------------------

(kdef Env52
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)] (forall [cap Nat] (forall [erasing Bool]
        (forall [D (List Exp)] (=> (List U) (List RV) (HEnv (skels D)) Prop)))))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool, D :- (List Exp)]
    (List.rec$1$0 Exp (fn [D :- (List Exp)] (=> (List U) (List RV) (HEnv (skels D)) Prop))
      (fn [us :- (List U), rho :- (List RV), _en :- Unit]
        (And (Eq (List U) us (List.nil U)) (Eq (List RV) rho (List.nil RV))))
      (fn [A :- Exp, rest :- (List Exp),
           ih :- (=> (List U) (List RV) (HEnv (skels rest)) Prop)]
        (fn [us :- (List U), rho :- (List RV), en :- (HEnv (skels (List.cons Exp A rest)))]
          (Exists (fn [r :- U] (Exists (fn [us2 :- (List U)]
            (Exists (fn [v :- RV] (Exists (fn [rho2 :- (List RV)]
              (And (Eq (List U) us (List.cons U r us2))
                (And (Eq (List RV) rho (List.cons RV v rho2))
                  (And (ih us2 rho2 (Prod.snd en))
                    (Entry52 erasing r
                      (S52 chkf dec encTy cap erasing A (skels rest) (Prod.snd en)
                        (skel A) v (Prod.fst en)))))))))))))))) D)))

(def ^:private pars
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), cap :- Nat, erasing :- Bool])
(defn- prove! [nm params goal tactics]
  (a/prove-theorem nm (lv (vec (concat pars params))) (lv goal) (lv tactics)))

(prove! 'env52_nil []
  '(Env52 chkf dec encTy cap erasing (List.nil Exp) (List.nil U) (List.nil RV) Unit.unit)
  '[(constructor) (rfl) (rfl)])

;; Extending a related environment by a related entry.
(prove! 'env52_cons
  '[A :- Exp, D :- (List Exp), r :- U, us :- (List U), v :- RV, rho :- (List RV),
    alpha :- (Car (skel A)), en :- (HEnv (skels D)),
    he :- (Entry52 erasing r (S52 chkf dec encTy cap erasing A (skels D) en (skel A) v alpha)),
    ht :- (Env52 chkf dec encTy cap erasing D us rho en)]
  '(Env52 chkf dec encTy cap erasing (List.cons Exp A D) (List.cons U r us)
     (List.cons RV v rho) (Prod.mk alpha en))
  '[(constructor) (exact r) (constructor) (exact us)
    (constructor) (exact v) (constructor) (exact rho)
    (constructor) (rfl) (constructor) (rfl) (constructor) (exact ht) (exact he)])

;; The same, when the evaluator is evalₙ or the usage is nonzero: the entry
;; is its relation (entry52_or).
(prove! 'env52_cons_rel
  '[A :- Exp, D :- (List Exp), r :- U, us :- (List U), v :- RV, rho :- (List RV),
    alpha :- (Car (skel A)), en :- (HEnv (skels D)),
    hnr :- (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true)),
    he :- (S52 chkf dec encTy cap erasing A (skels D) en (skel A) v alpha),
    ht :- (Env52 chkf dec encTy cap erasing D us rho en)]
  '(Env52 chkf dec encTy cap erasing (List.cons Exp A D) (List.cons U r us)
     (List.cons RV v rho) (Prod.mk alpha en))
  '[(exact (env52_cons chkf dec encTy cap erasing A D r us v rho alpha en
             (Eq.mpr (entry52_or erasing r (S52 chkf dec encTy cap erasing A (skels D) en (skel A) v alpha) hnr) he)
             ht))])

;; Restriction along SubU.  The same tail carrier environment is used on
;; both sides; only which entries are demanded changes.
(prove! 'env52_sub_cons
  '[A :- Exp, D :- (List Exp),
    ih :- (forall [us (List U)] (forall [ws (List U)] (forall [rho (List RV)] (forall [en (HEnv (skels D))]
      (=> (Env52 chkf dec encTy cap erasing D us rho en) (SubU ws us)
          (Env52 chkf dec encTy cap erasing D ws rho en)))))),
    r :- U, us :- (List U), v :- RV, rho :- (List RV), en :- (HEnv (skels (List.cons Exp A D))),
    ht :- (Env52 chkf dec encTy cap erasing D us rho (Prod.snd en)),
    he :- (Entry52 erasing r
      (S52 chkf dec encTy cap erasing A (skels D) (Prod.snd en) (skel A) v (Prod.fst en)))]
  '(forall [ws (List U)] (=> (SubU ws (List.cons U r us))
    (Env52 chkf dec encTy cap erasing (List.cons Exp A D) ws (List.cons RV v rho) en)))
  '[(intro ws) (cases ws)
    (intro hs) (exact (absurd (Eq.symm (And.left hs)) (Nat.succ_ne_zero (lenU us))))
    (intro hs)
    (have hl (Eq Nat (lenU tail) (lenU us)) (succ_eq (lenU tail) (lenU us) (And.left hs)))
    (have hs2 (SubU tail us)
      (And.intro hl (fn [i :- Nat, hi :- (Eq Bool (nzAt tail i) Bool.true)]
        ((And.right hs) (Nat.succ i) hi))))
    (constructor) (exact head) (constructor) (exact tail)
    (constructor) (exact v) (constructor) (exact rho)
    (constructor) (rfl) (constructor) (rfl) (constructor)
    (exact (ih us tail rho (Prod.snd en) ht hs2))
    (exact (entry52_mono erasing r head
      (S52 chkf dec encTy cap erasing A (skels D) (Prod.snd en) (skel A) v (Prod.fst en))
      ((And.right hs) 0) he))])

(prove! 'env52_sub_nil
  '[us :- (List U), rho :- (List RV), en :- (HEnv (skels (List.nil Exp)))]
  '(forall [ws (List U)]
    (=> (Env52 chkf dec encTy cap erasing (List.nil Exp) us rho en) (SubU ws us)
        (Env52 chkf dec encTy cap erasing (List.nil Exp) ws rho en)))
  '[(intro ws) (cases ws)
    (intro h hs) (constructor) (rfl) (exact (And.right h))
    (intro h hs) (exact (absurd (Eq.trans (And.left hs) (congrArg lenU (And.left h)))
      (Nat.succ_ne_zero (lenU tail))))])

(prove! 'env52_sub '[D :- (List Exp)]
  '(forall [us (List U)] (forall [ws (List U)] (forall [rho (List RV)] (forall [en (HEnv (skels D))]
    (=> (Env52 chkf dec encTy cap erasing D us rho en) (SubU ws us)
        (Env52 chkf dec encTy cap erasing D ws rho en))))))
  '[(induction D)
    (intro us ws rho en) (exact (env52_sub_nil chkf dec encTy cap erasing us rho en ws))
    (intro us ws rho en h hs)
    (refine' (exT U _ _ h _)) (intro r0 h0)
    (refine' (exT (List U) _ _ h0 _)) (intro us0 h1)
    (refine' (exT RV _ _ h1 _)) (intro v0 h2)
    (refine' (exT (List RV) _ _ h2 _)) (intro rho0 hp)
    (have hu (Eq (List U) us (List.cons U r0 us0)) (And.left hp))
    (have hr (Eq (List RV) rho (List.cons RV v0 rho0)) (And.left (And.right hp)))
    (have ht (Env52 chkf dec encTy cap erasing tail us0 rho0 (Prod.snd en))
      (And.left (And.right (And.right hp))))
    (have he (Entry52 erasing r0
      (S52 chkf dec encTy cap erasing head (skels tail) (Prod.snd en) (skel head) v0 (Prod.fst en)))
      (And.right (And.right (And.right hp))))
    (have hs2 (SubU ws (List.cons U r0 us0))
      (Eq.mp (congrArg (fn [q :- (List U)] (SubU ws q)) hu) hs))
    (exact (Eq.mpr (congrArg (fn [q :- (List RV)]
      (Env52 chkf dec encTy cap erasing (List.cons Exp head tail) ws q en)) hr)
      (env52_sub_cons chkf dec encTy cap erasing head tail ih_tail r0 us0 v0 rho0 en ht he ws hs2)))])

;; --- well-formed contexts, at the level of skeletons ------------------------------

(kdef WFS (=> (List Exp) Prop)
  (fn [D :- (List Exp)]
    (List.rec$1$0 Exp (fn [_ :- (List Exp)] Prop) True
      (fn [A :- Exp, rest :- (List Exp), ih :- Prop] (And (SkJ Bool.true (skels rest) A Sk.unit) ih)) D)))

(thm wfs_cons [A :- Exp, D :- (List Exp), h1 :- (SkJ Bool.true (skels D) A Sk.unit), h2 :- (WFS D)]
  (WFS (List.cons Exp A D))
  (exact (And.intro h1 h2)))

(thm wfs_tail [A :- Exp, D :- (List Exp), h :- (WFS (List.cons Exp A D))]
  (WFS D)
  (exact (And.right h)))

(thm wfs_head [A :- Exp, D :- (List Exp), h :- (WFS (List.cons Exp A D))]
  (SkJ Bool.true (skels D) A Sk.unit)
  (exact (And.left h)))

;; Lemma 3.6's well-formed contexts are well-formed here (Lemma 2.5).
(thm wfs_of_wfctx [chkf :- (=> Code Code Bool), D :- (List Exp)]
  (=> (WFCtx chkf D) (WFS D))
  (induction D)
  (intro h) (exact True.intro)
  (intro h)
  (exact (And.intro (lemma25_tl_type chkf tail head (And.left h)) (ih_tail (And.right h)))))

;; The token context Θₘ is well-formed: every entry is ◇.
(thm wfs_theta [m :- Nat]
  (WFS (thetaD m))
  (induction m)
  (exact True.intro)
  (exact (And.intro (SkJ.wDia (skels (thetaD n))) ih_n)))

;; The i-th entry's type, lifted past the entries before it, is well-formed
;; (as var_wf, fundamental.clj, from WFS).
(thm wfs_var [D :- (List Exp)]
  (=> (WFS D) (forall [i Nat] (forall [A Exp] (=> (Eq (Option Exp) (nthE D i) (Option.some Exp A))
    (SkJ Bool.true (skels D) (lift (+ i 1) 0 A) Sk.unit)))))
  (induction D)
  (intro hw i A h) (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  (intro hw i) (cases i) (all_goals (intro A h))
  (have hX (SkJ Bool.true (skels tail) (lift (+ n 1) 0 A) Sk.unit) (ih_tail (And.right hw) n A (Eq.trans (Eq.symm (nthE.eq_3 head tail n)) h)))
  (have hW (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) (lift 1 0 (lift (+ n 1) 0 A)) Sk.unit)
    (skj_weaken Bool.true (skels tail) (lift (+ n 1) 0 A) Sk.unit hX 0 (skel head)))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) e Sk.unit)) (lift_comp A 1 (+ n 1) 0)) hW))
  (have e (Eq Exp head A) (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h)))
  (exact (Eq.mp (congrArg (fn [X :- Exp] (SkJ Bool.true (List.cons Sk (skel head) (skels tail)) (lift 1 0 X) Sk.unit)) e)
                (skj_weaken Bool.true (skels tail) head Sk.unit (And.left hw) 0 (skel head)))))

;; --- weakening of S, at the lifted type's own skeleton ---------------------------

;; s52_lift (s52facts.clj) at a family of values g s, read at skel (lift 1 c A)
;; (equal to skel A, but not definitionally), as the case lemmas meet it.
(prove! 's52_lift_fam
  '[G :- (List Sk), A :- Exp, der :- (SkJ Bool.true G A Sk.unit), c :- Nat, x :- Sk, en :- (HEnv G),
    vx :- (Car x), rv :- RV, g :- (forall [s Sk] (Car s))]
  '(Eq Prop (S52 chkf dec encTy cap erasing (lift 1 c A) (insS c x G) (insE c x G en vx) (skel (lift 1 c A)) rv (g (skel (lift 1 c A))))
            (S52 chkf dec encTy cap erasing A G en (skel A) rv (g (skel A))))
  '[(rw [(skel_lift A 1 c)])
    (exact (s52_lift chkf dec encTy cap erasing G A der c x en vx rv (g (skel A))))])

;; Two binders at once (let's body, inspect's branches: lift 2 0 C under
;; two new entries).
(prove! 's52_lift2_fam
  '[G :- (List Sk), C :- Exp, der :- (SkJ Bool.true G C Sk.unit), sa :- Sk, sb :- Sk, en :- (HEnv G),
    va :- (Car sa), vb :- (Car sb), rv :- RV, g :- (forall [s Sk] (Car s))]
  '(Eq Prop (S52 chkf dec encTy cap erasing (lift 2 0 C) (List.cons Sk sb (List.cons Sk sa G)) (Prod.mk vb (Prod.mk va en))
              (skel (lift 2 0 C)) rv (g (skel (lift 2 0 C))))
            (S52 chkf dec encTy cap erasing C G en (skel C) rv (g (skel C))))
  '[(rw [(Eq.symm (lift_comp C 1 1 0))])
    (have hW (SkJ Bool.true (List.cons Sk sa G) (lift 1 0 C) Sk.unit) (skj_weaken Bool.true G C Sk.unit der 0 sa))
    (exact (Eq.trans
      (s52_lift_fam chkf dec encTy cap erasing (List.cons Sk sa G) (lift 1 0 C) hW 0 sb (Prod.mk va en) vb rv g)
      (s52_lift_fam chkf dec encTy cap erasing G C der 0 sa en va rv g)))])

;; --- the variable case ------------------------------------------------------------------

(defn- SVAR [D i A en v]
  (list 'S52 'chkf 'dec 'encTy 'cap 'erasing (list 'lift (list '+ i 1) 0 A) (list 'skels D) en
        (list 'skel (list 'lift (list '+ i 1) 0 A)) v
        (list 'lookup (list 'skels D) i (list 'skel (list 'lift (list '+ i 1) 0 A)) en)))
(defn- VAR-MOTIVE [D]
  (list '=> (list 'WFS D)
    (list 'forall '[i Nat] (list 'forall '[A Exp]
      (list '=> (list 'Eq '(Option Exp) (list 'nthE D 'i) '(Option.some Exp A))
        (list 'forall '[us (List U)] (list 'forall '[r U] (list 'forall '[rho (List RV)]
          (list 'forall ['en (list 'HEnv (list 'skels D))]
            (list '=> '(Eq (Option U) (nthU us i) (Option.some U r))
                      '(Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))
                      (list 'Env52 'chkf 'dec 'encTy 'cap 'erasing D 'us 'rho 'en)
              (list 'Exists (list 'fn '[v :- RV]
                (list 'And '(Eq (Option RV) (rlookup rho i) (Option.some RV v))
                      (SVAR D 'i 'A 'en 'v))))))))))))))
(def ^:private CT '(List.cons Exp head tail))
(def ^:private GT '(List.cons Sk (skel head) (skels tail)))
(def ^:private X '(lift (+ m 1) 0 A))

;; Open a related environment of head :: tail at the vector us and ρ.
(def ^:private env-open
  '[(refine' (exT U _ _ he _)) (intro r0 h0)
    (refine' (exT (List U) _ _ h0 _)) (intro us0 h1)
    (refine' (exT RV _ _ h1 _)) (intro v0 h2)
    (refine' (exT (List RV) _ _ h2 _)) (intro rho0 hp)
    (have hu0 (Eq (List U) us (List.cons U r0 us0)) (And.left hp))
    (have hr0 (Eq (List RV) rho (List.cons RV v0 rho0)) (And.left (And.right hp)))
    (have ht0 (Env52 chkf dec encTy cap erasing tail us0 rho0 (Prod.snd en)) (And.left (And.right (And.right hp))))
    (have he0 (Entry52 erasing r0
      (S52 chkf dec encTy cap erasing head (skels tail) (Prod.snd en) (skel head) v0 (Prod.fst en)))
      (And.right (And.right (And.right hp))))])

(prove! 'var52_succ
  ['head :- 'Exp 'tail :- '(List Exp) 'm :- 'Nat 'A :- 'Exp 'us :- '(List U) 'r :- 'U 'rho :- '(List RV)
   'en :- (list 'HEnv (list 'skels CT))
   'hw :- (list 'WFS CT) 'ih :- (VAR-MOTIVE 'tail)
   'h :- (list 'Eq '(Option Exp) (list 'nthE CT '(Nat.succ m)) '(Option.some Exp A))
   'hu :- '(Eq (Option U) (nthU us (Nat.succ m)) (Option.some U r))
   'hnr :- '(Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))
   'he :- (list 'Env52 'chkf 'dec 'encTy 'cap 'erasing CT 'us 'rho 'en)]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And '(Eq (Option RV) (rlookup rho (Nat.succ m)) (Option.some RV v)) (SVAR CT '(Nat.succ m) 'A 'en 'v))))
  (concat
    env-open
    ['(have hA2 (Eq (Option Exp) (nthE tail m) (Option.some Exp A)) (Eq.trans (Eq.symm (nthE.eq_3 head tail m)) h))
     '(have hu1 (Eq (Option U) (nthU (List.cons U r0 us0) (Nat.succ m)) (Option.some U r))
        (Eq.mp (congrArg (fn [q :- (List U)] (Eq (Option U) (nthU q (Nat.succ m)) (Option.some U r))) hu0) hu))
     '(have hu2 (Eq (Option U) (nthU us0 m) (Option.some U r)) (Eq.trans (Eq.symm (nthU.eq_3 r0 us0 m)) hu1))
     (list 'have 'hX (list 'SkJ 'Bool.true '(skels tail) X 'Sk.unit) '(wfs_var tail (And.right hw) m A hA2))
     '(refine' (exT RV _ _ (ih (And.right hw) m A hA2 us0 r rho0 (Prod.snd en) hu2 hnr ht0) _))
     '(intro v pv)
     '(constructor) '(exact v)
     '(constructor)
     '(exact (Eq.trans (congrArg (fn [q :- (List RV)] (rlookup q (Nat.succ m))) hr0) (And.left pv)))
     (list 'change (list 'S52 'chkf 'dec 'encTy 'cap 'erasing '(lift (+ (+ m 1) 1) 0 A) GT 'en '(skel (lift (+ (+ m 1) 1) 0 A)) 'v
                         (list 'lookup GT '(+ m 1) '(skel (lift (+ (+ m 1) 1) 0 A)) 'en)))
     '(rw [(Eq.symm (lift_comp A 1 (+ m 1) 0))])
     (list 'exact (list 'Eq.mpr (list 's52_lift_fam 'chkf 'dec 'encTy 'cap 'erasing '(skels tail) X 'hX 0 '(skel head)
                                      '(Prod.snd en) '(Prod.fst en) 'v
                                      (list 'fn '[s :- Sk] (list 'lookup '(skels tail) 'm 's '(Prod.snd en))))
                         '(And.right pv)))]))

(prove! 'var52_zero
  ['head :- 'Exp 'tail :- '(List Exp) 'A :- 'Exp 'us :- '(List U) 'r :- 'U 'rho :- '(List RV)
   'en :- (list 'HEnv (list 'skels CT))
   'hw :- (list 'WFS CT)
   'h :- (list 'Eq '(Option Exp) (list 'nthE CT 'Nat.zero) '(Option.some Exp A))
   'hu :- '(Eq (Option U) (nthU us Nat.zero) (Option.some U r))
   'hnr :- '(Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r) Bool.true))
   'he :- (list 'Env52 'chkf 'dec 'encTy 'cap 'erasing CT 'us 'rho 'en)]
  (list 'Exists (list 'fn '[v :- RV]
    (list 'And '(Eq (Option RV) (rlookup rho Nat.zero) (Option.some RV v)) (SVAR CT 'Nat.zero 'A 'en 'v))))
  (concat
    env-open
    ['(have e (Eq Exp head A) (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h)))
     '(have hu1 (Eq (Option U) (nthU (List.cons U r0 us0) Nat.zero) (Option.some U r))
        (Eq.mp (congrArg (fn [q :- (List U)] (Eq (Option U) (nthU q Nat.zero) (Option.some U r))) hu0) hu))
     '(have er (Eq U r0 r) (some_injU r0 r (Eq.trans (Eq.symm (nthU.eq_2 r0 us0)) hu1)))
     '(have hnr0 (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero r0) Bool.true))
        (Eq.mpr (congrArg (fn [q :- U] (Or (Eq Bool erasing Bool.false) (Eq Bool (nonzero q) Bool.true))) er) hnr))
     '(have hs0 (S52 chkf dec encTy cap erasing head (skels tail) (Prod.snd en) (skel head) v0 (Prod.fst en))
        (Eq.mp (entry52_or erasing r0 (S52 chkf dec encTy cap erasing head (skels tail) (Prod.snd en) (skel head) v0 (Prod.fst en)) hnr0) he0))
     '(constructor) '(exact v0)
     '(constructor)
     '(exact (congrArg (fn [q :- (List RV)] (rlookup q Nat.zero)) hr0))
     '(rw [(Eq.symm e)])
     (list 'change (list 'S52 'chkf 'dec 'encTy 'cap 'erasing '(lift 1 0 head) GT 'en '(skel (lift 1 0 head)) 'v0
                         (list 'lookup GT 0 '(skel (lift 1 0 head)) 'en)))
     (list 'exact (list 'Eq.mpr (list 's52_lift_fam 'chkf 'dec 'encTy 'cap 'erasing '(skels tail) 'head '(And.left hw) 0 '(skel head)
                                      '(Prod.snd en) '(Prod.fst en) 'v0
                                      (list 'fn '[s :- Sk] (list 'lookup GT 0 's 'en)))
                         (list 'Eq.mpr '(congrArg (fn [z :- (Car (skel head))]
                                          (S52 chkf dec encTy cap erasing head (skels tail) (Prod.snd en) (skel head) v0 z))
                                          (coe_self (skel head) (Prod.fst en)))
                               'hs0)))]))

(prove! 'var52 '[D :- (List Exp)]
  (VAR-MOTIVE 'D)
  '[(induction D)
    (intro hw i A h) (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
    (intro hw i) (cases i) (all_goals (intro A h us r rho en hu hnr he))
    (exact (var52_succ chkf dec encTy cap erasing head tail n A us r rho en hw ih_tail h hu hnr he))
    (exact (var52_zero chkf dec encTy cap erasing head tail A us r rho en hw h hu hnr he))])
