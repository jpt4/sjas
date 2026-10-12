(ns lcert.formal.theorem4
  "F5 — assembling Theorem 4 (R4-metatheory.md §5) from the adequacy lemmas.

  eval.clj states Adeq and Theorem_4 and proves one lemma per SkJ term
  constructor (adeq_*).  Two obligations were left open there.  This
  namespace discharges both, then proves Theorem 4.  skOf agrees with
  simple typing off the branch-list spine, and a derivable term has argsOK.
  adeq_skj inducts on SkJ, feeding the adequacy lemmas; the budget induction
  is strong because reflect decodes a certificate at a smaller budget.
  theorem4_spec inhabits the constant Theorem_4.  corollary51 is Corollary
  5.1, with ConvCase kept as a hypothesis.  corollary51_conv is the same
  statement with that hypothesis discharged by conv_all (Lemma 3.2 is the
  Conv case).  corollary51_nodes is the certificate-size half of Theorem 4′
  at Θₙ: an R denotation has at most n internal nodes, by theorem3.

  skOf (carrier.clj) reads one skeleton off a term.  On a branch list it
  returns none: sBnil and sBcons type bnil and bcons at every skeleton
  Lbl → s, so there is no single skeleton to return (skeletons.clj, brHead).
  argsOK does not exclude that case.  Its wildcard clause is true for bnil,
  and SkJ.sBnil types bnil, so

      SkJ false G t s  ∧  argsOK G t = true

  does not imply skOf G t = some s.  The counterexample is t = bnil
  (skOf_bnil_none, brHead_bnil, argsOK_bnil_true).  The repair is the extra
  hypothesis brHead t = false, i.e. the skOf-spine of t (the then-branch of
  if, the body of λ, the function of an application) does not end in a
  branch list.  Application and let, the two places den reads skOf, already
  require skOf of the argument or the scrutinee to be some; br_of_skSome
  turns that into brHead = false, so skOf_agree applies there.

  Goal order.  all_goals walks the goals from front to back and leaves each
  unsolved goal at the front, which reverses the unsolved ones.  skOf_agree
  runs three such passes (intro, noConfusion, try rfl) and therefore
  discharges the six non-rfl cases in reverse constructor order.  skOf_brHead
  runs four (it also intros the context) and discharges its three recursive
  cases in forward order.  Reordering either script without recounting the
  passes applies a proof to the wrong constructor."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.eval]
            [lcert.formal.skeletons]
            [lcert.formal.skof]
            [lcert.formal.substitution]
            [lcert.formal.lemma36]
            [lcert.formal.convcase]))

;; --- the clauses of skOf that inspect a recursive result ----------------------

;; arrCod (A → B) = some B.  skOf of an application is arrCod of the
;; function's skeleton, and sApp concludes at the codomain.
;; ‹lemma arrCod_arr: statement and proof elided, lines 57–59›

;; The none branch of skOf's λ clause, stated on an arbitrary option and
;; then read back at skOf (the some branch is lamOpt_some / skOf_lam).
;; ‹lemma lamOpt_none: statement and proof elided, lines 63–69›

;; ‹lemma skOf_lam_none: statement and proof elided, lines 71–74›

;; ‹lemma appOpt_none: statement and proof elided, lines 76–82›

;; ‹lemma skOf_app_none: statement and proof elided, lines 84–87›

;; bnil is the witness that skOf can be none of a simply typed term.
;; ‹lemma skOf_bnil_none: statement and proof elided, lines 90–92›

;; --- brHead is the spine along which skOf returns none -----------------------

;; ‹lemma brHead_bnil: statement and proof elided, lines 96–96›
;; ‹lemma brHead_bcons: statement and proof elided, lines 97–97›
;; ‹lemma brHead_ite: statement and proof elided, lines 98–100›
;; ‹lemma brHead_lam: statement and proof elided, lines 101–103›
;; ‹lemma brHead_app: statement and proof elided, lines 104–106›

;; argsOK unfolds on the constructors Theorem 4 splits.  rfl, so the
;; conjunction in an induction hypothesis is the unfolded one.
;; ‹lemma argsOK_bnil_true: statement and proof elided, lines 110–112›

;; ‹lemma argsOK_ite_eq: statement and proof elided, lines 114–117›

;; ‹lemma argsOK_lam_eq: statement and proof elided, lines 119–121›

;; ‹lemma argsOK_app_eq: statement and proof elided, lines 123–128›

;; If the skOf-spine ends in a branch list, skOf is none in every context.
;; ‹lemma skOf_brHead: statement and proof elided, lines 131–141›

;; A Boolean that cannot be true is false (the same fact as bool_false_of
;; in lemma36.clj, restated so this file does not depend on that proof).
;; ‹lemma not_true_is_false: statement and proof elided, lines 145–150›

;; The converse direction used by application and let: a defined skOf
;; rules the branch-list spine out, which is skOf_agree's third hypothesis.
;; ‹lemma br_of_skSome: statement and proof elided, lines 154–159›

;; --- agreement ---------------------------------------------------------------

;; skOf_agree.  For a term (not a skeleton-well-formed type), argsOK and
;; brHead = false recover the skeleton SkJ assigned:
;;
;;   SkJ false G t s  ∧  argsOK G t = true  ∧  brHead t = false
;;     ⟹  skOf G t = some s.
;;
;; Formation (w = true) is refuted.  Every constructor whose skOf is the
;; annotation's skeleton closes by rfl.  The six that do not:
;;   sApp   — codomain of the function's skeleton (skOf_app, arrCod_arr);
;;   sLam   — an arrow from the annotation to the body's skeleton;
;;   sBcons, sBnil — brHead is true, contradictory;
;;   sIte   — the then-branch, whose argsOK conjunct is true;
;;   sVar   — the context lookup the rule already stores.
;; ‹lemma skOf_agree: statement and proof elided, lines 176–201›

;; --- derivable terms have argsOK -------------------------------------------
;;
;; argsOK is the hypothesis Adeq takes in place of the paper's "no branch
;; list in argument position".  A runtime derivation supplies it: every App
;; argument and every Let scrutinee is typed at a formed type, so that type
;; is clean (skj_clean of its Tl formation) and skOf_rt / skOf_tl return its
;; skeleton.  Tl is proved first; it has no Rt premise.  The node branch of
;; recSyn is the one context that is not definitionally the one argsOK
;; recurses into: y1Ty and y2Ty have P's skeleton only by skel_y1Ty and
;; skel_y2Ty, and argsOK_recs_tn transports along that equality.

;; ‹lemma argsOK_base: statement and proof elided, lines 214–217›

;; ‹lemma argsOK_const: statement and proof elided, lines 219–225›

;; ‹lemma match_sk_true: statement and proof elided, lines 227–230›

;; ‹lemma argsOK_app_asm: statement and proof elided, lines 232–240›

;; ‹lemma argsOK_let_eq: statement and proof elided, lines 242–248›

;; ‹lemma letOpt_dep: statement and proof elided, lines 250–261›

;; ‹lemma let_ok_prod: statement and proof elided, lines 263–269›

(println :let-ok)

;; ‹lemma argsOK_ctx: statement and proof elided, lines 273–275›

(println :args-helpers-ok)

;; ‹lemma recs_ctx_eq: statement and proof elided, lines 279–292›

;; ‹lemma argsOK_recs_tn: statement and proof elided, lines 294–305›

(println :recs-ok)

;; ‹lemma tl_argsOK: statement and proof elided, lines 309–428›

(println :tl-args-ok)

;; ‹lemma rt_argsOK: statement and proof elided, lines 432–556›

(println :rt-args-ok)

;; --- what reflect's decoded term needs ---------------------------------------
;;
;; CheckSpec hands Theorem 4 a runtime derivation at a smaller budget.  The
;; outer induction applies Adeq there, which needs a skeleton judgment at
;; thetaSk (lemma25_rt, transported by skels_theta), argsOK in that context
;; (rt_argsOK, transported by argsOK_ctx), and the token environment related
;; to itself (tokens_rel).  The arithmetic m < cap is omega on
;; m < cnodes(⟦r⟧) ≤ cap.

;; match-is-true means the option is some.
;; ‹lemma sk_witness: statement and proof elided, lines 570–577›

;; ‹lemma skels_theta: statement and proof elided, lines 579–583›

;; isBaseTy D picks the seven data types reflect may target.  Each of them
;; has a base skeleton, so rel there is equality, and each is closed, so E1
;; of CheckSpec applies.
;; ‹lemma baseSk_base: statement and proof elided, lines 588–592›

;; ‹lemma closed_of_base: statement and proof elided, lines 594–598›

;; ‹lemma skj_along: statement and proof elided, lines 600–603›

;; Drop the relation from an adequacy witness.  adeq_node's token premise,
;; and reflect's evidence, are evaluation only.
;; ‹lemma eval_only: statement and proof elided, lines 607–617›

;; An argument whose skOf-match succeeded is typed at the skeleton SkJ gave
;; it.  sk_witness recovers the option; br_of_skSome turns it into the
;; brHead hypothesis skOf_agree needs.
;; ‹lemma app_arg_sk: statement and proof elided, lines 622–628›

;; The outer match of let's argsOK clause is true whenever the inner one is:
;; the none branch is false, so the scrutinee's skOf is some.
;; ‹lemma let_match_some: statement and proof elided, lines 632–637›

;; Once skOf of the scrutinee is the product, the inner clause of argsOK is
;; the body's argsOK in the extended context.  subst rewrites the option
;; variable; the product is not a variable, so the equation is a hypothesis.
;; ‹lemma let_body_of: statement and proof elided, lines 642–652›

;; m < b and b ≤ c give m < c.  omega is a tactic, not a proof term, so the
;; step is its own lemma.  Nat.le_of_ble_eq_true returns Nat.le, which omega
;; accepts.
;; ‹lemma lt_le_omega: statement and proof elided, lines 657–659›

;; Theorem 4, reflect (R4-metatheory §5, the sRefl clause).
;;
;; r and e are adequate by the induction hypothesis.  The guard is
;; cnodes(⟦r⟧) ≤ cap ∧ chkf(⟦r⟧, ⌜D⌝).  Failure returns the error token
;; (adeq_refl_no).  Success: CheckSpec decodes ⟦r⟧ to Θₘ ⊢ t₂ : A with
;; m < cnodes(⟦r⟧), hence m < cap; E1 and closed_of_base give A = D; the
;; outer hypothesis at m evaluates t₂ in the token environment.  adeq_refl_ok
;; assembles the certificate.
;; ‹lemma adeq_refl_asm: statement and proof elided, lines 669–729›

(println :refl-asm-ok)

;; --- Theorem 4 ----------------------------------------------------------------
;;
;; Each SkJ term constructor is one lemma (case_*).  adeq_skj is the inner
;; induction; adeq_from_below / below_succ / adeq_below_all are the strong
;; induction on the budget (reflect is the only appeal to a smaller budget).
;; theorem4 is that statement; theorem4_spec is the constant Theorem_4 in
;; eval.clj, which reduces to it.  corollary51 is Corollary 5.1: at a base
;; data type the value is the canonical form of an element of V(D).

;; ‹lemma case_abort: statement and proof elided, lines 742–761›

;; ‹lemma case_ite: statement and proof elided, lines 763–799›

;; ‹lemma case_elim: statement and proof elided, lines 801–837›

;; ‹lemma case_succ: statement and proof elided, lines 839–858›

;; ‹lemma case_recN: statement and proof elided, lines 860–908›

;; ‹lemma case_caseL: statement and proof elided, lines 910–937›

;; ‹lemma case_bcons: statement and proof elided, lines 939–967›

;; ‹lemma case_snode: statement and proof elided, lines 969–1005›

;; ‹lemma case_recS: statement and proof elided, lines 1007–1068›

;; ‹lemma case_node: statement and proof elided, lines 1070–1133›

;; ‹lemma case_itR: statement and proof elided, lines 1135–1176›

;; ‹lemma case_sleaf: statement and proof elided, lines 1178–1194›

;; ‹lemma case_leaf: statement and proof elided, lines 1196–1212›

;; ‹lemma case_prn: statement and proof elided, lines 1214–1230›

;; ‹lemma case_lam: statement and proof elided, lines 1232–1251›

;; ‹lemma case_pair: statement and proof elided, lines 1253–1280›

;; ‹lemma case_chk: statement and proof elided, lines 1282–1307›

;; ‹lemma case_app: statement and proof elided, lines 1309–1367›

;; ‹lemma case_let: statement and proof elided, lines 1369–1423›

;; ‹lemma case_refl: statement and proof elided, lines 1425–1454›

;; ‹lemma case_insp: statement and proof elided, lines 1456–1531›

;; ‹lemma h1_oks: statement and proof elided, lines 1533–1581›

;; ‹lemma case_h1: statement and proof elided, lines 1583–1644›

;; Theorem 4, inner induction on the simple-typing derivation
;; (R4-metatheory §5).  Each term constructor is one adequacy lemma.
;; ‹lemma adeq_skj: statement and proof elided, lines 1648–1730›

;; Adeq at cap, given Adeq at every smaller budget.  The SkJ induction is
;; adeq_skj; reflect is the only constructor that uses the smaller budgets.
;; ‹lemma adeq_from_below: statement and proof elided, lines 1734–1741›

;; ‹lemma below_zero: statement and proof elided, lines 1743–1748›

;; Strong step: m < n + 1 is m = n or m < n.  The equal case is Adeq at n
;; from the hypothesis for everything smaller; the rest is the hypothesis.
;; ‹lemma below_succ: statement and proof elided, lines 1752–1764›

;; Theorem 4 (R4-metatheory §5): under CheckSpec, evalₙ terminates and is
;; adequate for every simply typed term with argsOK, at every budget.
;; The constant Theorem_4 in eval.clj is this statement.  Induction on the
;; budget needs the index in context first (induction: hypothesis not in
;; context, otherwise).
;; ‹lemma adeq_below_all: statement and proof elided, lines 1771–1779›

(thm theorem4
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (Adeq chkf dec encTy n))
  ;; ‹proof elided: lines 1786–1787 of the full file; kernel-checked›
  )

;; ‹lemma theorem4_spec: statement and proof elided, lines 1789–1790›

;; Corollary 5.1 (R4-metatheory §5).  A derivable term of a base data type,
;; at the token context Θₙ, evaluates under evalₙ.  Theorem 4 supplies the
;; value and, because the skeleton is a base skeleton, relates it by equality
;; to the denotation: the value is that denotation's canonical runtime form.
;; Lemma 3.6 puts the same denotation in V(D).  ConvCase stays a hypothesis,
;; as it does for Lemma 3.6.  The two environments (tokenEnv and tokEnvD)
;; denote the same element (tok_transfer).
(thm corollary51
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy),
   hconv :- (forall [k Nat] (ConvCase chkf dec encTy k)),
   n :- Nat, t :- Exp, D :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t D),
   hb :- (Eq Bool (isBaseTy D) Bool.true)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n (rtokens n) t v)
      (And (rel chkf dec encTy n (skel D) v
             (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n)))
        (And (V chkf dec encTy n D (skels (thetaD n)) (tokEnvD n) n (skel D)
               (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n)))
             (Eq (Car (skel D))
               (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n))
               (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n))))))))
  ;; ‹proof elided: lines 1816–1833 of the full file; kernel-checked›
  )

;; Corollary 5.1 with the Conv case discharged.  conv_all is ConvAll: every
;; checker satisfying CheckSpec has ConvCase at every budget (F_conv).  That
;; is the hypothesis corollary51 was holding.  The conclusion is unchanged:
;; evalₙ terminates at the denotation, and that denotation lies in V(D).
(thm corollary51_conv
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp, D :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t D),
   hb :- (Eq Bool (isBaseTy D) Bool.true)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy n (rtokens n) t v)
      (And (rel chkf dec encTy n (skel D) v
             (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n)))
        (And (V chkf dec encTy n D (skels (thetaD n)) (tokEnvD n) n (skel D)
               (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n)))
             (Eq (Car (skel D))
               (den chkf dec encTy n t (thetaSk n) (skel D) (tokenEnv n))
               (den chkf dec encTy n t (skels (thetaD n)) (skel D) (tokEnvD n))))))))
  ;; ‹proof elided: lines 1855–1855 of the full file; kernel-checked›
  )

;; Theorem 4′, the size clause, for the denotation evalₙ returns.  At Θₙ an
;; R term's value has at most n internal nodes (Theorem 3, footprint n, the
;; all-token environment).  conv_all discharges ConvCase, as in
;; corollary51_conv.  Combined with the equality at base skeletons, the
;; runtime certificate evalₙ returns is that tree.
(thm corollary51_nodes
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy),
   n :- Nat, t :- Exp,
   hd :- (Rt chkf (thetaD n) (thetaU n) t Exp.tR)]
  (LE.le (cnodes (den chkf dec encTy n t (skels (thetaD n)) Sk.cert (tokEnvD n))) n)
  ;; ‹proof elided: lines 1869–1872 of the full file; kernel-checked›
  )
