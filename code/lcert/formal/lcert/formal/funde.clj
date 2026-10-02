(ns lcert.formal.funde
  "F5 — the fundamental property of evalᴱ (R4-metatheory.md §5, Theorem 4′).

  A runtime derivation Γ ⊢ t :¹ A erases to a term e (Er).  In an environment
  related by envE, e evaluates under EvE, and the value is E-related to the
  denotation ⟦t⟧ⁿη.  The proof is strong induction on the budget, then
  induction on Er.  Reflect is the only case that appeals to a smaller
  budget, and it needs CheckSpec, as theorem4.clj's adeq_refl_asm does.

  denU is that denotation read at the carrier of usk A.  ⟦·⟧ returns a
  value in Car(skel A); Erel expects Car(uskSk(usk A)).  usk_skel identifies
  the two skeletons, and henv_of_usk (uskel.clj) identifies the two
  environments.  Eq.mp along the skeleton equation is the cast.  It is a
  function, as henv_of_usk is: the cast value lives in a type.

  The constant cases do not read the environment.  At 1, E's clause ignores
  the carrier value, so ⋆ is related however the cast computes.  At Bool,
  Nat and Lbl the cast reduces to ⟦·⟧ at that base skeleton, and den_*_at
  plus coe_self are the same rewrites Theorem 4 uses.  A variable of usage
  1 or ω is the environment entry, cast from lookup to ⟦var⟧ by var_carrier.

  λ is a closure of the erased body.  At Π₀ the arrow clause applies it to
  ⋆; at Π₁ and Πω it applies it to a related argument.  denU of the λ is
  ⟦λ⟧ cast along usk_skel, which at a Π is the two component casts, so
  applying it is the cast of the body's denotation (denU_lam0, denU_lam1,
  denU_lamw).  The body's induction hypothesis, in the environment extended
  by that argument, is then that value.  Application instantiates that
  clause at ⟦u⟧.  At Π₀ the runtime argument is ⋆ and u is a logical
  premise; at Π₁ and Πω the argument is evaluated and E-related.  The
  codomain is B[u/x], and E there is E at B (erel_subst_unit) because a
  term has skeleton Unit.  A pair is the pair of its components.  At Σ₀
  the first is erased to ⋆ and forgotten; at Σ₁ and Σω both are kept, and
  the second component's type B[x/y] is brought back to B by denU_unsubst.

  Let binds the scrutinee's two components.  Variable 0 is the second, at
  usage 1; variable 1 is the first, at the Σ's usage.  The body's judgment
  is at lift 2 0 C; usk_lift and denU_lift read that carrier back at C.
  den of let is the body under skOf of the scrutinee (den_letp_some).  The
  two new environment entries are the casts of denU's projections.  At Σ₀
  the runtime first component is unconstrained; the denotation still reads
  it.  The tail environment is envE at the body's usage vector, which is a
  summand of the conclusion's vector; restricting envE along vadd is the
  assembly's obligation, not this case's.

  if selects one branch at the same type.  elimBool's branches are judged
  at P[tt] and P[ff] and the conclusion at P[b]; a Boolean term has skeleton
  Unit, so those three types have one usage skeleton (skel_subst_eq) and
  the carriers agree (denU_ty).  caseLbl applies the branch list, and the
  conclusion P[a/x] is the codomain P after a Unit substitution.

  recN iterates the step.  The base is judged at P[zero] and the step at
  stepTy, both of which have the usage skeleton of P: zero, a successor and
  the numeral are terms.  denU of recN is the Nat.rec of those denotations.
  The conclusion is P[n], and that carrier is the one at P because the
  numeral has skeleton Unit.

  recSyn inducts on the code.  The leaf binds the label at usage ω.  The
  node binds the two recursive results at usage 1 and the two codes and the
  label at usage ω.  leafTy, nodeTy, y1Ty and y2Ty are substitutions of
  terms, so they share the motive's usage skeleton, and the node entries
  are those carriers cast along the equation.  recsDen is the Code.rec of
  the two methods.  denU of recS is that recursor (denU_recS): the cast
  stays outside Code.rec, as cast_iter stays outside Nat.rec.  The
  conclusion is P[c], and that carrier is the one at P because a code
  term has skeleton Unit.

  itR inducts on the certificate.  g, at Lbl ⊸ X, is applied to the label.
  h is applied to the token, the label, and the two recursive results;
  gTy and hTy quantify the motive under lifts, so each result is cast
  back to usk X.  A token is related to itself at ◇.  denU of itR is
  that Code.rec (denU_itR): the cast stays outside Code.rec.  adeqE_itR
  evaluates g, h, and the certificate, then folds them.  print returns
  the certificate's code.  chk′ returns Check of the two codes."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.erase]
            [lcert.formal.uskel]))

;; ⟦t⟧ⁿη at the carrier E reads.  The environment η is for usks(uskCtx D);
;; henv_of_usk sends it to the skeleton context ⟦·⟧ quantifies over.
(kdef denU
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [D (List Exp)] (forall [t Exp] (forall [A Exp]
          (=> (HEnv (usks (uskCtx D))) (Car (uskSk (usk A)))))))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, D :- (List Exp), t :- Exp, A :- Exp,
       eta :- (HEnv (usks (uskCtx D)))]
    (Eq.mp (congrArg Car (Eq.symm (usk_skel A)))
      (den chkf dec encTy n t (skels D) (skel A) (henv_of_usk D eta)))))

;; AdeqE n: the fundamental property at budget n.  The outer induction
;; hypothesis is AdeqE at every smaller budget.  Er is the derivation the
;; induction walks: it carries the erased term, which Rt, being a
;; proposition, cannot return.
(kdef AdeqE
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (=> Nat Prop))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat]
    (forall [D (List Exp)] (forall [us (List U)] (forall [t Exp] (forall [A Exp] (forall [e Exp]
      (=> (Er chkf D us t A e)
        (forall [rho (List RV)] (forall [eta (HEnv (usks (uskCtx D)))]
          (=> (envE chkf dec encTy n (uskCtx D) us rho eta)
            (Exists (fn [v :- RV]
              (And (EvalE chkf dec encTy n rho e v)
                   (Erel chkf dec encTy n (usk A) v
                     (denU chkf dec encTy n D t A eta))))))))))))))))

;; ⋆ : 1.  E at the unit skeleton is equality with ⋆, and it does not read
;; the denotation, so the cast does not have to compute.
(thm adeqE_star
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), rho :- (List RV),
   eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.star v)
         (Erel chkf dec encTy n (usk Exp.tUnit) v
           (denU chkf dec encTy n D Exp.star Exp.tUnit eta)))))
  (constructor) (exact RV.star)
  (constructor) (exact (EvE.eStar chkf dec encTy n rho))
  (rfl))

;; tt, ff : Bool.  denU reduces to ⟦·⟧ at Sk.bool, and that is the Boolean.
(thm adeqE_tt
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), rho :- (List RV),
   eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.tt v)
         (Erel chkf dec encTy n (usk Exp.tBool) v
           (denU chkf dec encTy n D Exp.tt Exp.tBool eta)))))
  (rw [(den_tt_at chkf dec encTy n (skels D) (skel Exp.tBool) (henv_of_usk D eta))])
  (rw [(coe_self Sk.bool Bool.true)])
  (constructor) (exact (RV.bool Bool.true))
  (constructor) (exact (EvE.eTT chkf dec encTy n rho))
  (rfl))

(thm adeqE_ff
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), rho :- (List RV),
   eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.ff v)
         (Erel chkf dec encTy n (usk Exp.tBool) v
           (denU chkf dec encTy n D Exp.ff Exp.tBool eta)))))
  (rw [(den_ff_at chkf dec encTy n (skels D) (skel Exp.tBool) (henv_of_usk D eta))])
  (rw [(coe_self Sk.bool Bool.false)])
  (constructor) (exact (RV.bool Bool.false))
  (constructor) (exact (EvE.eFF chkf dec encTy n rho))
  (rfl))

(thm adeqE_zero
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), rho :- (List RV),
   eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.zero v)
         (Erel chkf dec encTy n (usk Exp.tNat) v
           (denU chkf dec encTy n D Exp.zero Exp.tNat eta)))))
  (rw [(den_zero_at chkf dec encTy n (skels D) (skel Exp.tNat) (henv_of_usk D eta))])
  (rw [(coe_self Sk.nat 0)])
  (constructor) (exact (RV.nat 0))
  (constructor) (exact (EvE.eZero chkf dec encTy n rho))
  (rfl))

(thm adeqE_lbl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, l :- Nat, D :- (List Exp), rho :- (List RV),
   eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.lbl l) v)
         (Erel chkf dec encTy n (usk Exp.tLbl) v
           (denU chkf dec encTy n D (Exp.lbl l) Exp.tLbl eta)))))
  (rw [(den_lbl_at chkf dec encTy n l (skels D) (skel Exp.tLbl) (henv_of_usk D eta))])
  (rw [(coe_self Sk.lbl l)])
  (constructor) (exact (RV.lbl l))
  (constructor) (exact (EvE.eLbl chkf dec encTy n rho l))
  (rfl))

;; --- variables ----------------------------------------------------------------
;; The runtime value of a variable is the environment entry.  Usage 1 or ω
;; is E-related to the carrier entry; usage 0 is not, and the variable rule
;; excludes it.  The budget is named cap: cases on the index names the
;; predecessor n (env_at, eval.clj).

(a/defn nthUS [G :- (List USk), i :- Nat] (Option USk)
  (match G
    [nil (Option.none USk)]
    [(cons x rest) (match i
                     [zero (Option.some USk x)]
                     [(succ j) (nthUS rest j)])]))

(thm some_injUS [x :- USk, y :- USk,
                 h :- (Eq (Option USk) (Option.some USk x) (Option.some USk y))]
  (Eq USk x y)
  (cases h) (rfl))

(thm none_ne_someUS [u :- USk,
                     h :- (Eq (Option USk) (Option.none USk) (Option.some USk u))]
  False
  (cases h))

;; Nonzero usage is the E-clause.  Usage 0 is True, and the variable rule
;; has already excluded it.
(thm entryE_nz
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, r :- U, u :- USk, v :- RV, a :- (Car (uskSk u)),
   h :- (Eq Bool (nonzero r) Bool.true)]
  (Eq Prop (entryE chkf dec encTy cap r u v a) (Erel chkf dec encTy cap u v a))
  (cases r)
  (exact (Bool.noConfusion h))
  (rfl)
  (rfl))

;; The head of a usage-skeleton environment, read back by lookup, is the
;; component itself: the coercion is along one skeleton.
(thm lookup_usk_head
  [u :- USk, rest :- (List USk), a :- (Car (uskSk u)), e :- (HEnv (usks rest))]
  (Eq (Car (uskSk u))
    (lookup (usks (List.cons USk u rest)) 0 (uskSk u) (Prod.mk a e))
    a)
  (exact (coe_self (uskSk u) a)))

(thm envE_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, G :- (List USk)]
  (forall [rs (List U)] (forall [rho (List RV)] (forall [eta (HEnv (usks G))]
    (=> (envE chkf dec encTy cap G rs rho eta)
      (forall [i Nat] (forall [u USk] (forall [r U]
        (=> (Eq (Option USk) (nthUS G i) (Option.some USk u))
          (=> (Eq (Option U) (nthU rs i) (Option.some U r))
            (=> (Eq Bool (nonzero r) Bool.true)
              (Exists (fn [v :- RV]
                (And (Eq (Option RV) (rlookup rho i) (Option.some RV v))
                     (Erel chkf dec encTy cap u v
                       (lookup (usks G) i (uskSk u) eta)))))))))))))))
  (induction G)
  (intro rs) (intro rho) (intro eta) (intro hr) (intro i) (intro u) (intro r)
  (intro hu) (intro hrU) (intro hnz)
  (exact (False.elim (none_ne_someUS u (Eq.trans (Eq.symm (nthUS.eq_1 i)) hu))))
  (intro rs) (intro rho) (intro eta) (intro hr) (intro i) (intro u) (intro r)
  (intro hu) (intro hrU) (intro hnz)
  (cases i)
  (have hu2 (Eq (Option USk) (Option.some USk head) (Option.some USk u))
    (Eq.trans (Eq.symm (nthUS.eq_2 head tail)) hu))
  (have hhd (Eq USk head u) (some_injUS head u hu2))
  (subst hhd)
  (refine' (exT U _ _ hr _)) (intro r0 hex)
  (refine' (exT (List U) _ _ hex _)) (intro rs2 hex2)
  (refine' (exT RV _ _ hex2 _)) (intro v hex3)
  (refine' (exT (List RV) _ _ hex3 _)) (intro rho2 hp)
  (have hrs (Eq (List U) rs (List.cons U r0 rs2)) (And.left hp))
  (have hl (Eq (List RV) rho (List.cons RV v rho2)) (And.left (And.right hp)))
  (have he (entryE chkf dec encTy cap r0 u v (Prod.fst eta))
    (And.right (And.right (And.right hp))))
  (have hru (Eq (Option U) (Option.some U r0) (Option.some U r))
    (Eq.trans (Eq.symm (nthU.eq_2 r0 rs2))
      (Eq.trans (Eq.symm (congrArg (fn [xs :- (List U)] (nthU xs 0)) hrs)) hrU)))
  (have hru2 (Eq U r0 r) (some_injU r0 r hru))
  (subst hru2)
  (have hr0 (Eq (Option RV) (rlookup rho 0) (Option.some RV v))
    (Eq.trans (congrArg (fn [xs :- (List RV)] (rlookup xs 0)) hl) (rlookup_zero v rho2)))
  (have heE (Erel chkf dec encTy cap u v (Prod.fst eta))
    (Eq.mp (entryE_nz chkf dec encTy cap r u v (Prod.fst eta) hnz) he))
  (have hv (Erel chkf dec encTy cap u v
             (lookup (usks (List.cons USk u tail)) 0 (uskSk u) eta))
    (erel_car chkf dec encTy cap u v (Prod.fst eta)
      (lookup (usks (List.cons USk u tail)) 0 (uskSk u) (Prod.mk (Prod.fst eta) (Prod.snd eta)))
      heE
      (Eq.symm (lookup_usk_head u tail (Prod.fst eta) (Prod.snd eta)))))
  (constructor) (exact v) (constructor) (exact hr0) (exact hv)
  (refine' (exT U _ _ hr _)) (intro r0 hex)
  (refine' (exT (List U) _ _ hex _)) (intro rs2 hex2)
  (refine' (exT RV _ _ hex2 _)) (intro v0 hex3)
  (refine' (exT (List RV) _ _ hex3 _)) (intro rho2 hp)
  (have hrs (Eq (List U) rs (List.cons U r0 rs2)) (And.left hp))
  (have hl (Eq (List RV) rho (List.cons RV v0 rho2)) (And.left (And.right hp)))
  (have hrest (envE chkf dec encTy cap tail rs2 rho2 (Prod.snd eta))
    (And.left (And.right (And.right hp))))
  (have hs3 (Eq (Option USk) (nthUS tail n) (Option.some USk u))
    (Eq.trans (Eq.symm (nthUS.eq_3 head tail n)) hu))
  (have hruS (Eq (Option U) (nthU rs2 n) (Option.some U r))
    (Eq.trans (Eq.symm (nthU.eq_3 r0 rs2 n))
      (Eq.trans (Eq.symm (congrArg (fn [xs :- (List U)] (nthU xs (Nat.succ n))) hrs)) hrU)))
  (refine' (exT RV _ _ (ih_tail rs2 rho2 (Prod.snd eta) hrest n u r hs3 hruS hnz) _))
  (intro w hand)
  (have hrn (Eq (Option RV) (rlookup rho2 n) (Option.some RV w)) (And.left hand))
  (have hrel (Erel chkf dec encTy cap u w (lookup (usks tail) n (uskSk u) (Prod.snd eta)))
    (And.right hand))
  (have hr1 (Eq (Option RV) (rlookup rho (Nat.succ n)) (Option.some RV w))
    (Eq.trans (congrArg (fn [xs :- (List RV)] (rlookup xs (Nat.succ n))) hl)
              (Eq.trans (rlookup_succ v0 rho2 n) hrn)))
  (constructor) (exact w) (constructor) (exact hr1) (exact hrel))

;; lookup does not see a propositional equality of contexts: the environment
;; is Eq.mp'd, and cases on that parameter reduces it (dflt_cast).
(thm lookup_mp
  [G1 :- (List Sk), G2 :- (List Sk), e :- (Eq (List Sk) G1 G2),
   i :- Nat, s :- Sk, eta :- (HEnv G1)]
  (Eq (Car s)
    (lookup G2 i s (Eq.mp (congrArg HEnv e) eta))
    (lookup G1 i s eta))
  (cases e)
  (rfl))

;; The same for the skeleton lookup coerces into.
(thm lookup_sk
  [G :- (List Sk), i :- Nat, s1 :- Sk, s2 :- Sk,
   e :- (Eq Sk s1 s2), eta :- (HEnv G)]
  (Eq (Car s2)
    (Eq.mp (congrArg Car e) (lookup G i s1 eta))
    (lookup G i s2 eta))
  (cases e)
  (rfl))

;; congrArg through uskSk agrees with congrArg of the composite.  The
;; variable cast uses the composite; lookup_sk wants the skeleton equation.
(thm congr_uskSk
  [u1 :- USk, u2 :- USk, e :- (Eq USk u1 u2), a :- (Car (uskSk u1))]
  (Eq (Car (uskSk u2))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) e) a)
    (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w)) e)) a))
  (cases e)
  (rfl))


;; den inside denU, at a variable.  rw cannot see under the cast, so the
;; equation is congrArg of den_var_at.  Conversion unfolds denU.
(thm denU_var
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), i :- Nat, A :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk (lift (+ i 1) 0 A))))
    (denU chkf dec encTy n D (Exp.var i) (lift (+ i 1) 0 A) eta)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift (+ i 1) 0 A))))
      (lookup (skels D) i (skel (lift (+ i 1) 0 A)) (henv_of_usk D eta))))
  (exact (congrArg
    (fn [x :- (Car (skel (lift (+ i 1) 0 A)))]
      (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift (+ i 1) 0 A)))) x))
    (den_var_at chkf dec encTy n i (skels D) (skel (lift (+ i 1) 0 A))
      (henv_of_usk D eta)))))

;; The environment component, cast along usk_lift, is ⟦var i⟧.  Each step is
;; one of lookup_sk, lookup_mp, or denU_var, so the carriers stay aligned.
(thm var_carrier
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), i :- Nat, A :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk (lift (+ i 1) 0 A))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm (usk_lift A (+ i 1) 0)))
      (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta))
    (denU chkf dec encTy n D (Exp.var i) (lift (+ i 1) 0 A) eta))
  (exact (Eq.trans
    (Eq.trans
      (congr_uskSk (usk A) (usk (lift (+ i 1) 0 A))
        (Eq.symm (usk_lift A (+ i 1) 0))
        (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta))
      (lookup_sk (usks (uskCtx D)) i
        (uskSk (usk A)) (uskSk (usk (lift (+ i 1) 0 A)))
        (congrArg (fn [w :- USk] (uskSk w)) (Eq.symm (usk_lift A (+ i 1) 0)))
        eta))
    (Eq.trans
      (Eq.trans
        (Eq.symm (lookup_mp (usks (uskCtx D)) (skels D) (usk_ctx D) i
                   (uskSk (usk (lift (+ i 1) 0 A))) eta))
        (Eq.symm (lookup_sk (skels D) i
                   (skel (lift (+ i 1) 0 A))
                   (uskSk (usk (lift (+ i 1) 0 A)))
                   (Eq.symm (usk_skel (lift (+ i 1) 0 A)))
                   (henv_of_usk D eta))))
      (Eq.symm (denU_var chkf dec encTy n D i A eta))))))

;; nth of the usage-skeleton context is usk of nth of the type context.
(thm uskCtx_nth [D :- (List Exp)]
  (forall [i Nat] (forall [A Exp]
    (=> (Eq (Option Exp) (nthE D i) (Option.some Exp A))
        (Eq (Option USk) (nthUS (uskCtx D) i) (Option.some USk (usk A))))))
  (induction D)
  (intro i) (intro A) (intro h)
  (exact (False.elim (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 i)) h))))
  (intro i) (intro A) (intro h)
  (cases i)
  (have hs (Eq (Option Exp) (Option.some Exp head) (Option.some Exp A))
    (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h))
  (have heq (Eq Exp head A) (some_inj head A hs))
  (subst heq)
  (exact (nthUS.eq_2 (usk A) (uskCtx tail)))
  (have hs3 (Eq (Option Exp) (nthE tail n) (Option.some Exp A))
    (Eq.trans (Eq.symm (nthE.eq_3 head tail n)) h))
  (exact (Eq.trans
    (congrArg (fn [g :- (List USk)] (nthUS g (Nat.succ n))) (uskCtx_cons head tail))
    (Eq.trans (nthUS.eq_3 (usk head) (uskCtx tail) n) (ih_tail n A hs3)))))

;; Variables (Theorem 4′).  The entry has usage 1 or ω, so envE_at finds a
;; value E-related to the carrier component.  The conclusion type is the
;; lifted context type; usk_lift and var_carrier move the witness there.
(thm adeqE_var
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us :- (List U), i :- Nat, A :- Exp, r :- U,
   hA :- (Eq (Option Exp) (nthE D i) (Option.some Exp A)),
   hu :- (Eq (Option U) (nthU us i) (Option.some U r)),
   hnz :- (Eq Bool (nonzero r) Bool.true),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us rho eta)]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.var i) v)
         (Erel chkf dec encTy n (usk (lift (+ i 1) 0 A)) v
           (denU chkf dec encTy n D (Exp.var i) (lift (+ i 1) 0 A) eta)))))
  (have hUS (Eq (Option USk) (nthUS (uskCtx D) i) (Option.some USk (usk A)))
    (uskCtx_nth D i A hA))
  (refine' (exT RV _ _ (envE_at chkf dec encTy n (uskCtx D) us rho eta henv i (usk A) r hUS hu hnz) _))
  (intro v hand)
  (have hl (Eq (Option RV) (rlookup rho i) (Option.some RV v)) (And.left hand))
  (have hv (Erel chkf dec encTy n (usk A) v
             (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta))
    (And.right hand))
  (have hv2 (Erel chkf dec encTy n (usk (lift (+ i 1) 0 A)) v
              (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm (usk_lift A (+ i 1) 0)))
                (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta)))
    (erel_at chkf dec encTy n (usk A) (usk (lift (+ i 1) 0 A)) v
      (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta) hv
      (Eq.symm (usk_lift A (+ i 1) 0))))
  (have hv3 (Erel chkf dec encTy n (usk (lift (+ i 1) 0 A)) v
              (denU chkf dec encTy n D (Exp.var i) (lift (+ i 1) 0 A) eta))
    (erel_car chkf dec encTy n (usk (lift (+ i 1) 0 A)) v
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm (usk_lift A (+ i 1) 0)))
        (lookup (usks (uskCtx D)) i (uskSk (usk A)) eta))
      (denU chkf dec encTy n D (Exp.var i) (lift (+ i 1) 0 A) eta)
      hv2 (var_carrier chkf dec encTy n D i A eta)))
  (constructor) (exact v)
  (constructor) (exact (EvE.eVar chkf dec encTy n rho i v hl))
  (exact hv3))

;; Applying a function cast along skeleton equations is the cast of the
;; application.  The equations are parameters, so cases reduces Eq.mp.
(thm cast_app
  [x1 :- Sk, y1 :- Sk, x2 :- Sk, y2 :- Sk,
   ex :- (Eq Sk x1 x2), ey :- (Eq Sk y1 y2),
   f :- (Car (Sk.arr x1 y1)), a :- (Car x1)]
  (Eq (Car y2)
    ((Eq.mp (congrArg Car
              (Eq.trans (congrArg (fn [x :- Sk] (Sk.arr x y1)) ex)
                        (congrArg (fn [y :- Sk] (Sk.arr x2 y)) ey)))
            f)
     (Eq.mp (congrArg Car ex) a))
    (Eq.mp (congrArg Car ey) (f a)))
  (cases ex)
  (cases ey)
  (rfl))

;; A cast of a cons-environment is the cons of the casts.  Parameters, so
;; cases reduces Eq.mp (the same reason as cast_app).
(thm henv_prod
  [s1 :- Sk, s2 :- Sk, es :- (Eq Sk s1 s2),
   g1 :- (List Sk), g2 :- (List Sk), eg :- (Eq (List Sk) g1 g2),
   a :- (Car s1), b :- (HEnv g1)]
  (Eq (HEnv (List.cons Sk s2 g2))
    (Eq.mp (congrArg HEnv
             (Eq.trans (congrArg (fn [s :- Sk] (List.cons Sk s g1)) es)
                       (congrArg (fn [g :- (List Sk)] (List.cons Sk s2 g)) eg)))
           (Prod.mk a b))
    (Prod.mk (Eq.mp (congrArg Car es) a) (Eq.mp (congrArg HEnv eg) b)))
  (cases es)
  (cases eg)
  (rfl))

;; usk_skel at Π₀ is the two component casts, in this order.  rfl, so the
;; proof reduces.  Usage 1 and ω reduce to the same term (usk_skel cases on
;; the usage and then builds this trans), which is why cast_app_pi matches
;; denU at every Π.
(thm usk_skel_pi0 [A :- Exp, B :- Exp]
  (Eq (Eq Sk (uskSk (usk (Exp.tPi U.u0 A B))) (skel (Exp.tPi U.u0 A B)))
    (usk_skel (Exp.tPi U.u0 A B))
    (Eq.trans
      (congrArg (fn [x :- Sk] (Sk.arr x (uskSk (usk B)))) (usk_skel A))
      (congrArg (fn [y :- Sk] (Sk.arr (skel A) y)) (usk_skel B))))
  (rfl))

;; Pointwise, denU's cast of a function is the cast of its application.
;; The arrow equation is the symm of usk_skel's trans, so it matches denU
;; once the usage is a constructor and usk_skel reduces.  The skeletons are
;; parameters: cases on an equation whose sides are computed (uskSk, skel)
;; drops the function (the attempt with those sides failed that way).
(thm cast_app_pi
  [d :- Sk, c :- Sk, sD :- Sk, sC :- Sk,
   eA :- (Eq Sk d sD), eB :- (Eq Sk c sC),
   f :- (Car (Sk.arr sD sC)), a :- (Car d)]
  (Eq (Car c)
    ((Eq.mp (congrArg Car (Eq.symm (Eq.trans
              (congrArg (fn [x :- Sk] (Sk.arr x c)) eA)
              (congrArg (fn [y :- Sk] (Sk.arr sD y)) eB))))
            f)
     a)
    (Eq.mp (congrArg Car (Eq.symm eB))
      (f (Eq.mp (congrArg Car eA) a))))
  (cases eA)
  (cases eB)
  (rfl))

;; Extending a context transports the carrier environment to the pair of
;; the cast head and the transported tail.  usk_ctx of a cons reduces to
;; henv_prod's trans, so this is that lemma at usk_skel and usk_ctx.
(thm henv_ext
  [A :- Exp, D :- (List Exp),
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (HEnv (skels (List.cons Exp A D)))
    (henv_of_usk (List.cons Exp A D) (Prod.mk alpha eta))
    (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
             (henv_of_usk D eta)))
  (exact (henv_prod (uskSk (usk A)) (skel A) (usk_skel A)
                    (usks (uskCtx D)) (skels D) (usk_ctx D)
                    alpha eta)))

;; ⟦λ(r : A). t⟧ at the arrow, applied to v, is ⟦t⟧ in the extended
;; environment.  den_lam_arr (conversion.clj) is the same fact; it is
;; restated here so this file does not depend on that namespace.
(thm denE_lam_arr
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, r :- U, A :- Exp, t :- Exp, G :- (List Sk), sb :- Sk,
   en :- (HEnv G), v :- (Car (skel A))]
  (Eq (Car sb)
    ((den chkf dec encTy n (Exp.lam r A t) G (Sk.arr (skel A) sb) en) v)
    (den chkf dec encTy n t (List.cons Sk (skel A) G) sb (Prod.mk v en)))
  (rw [(den_lam_at chkf dec encTy n r A t G (Sk.arr (skel A) sb) en)]))

;; The three usages are separate theorems.  A single statement quantified
;; over the usage does not typecheck: until the usage is a constructor,
;; usk of the Π is not an arrow, so denU is not a function.

(thm denU_lam_cast
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.u0 A t) (Exp.tPi U.u0 A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.u0 A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.lam U.u0 A t) (skels D)
                (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_lam_body
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.u0 A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n t (List.cons Sk (skel A) (skels D)) (skel B)
           (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
                    (henv_of_usk D eta)))))
  (exact (congrArg
           (fn [x :- (Car (skel B))]
             (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
           (denE_lam_arr chkf dec encTy n U.u0 A t (skels D) (skel B)
             (henv_of_usk D eta)
             (Eq.mp (congrArg Car (usk_skel A)) alpha)))))

;; denU of the body uses henv_of_usk of the extended context.  henv_ext
;; says that environment is the pair the λ's application builds, and the
;; outer cast is denU's own cast, so the two sides meet.
(thm denU_body_env
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n t (List.cons Sk (skel A) (skels D)) (skel B)
           (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
                    (henv_of_usk D eta)))))
  (exact (congrArg
           (fn [en :- (HEnv (skels (List.cons Exp A D)))]
             (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
               (den chkf dec encTy n t (skels (List.cons Exp A D)) (skel B) en)))
           (henv_ext A D alpha eta))))

;; Theorem 4′, the λ clause at Π₀: applying ⟦λ(0:A). t⟧ to α is ⟦t⟧ in the
;; environment extended by α.  The runtime argument is ⋆; it does not appear
;; here, because the denotation of a usage-0 λ does not read it.
(thm denU_lam0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.u0 A t) (Exp.tPi U.u0 A B) eta) alpha)
    (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
  (exact (Eq.trans
           (denU_lam_cast chkf dec encTy n D A B t alpha eta)
           (Eq.trans
             (denU_lam_body chkf dec encTy n D A B t alpha eta)
             (Eq.symm (denU_body_env chkf dec encTy n D A B t alpha eta))))))

(thm denU_lam_cast1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.u1 A t) (Exp.tPi U.u1 A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.u1 A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.lam U.u1 A t) (skels D)
                (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_lam_body1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.u1 A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n t (List.cons Sk (skel A) (skels D)) (skel B)
           (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
                    (henv_of_usk D eta)))))
  (exact (congrArg
           (fn [x :- (Car (skel B))]
             (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
           (denE_lam_arr chkf dec encTy n U.u1 A t (skels D) (skel B)
             (henv_of_usk D eta)
             (Eq.mp (congrArg Car (usk_skel A)) alpha)))))

(thm denU_lam1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.u1 A t) (Exp.tPi U.u1 A B) eta) alpha)
    (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
  (exact (Eq.trans
           (denU_lam_cast1 chkf dec encTy n D A B t alpha eta)
           (Eq.trans
             (denU_lam_body1 chkf dec encTy n D A B t alpha eta)
             (Eq.symm (denU_body_env chkf dec encTy n D A B t alpha eta))))))

(thm denU_lam_castw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.uw A t) (Exp.tPi U.uw A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.uw A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.lam U.uw A t) (skels D)
                (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_lam_bodyw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n (Exp.lam U.uw A t) (skels D)
            (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n t (List.cons Sk (skel A) (skels D)) (skel B)
           (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
                    (henv_of_usk D eta)))))
  (exact (congrArg
           (fn [x :- (Car (skel B))]
             (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
           (denE_lam_arr chkf dec encTy n U.uw A t (skels D) (skel B)
             (henv_of_usk D eta)
             (Eq.mp (congrArg Car (usk_skel A)) alpha)))))

(thm denU_lamw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, t :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D (Exp.lam U.uw A t) (Exp.tPi U.uw A B) eta) alpha)
    (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
  (exact (Eq.trans
           (denU_lam_castw chkf dec encTy n D A B t alpha eta)
           (Eq.trans
             (denU_lam_bodyw chkf dec encTy n D A B t alpha eta)
             (Eq.symm (denU_body_env chkf dec encTy n D A B t alpha eta))))))

;; Theorem 4′, fundamental property, λ at Π₀.  The value is the closure of
;; the erased body.  The arrow clause applies it to ⋆; the body's hypothesis
;; runs in the environment extended by ⋆ and an arbitrary carrier, and
;; denU_lam0 moves that relation onto the application of ⟦λ⟧.
(thm adeqE_lam0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us :- (List U),
   A :- Exp, B :- Exp, t :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us rho eta),
   ih :- (forall [rho2 (List RV)]
           (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp A D))))]
             (=> (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                       (List.cons U U.u0 us) rho2 eta2)
               (Exists (fn [w :- RV]
                 (And (EvalE chkf dec encTy n rho2 te w)
                      (Erel chkf dec encTy n (usk B) w
                        (denU chkf dec encTy n (List.cons Exp A D) t B eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.lam U.u0 A te) v)
         (Erel chkf dec encTy n (usk (Exp.tPi U.u0 A B)) v
           (denU chkf dec encTy n D (Exp.lam U.u0 A t) (Exp.tPi U.u0 A B) eta)))))
  (constructor) (exact (RV.clos rho te))
  (constructor) (exact (EvE.eLam chkf dec encTy n rho U.u0 A te))
  (intro alpha)
  (have hext (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
               (List.cons U U.u0 us) (List.cons RV RV.star rho) (Prod.mk alpha eta))
    (envE_cons0 chkf dec encTy n (usk A) (uskCtx D) us RV.star rho alpha eta henv))
  (refine' (exT RV _ _ (ih (List.cons RV RV.star rho) (Prod.mk alpha eta) hext) _))
  (intro w hand)
  (have hev (EvalE chkf dec encTy n (List.cons RV RV.star rho) te w) (And.left hand))
  (have hrel (Erel chkf dec encTy n (usk B) w
               (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
    (And.right hand))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.apClos chkf dec encTy n rho te RV.star w hev))
  (exact (erel_car chkf dec encTy n (usk B) w
           (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta))
           ((denU chkf dec encTy n D (Exp.lam U.u0 A t) (Exp.tPi U.u0 A B) eta) alpha)
           hrel
           (Eq.symm (denU_lam0 chkf dec encTy n D A B t alpha eta)))))

;; Theorem 4′, fundamental property, λ at Π₁.  The argument is kept, and it
;; must be E-related; envE_cons1 is that extension.
(thm adeqE_lam1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us :- (List U),
   A :- Exp, B :- Exp, t :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us rho eta),
   ih :- (forall [rho2 (List RV)]
           (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp A D))))]
             (=> (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                       (List.cons U U.u1 us) rho2 eta2)
               (Exists (fn [w :- RV]
                 (And (EvalE chkf dec encTy n rho2 te w)
                      (Erel chkf dec encTy n (usk B) w
                        (denU chkf dec encTy n (List.cons Exp A D) t B eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.lam U.u1 A te) v)
         (Erel chkf dec encTy n (usk (Exp.tPi U.u1 A B)) v
           (denU chkf dec encTy n D (Exp.lam U.u1 A t) (Exp.tPi U.u1 A B) eta)))))
  (constructor) (exact (RV.clos rho te))
  (constructor) (exact (EvE.eLam chkf dec encTy n rho U.u1 A te))
  (intro arg) (intro alpha) (intro harg)
  (have hext (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
               (List.cons U U.u1 us) (List.cons RV arg rho) (Prod.mk alpha eta))
    (envE_cons1 chkf dec encTy n (usk A) (uskCtx D) us arg rho alpha eta harg henv))
  (refine' (exT RV _ _ (ih (List.cons RV arg rho) (Prod.mk alpha eta) hext) _))
  (intro w hand)
  (have hev (EvalE chkf dec encTy n (List.cons RV arg rho) te w) (And.left hand))
  (have hrel (Erel chkf dec encTy n (usk B) w
               (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
    (And.right hand))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.apClos chkf dec encTy n rho te arg w hev))
  (exact (erel_car chkf dec encTy n (usk B) w
           (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta))
           ((denU chkf dec encTy n D (Exp.lam U.u1 A t) (Exp.tPi U.u1 A B) eta) alpha)
           hrel
           (Eq.symm (denU_lam1 chkf dec encTy n D A B t alpha eta)))))

;; Theorem 4′, fundamental property, λ at Πω.  The same clause as usage 1.
(thm adeqE_lamw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us :- (List U),
   A :- Exp, B :- Exp, t :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us rho eta),
   ih :- (forall [rho2 (List RV)]
           (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp A D))))]
             (=> (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                       (List.cons U U.uw us) rho2 eta2)
               (Exists (fn [w :- RV]
                 (And (EvalE chkf dec encTy n rho2 te w)
                      (Erel chkf dec encTy n (usk B) w
                        (denU chkf dec encTy n (List.cons Exp A D) t B eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.lam U.uw A te) v)
         (Erel chkf dec encTy n (usk (Exp.tPi U.uw A B)) v
           (denU chkf dec encTy n D (Exp.lam U.uw A t) (Exp.tPi U.uw A B) eta)))))
  (constructor) (exact (RV.clos rho te))
  (constructor) (exact (EvE.eLam chkf dec encTy n rho U.uw A te))
  (intro arg) (intro alpha) (intro harg)
  (have hext (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
               (List.cons U U.uw us) (List.cons RV arg rho) (Prod.mk alpha eta))
    (envE_consw chkf dec encTy n (usk A) (uskCtx D) us arg rho alpha eta harg henv))
  (refine' (exT RV _ _ (ih (List.cons RV arg rho) (Prod.mk alpha eta) hext) _))
  (intro w hand)
  (have hev (EvalE chkf dec encTy n (List.cons RV arg rho) te w) (And.left hand))
  (have hrel (Erel chkf dec encTy n (usk B) w
               (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta)))
    (And.right hand))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.apClos chkf dec encTy n rho te arg w hev))
  (exact (erel_car chkf dec encTy n (usk B) w
           (denU chkf dec encTy n (List.cons Exp A D) t B (Prod.mk alpha eta))
           ((denU chkf dec encTy n D (Exp.lam U.uw A t) (Exp.tPi U.uw A B) eta) alpha)
           hrel
           (Eq.symm (denU_lamw chkf dec encTy n D A B t alpha eta)))))

;; --- application (Theorem 4′, the App clauses) ------------------------------
;;
;; ⟦f u⟧ is ⟦f⟧(⟦u⟧) when skOf finds u's skeleton (den_app_some).  denU reads
;; that at the usage skeleton, so the value is a cast.  cast_back undoes
;; denU on the argument.  The codomain of the judgment is B[u/x], whose
;; usage skeleton is usk B (usk_subst1, because skel u is Unit).  The
;; carrier E obtains from the arrow clause is the cast of ⟦f⟧(⟦u⟧) along
;; that equation (erel_subst_unit).  cast_uskSk, cast_usk_path and
;; cast_square say that cast is denU of the application.

(thm cast_back
  [s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2), a :- (Car s2)]
  (Eq (Car s2)
    (Eq.mp (congrArg Car e) (Eq.mp (congrArg Car (Eq.symm e)) a))
    a)
  (cases e)
  (rfl))

(thm den_at_eq
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, t :- Exp, G :- (List Sk), s1 :- Sk, s2 :- Sk,
   e :- (Eq Sk s1 s2), en :- (HEnv G)]
  (Eq (Car s2)
    (Eq.mp (congrArg Car e) (den chkf dec encTy n t G s1 en))
    (den chkf dec encTy n t G s2 en))
  (cases e)
  (rfl))

;; A cast along a usage-skeleton equation is the cast along the underlying
;; skeletons.  u1 and u2 are parameters, so cases reduces both Eq.mps.
(thm cast_uskSk
  [u1 :- USk, u2 :- USk, eu :- (Eq USk u1 u2), a :- (Car (uskSk u2))]
  (Eq (Car (uskSk u1))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu)) a)
    (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w)) (Eq.symm eu))) a))
  (cases eu)
  (rfl))

;; The skeleton path usk B → skel B → skel(B[u/x]) → usk(B[u/x]), built as
;; one trans so the four equations are not an independent cycle (casing an
;; independent cycle was rejected).
(thm cast_square
  [sU1 :- Sk, sU2 :- Sk, sB :- Sk, sS :- Sk,
   e2 :- (Eq Sk sU2 sB),
   eS :- (Eq Sk sS sB),
   e1 :- (Eq Sk sU1 sS),
   d :- (Car sB)]
  (Eq (Car sU1)
    (Eq.mp (congrArg Car
             (Eq.trans e2 (Eq.trans (Eq.symm eS) (Eq.symm e1))))
      (Eq.mp (congrArg Car (Eq.symm e2)) d))
    (Eq.mp (congrArg Car (Eq.symm e1))
      (Eq.mp (congrArg Car (Eq.symm eS)) d)))
  (cases e2)
  (cases eS)
  (cases e1)
  (rfl))

;; Once the two usage skeletons are identified, the usage-skeleton cast and
;; the skeleton path are the same transport.  The skeleton equations stay
;; in the goal; they do not have to be cased.
(thm cast_usk_path
  [u1 :- USk, u2 :- USk, eu :- (Eq USk u1 u2),
   sB :- Sk, sS :- Sk,
   e2 :- (Eq Sk (uskSk u2) sB),
   e1 :- (Eq Sk (uskSk u1) sS),
   eS :- (Eq Sk sS sB),
   d :- (Car sB)]
  (Eq (Car (uskSk u1))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu))
      (Eq.mp (congrArg Car (Eq.symm e2)) d))
    (Eq.mp (congrArg Car
             (Eq.trans e2 (Eq.trans (Eq.symm eS) (Eq.symm e1))))
      (Eq.mp (congrArg Car (Eq.symm e2)) d)))
  (cases eu)
  (rfl))

;; denU of an argument, cast back to ⟦·⟧'s carrier, is ⟦·⟧.
(thm denU_back
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), u :- Exp, A :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel A))
    (Eq.mp (congrArg Car (usk_skel A))
      (denU chkf dec encTy n D u A eta))
    (den chkf dec encTy n u (skels D) (skel A) (henv_of_usk D eta)))
  (exact (cast_back (uskSk (usk A)) (skel A) (usk_skel A)
           (den chkf dec encTy n u (skels D) (skel A) (henv_of_usk D eta)))))

(thm denU_pi0_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, A :- Exp, B :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_app_fun
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
     (denU chkf dec encTy n D u A eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_pi0_at chkf dec encTy n D f A B
             (denU chkf dec encTy n D u A eta) eta)
           (Eq.trans
             (congrArg
               (fn [a :- (Car (skel A))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
                   ((den chkf dec encTy n f (skels D)
                         (Sk.arr (skel A) (skel B)) (henv_of_usk D eta)) a)))
               (denU_back chkf dec encTy n D u A eta))
             (congrArg
               (fn [x :- (Car (skel B))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
               (Eq.symm (den_app_some chkf dec encTy n f u (skels D) (skel A) (skel B)
                          (henv_of_usk D eta) hsu)))))))

(thm denU_app0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A))),
   hU :- (Eq Sk (skel u) Sk.unit)]
  (Eq (Car (uskSk (usk (subst1 u B))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
      ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
       (denU chkf dec encTy n D u A eta)))
    (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (Eq.symm (usk_subst1 u B (usk_of_unit u hU)))) x))
      (denU_app_fun chkf dec encTy n D f u A B eta hsu))
    (Eq.trans
      (cast_usk_path (usk (subst1 u B)) (usk B) (usk_subst1 u B (usk_of_unit u hU))
        (skel B) (skel (subst1 u B))
        (usk_skel B) (usk_skel (subst1 u B)) (skel_subst1 u B hU)
        (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
      (Eq.trans
        (cast_square
          (uskSk (usk (subst1 u B))) (uskSk (usk B)) (skel B) (skel (subst1 u B))
          (usk_skel B) (skel_subst1 u B hU) (usk_skel (subst1 u B))
          (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
        (congrArg
          (fn [x :- (Car (skel (subst1 u B)))]
            (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 u B)))) x))
          (den_at_eq chkf dec encTy n (Exp.app f u) (skels D)
            (skel B) (skel (subst1 u B)) (Eq.symm (skel_subst1 u B hU))
            (henv_of_usk D eta))))))))

;; Theorem 4′, fundamental property, application at Π₀.  The argument is
;; not evaluated: erasure replaced it by ⋆.  Its denotation still feeds the
;; arrow clause, because that clause quantifies over every carrier.  skOf
;; of a logical premise is skOf_tl.
(thm adeqE_app0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us :- (List U),
   f :- Exp, u :- Exp, A :- Exp, B :- Exp, fe :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hu :- (Tl chkf Bool.false D u A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihf :- (Exists (fn [vf :- RV]
            (And (EvalE chkf dec encTy n rho fe vf)
                 (Erel chkf dec encTy n (usk (Exp.tPi U.u0 A B)) vf
                   (denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.app fe Exp.star) v)
         (Erel chkf dec encTy n (usk (subst1 u B)) v
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)))))
  (have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
  (have hcl (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
  (have hsu (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))
    (skOf_tl chkf Bool.false D u A hu rfl hcl))
  (have huS (SkJ Bool.false (skels D) u (skel A)) (lemma25_tl_term chkf D u A hu))
  (have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false (skels D) u (skel A) huS rfl))
  (refine' (exT RV _ _ ihf _)) (intro vf hf)
  (have hef (EvalE chkf dec encTy n rho fe vf) (And.left hf))
  (have hrf (Erel chkf dec encTy n (usk (Exp.tPi U.u0 A B)) vf
              (denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta))
    (And.right hf))
  (refine' (exT RV _ _ (hrf (denU chkf dec encTy n D u A eta)) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy n (EvSrc.ap vf RV.star) w) (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk B) w
               ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
                (denU chkf dec encTy n D u A eta)))
    (And.right hw))
  (have hsub (Erel chkf dec encTy n (usk (subst1 u B)) w
               (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                        (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
                 ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
                  (denU chkf dec encTy n D u A eta))))
    (erel_subst_unit chkf dec encTy n u B w
      ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
       (denU chkf dec encTy n D u A eta))
      hU hrel))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eApp chkf dec encTy n rho fe Exp.star vf RV.star w hef
           (EvE.eStar chkf dec encTy n rho) hap))
  (exact (erel_car chkf dec encTy n (usk (subst1 u B)) w
           (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                    (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
             ((denU chkf dec encTy n D f (Exp.tPi U.u0 A B) eta)
              (denU chkf dec encTy n D u A eta)))
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)
           hsub
           (denU_app0 chkf dec encTy n D f u A B eta hsu hU))))

(thm denU_pi1_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, A :- Exp, B :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_app_fun1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
     (denU chkf dec encTy n D u A eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_pi1_at chkf dec encTy n D f A B
             (denU chkf dec encTy n D u A eta) eta)
           (Eq.trans
             (congrArg
               (fn [a :- (Car (skel A))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
                   ((den chkf dec encTy n f (skels D)
                         (Sk.arr (skel A) (skel B)) (henv_of_usk D eta)) a)))
               (denU_back chkf dec encTy n D u A eta))
             (congrArg
               (fn [x :- (Car (skel B))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
               (Eq.symm (den_app_some chkf dec encTy n f u (skels D) (skel A) (skel B)
                          (henv_of_usk D eta) hsu)))))))

(thm denU_app1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A))),
   hU :- (Eq Sk (skel u) Sk.unit)]
  (Eq (Car (uskSk (usk (subst1 u B))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
      ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
       (denU chkf dec encTy n D u A eta)))
    (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (Eq.symm (usk_subst1 u B (usk_of_unit u hU)))) x))
      (denU_app_fun1 chkf dec encTy n D f u A B eta hsu))
    (Eq.trans
      (cast_usk_path (usk (subst1 u B)) (usk B) (usk_subst1 u B (usk_of_unit u hU))
        (skel B) (skel (subst1 u B))
        (usk_skel B) (usk_skel (subst1 u B)) (skel_subst1 u B hU)
        (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
      (Eq.trans
        (cast_square
          (uskSk (usk (subst1 u B))) (uskSk (usk B)) (skel B) (skel (subst1 u B))
          (usk_skel B) (skel_subst1 u B hU) (usk_skel (subst1 u B))
          (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
        (congrArg
          (fn [x :- (Car (skel (subst1 u B)))]
            (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 u B)))) x))
          (den_at_eq chkf dec encTy n (Exp.app f u) (skels D)
            (skel B) (skel (subst1 u B)) (Eq.symm (skel_subst1 u B hU))
            (henv_of_usk D eta))))))))

;; Theorem 4′, fundamental property, application at U.u1.  The argument
;; is evaluated, and the arrow clause demands that it be E-related.
(thm adeqE_app1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us2 :- (List U),
   f :- Exp, u :- Exp, A :- Exp, B :- Exp, fe :- Exp, ue :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hu :- (Rt chkf D us2 u A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihf :- (Exists (fn [vf :- RV]
            (And (EvalE chkf dec encTy n rho fe vf)
                 (Erel chkf dec encTy n (usk (Exp.tPi U.u1 A B)) vf
                   (denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta))))),
   ihu :- (Exists (fn [vu :- RV]
            (And (EvalE chkf dec encTy n rho ue vu)
                 (Erel chkf dec encTy n (usk A) vu
                   (denU chkf dec encTy n D u A eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.app fe ue) v)
         (Erel chkf dec encTy n (usk (subst1 u B)) v
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)))))
  (have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
  (have hcl (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
  (have hsu (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))
    (skOf_rt chkf D us2 u A hu hcl))
  (have huS (SkJ Bool.false (skels D) u (skel A)) (lemma25_rt chkf D us2 u A hu))
  (have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false (skels D) u (skel A) huS rfl))
  (refine' (exT RV _ _ ihf _)) (intro vf hf)
  (have hef (EvalE chkf dec encTy n rho fe vf) (And.left hf))
  (have hrf (Erel chkf dec encTy n (usk (Exp.tPi U.u1 A B)) vf
              (denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta))
    (And.right hf))
  (refine' (exT RV _ _ ihu _)) (intro vu hpu)
  (have heu (EvalE chkf dec encTy n rho ue vu) (And.left hpu))
  (have hru (Erel chkf dec encTy n (usk A) vu (denU chkf dec encTy n D u A eta))
    (And.right hpu))
  (refine' (exT RV _ _ (hrf vu (denU chkf dec encTy n D u A eta) hru) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy n (EvSrc.ap vf vu) w) (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk B) w
               ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
                (denU chkf dec encTy n D u A eta)))
    (And.right hw))
  (have hsub (Erel chkf dec encTy n (usk (subst1 u B)) w
               (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                        (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
                 ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
                  (denU chkf dec encTy n D u A eta))))
    (erel_subst_unit chkf dec encTy n u B w
      ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
       (denU chkf dec encTy n D u A eta))
      hU hrel))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eApp chkf dec encTy n rho fe ue vf vu w hef heu hap))
  (exact (erel_car chkf dec encTy n (usk (subst1 u B)) w
           (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                    (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
             ((denU chkf dec encTy n D f (Exp.tPi U.u1 A B) eta)
              (denU chkf dec encTy n D u A eta)))
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)
           hsub
           (denU_app1 chkf dec encTy n D f u A B eta hsu hU))))

(thm denU_piw_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, A :- Exp, B :- Exp,
   alpha :- (Car (uskSk (usk A))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      ((den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
       (Eq.mp (congrArg Car (usk_skel A)) alpha))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n f (skels D) (Sk.arr (skel A) (skel B)) (henv_of_usk D eta))
           alpha)))

(thm denU_app_funw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))]
  (Eq (Car (uskSk (usk B)))
    ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
     (denU chkf dec encTy n D u A eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_piw_at chkf dec encTy n D f A B
             (denU chkf dec encTy n D u A eta) eta)
           (Eq.trans
             (congrArg
               (fn [a :- (Car (skel A))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
                   ((den chkf dec encTy n f (skels D)
                         (Sk.arr (skel A) (skel B)) (henv_of_usk D eta)) a)))
               (denU_back chkf dec encTy n D u A eta))
             (congrArg
               (fn [x :- (Car (skel B))]
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
               (Eq.symm (den_app_some chkf dec encTy n f u (skels D) (skel A) (skel B)
                          (henv_of_usk D eta) hsu)))))))

(thm denU_appw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), f :- Exp, u :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hsu :- (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A))),
   hU :- (Eq Sk (skel u) Sk.unit)]
  (Eq (Car (uskSk (usk (subst1 u B))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
      ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
       (denU chkf dec encTy n D u A eta)))
    (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (Eq.symm (usk_subst1 u B (usk_of_unit u hU)))) x))
      (denU_app_funw chkf dec encTy n D f u A B eta hsu))
    (Eq.trans
      (cast_usk_path (usk (subst1 u B)) (usk B) (usk_subst1 u B (usk_of_unit u hU))
        (skel B) (skel (subst1 u B))
        (usk_skel B) (usk_skel (subst1 u B)) (skel_subst1 u B hU)
        (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
      (Eq.trans
        (cast_square
          (uskSk (usk (subst1 u B))) (uskSk (usk B)) (skel B) (skel (subst1 u B))
          (usk_skel B) (skel_subst1 u B hU) (usk_skel (subst1 u B))
          (den chkf dec encTy n (Exp.app f u) (skels D) (skel B) (henv_of_usk D eta)))
        (congrArg
          (fn [x :- (Car (skel (subst1 u B)))]
            (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 u B)))) x))
          (den_at_eq chkf dec encTy n (Exp.app f u) (skels D)
            (skel B) (skel (subst1 u B)) (Eq.symm (skel_subst1 u B hU))
            (henv_of_usk D eta))))))))

;; Theorem 4′, fundamental property, application at U.uw.  The argument
;; is evaluated, and the arrow clause demands that it be E-related.
(thm adeqE_appw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us2 :- (List U),
   f :- Exp, u :- Exp, A :- Exp, B :- Exp, fe :- Exp, ue :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hu :- (Rt chkf D us2 u A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihf :- (Exists (fn [vf :- RV]
            (And (EvalE chkf dec encTy n rho fe vf)
                 (Erel chkf dec encTy n (usk (Exp.tPi U.uw A B)) vf
                   (denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta))))),
   ihu :- (Exists (fn [vu :- RV]
            (And (EvalE chkf dec encTy n rho ue vu)
                 (Erel chkf dec encTy n (usk A) vu
                   (denU chkf dec encTy n D u A eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.app fe ue) v)
         (Erel chkf dec encTy n (usk (subst1 u B)) v
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)))))
  (have hAS (SkJ Bool.true (skels D) A Sk.unit) (lemma25_tl_type chkf D A hA))
  (have hcl (Eq Bool (clean A) Bool.true) (skj_clean Bool.true (skels D) A Sk.unit hAS))
  (have hsu (Eq (Option Sk) (skOf (skels D) u) (Option.some Sk (skel A)))
    (skOf_rt chkf D us2 u A hu hcl))
  (have huS (SkJ Bool.false (skels D) u (skel A)) (lemma25_rt chkf D us2 u A hu))
  (have hU (Eq Sk (skel u) Sk.unit) (skj_term_unit Bool.false (skels D) u (skel A) huS rfl))
  (refine' (exT RV _ _ ihf _)) (intro vf hf)
  (have hef (EvalE chkf dec encTy n rho fe vf) (And.left hf))
  (have hrf (Erel chkf dec encTy n (usk (Exp.tPi U.uw A B)) vf
              (denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta))
    (And.right hf))
  (refine' (exT RV _ _ ihu _)) (intro vu hpu)
  (have heu (EvalE chkf dec encTy n rho ue vu) (And.left hpu))
  (have hru (Erel chkf dec encTy n (usk A) vu (denU chkf dec encTy n D u A eta))
    (And.right hpu))
  (refine' (exT RV _ _ (hrf vu (denU chkf dec encTy n D u A eta) hru) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy n (EvSrc.ap vf vu) w) (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk B) w
               ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
                (denU chkf dec encTy n D u A eta)))
    (And.right hw))
  (have hsub (Erel chkf dec encTy n (usk (subst1 u B)) w
               (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                        (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
                 ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
                  (denU chkf dec encTy n D u A eta))))
    (erel_subst_unit chkf dec encTy n u B w
      ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
       (denU chkf dec encTy n D u A eta))
      hU hrel))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eApp chkf dec encTy n rho fe ue vf vu w hef heu hap))
  (exact (erel_car chkf dec encTy n (usk (subst1 u B)) w
           (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                    (Eq.symm (usk_subst1 u B (usk_of_unit u hU))))
             ((denU chkf dec encTy n D f (Exp.tPi U.uw A B) eta)
              (denU chkf dec encTy n D u A eta)))
           (denU chkf dec encTy n D (Exp.app f u) (subst1 u B) eta)
           hsub
           (denU_appw chkf dec encTy n D f u A B eta hsu hU))))

;; --- pairs (Theorem 4′, the Pair clauses) -----------------------------------
;;
;; A pair's denotation is the pair of the components (denE_pair_prod).  The
;; usage-skeleton cast of a product commutes with the projections
;; (cast_fst, cast_snd).  The second component is typed at B[x/y], and
;; denU_unsubst brings that denotation back to usk B, which is the carrier
;; the product clause of E reads.  At Σ₀ the first component is erased to ⋆
;; and forgotten by E.  At Σ₁ and Σω both components are kept.

(thm cast_snd
  [d :- Sk, c :- Sk, sD :- Sk, sC :- Sk,
   eA :- (Eq Sk d sD), eB :- (Eq Sk c sC),
   p :- (Car (Sk.prod sD sC))]
  (Eq (Car c)
    (Prod.snd
      (Eq.mp (congrArg Car (Eq.symm (Eq.trans
               (congrArg (fn [x :- Sk] (Sk.prod x c)) eA)
               (congrArg (fn [y :- Sk] (Sk.prod sD y)) eB))))
        p))
    (Eq.mp (congrArg Car (Eq.symm eB)) (Prod.snd p)))
  (cases eA)
  (cases eB)
  (rfl))

(thm cast_fst
  [d :- Sk, c :- Sk, sD :- Sk, sC :- Sk,
   eA :- (Eq Sk d sD), eB :- (Eq Sk c sC),
   p :- (Car (Sk.prod sD sC))]
  (Eq (Car d)
    (Prod.fst
      (Eq.mp (congrArg Car (Eq.symm (Eq.trans
               (congrArg (fn [x :- Sk] (Sk.prod x c)) eA)
               (congrArg (fn [y :- Sk] (Sk.prod sD y)) eB))))
        p))
    (Eq.mp (congrArg Car (Eq.symm eA)) (Prod.fst p)))
  (cases eA)
  (cases eB)
  (rfl))

(thm denE_pair_prod
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, S :- Exp, x :- Exp, y :- Exp, G :- (List Sk),
   sa :- Sk, sb :- Sk, en :- (HEnv G)]
  (Eq (Prod (Car sa) (Car sb))
    (den chkf dec encTy n (Exp.pair S x y) G (Sk.prod sa sb) en)
    (Prod.mk (den chkf dec encTy n x G sa en) (den chkf dec encTy n y G sb en)))
  (rw [(den_pair_at chkf dec encTy n S x y G (Sk.prod sa sb) en)]))

;; Casting along a usage-skeleton equation and casting back is the identity.
(thm cast_usk_back
  [u1 :- USk, u2 :- USk, eu :- (Eq USk u1 u2), a :- (Car (uskSk u2))]
  (Eq (Car (uskSk u2))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu)
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu)) a))
    a)
  (cases eu)
  (rfl))

;; ⟦t⟧ at B, cast into the substituted type B[u/x].  The argument has
;; skeleton Unit, so the skeletons agree (skel_subst1, usk_subst1).
(thm denU_subst_from
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), u :- Exp, e :- Exp, t :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel u) Sk.unit)]
  (Eq (Car (uskSk (usk (subst1 u e))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 u e (usk_of_unit u hU))))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel e)))
        (den chkf dec encTy n t (skels D) (skel e) (henv_of_usk D eta))))
    (denU chkf dec encTy n D t (subst1 u e) eta))
  (exact (Eq.trans
    (cast_usk_path (usk (subst1 u e)) (usk e) (usk_subst1 u e (usk_of_unit u hU))
      (skel e) (skel (subst1 u e))
      (usk_skel e) (usk_skel (subst1 u e)) (skel_subst1 u e hU)
      (den chkf dec encTy n t (skels D) (skel e) (henv_of_usk D eta)))
    (Eq.trans
      (cast_square
        (uskSk (usk (subst1 u e))) (uskSk (usk e)) (skel e) (skel (subst1 u e))
        (usk_skel e) (skel_subst1 u e hU) (usk_skel (subst1 u e))
        (den chkf dec encTy n t (skels D) (skel e) (henv_of_usk D eta)))
      (congrArg
        (fn [x :- (Car (skel (subst1 u e)))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 u e)))) x))
        (den_at_eq chkf dec encTy n t (skels D)
          (skel e) (skel (subst1 u e)) (Eq.symm (skel_subst1 u e hU))
          (henv_of_usk D eta)))))))

;; The other direction: ⟦t⟧ read at B[u/x], brought back to usk B.
(thm denU_unsubst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), u :- Exp, e :- Exp, t :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel u) Sk.unit)]
  (Eq (Car (uskSk (usk e)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_subst1 u e (usk_of_unit u hU)))
      (denU chkf dec encTy n D t (subst1 u e) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel e)))
      (den chkf dec encTy n t (skels D) (skel e) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk (subst1 u e))))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (usk_subst1 u e (usk_of_unit u hU))) x))
      (Eq.symm (denU_subst_from chkf dec encTy n D u e t eta hU)))
    (cast_usk_back (usk (subst1 u e)) (usk e) (usk_subst1 u e (usk_of_unit u hU))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel e)))
        (den chkf dec encTy n t (skels D) (skel e) (henv_of_usk D eta)))))))

(thm denU_sig0_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (Prod.snd (den chkf dec encTy n (Exp.pair (Exp.tSig U.u0 A B) x y) (skels D)
                 (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))
  (exact (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.pair (Exp.tSig U.u0 A B) x y) (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))

(thm denU_sig0_y
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n y (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_sig0_snd chkf dec encTy n D A B x y eta)
           (congrArg
             (fn [p :- (Prod (Car (skel A)) (Car (skel B)))]
               (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) (Prod.snd p)))
             (denE_pair_prod chkf dec encTy n (Exp.tSig U.u0 A B) x y (skels D)
               (skel A) (skel B) (henv_of_usk D eta))))))

(thm denU_pair0_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel x) Sk.unit)]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D
                (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_subst1 x B (usk_of_unit x hU)))
      (denU chkf dec encTy n D y (subst1 x B) eta)))
  (exact (Eq.trans
           (denU_sig0_y chkf dec encTy n D A B x y eta)
           (Eq.symm (denU_unsubst chkf dec encTy n D x B y eta hU)))))

(thm denU_sig1_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (Prod.snd (den chkf dec encTy n (Exp.pair (Exp.tSig U.u1 A B) x y) (skels D)
                 (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))
  (exact (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.pair (Exp.tSig U.u1 A B) x y) (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))

(thm denU_sig1_y
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n y (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_sig1_snd chkf dec encTy n D A B x y eta)
           (congrArg
             (fn [p :- (Prod (Car (skel A)) (Car (skel B)))]
               (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) (Prod.snd p)))
             (denE_pair_prod chkf dec encTy n (Exp.tSig U.u1 A B) x y (skels D)
               (skel A) (skel B) (henv_of_usk D eta))))))

(thm denU_pair1_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel x) Sk.unit)]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D
                (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_subst1 x B (usk_of_unit x hU)))
      (denU chkf dec encTy n D y (subst1 x B) eta)))
  (exact (Eq.trans
           (denU_sig1_y chkf dec encTy n D A B x y eta)
           (Eq.symm (denU_unsubst chkf dec encTy n D x B y eta hU)))))

(thm denU_sig1_fst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk A)))
    (Prod.fst (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
    (denU chkf dec encTy n D x A eta))
  (exact (Eq.trans
           (cast_fst (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
             (usk_skel A) (usk_skel B)
             (den chkf dec encTy n (Exp.pair (Exp.tSig U.u1 A B) x y) (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))
           (congrArg
             (fn [p :- (Prod (Car (skel A)) (Car (skel B)))]
               (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (Prod.fst p)))
             (denE_pair_prod chkf dec encTy n (Exp.tSig U.u1 A B) x y (skels D)
               (skel A) (skel B) (henv_of_usk D eta))))))

(thm denU_sigw_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (Prod.snd (den chkf dec encTy n (Exp.pair (Exp.tSig U.uw A B) x y) (skels D)
                 (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))
  (exact (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B)
           (den chkf dec encTy n (Exp.pair (Exp.tSig U.uw A B) x y) (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))

(thm denU_sigw_y
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (den chkf dec encTy n y (skels D) (skel B) (henv_of_usk D eta))))
  (exact (Eq.trans
           (denU_sigw_snd chkf dec encTy n D A B x y eta)
           (congrArg
             (fn [p :- (Prod (Car (skel A)) (Car (skel B)))]
               (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) (Prod.snd p)))
             (denE_pair_prod chkf dec encTy n (Exp.tSig U.uw A B) x y (skels D)
               (skel A) (skel B) (henv_of_usk D eta))))))

(thm denU_pairw_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel x) Sk.unit)]
  (Eq (Car (uskSk (usk B)))
    (Prod.snd (denU chkf dec encTy n D
                (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_subst1 x B (usk_of_unit x hU)))
      (denU chkf dec encTy n D y (subst1 x B) eta)))
  (exact (Eq.trans
           (denU_sigw_y chkf dec encTy n D A B x y eta)
           (Eq.symm (denU_unsubst chkf dec encTy n D x B y eta hU)))))

(thm denU_sigw_fst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk A)))
    (Prod.fst (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
    (denU chkf dec encTy n D x A eta))
  (exact (Eq.trans
           (cast_fst (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
             (usk_skel A) (usk_skel B)
             (den chkf dec encTy n (Exp.pair (Exp.tSig U.uw A B) x y) (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))
           (congrArg
             (fn [p :- (Prod (Car (skel A)) (Car (skel B)))]
               (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) (Prod.fst p)))
             (denE_pair_prod chkf dec encTy n (Exp.tSig U.uw A B) x y (skels D)
               (skel A) (skel B) (henv_of_usk D eta))))))

;; Theorem 4′, fundamental property, pair at Σ₀.  The first component is
;; erased to ⋆ and E forgets it.  The second is E-related at B[x/y];
;; denU_pair0_snd is that value as the second projection of ⟦pair⟧.
(thm adeqE_pair0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   A :- Exp, B :- Exp, x :- Exp, y :- Exp, ye :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hx :- (Tl chkf Bool.false D x A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihy :- (Exists (fn [vy :- RV]
            (And (EvalE chkf dec encTy n rho ye vy)
                 (Erel chkf dec encTy n (usk (subst1 x B)) vy
                   (denU chkf dec encTy n D y (subst1 x B) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.pair (Exp.tSig U.u0 A B) Exp.star ye) v)
         (Erel chkf dec encTy n (usk (Exp.tSig U.u0 A B)) v
           (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta)))))
  (have hxS (SkJ Bool.false (skels D) x (skel A)) (lemma25_tl_term chkf D x A hx))
  (have hU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false (skels D) x (skel A) hxS rfl))
  (refine' (exT RV _ _ ihy _)) (intro vy hy)
  (have hey (EvalE chkf dec encTy n rho ye vy) (And.left hy))
  (have hry (Erel chkf dec encTy n (usk (subst1 x B)) vy
              (denU chkf dec encTy n D y (subst1 x B) eta))
    (And.right hy))
  (have hsub (Erel chkf dec encTy n (usk B) vy
               (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                        (usk_subst1 x B (usk_of_unit x hU)))
                 (denU chkf dec encTy n D y (subst1 x B) eta)))
    (erel_at chkf dec encTy n (usk (subst1 x B)) (usk B) vy
      (denU chkf dec encTy n D y (subst1 x B) eta)
      hry (usk_subst1 x B (usk_of_unit x hU))))
  (have hrel (Erel chkf dec encTy n (usk B) vy
               (Prod.snd (denU chkf dec encTy n D
                           (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta)))
    (erel_car chkf dec encTy n (usk B) vy
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
               (usk_subst1 x B (usk_of_unit x hU)))
        (denU chkf dec encTy n D y (subst1 x B) eta))
      (Prod.snd (denU chkf dec encTy n D
                  (Exp.pair (Exp.tSig U.u0 A B) x y) (Exp.tSig U.u0 A B) eta))
      hsub
      (Eq.symm (denU_pair0_snd chkf dec encTy n D A B x y eta hU))))
  (constructor) (exact (RV.pair RV.star vy))
  (constructor)
  (exact (EvE.ePair chkf dec encTy n rho (Exp.tSig U.u0 A B) Exp.star ye RV.star vy
           (EvE.eStar chkf dec encTy n rho) hey))
  (constructor) (exact RV.star)
  (constructor) (exact vy)
  (constructor) (rfl)
  (exact hrel))

;; Theorem 4′, fundamental property, pair at Σ₁.  Both components are
;; evaluated.  The first is E-related at A; the second, at B[x/y].
(thm adeqE_pair1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U),
   A :- Exp, B :- Exp, x :- Exp, y :- Exp, xe :- Exp, ye :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hx :- (Rt chkf D us1 x A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   ihx :- (Exists (fn [vx :- RV]
            (And (EvalE chkf dec encTy n rho xe vx)
                 (Erel chkf dec encTy n (usk A) vx
                   (denU chkf dec encTy n D x A eta))))),
   ihy :- (Exists (fn [vy :- RV]
            (And (EvalE chkf dec encTy n rho ye vy)
                 (Erel chkf dec encTy n (usk (subst1 x B)) vy
                   (denU chkf dec encTy n D y (subst1 x B) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.pair (Exp.tSig U.u1 A B) xe ye) v)
         (Erel chkf dec encTy n (usk (Exp.tSig U.u1 A B)) v
           (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta)))))
  (have hxS (SkJ Bool.false (skels D) x (skel A)) (lemma25_rt chkf D us1 x A hx))
  (have hU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false (skels D) x (skel A) hxS rfl))
  (refine' (exT RV _ _ ihx _)) (intro vx px)
  (have hex (EvalE chkf dec encTy n rho xe vx) (And.left px))
  (have hrx (Erel chkf dec encTy n (usk A) vx (denU chkf dec encTy n D x A eta))
    (And.right px))
  (refine' (exT RV _ _ ihy _)) (intro vy py)
  (have hey (EvalE chkf dec encTy n rho ye vy) (And.left py))
  (have hry (Erel chkf dec encTy n (usk (subst1 x B)) vy
              (denU chkf dec encTy n D y (subst1 x B) eta))
    (And.right py))
  (have hsub (Erel chkf dec encTy n (usk B) vy
               (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                        (usk_subst1 x B (usk_of_unit x hU)))
                 (denU chkf dec encTy n D y (subst1 x B) eta)))
    (erel_at chkf dec encTy n (usk (subst1 x B)) (usk B) vy
      (denU chkf dec encTy n D y (subst1 x B) eta)
      hry (usk_subst1 x B (usk_of_unit x hU))))
  (have hrys (Erel chkf dec encTy n (usk B) vy
               (Prod.snd (denU chkf dec encTy n D
                           (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta)))
    (erel_car chkf dec encTy n (usk B) vy
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
               (usk_subst1 x B (usk_of_unit x hU)))
        (denU chkf dec encTy n D y (subst1 x B) eta))
      (Prod.snd (denU chkf dec encTy n D
                  (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
      hsub
      (Eq.symm (denU_pair1_snd chkf dec encTy n D A B x y eta hU))))
  (have hrxf (Erel chkf dec encTy n (usk A) vx
               (Prod.fst (denU chkf dec encTy n D
                           (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta)))
    (erel_car chkf dec encTy n (usk A) vx
      (denU chkf dec encTy n D x A eta)
      (Prod.fst (denU chkf dec encTy n D
                  (Exp.pair (Exp.tSig U.u1 A B) x y) (Exp.tSig U.u1 A B) eta))
      hrx
      (Eq.symm (denU_sig1_fst chkf dec encTy n D A B x y eta))))
  (constructor) (exact (RV.pair vx vy))
  (constructor)
  (exact (EvE.ePair chkf dec encTy n rho (Exp.tSig U.u1 A B) xe ye vx vy hex hey))
  (constructor) (exact vx)
  (constructor) (exact vy)
  (constructor) (rfl)
  (constructor) (exact hrxf) (exact hrys))

;; Theorem 4′, fundamental property, pair at Σω.  The same clause as usage 1.
(thm adeqE_pairw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U),
   A :- Exp, B :- Exp, x :- Exp, y :- Exp, xe :- Exp, ye :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hx :- (Rt chkf D us1 x A),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   ihx :- (Exists (fn [vx :- RV]
            (And (EvalE chkf dec encTy n rho xe vx)
                 (Erel chkf dec encTy n (usk A) vx
                   (denU chkf dec encTy n D x A eta))))),
   ihy :- (Exists (fn [vy :- RV]
            (And (EvalE chkf dec encTy n rho ye vy)
                 (Erel chkf dec encTy n (usk (subst1 x B)) vy
                   (denU chkf dec encTy n D y (subst1 x B) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.pair (Exp.tSig U.uw A B) xe ye) v)
         (Erel chkf dec encTy n (usk (Exp.tSig U.uw A B)) v
           (denU chkf dec encTy n D (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta)))))
  (have hxS (SkJ Bool.false (skels D) x (skel A)) (lemma25_rt chkf D us1 x A hx))
  (have hU (Eq Sk (skel x) Sk.unit) (skj_term_unit Bool.false (skels D) x (skel A) hxS rfl))
  (refine' (exT RV _ _ ihx _)) (intro vx px)
  (have hex (EvalE chkf dec encTy n rho xe vx) (And.left px))
  (have hrx (Erel chkf dec encTy n (usk A) vx (denU chkf dec encTy n D x A eta))
    (And.right px))
  (refine' (exT RV _ _ ihy _)) (intro vy py)
  (have hey (EvalE chkf dec encTy n rho ye vy) (And.left py))
  (have hry (Erel chkf dec encTy n (usk (subst1 x B)) vy
              (denU chkf dec encTy n D y (subst1 x B) eta))
    (And.right py))
  (have hsub (Erel chkf dec encTy n (usk B) vy
               (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                        (usk_subst1 x B (usk_of_unit x hU)))
                 (denU chkf dec encTy n D y (subst1 x B) eta)))
    (erel_at chkf dec encTy n (usk (subst1 x B)) (usk B) vy
      (denU chkf dec encTy n D y (subst1 x B) eta)
      hry (usk_subst1 x B (usk_of_unit x hU))))
  (have hrys (Erel chkf dec encTy n (usk B) vy
               (Prod.snd (denU chkf dec encTy n D
                           (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta)))
    (erel_car chkf dec encTy n (usk B) vy
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
               (usk_subst1 x B (usk_of_unit x hU)))
        (denU chkf dec encTy n D y (subst1 x B) eta))
      (Prod.snd (denU chkf dec encTy n D
                  (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
      hsub
      (Eq.symm (denU_pairw_snd chkf dec encTy n D A B x y eta hU))))
  (have hrxf (Erel chkf dec encTy n (usk A) vx
               (Prod.fst (denU chkf dec encTy n D
                           (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta)))
    (erel_car chkf dec encTy n (usk A) vx
      (denU chkf dec encTy n D x A eta)
      (Prod.fst (denU chkf dec encTy n D
                  (Exp.pair (Exp.tSig U.uw A B) x y) (Exp.tSig U.uw A B) eta))
      hrx
      (Eq.symm (denU_sigw_fst chkf dec encTy n D A B x y eta))))
  (constructor) (exact (RV.pair vx vy))
  (constructor)
  (exact (EvE.ePair chkf dec encTy n rho (Exp.tSig U.uw A B) xe ye vx vy hex hey))
  (constructor) (exact vx)
  (constructor) (exact vy)
  (constructor) (rfl)
  (constructor) (exact hrxf) (exact hrys))

;; --- conversion (Theorem 4′, the Conv clause) -------------------------------
;;
;; E does not depend on the terms inside a type, so a conversion A ≡ B
;; leaves the usage skeleton (usk_cv) and therefore E.  The denotation is
;; read at B; denU_cv says that is the cast of the denotation read at A.
;; The erased term is the premise's erasure.

(thm denU_cv_from
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hcv :- (Cv chkf (skels D) A B)]
  (Eq (Car (uskSk (usk A)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_cv chkf (skels D) A B hcv)))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
        (den chkf dec encTy n t (skels D) (skel B) (henv_of_usk D eta))))
    (denU chkf dec encTy n D t A eta))
  (exact (Eq.trans
    (cast_usk_path (usk A) (usk B) (usk_cv chkf (skels D) A B hcv)
      (skel B) (skel A)
      (usk_skel B) (usk_skel A) (cv_skel chkf (skels D) A B hcv)
      (den chkf dec encTy n t (skels D) (skel B) (henv_of_usk D eta)))
    (Eq.trans
      (cast_square
        (uskSk (usk A)) (uskSk (usk B)) (skel B) (skel A)
        (usk_skel B) (cv_skel chkf (skels D) A B hcv) (usk_skel A)
        (den chkf dec encTy n t (skels D) (skel B) (henv_of_usk D eta)))
      (congrArg
        (fn [x :- (Car (skel A))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel A))) x))
        (den_at_eq chkf dec encTy n t (skels D)
          (skel B) (skel A) (Eq.symm (cv_skel chkf (skels D) A B hcv))
          (henv_of_usk D eta)))))))

(thm denU_cv
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hcv :- (Cv chkf (skels D) A B)]
  (Eq (Car (uskSk (usk B)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_cv chkf (skels D) A B hcv))
      (denU chkf dec encTy n D t A eta))
    (denU chkf dec encTy n D t B eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk A)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (usk_cv chkf (skels D) A B hcv)) x))
      (Eq.symm (denU_cv_from chkf dec encTy n D t A B eta hcv)))
    (cast_usk_back (usk A) (usk B) (usk_cv chkf (skels D) A B hcv)
      (denU chkf dec encTy n D t B eta)))))

;; Theorem 4′, fundamental property, conversion.  The value is the premise's
;; value.  erel_cv moves the relation to the converted type, and denU_cv
;; moves the carrier onto ⟦t⟧ at that type.
(thm adeqE_conv
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hcv :- (Cv chkf (skels D) A B),
   ih :- (Exists (fn [v :- RV]
           (And (EvalE chkf dec encTy n rho te v)
                (Erel chkf dec encTy n (usk A) v
                  (denU chkf dec encTy n D t A eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho te v)
         (Erel chkf dec encTy n (usk B) v
           (denU chkf dec encTy n D t B eta)))))
  (refine' (exT RV _ _ ih _)) (intro v hv)
  (have he (EvalE chkf dec encTy n rho te v) (And.left hv))
  (have hr (Erel chkf dec encTy n (usk A) v (denU chkf dec encTy n D t A eta))
    (And.right hv))
  (have h2 (Erel chkf dec encTy n (usk B) v
             (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                      (usk_cv chkf (skels D) A B hcv))
               (denU chkf dec encTy n D t A eta)))
    (erel_cv chkf dec encTy n (skels D) A B v
      (denU chkf dec encTy n D t A eta) hcv hr))
  (constructor) (exact v)
  (constructor) (exact he)
  (exact (erel_car chkf dec encTy n (usk B) v
           (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                    (usk_cv chkf (skels D) A B hcv))
             (denU chkf dec encTy n D t A eta))
           (denU chkf dec encTy n D t B eta)
           h2
           (denU_cv chkf dec encTy n D t A B eta hcv))))

;; --- let (Theorem 4′, the Let clause) ---------------------------------------
;;
;; evalᴱ of let runs the erased scrutinee to a pair, then the erased body
;; in (second, (first, ρ)).  The denotation does the same under skOf of the
;; original scrutinee (den_letp_some).  The body's type in the derivation is
;; lift 2 0 C, so the induction hypothesis is E at that usage skeleton;
;; usk_lift / denU_lift bring it back to C, and denU_let is den_letp_some
;; at the carrier E reads.  Three theorems, one per usage of the Σ: until
;; the usage is a constructor, usk of the product does not reduce, and E's
;; clause is not yet a product.  The scrutinee's typing derivation is an
;; Rt, as in the App cases, because skOf_rt reads a derivation and Er does
;; not store one.  The assembly rebuilds it.

;; EvalE is a proposition indexed by the result, as Eval is.  The product
;; clause of E gives an equality with a pair; eLet wants that pair.
(thm evalE_cast
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, rho :- (List RV), t :- Exp, v :- RV, w :- RV,
   h :- (EvalE chkf dec encTy n rho t v),
   e :- (Eq RV v w)]
  (EvalE chkf dec encTy n rho t w)
  (exact (Eq.mp (congrArg (fn [x :- RV] (EvalE chkf dec encTy n rho t x)) e) h)))

;; The two new binders, written as a context and as the skeleton list den
;; quantifies over.  skels is the match on cons, so this is rfl, and every
;; later denotation at that context may use either spelling.
(thm skels_let_ctx [A :- Exp, B :- Exp, D :- (List Exp)]
  (Eq (List Sk)
    (skels (List.cons Exp B (List.cons Exp A D)))
    (List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))))
  (rfl))

;; ⟦t⟧ at C, cast into the lifted type.  The same square as denU_cv_from:
;; usk_lift is the usage-skeleton equation and skel_lift is the skeleton
;; equation.  Both are proved for every expression, with no formation
;; hypothesis, so the body's type annotation does not have to be re-derived.
(thm denU_lift_from
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, C :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk (lift 2 0 C))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_lift C 2 0)))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel C)))
        (den chkf dec encTy n t (skels D) (skel C) (henv_of_usk D eta))))
    (denU chkf dec encTy n D t (lift 2 0 C) eta))
  (exact (Eq.trans
    (cast_usk_path (usk (lift 2 0 C)) (usk C) (usk_lift C 2 0)
      (skel C) (skel (lift 2 0 C))
      (usk_skel C) (usk_skel (lift 2 0 C)) (skel_lift C 2 0)
      (den chkf dec encTy n t (skels D) (skel C) (henv_of_usk D eta)))
    (Eq.trans
      (cast_square
        (uskSk (usk (lift 2 0 C))) (uskSk (usk C)) (skel C) (skel (lift 2 0 C))
        (usk_skel C) (skel_lift C 2 0) (usk_skel (lift 2 0 C))
        (den chkf dec encTy n t (skels D) (skel C) (henv_of_usk D eta)))
      (congrArg
        (fn [x :- (Car (skel (lift 2 0 C)))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 2 0 C)))) x))
        (den_at_eq chkf dec encTy n t (skels D)
          (skel C) (skel (lift 2 0 C)) (Eq.symm (skel_lift C 2 0))
          (henv_of_usk D eta)))))))

;; The other direction: the body's denotation, read at lift 2 0 C, brought
;; back to usk C.  C mentions neither binder, so the two carriers agree.
(thm denU_lift
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, C :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk C)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (usk_lift C 2 0))
      (denU chkf dec encTy n D t (lift 2 0 C) eta))
    (denU chkf dec encTy n D t C eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk (lift 2 0 C))))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (usk_lift C 2 0)) x))
      (Eq.symm (denU_lift_from chkf dec encTy n D t C eta)))
    (cast_usk_back (usk (lift 2 0 C)) (usk C) (usk_lift C 2 0)
      (denU chkf dec encTy n D t C eta)))))

;; Prod.fst of ⟦p⟧ at a Σ₁, as the cast of Prod.fst of denU.  usk_skel of a
;; concrete Σ reduces to the component trans cast_fst is stated at, for
;; every usage (usk_skel, cases on the usage, the same trans).  The term is
;; any scrutinee, not only a pair: let eliminates a value it did not build.
(thm denU_proj1_fst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel A))
    (Eq.mp (congrArg Car (usk_skel A))
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)))
    (Prod.fst (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk A)))]
        (Eq.mp (congrArg Car (usk_skel A)) x))
      (cast_fst (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk A)) (skel A) (usk_skel A)
      (Prod.fst (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

(thm denU_proj1_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel B))
    (Eq.mp (congrArg Car (usk_skel B))
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)))
    (Prod.snd (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg Car (usk_skel B)) x))
      (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk B)) (skel B) (usk_skel B)
      (Prod.snd (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

;; Σ₀.  E forgets the first component, but the denotation of the body still
;; reads it, so both projections are needed.
(thm denU_proj0_fst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel A))
    (Eq.mp (congrArg Car (usk_skel A))
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)))
    (Prod.fst (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk A)))]
        (Eq.mp (congrArg Car (usk_skel A)) x))
      (cast_fst (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk A)) (skel A) (usk_skel A)
      (Prod.fst (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

(thm denU_proj0_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel B))
    (Eq.mp (congrArg Car (usk_skel B))
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)))
    (Prod.snd (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg Car (usk_skel B)) x))
      (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk B)) (skel B) (usk_skel B)
      (Prod.snd (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

;; Σω, the same product clause as Σ₁.
(thm denU_projw_fst
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel A))
    (Eq.mp (congrArg Car (usk_skel A))
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)))
    (Prod.fst (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk A)))]
        (Eq.mp (congrArg Car (usk_skel A)) x))
      (cast_fst (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk A)) (skel A) (usk_skel A)
      (Prod.fst (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

(thm denU_projw_snd
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, p :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (skel B))
    (Eq.mp (congrArg Car (usk_skel B))
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)))
    (Prod.snd (den chkf dec encTy n p (skels D)
                (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg Car (usk_skel B)) x))
      (cast_snd (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
        (usk_skel A) (usk_skel B)
        (den chkf dec encTy n p (skels D)
          (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))
    (cast_back (uskSk (usk B)) (skel B) (usk_skel B)
      (Prod.snd (den chkf dec encTy n p (skels D)
                  (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))))))

;; Two binders.  henv_ext once is the outer pair; the tail is henv_ext again.
;; The head is the second component (variable 0).
(thm henv_ext2
  [A :- Exp, B :- Exp, D :- (List Exp),
   alpha :- (Car (uskSk (usk A))),
   beta :- (Car (uskSk (usk B))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (HEnv (skels (List.cons Exp B (List.cons Exp A D))))
    (henv_of_usk (List.cons Exp B (List.cons Exp A D))
      (Prod.mk beta (Prod.mk alpha eta)))
    (Prod.mk (Eq.mp (congrArg Car (usk_skel B)) beta)
      (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
        (henv_of_usk D eta))))
  (exact (Eq.trans
    (henv_ext B (List.cons Exp A D) beta (Prod.mk alpha eta))
    (congrArg
      (fn [en :- (HEnv (skels (List.cons Exp A D)))]
        (Prod.mk (Eq.mp (congrArg Car (usk_skel B)) beta) en))
      (henv_ext A D alpha eta)))))

;; ⟦let C p t⟧ at skel C is ⟦t⟧ under (snd ⟦p⟧, (fst ⟦p⟧, η)), once skOf p
;; is the product.  The projections are the casts of denU's, so the
;; environment henv_ext2 builds is the one den_letp_some wants.  The
;; skeleton context is written out as the cons list, which skels_let_ctx
;; identifies with skels of the two-binder context.
(thm den_let_body
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, C :- Exp,
   p :- Exp, t :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   alpha :- (Car (uskSk (usk A))),
   beta :- (Car (uskSk (usk B))),
   hsu :- (Eq (Option Sk) (skOf (skels D) p)
             (Option.some Sk (Sk.prod (skel A) (skel B)))),
   hfst :- (Eq (Car (skel A))
             (Eq.mp (congrArg Car (usk_skel A)) alpha)
             (Prod.fst (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))),
   hsnd :- (Eq (Car (skel B))
             (Eq.mp (congrArg Car (usk_skel B)) beta)
             (Prod.snd (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))]
  (Eq (Car (skel C))
    (den chkf dec encTy n t
      (List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))) (skel C)
      (henv_of_usk (List.cons Exp B (List.cons Exp A D))
        (Prod.mk beta (Prod.mk alpha eta))))
    (den chkf dec encTy n (Exp.letp C p t) (skels D) (skel C)
      (henv_of_usk D eta)))
  (exact (Eq.trans
    (congrArg
      (fn [en :- (HEnv (skels (List.cons Exp B (List.cons Exp A D))))]
        (den chkf dec encTy n t
          (List.cons Sk (skel B) (List.cons Sk (skel A) (skels D)))
          (skel C) en))
      (henv_ext2 A B D alpha beta eta))
    (Eq.trans
      (congrArg
        (fn [vb :- (Car (skel B))]
          (den chkf dec encTy n t
            (List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))) (skel C)
            (Prod.mk vb
              (Prod.mk (Eq.mp (congrArg Car (usk_skel A)) alpha)
                (henv_of_usk D eta)))))
        hsnd)
      (Eq.trans
        (congrArg
          (fn [va :- (Car (skel A))]
            (den chkf dec encTy n t
              (List.cons Sk (skel B) (List.cons Sk (skel A) (skels D))) (skel C)
              (Prod.mk
                (Prod.snd (den chkf dec encTy n p (skels D)
                            (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))
                (Prod.mk va (henv_of_usk D eta)))))
          hfst)
        (Eq.symm (den_letp_some chkf dec encTy n C p t (skels D)
                   (skel A) (skel B) (skel C) (henv_of_usk D eta) hsu)))))))

;; The same equation at the carrier E reads.  Both sides are the cast of
;; the denotations den_let_body equates, along usk_skel C.
(thm denU_let
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, C :- Exp,
   p :- Exp, t :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   alpha :- (Car (uskSk (usk A))),
   beta :- (Car (uskSk (usk B))),
   hsu :- (Eq (Option Sk) (skOf (skels D) p)
             (Option.some Sk (Sk.prod (skel A) (skel B)))),
   hfst :- (Eq (Car (skel A))
             (Eq.mp (congrArg Car (usk_skel A)) alpha)
             (Prod.fst (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))),
   hsnd :- (Eq (Car (skel B))
             (Eq.mp (congrArg Car (usk_skel B)) beta)
             (Prod.snd (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta))))]
  (Eq (Car (uskSk (usk C)))
    (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D)) t C
      (Prod.mk beta (Prod.mk alpha eta)))
    (denU chkf dec encTy n D (Exp.letp C p t) C eta))
  (exact (congrArg
    (fn [x :- (Car (skel C))]
      (Eq.mp (congrArg Car (Eq.symm (usk_skel C))) x))
    (den_let_body chkf dec encTy n D A B C p t eta alpha beta hsu hfst hsnd))))

;; From the body's induction hypothesis at lift 2 0 C to E at C of the
;; let's denotation, and the eLet node.  Usage-independent: the three
;; cases below only differ in how the product is eliminated and how the
;; first binder extends the environment.
(thm adeqE_let_finish
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), A :- Exp, B :- Exp, C :- Exp,
   p :- Exp, t :- Exp, pe :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   va :- RV, vb :- RV, w :- RV,
   alpha :- (Car (uskSk (usk A))),
   beta :- (Car (uskSk (usk B))),
   hsu :- (Eq (Option Sk) (skOf (skels D) p)
             (Option.some Sk (Sk.prod (skel A) (skel B)))),
   hfst :- (Eq (Car (skel A))
             (Eq.mp (congrArg Car (usk_skel A)) alpha)
             (Prod.fst (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))),
   hsnd :- (Eq (Car (skel B))
             (Eq.mp (congrArg Car (usk_skel B)) beta)
             (Prod.snd (den chkf dec encTy n p (skels D)
                         (Sk.prod (skel A) (skel B)) (henv_of_usk D eta)))),
   hep :- (EvalE chkf dec encTy n rho pe (RV.pair va vb)),
   het :- (EvalE chkf dec encTy n
            (List.cons RV vb (List.cons RV va rho)) te w),
   hrel :- (Erel chkf dec encTy n (usk (lift 2 0 C)) w
             (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
               t (lift 2 0 C) (Prod.mk beta (Prod.mk alpha eta))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.letp C pe te) v)
         (Erel chkf dec encTy n (usk C) v
           (denU chkf dec encTy n D (Exp.letp C p t) C eta)))))
  (have hrelC (Erel chkf dec encTy n (usk C) w
                (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                         (usk_lift C 2 0))
                  (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
                    t (lift 2 0 C) (Prod.mk beta (Prod.mk alpha eta)))))
    (erel_at chkf dec encTy n (usk (lift 2 0 C)) (usk C) w
      (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
        t (lift 2 0 C) (Prod.mk beta (Prod.mk alpha eta)))
      hrel (usk_lift C 2 0)))
  (have hrelB (Erel chkf dec encTy n (usk C) w
                (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
                  t C (Prod.mk beta (Prod.mk alpha eta))))
    (erel_car chkf dec encTy n (usk C) w
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
               (usk_lift C 2 0))
        (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
          t (lift 2 0 C) (Prod.mk beta (Prod.mk alpha eta))))
      (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
        t C (Prod.mk beta (Prod.mk alpha eta)))
      hrelC
      (denU_lift chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
        t C (Prod.mk beta (Prod.mk alpha eta)))))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eLet chkf dec encTy n rho C pe te va vb w hep het))
  (exact (erel_car chkf dec encTy n (usk C) w
           (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
             t C (Prod.mk beta (Prod.mk alpha eta)))
           (denU chkf dec encTy n D (Exp.letp C p t) C eta)
           hrelB
           (denU_let chkf dec encTy n D A B C p t eta alpha beta hsu hfst hsnd))))

;; Theorem 4′, fundamental property, let at Σ₁.  Both components are
;; E-related.  Variable 0 (the second) is usage 1; variable 1 (the first)
;; is usage 1 as well.  skOf of the scrutinee is the product the typing
;; assigned (skOf_rt, the Σ is formed, hence clean).
(thm adeqE_let1
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U),
   A :- Exp, B :- Exp, C :- Exp, p :- Exp, t :- Exp, pe :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us2 rho eta),
   hp :- (Rt chkf D us1 p (Exp.tSig U.u1 A B)),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihp :- (Exists (fn [vp :- RV]
            (And (EvalE chkf dec encTy n rho pe vp)
                 (Erel chkf dec encTy n (usk (Exp.tSig U.u1 A B)) vp
                   (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))))),
   iht :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp B (List.cons Exp A D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp B (List.cons Exp A D)))
                    (List.cons U U.u1 (List.cons U U.u1 us2)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 te w)
                       (Erel chkf dec encTy n (usk (lift 2 0 C)) w
                         (denU chkf dec encTy n
                           (List.cons Exp B (List.cons Exp A D)) t
                           (lift 2 0 C) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.letp C pe te) v)
         (Erel chkf dec encTy n (usk C) v
           (denU chkf dec encTy n D (Exp.letp C p t) C eta)))))
  (have hSS (SkJ Bool.true (skels D) (Exp.tSig U.u1 A B) Sk.unit)
    (SkJ.wSig (skels D) U.u1 A B
      (lemma25_tl_type chkf D A hA)
      (lemma25_tl_type chkf (List.cons Exp A D) B hB)))
  (have hsu (Eq (Option Sk) (skOf (skels D) p)
                (Option.some Sk (Sk.prod (skel A) (skel B))))
    (skOf_rt chkf D us1 p (Exp.tSig U.u1 A B) hp
      (skj_clean Bool.true (skels D) (Exp.tSig U.u1 A B) Sk.unit hSS)))
  (refine' (exT RV _ _ ihp _)) (intro vp hvp)
  (have hep0 (EvalE chkf dec encTy n rho pe vp) (And.left hvp))
  (have hrp (Erel chkf dec encTy n (usk (Exp.tSig U.u1 A B)) vp
              (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
    (And.right hvp))
  (refine' (exT RV _ _ hrp _)) (intro va ha)
  (refine' (exT RV _ _ ha _)) (intro vb hb)
  (have heq (Eq RV vp (RV.pair va vb)) (And.left hb))
  (have hra (Erel chkf dec encTy n (usk A) va
              (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)))
    (And.left (And.right hb)))
  (have hrb (Erel chkf dec encTy n (usk B) vb
              (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)))
    (And.right (And.right hb)))
  (have hep (EvalE chkf dec encTy n rho pe (RV.pair va vb))
    (evalE_cast chkf dec encTy n rho pe vp (RV.pair va vb) hep0 heq))
  (have henvA (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                (List.cons U U.u1 us2) (List.cons RV va rho)
                (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta))
    (envE_cons1 chkf dec encTy n (usk A) (uskCtx D) us2 va rho
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta hra henv))
  (have henvB (envE chkf dec encTy n
                (uskCtx (List.cons Exp B (List.cons Exp A D)))
                (List.cons U U.u1 (List.cons U U.u1 us2))
                (List.cons RV vb (List.cons RV va rho))
                (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
                  (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta)))
    (envE_cons1 chkf dec encTy n (usk B) (uskCtx (List.cons Exp A D))
      (List.cons U U.u1 us2) vb (List.cons RV va rho)
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
      (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta)
      hrb henvA))
  (refine' (exT RV _ _
    (iht (List.cons RV vb (List.cons RV va rho))
         (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
           (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta))
         henvB) _))
  (intro w hw)
  (have het (EvalE chkf dec encTy n (List.cons RV vb (List.cons RV va rho)) te w)
    (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk (lift 2 0 C)) w
               (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
                 t (lift 2 0 C)
                 (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
                   (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta)) eta))))
    (And.right hw))
  (exact (adeqE_let_finish chkf dec encTy n D A B C p t pe te rho eta va vb w
           (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
           (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u1 A B) eta))
           hsu
           (denU_proj1_fst chkf dec encTy n D A B p eta)
           (denU_proj1_snd chkf dec encTy n D A B p eta)
           hep het hrel)))

;; Theorem 4′, fundamental property, let at Σ₀.  E's product clause gives
;; only the second component.  The first runtime value extends the
;; environment at usage 0, which is unconstrained; the denotation still
;; binds Prod.fst of ⟦p⟧.
(thm adeqE_let0
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U),
   A :- Exp, B :- Exp, C :- Exp, p :- Exp, t :- Exp, pe :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us2 rho eta),
   hp :- (Rt chkf D us1 p (Exp.tSig U.u0 A B)),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihp :- (Exists (fn [vp :- RV]
            (And (EvalE chkf dec encTy n rho pe vp)
                 (Erel chkf dec encTy n (usk (Exp.tSig U.u0 A B)) vp
                   (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))))),
   iht :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp B (List.cons Exp A D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp B (List.cons Exp A D)))
                    (List.cons U U.u1 (List.cons U U.u0 us2)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 te w)
                       (Erel chkf dec encTy n (usk (lift 2 0 C)) w
                         (denU chkf dec encTy n
                           (List.cons Exp B (List.cons Exp A D)) t
                           (lift 2 0 C) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.letp C pe te) v)
         (Erel chkf dec encTy n (usk C) v
           (denU chkf dec encTy n D (Exp.letp C p t) C eta)))))
  (have hSS (SkJ Bool.true (skels D) (Exp.tSig U.u0 A B) Sk.unit)
    (SkJ.wSig (skels D) U.u0 A B
      (lemma25_tl_type chkf D A hA)
      (lemma25_tl_type chkf (List.cons Exp A D) B hB)))
  (have hsu (Eq (Option Sk) (skOf (skels D) p)
                (Option.some Sk (Sk.prod (skel A) (skel B))))
    (skOf_rt chkf D us1 p (Exp.tSig U.u0 A B) hp
      (skj_clean Bool.true (skels D) (Exp.tSig U.u0 A B) Sk.unit hSS)))
  (refine' (exT RV _ _ ihp _)) (intro vp hvp)
  (have hep0 (EvalE chkf dec encTy n rho pe vp) (And.left hvp))
  (have hrp (Erel chkf dec encTy n (usk (Exp.tSig U.u0 A B)) vp
              (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
    (And.right hvp))
  (refine' (exT RV _ _ hrp _)) (intro va ha)
  (refine' (exT RV _ _ ha _)) (intro vb hb)
  (have heq (Eq RV vp (RV.pair va vb)) (And.left hb))
  (have hrb (Erel chkf dec encTy n (usk B) vb
              (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)))
    (And.right hb))
  (have hep (EvalE chkf dec encTy n rho pe (RV.pair va vb))
    (evalE_cast chkf dec encTy n rho pe vp (RV.pair va vb) hep0 heq))
  (have henvA (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                (List.cons U U.u0 us2) (List.cons RV va rho)
                (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta))
    (envE_cons0 chkf dec encTy n (usk A) (uskCtx D) us2 va rho
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta henv))
  (have henvB (envE chkf dec encTy n
                (uskCtx (List.cons Exp B (List.cons Exp A D)))
                (List.cons U U.u1 (List.cons U U.u0 us2))
                (List.cons RV vb (List.cons RV va rho))
                (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
                  (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta)))
    (envE_cons1 chkf dec encTy n (usk B) (uskCtx (List.cons Exp A D))
      (List.cons U U.u0 us2) vb (List.cons RV va rho)
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
      (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta)
      hrb henvA))
  (refine' (exT RV _ _
    (iht (List.cons RV vb (List.cons RV va rho))
         (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
           (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta))
         henvB) _))
  (intro w hw)
  (have het (EvalE chkf dec encTy n (List.cons RV vb (List.cons RV va rho)) te w)
    (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk (lift 2 0 C)) w
               (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
                 t (lift 2 0 C)
                 (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
                   (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta)) eta))))
    (And.right hw))
  (exact (adeqE_let_finish chkf dec encTy n D A B C p t pe te rho eta va vb w
           (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
           (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.u0 A B) eta))
           hsu
           (denU_proj0_fst chkf dec encTy n D A B p eta)
           (denU_proj0_snd chkf dec encTy n D A B p eta)
           hep het hrel)))

;; Theorem 4′, fundamental property, let at Σω.  The same clause as Σ₁,
;; with the first binder at usage ω.
(thm adeqE_letw
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U), us2 :- (List U),
   A :- Exp, B :- Exp, C :- Exp, p :- Exp, t :- Exp, pe :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   henv :- (envE chkf dec encTy n (uskCtx D) us2 rho eta),
   hp :- (Rt chkf D us1 p (Exp.tSig U.uw A B)),
   hA :- (Tl chkf Bool.true D A Exp.tUnit),
   hB :- (Tl chkf Bool.true (List.cons Exp A D) B Exp.tUnit),
   ihp :- (Exists (fn [vp :- RV]
            (And (EvalE chkf dec encTy n rho pe vp)
                 (Erel chkf dec encTy n (usk (Exp.tSig U.uw A B)) vp
                   (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))))),
   iht :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp B (List.cons Exp A D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp B (List.cons Exp A D)))
                    (List.cons U U.u1 (List.cons U U.uw us2)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 te w)
                       (Erel chkf dec encTy n (usk (lift 2 0 C)) w
                         (denU chkf dec encTy n
                           (List.cons Exp B (List.cons Exp A D)) t
                           (lift 2 0 C) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.letp C pe te) v)
         (Erel chkf dec encTy n (usk C) v
           (denU chkf dec encTy n D (Exp.letp C p t) C eta)))))
  (have hSS (SkJ Bool.true (skels D) (Exp.tSig U.uw A B) Sk.unit)
    (SkJ.wSig (skels D) U.uw A B
      (lemma25_tl_type chkf D A hA)
      (lemma25_tl_type chkf (List.cons Exp A D) B hB)))
  (have hsu (Eq (Option Sk) (skOf (skels D) p)
                (Option.some Sk (Sk.prod (skel A) (skel B))))
    (skOf_rt chkf D us1 p (Exp.tSig U.uw A B) hp
      (skj_clean Bool.true (skels D) (Exp.tSig U.uw A B) Sk.unit hSS)))
  (refine' (exT RV _ _ ihp _)) (intro vp hvp)
  (have hep0 (EvalE chkf dec encTy n rho pe vp) (And.left hvp))
  (have hrp (Erel chkf dec encTy n (usk (Exp.tSig U.uw A B)) vp
              (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
    (And.right hvp))
  (refine' (exT RV _ _ hrp _)) (intro va ha)
  (refine' (exT RV _ _ ha _)) (intro vb hb)
  (have heq (Eq RV vp (RV.pair va vb)) (And.left hb))
  (have hra (Erel chkf dec encTy n (usk A) va
              (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)))
    (And.left (And.right hb)))
  (have hrb (Erel chkf dec encTy n (usk B) vb
              (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)))
    (And.right (And.right hb)))
  (have hep (EvalE chkf dec encTy n rho pe (RV.pair va vb))
    (evalE_cast chkf dec encTy n rho pe vp (RV.pair va vb) hep0 heq))
  (have henvA (envE chkf dec encTy n (uskCtx (List.cons Exp A D))
                (List.cons U U.uw us2) (List.cons RV va rho)
                (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta))
    (envE_consw chkf dec encTy n (usk A) (uskCtx D) us2 va rho
      (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta hra henv))
  (have henvB (envE chkf dec encTy n
                (uskCtx (List.cons Exp B (List.cons Exp A D)))
                (List.cons U U.u1 (List.cons U U.uw us2))
                (List.cons RV vb (List.cons RV va rho))
                (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
                  (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta)))
    (envE_cons1 chkf dec encTy n (usk B) (uskCtx (List.cons Exp A D))
      (List.cons U U.uw us2) vb (List.cons RV va rho)
      (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
      (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta)
      hrb henvA))
  (refine' (exT RV _ _
    (iht (List.cons RV vb (List.cons RV va rho))
         (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
           (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta))
         henvB) _))
  (intro w hw)
  (have het (EvalE chkf dec encTy n (List.cons RV vb (List.cons RV va rho)) te w)
    (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk (lift 2 0 C)) w
               (denU chkf dec encTy n (List.cons Exp B (List.cons Exp A D))
                 t (lift 2 0 C)
                 (Prod.mk (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
                   (Prod.mk (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta)) eta))))
    (And.right hw))
  (exact (adeqE_let_finish chkf dec encTy n D A B C p t pe te rho eta va vb w
           (Prod.fst (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
           (Prod.snd (denU chkf dec encTy n D p (Exp.tSig U.uw A B) eta))
           hsu
           (denU_projw_fst chkf dec encTy n D A B p eta)
           (denU_projw_snd chkf dec encTy n D A B p eta)
           hep het hrel)))

;; --- if (Theorem 4′, the conditional) ---------------------------------------
;;
;; At Bool, E is equality with the denotation (denU_bool), so both sides see
;; the same Boolean and select the same branch.  The branches are judged at
;; one type, so the hypothesis applies with no transport.  Bool.rec's
;; arguments are else, then, scrutinee, and cases on the Boolean meets the
;; false goal first.

(thm denU_bool
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), b :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq Bool
    (denU chkf dec encTy n D b Exp.tBool eta)
    (den chkf dec encTy n b (skels D) Sk.bool (henv_of_usk D eta)))
  (rfl))

(thm cast_boolrec
  [s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2),
   a :- (Car s2), b :- (Car s2), c :- Bool]
  (Eq (Car s1)
    (Eq.mp (congrArg Car (Eq.symm e))
      (Bool.rec$1 (fn [_ :- Bool] (Car s2)) a b c))
    (Bool.rec$1 (fn [_ :- Bool] (Car s1))
      (Eq.mp (congrArg Car (Eq.symm e)) a)
      (Eq.mp (congrArg Car (Eq.symm e)) b)
      c))
  (cases e)
  (cases c)
  (rfl)
  (rfl))

(thm denU_ite
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), b :- Exp, t :- Exp, e :- Exp, C :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk C)))
    (denU chkf dec encTy n D (Exp.ite b t e) C eta)
    (Bool.rec$1 (fn [_ :- Bool] (Car (uskSk (usk C))))
      (denU chkf dec encTy n D e C eta)
      (denU chkf dec encTy n D t C eta)
      (denU chkf dec encTy n D b Exp.tBool eta)))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (skel C))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel C))) x))
      (den_ite_at chkf dec encTy n b t e (skels D) (skel C) (henv_of_usk D eta)))
    (cast_boolrec (uskSk (usk C)) (skel C) (usk_skel C)
      (den chkf dec encTy n e (skels D) (skel C) (henv_of_usk D eta))
      (den chkf dec encTy n t (skels D) (skel C) (henv_of_usk D eta))
      (den chkf dec encTy n b (skels D) Sk.bool (henv_of_usk D eta))))))

(thm adeqE_ite_cases
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   b :- Exp, t :- Exp, e :- Exp, C :- Exp,
   be :- Exp, te :- Exp, ee :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   c :- Bool,
   hb :- (EvalE chkf dec encTy n rho be (RV.bool c)),
   iht :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho te v)
                 (Erel chkf dec encTy n (usk C) v
                   (denU chkf dec encTy n D t C eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho ee v)
                 (Erel chkf dec encTy n (usk C) v
                   (denU chkf dec encTy n D e C eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.ite be te ee) v)
         (Erel chkf dec encTy n (usk C) v
           (Bool.rec$1 (fn [_ :- Bool] (Car (uskSk (usk C))))
             (denU chkf dec encTy n D e C eta)
             (denU chkf dec encTy n D t C eta)
             c)))))
  (cases c)
  (refine' (exT RV _ _ ihe _)) (intro ve he)
  (have hee (EvalE chkf dec encTy n rho ee ve) (And.left he))
  (have hre (Erel chkf dec encTy n (usk C) ve (denU chkf dec encTy n D e C eta))
    (And.right he))
  (constructor) (exact ve)
  (constructor)
  (exact (EvE.eIteF chkf dec encTy n rho be te ee ve hb hee))
  (exact hre)
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (EvalE chkf dec encTy n rho te vt) (And.left ht))
  (have hrt (Erel chkf dec encTy n (usk C) vt (denU chkf dec encTy n D t C eta))
    (And.right ht))
  (constructor) (exact vt)
  (constructor)
  (exact (EvE.eIteT chkf dec encTy n rho be te ee vt hb het))
  (exact hrt))

(thm adeqE_ite
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   b :- Exp, t :- Exp, e :- Exp, C :- Exp,
   be :- Exp, te :- Exp, ee :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihb :- (Exists (fn [vb :- RV]
            (And (EvalE chkf dec encTy n rho be vb)
                 (Erel chkf dec encTy n (usk Exp.tBool) vb
                   (denU chkf dec encTy n D b Exp.tBool eta))))),
   iht :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho te v)
                 (Erel chkf dec encTy n (usk C) v
                   (denU chkf dec encTy n D t C eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho ee v)
                 (Erel chkf dec encTy n (usk C) v
                   (denU chkf dec encTy n D e C eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.ite be te ee) v)
         (Erel chkf dec encTy n (usk C) v
           (denU chkf dec encTy n D (Exp.ite b t e) C eta)))))
  (rw [(denU_ite chkf dec encTy n D b t e C eta)])
  (refine' (exT RV _ _ ihb _)) (intro vb hb0)
  (have hev (EvalE chkf dec encTy n rho be vb) (And.left hb0))
  (have hr (Eq RV vb (RV.bool (denU chkf dec encTy n D b Exp.tBool eta)))
    (And.right hb0))
  (exact (adeqE_ite_cases chkf dec encTy n D b t e C be te ee rho eta
           (denU chkf dec encTy n D b Exp.tBool eta)
           (evalE_cast chkf dec encTy n rho be vb
             (RV.bool (denU chkf dec encTy n D b Exp.tBool eta)) hev hr)
           iht ihe)))


;; --- substitution of equal skeletons ----------------------------------------
;;
;; elimBool and caseLbl conclude at P[b/x] while a premise is judged at
;; P[c/x] or at P.  usk and skel read a substituted type only through the
;; skeletons of the values the substitution inserts (the variable case) and
;; through Π, Σ and branch lists.  Two substitutions that agree pointwise
;; on those skeletons therefore agree (skel_subst_eq, usk_subst_eq), and
;; denU transports along that agreement (denU_ty).  A Boolean or label term
;; has skeleton Unit, so it agrees with tt, ff and with the Unit hypothesis
;; of usk_subst1.

(thm up_skel_eq
  [sg1 :- (=> Nat Exp), sg2 :- (=> Nat Exp),
   hs :- (forall [i Nat] (Eq Sk (skel (sg1 i)) (skel (sg2 i)))),
   i :- Nat]
  (Eq Sk (skel (up sg1 i)) (skel (up sg2 i)))
  (cases i)
  (rfl)
  (exact (Eq.trans (skel_lift (sg1 n) 1 0)
           (Eq.trans (hs n) (Eq.symm (skel_lift (sg2 n) 1 0))))))

(thm upn_skel_eq [n :- Nat]
  (forall [sg1 (=> Nat Exp)]
    (forall [sg2 (=> Nat Exp)]
      (=> (forall [i Nat] (Eq Sk (skel (sg1 i)) (skel (sg2 i))))
          (forall [i Nat] (Eq Sk (skel ((upn n sg1) i)) (skel ((upn n sg2) i)))))))
  (induction n)
  (intro sg1 sg2 hs)
  (exact hs)
  (intro sg1 sg2 hs i)
  (exact (up_skel_eq (upn n sg1) (upn n sg2) (ih_n sg1 sg2 hs) i)))

(thm up_usk_eq
  [sg1 :- (=> Nat Exp), sg2 :- (=> Nat Exp),
   hs :- (forall [i Nat] (Eq USk (usk (sg1 i)) (usk (sg2 i)))),
   i :- Nat]
  (Eq USk (usk (up sg1 i)) (usk (up sg2 i)))
  (cases i)
  (rfl)
  (exact (Eq.trans (usk_lift (sg1 n) 1 0)
           (Eq.trans (hs n) (Eq.symm (usk_lift (sg2 n) 1 0))))))

(thm upn_usk_eq [n :- Nat]
  (forall [sg1 (=> Nat Exp)]
    (forall [sg2 (=> Nat Exp)]
      (=> (forall [i Nat] (Eq USk (usk (sg1 i)) (usk (sg2 i))))
          (forall [i Nat] (Eq USk (usk ((upn n sg1) i)) (usk ((upn n sg2) i)))))))
  (induction n)
  (intro sg1 sg2 hs)
  (exact hs)
  (intro sg1 sg2 hs i)
  (exact (up_usk_eq (upn n sg1) (upn n sg2) (ih_n sg1 sg2 hs) i)))


(let [exp-fields @#'lcert.formal.syntactic/exp-fields
      congruence @#'lcert.formal.syntactic/congruence
      args (fn [ty fun hyp-name fields]
             (for [[f fty depth] fields :when (= fty 'Exp)]
               [ty
                (list fun (list 'subst (if (zero? depth) 'sg1 (list 'upn depth 'sg1)) f))
                (list fun (list 'subst (if (zero? depth) 'sg2 (list 'upn depth 'sg2)) f))
                (list (symbol (str "ih_" f))
                      (if (zero? depth) 'sg1 (list 'upn depth 'sg1))
                      (if (zero? depth) 'sg2 (list 'upn depth 'sg2))
                      (if (zero? depth) 'hs (list hyp-name depth 'sg1 'sg2 'hs)))]))]
  (a/prove-theorem 'skel_subst_eq (lv '[e :- Exp])
    (lv '(forall [sg1 (=> Nat Exp)]
           (forall [sg2 (=> Nat Exp)]
             (=> (forall [i Nat] (Eq Sk (skel (sg1 i)) (skel (sg2 i))))
                 (Eq Sk (skel (subst sg1 e)) (skel (subst sg2 e)))))))
    (lv (into ['(induction e)]
              (mapcat
               (fn [[ctor fields]]
                 (cons '(intro sg1 sg2 hs)
                       (cond
                         (= ctor 'var) '[(exact (hs i))]
                         (#{'tPi 'tSig 'tBrs} ctor)
                         [(list 'exact
                                (congruence (if (= ctor 'tSig) 'Sk.prod 'Sk.arr)
                                  (into (if (= ctor 'tBrs) [['Sk 'Sk.lbl 'Sk.lbl nil]] [])
                                        (args 'Sk 'skel 'upn_skel_eq fields))))]
                         :else '[(rfl)])))
               exp-fields))))
  (a/prove-theorem 'usk_subst_eq (lv '[e :- Exp])
    (lv '(forall [sg1 (=> Nat Exp)]
           (forall [sg2 (=> Nat Exp)]
             (=> (forall [i Nat] (Eq USk (usk (sg1 i)) (usk (sg2 i))))
                 (Eq USk (usk (subst sg1 e)) (usk (subst sg2 e)))))))
    (lv (into ['(induction e)]
              (mapcat
               (fn [[ctor fields]]
                 (cons '(intro sg1 sg2 hs)
                       (cond
                         (= ctor 'var) '[(exact (hs i))]
                         (= ctor 'tPi)
                         (into ['(cases r)]
                               (for [k '[USk.arr0 USk.arrN USk.arrN]]
                                 (list 'exact (congruence k (args 'USk 'usk 'upn_usk_eq fields)))))
                         (= ctor 'tSig)
                         (into ['(cases r)]
                               (for [k '[USk.prod0 USk.prodN USk.prodN]]
                                 (list 'exact (congruence k (args 'USk 'usk 'upn_usk_eq fields)))))
                         (= ctor 'tBrs)
                         [(list 'exact
                                (congruence 'USk.arrN
                                  (into [['USk '(USk.base Sk.lbl) '(USk.base Sk.lbl) nil]]
                                        (args 'USk 'usk 'upn_usk_eq fields))))]
                         :else '[(rfl)])))
               exp-fields)))))


(thm skel_subst1_eq
  [u :- Exp, v :- Exp, e :- Exp, h :- (Eq Sk (skel u) (skel v))]
  (Eq Sk (skel (subst1 u e)) (skel (subst1 v e)))
  (exact (skel_subst_eq e
           (fn [i :- Nat] (inst1 u i))
           (fn [i :- Nat] (inst1 v i))
           (fn [i :- Nat]
             (Nat.rec (fn [j :- Nat] (Eq Sk (skel (inst1 u j)) (skel (inst1 v j))))
               h
               (fn [j :- Nat, _ :- (Eq Sk (skel (inst1 u j)) (skel (inst1 v j)))]
                 (Eq.refl$1 (skel (Exp.var j))))
               i)))))

(thm usk_subst1_eq
  [u :- Exp, v :- Exp, e :- Exp, h :- (Eq USk (usk u) (usk v))]
  (Eq USk (usk (subst1 u e)) (usk (subst1 v e)))
  (exact (usk_subst_eq e
           (fn [i :- Nat] (inst1 u i))
           (fn [i :- Nat] (inst1 v i))
           (fn [i :- Nat]
             (Nat.rec (fn [j :- Nat] (Eq USk (usk (inst1 u j)) (usk (inst1 v j))))
               h
               (fn [j :- Nat, _ :- (Eq USk (usk (inst1 u j)) (usk (inst1 v j)))]
                 (Eq.refl$1 (usk (Exp.var j))))
               i)))))

(thm denU_ty_from
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   eu :- (Eq USk (usk B) (usk A)),
   eS :- (Eq Sk (skel B) (skel A))]
  (Eq (Car (uskSk (usk B)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel A)))
        (den chkf dec encTy n t (skels D) (skel A) (henv_of_usk D eta))))
    (denU chkf dec encTy n D t B eta))
  (exact (Eq.trans
    (cast_usk_path (usk B) (usk A) eu
      (skel A) (skel B)
      (usk_skel A) (usk_skel B) eS
      (den chkf dec encTy n t (skels D) (skel A) (henv_of_usk D eta)))
    (Eq.trans
      (cast_square
        (uskSk (usk B)) (uskSk (usk A)) (skel A) (skel B)
        (usk_skel A) eS (usk_skel B)
        (den chkf dec encTy n t (skels D) (skel A) (henv_of_usk D eta)))
      (congrArg
        (fn [x :- (Car (skel B))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel B))) x))
        (den_at_eq chkf dec encTy n t (skels D)
          (skel A) (skel B) (Eq.symm eS)
          (henv_of_usk D eta)))))))

(thm denU_ty
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   eu :- (Eq USk (usk B) (usk A)),
   eS :- (Eq Sk (skel B) (skel A))]
  (Eq (Car (uskSk (usk A)))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu)
      (denU chkf dec encTy n D t B eta))
    (denU chkf dec encTy n D t A eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk B)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu) x))
      (Eq.symm (denU_ty_from chkf dec encTy n D t A B eta eu eS)))
    (cast_usk_back (usk B) (usk A) eu
      (denU chkf dec encTy n D t A eta)))))


;; --- elimBool (Theorem 4′) --------------------------------------------------
;;
;; The same Boolean split as if.  The then branch is judged at P[tt] and the
;; else branch at P[ff]; the conclusion is P[b].  adeqE_retarget moves each
;; branch's E-witness onto P[b] once the skeletons agree, which they do
;; because a Boolean term has skeleton Unit (skj_term_unit, from the Rt the
;; assembly already has for skOf).  adeqE_elim is the split given those
;; equations; adeqE_elimB derives the equations.

(thm denU_elim
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk (subst1 b P))))
    (denU chkf dec encTy n D (Exp.elimB P b t e) (subst1 b P) eta)
    (Bool.rec$1 (fn [_ :- Bool] (Car (uskSk (usk (subst1 b P)))))
      (denU chkf dec encTy n D e (subst1 b P) eta)
      (denU chkf dec encTy n D t (subst1 b P) eta)
      (denU chkf dec encTy n D b Exp.tBool eta)))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (skel (subst1 b P)))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 b P)))) x))
      (den_elimB_at chkf dec encTy n P b t e (skels D) (skel (subst1 b P))
        (henv_of_usk D eta)))
    (cast_boolrec (uskSk (usk (subst1 b P))) (skel (subst1 b P))
      (usk_skel (subst1 b P))
      (den chkf dec encTy n e (skels D) (skel (subst1 b P)) (henv_of_usk D eta))
      (den chkf dec encTy n t (skels D) (skel (subst1 b P)) (henv_of_usk D eta))
      (den chkf dec encTy n b (skels D) Sk.bool (henv_of_usk D eta))))))

(thm adeqE_retarget
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp, te :- Exp, A :- Exp, B :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   eu :- (Eq USk (usk A) (usk B)),
   eS :- (Eq Sk (skel A) (skel B)),
   ih :- (Exists (fn [v :- RV]
           (And (EvalE chkf dec encTy n rho te v)
                (Erel chkf dec encTy n (usk A) v
                  (denU chkf dec encTy n D t A eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho te v)
         (Erel chkf dec encTy n (usk B) v
           (denU chkf dec encTy n D t B eta)))))
  (refine' (exT RV _ _ ih _)) (intro v hv)
  (have he (EvalE chkf dec encTy n rho te v) (And.left hv))
  (have hr (Erel chkf dec encTy n (usk A) v (denU chkf dec encTy n D t A eta))
    (And.right hv))
  (constructor) (exact v)
  (constructor)
  (exact he)
  (exact (erel_car chkf dec encTy n (usk B) v
           (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu)
             (denU chkf dec encTy n D t A eta))
           (denU chkf dec encTy n D t B eta)
           (erel_at chkf dec encTy n (usk A) (usk B) v
             (denU chkf dec encTy n D t A eta) hr eu)
           (denU_ty chkf dec encTy n D t B A eta eu eS))))

(thm adeqE_elim_cases
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   be :- Exp, te :- Exp, ee :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   c :- Bool,
   hb :- (EvalE chkf dec encTy n rho be (RV.bool c)),
   iht :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho te v)
                 (Erel chkf dec encTy n (usk (subst1 b P)) v
                   (denU chkf dec encTy n D t (subst1 b P) eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho ee v)
                 (Erel chkf dec encTy n (usk (subst1 b P)) v
                   (denU chkf dec encTy n D e (subst1 b P) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.elimB P be te ee) v)
         (Erel chkf dec encTy n (usk (subst1 b P)) v
           (Bool.rec$1 (fn [_ :- Bool] (Car (uskSk (usk (subst1 b P)))))
             (denU chkf dec encTy n D e (subst1 b P) eta)
             (denU chkf dec encTy n D t (subst1 b P) eta)
             c)))))
  (cases c)
  (refine' (exT RV _ _ ihe _)) (intro ve he)
  (have hee (EvalE chkf dec encTy n rho ee ve) (And.left he))
  (have hre (Erel chkf dec encTy n (usk (subst1 b P)) ve
              (denU chkf dec encTy n D e (subst1 b P) eta))
    (And.right he))
  (constructor) (exact ve)
  (constructor)
  (exact (EvE.eElimF chkf dec encTy n rho P be te ee ve hb hee))
  (exact hre)
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (EvalE chkf dec encTy n rho te vt) (And.left ht))
  (have hrt (Erel chkf dec encTy n (usk (subst1 b P)) vt
              (denU chkf dec encTy n D t (subst1 b P) eta))
    (And.right ht))
  (constructor) (exact vt)
  (constructor)
  (exact (EvE.eElimT chkf dec encTy n rho P be te ee vt hb het))
  (exact hrt))

(thm adeqE_elim
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   be :- Exp, te :- Exp, ee :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hub :- (Eq USk (usk b) (USk.base Sk.unit)),
   hsb :- (Eq Sk (skel b) Sk.unit),
   ihb :- (Exists (fn [vb :- RV]
            (And (EvalE chkf dec encTy n rho be vb)
                 (Erel chkf dec encTy n (usk Exp.tBool) vb
                   (denU chkf dec encTy n D b Exp.tBool eta))))),
   iht :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho te v)
                 (Erel chkf dec encTy n (usk (subst1 Exp.tt P)) v
                   (denU chkf dec encTy n D t (subst1 Exp.tt P) eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho ee v)
                 (Erel chkf dec encTy n (usk (subst1 Exp.ff P)) v
                   (denU chkf dec encTy n D e (subst1 Exp.ff P) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.elimB P be te ee) v)
         (Erel chkf dec encTy n (usk (subst1 b P)) v
           (denU chkf dec encTy n D (Exp.elimB P b t e) (subst1 b P) eta)))))
  (rw [(denU_elim chkf dec encTy n D P b t e eta)])
  (refine' (exT RV _ _ ihb _)) (intro vb hb0)
  (have hev (EvalE chkf dec encTy n rho be vb) (And.left hb0))
  (have hr (Eq RV vb (RV.bool (denU chkf dec encTy n D b Exp.tBool eta)))
    (And.right hb0))
  (have htt (Eq USk (usk Exp.tt) (USk.base Sk.unit)) (rfl))
  (have hff (Eq USk (usk Exp.ff) (USk.base Sk.unit)) (rfl))
  (have hst (Eq Sk (skel Exp.tt) Sk.unit) (rfl))
  (have hsf (Eq Sk (skel Exp.ff) Sk.unit) (rfl))
  (exact (adeqE_elim_cases chkf dec encTy n D P b t e be te ee rho eta
           (denU chkf dec encTy n D b Exp.tBool eta)
           (evalE_cast chkf dec encTy n rho be vb
             (RV.bool (denU chkf dec encTy n D b Exp.tBool eta)) hev hr)
           (adeqE_retarget chkf dec encTy n D t te
             (subst1 Exp.tt P) (subst1 b P) rho eta
             (usk_subst1_eq Exp.tt b P (Eq.trans htt (Eq.symm hub)))
             (skel_subst1_eq Exp.tt b P (Eq.trans hst (Eq.symm hsb)))
             iht)
           (adeqE_retarget chkf dec encTy n D e ee
             (subst1 Exp.ff P) (subst1 b P) rho eta
             (usk_subst1_eq Exp.ff b P (Eq.trans hff (Eq.symm hub)))
             (skel_subst1_eq Exp.ff b P (Eq.trans hsf (Eq.symm hsb)))
             ihe))))


;; --- caseLbl (Theorem 4′) ---------------------------------------------------
;;
;; caseLbl is application of the branch list.  usk of tBrs is an arrow from
;; labels whose codomain cast is usk of the motive (usk_skel_brs), so the
;; arrow clause of E applies.  The conclusion is P[a/x].  a is a term, so
;; its skeleton is Unit and that substitution does not change the usage
;; skeleton (usk_subst1); denU_case is the application, transported.

(thm usk_skel_brs [P :- Exp, k :- Nat]
  (Eq (Eq Sk (Sk.arr Sk.lbl (uskSk (usk P))) (Sk.arr Sk.lbl (skel P)))
      (usk_skel (Exp.tBrs P k))
      (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) (usk_skel P)))
  (rfl))

(thm cast_app_brs
  [c :- Sk, sC :- Sk, eB :- (Eq Sk c sC),
   f :- (Car (Sk.arr Sk.lbl sC)),
   a :- (Car Sk.lbl)]
  (Eq (Car c)
    ((Eq.mp (congrArg Car (Eq.symm (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) eB))) f) a)
    (Eq.mp (congrArg Car (Eq.symm eB)) (f a)))
  (cases eB)
  (rfl))

(thm denU_brs_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), bs :- Exp, P :- Exp,
   alpha :- (Car Sk.lbl),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      ((den chkf dec encTy n bs (skels D) (Sk.arr Sk.lbl (skel P)) (henv_of_usk D eta))
       alpha)))
  (exact (cast_app_brs (uskSk (usk P)) (skel P) (usk_skel P)
           (den chkf dec encTy n bs (skels D) (Sk.arr Sk.lbl (skel P)) (henv_of_usk D eta))
           alpha)))

(thm denU_lbl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), a :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car Sk.lbl)
    (denU chkf dec encTy n D a Exp.tLbl eta)
    (den chkf dec encTy n a (skels D) Sk.lbl (henv_of_usk D eta)))
  (rfl))

(thm denU_case_fun
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, a :- Exp, bs :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
     (denU chkf dec encTy n D a Exp.tLbl eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      (den chkf dec encTy n (Exp.caseL P a bs) (skels D) (skel P)
        (henv_of_usk D eta))))
  (exact (Eq.trans
    (denU_brs_at chkf dec encTy n D bs P
      (denU chkf dec encTy n D a Exp.tLbl eta) eta)
    (congrArg
      (fn [x :- (Car (skel P))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
      (Eq.symm (den_caseL_at chkf dec encTy n P a bs (skels D) (skel P)
                 (henv_of_usk D eta)))))))

(thm denU_case
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, a :- Exp, bs :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   hU :- (Eq Sk (skel a) Sk.unit)]
  (Eq (Car (uskSk (usk (subst1 a P))))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
             (Eq.symm (usk_subst1 a P (usk_of_unit a hU))))
      ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
       (denU chkf dec encTy n D a Exp.tLbl eta)))
    (denU chkf dec encTy n D (Exp.caseL P a bs) (subst1 a P) eta))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk (usk P)))]
        (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w)))
                 (Eq.symm (usk_subst1 a P (usk_of_unit a hU)))) x))
      (denU_case_fun chkf dec encTy n D P a bs eta))
    (Eq.trans
      (cast_usk_path (usk (subst1 a P)) (usk P) (usk_subst1 a P (usk_of_unit a hU))
        (skel P) (skel (subst1 a P))
        (usk_skel P) (usk_skel (subst1 a P)) (skel_subst1 a P hU)
        (den chkf dec encTy n (Exp.caseL P a bs) (skels D) (skel P)
          (henv_of_usk D eta)))
      (Eq.trans
        (cast_square
          (uskSk (usk (subst1 a P))) (uskSk (usk P)) (skel P) (skel (subst1 a P))
          (usk_skel P) (skel_subst1 a P hU) (usk_skel (subst1 a P))
          (den chkf dec encTy n (Exp.caseL P a bs) (skels D) (skel P)
            (henv_of_usk D eta)))
        (congrArg
          (fn [x :- (Car (skel (subst1 a P)))]
            (Eq.mp (congrArg Car (Eq.symm (usk_skel (subst1 a P)))) x))
          (den_at_eq chkf dec encTy n (Exp.caseL P a bs) (skels D)
            (skel P) (skel (subst1 a P)) (Eq.symm (skel_subst1 a P hU))
            (henv_of_usk D eta))))))))

(thm adeqE_case
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U),
   P :- Exp, a :- Exp, bs :- Exp, ae :- Exp, bse :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ha :- (Rt chkf D us1 a Exp.tLbl),
   iha :- (Exists (fn [va :- RV]
            (And (EvalE chkf dec encTy n rho ae va)
                 (Erel chkf dec encTy n (usk Exp.tLbl) va
                   (denU chkf dec encTy n D a Exp.tLbl eta))))),
   ihb :- (Exists (fn [vf :- RV]
            (And (EvalE chkf dec encTy n rho bse vf)
                 (Erel chkf dec encTy n (usk (Exp.tBrs P 0)) vf
                   (denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.caseL P ae bse) v)
         (Erel chkf dec encTy n (usk (subst1 a P)) v
           (denU chkf dec encTy n D (Exp.caseL P a bs) (subst1 a P) eta)))))
  (have haS (SkJ Bool.false (skels D) a (skel Exp.tLbl))
    (lemma25_rt chkf D us1 a Exp.tLbl ha))
  (have hU (Eq Sk (skel a) Sk.unit)
    (skj_term_unit Bool.false (skels D) a (skel Exp.tLbl) haS rfl))
  (refine' (exT RV _ _ ihb _)) (intro vf hf)
  (have hef (EvalE chkf dec encTy n rho bse vf) (And.left hf))
  (have hrf (Erel chkf dec encTy n (usk (Exp.tBrs P 0)) vf
              (denU chkf dec encTy n D bs (Exp.tBrs P 0) eta))
    (And.right hf))
  (refine' (exT RV _ _ iha _)) (intro va hpa)
  (have hea (EvalE chkf dec encTy n rho ae va) (And.left hpa))
  (have hra (Erel chkf dec encTy n (usk Exp.tLbl) va
              (denU chkf dec encTy n D a Exp.tLbl eta))
    (And.right hpa))
  (refine' (exT RV _ _ (hrf va (denU chkf dec encTy n D a Exp.tLbl eta) hra) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy n (EvSrc.ap vf va) w) (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk P) w
               ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
                (denU chkf dec encTy n D a Exp.tLbl eta)))
    (And.right hw))
  (have hsub (Erel chkf dec encTy n (usk (subst1 a P)) w
               (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                        (Eq.symm (usk_subst1 a P (usk_of_unit a hU))))
                 ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
                  (denU chkf dec encTy n D a Exp.tLbl eta))))
    (erel_subst_unit chkf dec encTy n a P w
      ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
       (denU chkf dec encTy n D a Exp.tLbl eta))
      hU hrel))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eCaseL chkf dec encTy n rho P ae bse va vf w hea hef hap))
  (exact (erel_car chkf dec encTy n (usk (subst1 a P)) w
           (Eq.mp (congrArg (fn [z :- USk] (Car (uskSk z)))
                    (Eq.symm (usk_subst1 a P (usk_of_unit a hU))))
             ((denU chkf dec encTy n D bs (Exp.tBrs P 0) eta)
              (denU chkf dec encTy n D a Exp.tLbl eta)))
           (denU chkf dec encTy n D (Exp.caseL P a bs) (subst1 a P) eta)
           hsub
           (denU_case chkf dec encTy n D P a bs eta hU))))

;; adeqE_elim is the split.  The derivation of the scrutinee is a term
;; judgment, so its skeleton is Unit and the two equations are derived.
(thm adeqE_elimB
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), us1 :- (List U),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   be :- Exp, te :- Exp, ee :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hb :- (Rt chkf D us1 b Exp.tBool),
   ihb :- (Exists (fn [vb :- RV]
            (And (EvalE chkf dec encTy n rho be vb)
                 (Erel chkf dec encTy n (usk Exp.tBool) vb
                   (denU chkf dec encTy n D b Exp.tBool eta))))),
   iht :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho te v)
                 (Erel chkf dec encTy n (usk (subst1 Exp.tt P)) v
                   (denU chkf dec encTy n D t (subst1 Exp.tt P) eta))))),
   ihe :- (Exists (fn [v :- RV]
            (And (EvalE chkf dec encTy n rho ee v)
                 (Erel chkf dec encTy n (usk (subst1 Exp.ff P)) v
                   (denU chkf dec encTy n D e (subst1 Exp.ff P) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.elimB P be te ee) v)
         (Erel chkf dec encTy n (usk (subst1 b P)) v
           (denU chkf dec encTy n D (Exp.elimB P b t e) (subst1 b P) eta)))))
  (have hbS (SkJ Bool.false (skels D) b (skel Exp.tBool))
    (lemma25_rt chkf D us1 b Exp.tBool hb))
  (have hsb (Eq Sk (skel b) Sk.unit)
    (skj_term_unit Bool.false (skels D) b (skel Exp.tBool) hbS rfl))
  (exact (adeqE_elim chkf dec encTy n D P b t e be te ee rho eta
           (usk_of_unit b hsb) hsb ihb iht ihe)))

;; --- constructors (Theorem 4′) ----------------------------------------------
;;
;; At a base skeleton E is equality, so a constructor of related arguments is
;; the constructor of their denotations.  succ, sleaf and snode build Nat and
;; Syn; leaf and node build a certificate, and node evaluates its token only
;; to discard it.  bnil is the default branch list: applying it returns the
;; runtime default, which erdflt_ty relates to the carrier default.  bcons is
;; the positional list.  Its head is judged at P[ℓ], and a label has skeleton
;; Unit, so that carrier is the head's denotation at P.  The arrow clause
;; cases on the label; the hypothesis is an E-witness, and the equality with
;; RV.lbl is read out after the case so the numeral is a constructor.


(thm adeqE_succ
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), m :- Exp, me :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ih :- (Exists (fn [vt :- RV]
           (And (EvalE chkf dec encTy n rho me vt)
                (Erel chkf dec encTy n (usk Exp.tNat) vt
                  (denU chkf dec encTy n D m Exp.tNat eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.succ me) v)
         (Erel chkf dec encTy n (usk Exp.tNat) v
           (denU chkf dec encTy n D (Exp.succ m) Exp.tNat eta)))))
  (rw [(den_succ_at chkf dec encTy n m (skels D) (skel Exp.tNat) (henv_of_usk D eta))])
  (rw [(coe_self Sk.nat (Nat.succ (den chkf dec encTy n m (skels D) Sk.nat (henv_of_usk D eta))))])
  (refine' (exT RV _ _ ih _)) (intro vt ht)
  (have he (EvalE chkf dec encTy n rho me vt) (And.left ht))
  (have hr (Eq RV vt (RV.nat (denU chkf dec encTy n D m Exp.tNat eta))) (And.right ht))
  (constructor)
  (exact (RV.nat (Nat.succ (denU chkf dec encTy n D m Exp.tNat eta))))
  (constructor)
  (exact (EvE.eSucc chkf dec encTy n rho me
           (denU chkf dec encTy n D m Exp.tNat eta)
           (evalE_cast chkf dec encTy n rho me vt
             (RV.nat (denU chkf dec encTy n D m Exp.tNat eta)) he hr)))
  (rfl))

(thm adeqE_sleaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), a :- Exp, ae :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ih :- (Exists (fn [va :- RV]
           (And (EvalE chkf dec encTy n rho ae va)
                (Erel chkf dec encTy n (usk Exp.tLbl) va
                  (denU chkf dec encTy n D a Exp.tLbl eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.sleaf ae) v)
         (Erel chkf dec encTy n (usk Exp.tSyn) v
           (denU chkf dec encTy n D (Exp.sleaf a) Exp.tSyn eta)))))
  (rw [(den_sleaf_at chkf dec encTy n a (skels D) (skel Exp.tSyn) (henv_of_usk D eta))])
  (rw [(coe_self Sk.syn (Code.sl (den chkf dec encTy n a (skels D) Sk.lbl (henv_of_usk D eta))))])
  (refine' (exT RV _ _ ih _)) (intro va ha)
  (have he (EvalE chkf dec encTy n rho ae va) (And.left ha))
  (have hr (Eq RV va (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta))) (And.right ha))
  (constructor)
  (exact (RV.code (Code.sl (denU chkf dec encTy n D a Exp.tLbl eta))))
  (constructor)
  (exact (EvE.eSleaf chkf dec encTy n rho ae
           (denU chkf dec encTy n D a Exp.tLbl eta)
           (evalE_cast chkf dec encTy n rho ae va
             (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta)) he hr)))
  (rfl))

(thm adeqE_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), a :- Exp, ae :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ih :- (Exists (fn [va :- RV]
           (And (EvalE chkf dec encTy n rho ae va)
                (Erel chkf dec encTy n (usk Exp.tLbl) va
                  (denU chkf dec encTy n D a Exp.tLbl eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.leaf ae) v)
         (Erel chkf dec encTy n (usk Exp.tR) v
           (denU chkf dec encTy n D (Exp.leaf a) Exp.tR eta)))))
  (rw [(den_leaf_at chkf dec encTy n a (skels D) (skel Exp.tR) (henv_of_usk D eta))])
  (rw [(coe_self Sk.cert (Code.sl (den chkf dec encTy n a (skels D) Sk.lbl (henv_of_usk D eta))))])
  (refine' (exT RV _ _ ih _)) (intro va ha)
  (have he (EvalE chkf dec encTy n rho ae va) (And.left ha))
  (have hr (Eq RV va (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta))) (And.right ha))
  (constructor)
  (exact (RV.cert (Code.sl (denU chkf dec encTy n D a Exp.tLbl eta))))
  (constructor)
  (exact (EvE.eLeaf chkf dec encTy n rho ae
           (denU chkf dec encTy n D a Exp.tLbl eta)
           (evalE_cast chkf dec encTy n rho ae va
             (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta)) he hr)))
  (rfl))

(thm adeqE_snode
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   a :- Exp, c1 :- Exp, c2 :- Exp, ae :- Exp, c1e :- Exp, c2e :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   iha :- (Exists (fn [va :- RV]
            (And (EvalE chkf dec encTy n rho ae va)
                 (Erel chkf dec encTy n (usk Exp.tLbl) va
                   (denU chkf dec encTy n D a Exp.tLbl eta))))),
   ih1 :- (Exists (fn [v1 :- RV]
            (And (EvalE chkf dec encTy n rho c1e v1)
                 (Erel chkf dec encTy n (usk Exp.tSyn) v1
                   (denU chkf dec encTy n D c1 Exp.tSyn eta))))),
   ih2 :- (Exists (fn [v2 :- RV]
            (And (EvalE chkf dec encTy n rho c2e v2)
                 (Erel chkf dec encTy n (usk Exp.tSyn) v2
                   (denU chkf dec encTy n D c2 Exp.tSyn eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.snode ae c1e c2e) v)
         (Erel chkf dec encTy n (usk Exp.tSyn) v
           (denU chkf dec encTy n D (Exp.snode a c1 c2) Exp.tSyn eta)))))
  (rw [(den_snode_at chkf dec encTy n a c1 c2 (skels D) (skel Exp.tSyn) (henv_of_usk D eta))])
  (rw [(coe_self Sk.syn (Code.sn
         (den chkf dec encTy n a (skels D) Sk.lbl (henv_of_usk D eta))
         (den chkf dec encTy n c1 (skels D) Sk.syn (henv_of_usk D eta))
         (den chkf dec encTy n c2 (skels D) Sk.syn (henv_of_usk D eta))))])
  (refine' (exT RV _ _ iha _)) (intro va ha)
  (have hea (EvalE chkf dec encTy n rho ae va) (And.left ha))
  (have hra (Eq RV va (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta))) (And.right ha))
  (refine' (exT RV _ _ ih1 _)) (intro v1 h1)
  (have he1 (EvalE chkf dec encTy n rho c1e v1) (And.left h1))
  (have hr1 (Eq RV v1 (RV.code (denU chkf dec encTy n D c1 Exp.tSyn eta))) (And.right h1))
  (refine' (exT RV _ _ ih2 _)) (intro v2 h2)
  (have he2 (EvalE chkf dec encTy n rho c2e v2) (And.left h2))
  (have hr2 (Eq RV v2 (RV.code (denU chkf dec encTy n D c2 Exp.tSyn eta))) (And.right h2))
  (constructor)
  (exact (RV.code (Code.sn (denU chkf dec encTy n D a Exp.tLbl eta)
                    (denU chkf dec encTy n D c1 Exp.tSyn eta)
                    (denU chkf dec encTy n D c2 Exp.tSyn eta))))
  (constructor)
  (exact (EvE.eSnode chkf dec encTy n rho ae c1e c2e
           (denU chkf dec encTy n D a Exp.tLbl eta)
           (denU chkf dec encTy n D c1 Exp.tSyn eta)
           (denU chkf dec encTy n D c2 Exp.tSyn eta)
           (evalE_cast chkf dec encTy n rho ae va
             (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta)) hea hra)
           (evalE_cast chkf dec encTy n rho c1e v1
             (RV.code (denU chkf dec encTy n D c1 Exp.tSyn eta)) he1 hr1)
           (evalE_cast chkf dec encTy n rho c2e v2
             (RV.code (denU chkf dec encTy n D c2 Exp.tSyn eta)) he2 hr2)))
  (rfl))

(thm adeqE_node
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp),
   d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp,
   de :- Exp, ae :- Exp, r1e :- Exp, r2e :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihd :- (Exists (fn [vd :- RV]
            (And (EvalE chkf dec encTy n rho de vd)
                 (Erel chkf dec encTy n (usk Exp.tDia) vd
                   (denU chkf dec encTy n D d Exp.tDia eta))))),
   iha :- (Exists (fn [va :- RV]
            (And (EvalE chkf dec encTy n rho ae va)
                 (Erel chkf dec encTy n (usk Exp.tLbl) va
                   (denU chkf dec encTy n D a Exp.tLbl eta))))),
   ih1 :- (Exists (fn [v1 :- RV]
            (And (EvalE chkf dec encTy n rho r1e v1)
                 (Erel chkf dec encTy n (usk Exp.tR) v1
                   (denU chkf dec encTy n D r1 Exp.tR eta))))),
   ih2 :- (Exists (fn [v2 :- RV]
            (And (EvalE chkf dec encTy n rho r2e v2)
                 (Erel chkf dec encTy n (usk Exp.tR) v2
                   (denU chkf dec encTy n D r2 Exp.tR eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.node de ae r1e r2e) v)
         (Erel chkf dec encTy n (usk Exp.tR) v
           (denU chkf dec encTy n D (Exp.node d a r1 r2) Exp.tR eta)))))
  (rw [(den_node_at chkf dec encTy n d a r1 r2 (skels D) (skel Exp.tR) (henv_of_usk D eta))])
  (rw [(coe_self Sk.cert (Code.sn
         (den chkf dec encTy n a (skels D) Sk.lbl (henv_of_usk D eta))
         (den chkf dec encTy n r1 (skels D) Sk.cert (henv_of_usk D eta))
         (den chkf dec encTy n r2 (skels D) Sk.cert (henv_of_usk D eta))))])
  (refine' (exT RV _ _ ihd _)) (intro vd hd)
  (have hed (EvalE chkf dec encTy n rho de vd) (And.left hd))
  (refine' (exT RV _ _ iha _)) (intro va ha)
  (have hea (EvalE chkf dec encTy n rho ae va) (And.left ha))
  (have hra (Eq RV va (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta))) (And.right ha))
  (refine' (exT RV _ _ ih1 _)) (intro v1 h1)
  (have he1 (EvalE chkf dec encTy n rho r1e v1) (And.left h1))
  (have hr1 (Eq RV v1 (RV.cert (denU chkf dec encTy n D r1 Exp.tR eta))) (And.right h1))
  (refine' (exT RV _ _ ih2 _)) (intro v2 h2)
  (have he2 (EvalE chkf dec encTy n rho r2e v2) (And.left h2))
  (have hr2 (Eq RV v2 (RV.cert (denU chkf dec encTy n D r2 Exp.tR eta))) (And.right h2))
  (constructor)
  (exact (RV.cert (Code.sn (denU chkf dec encTy n D a Exp.tLbl eta)
                    (denU chkf dec encTy n D r1 Exp.tR eta)
                    (denU chkf dec encTy n D r2 Exp.tR eta))))
  (constructor)
  (exact (EvE.eNode chkf dec encTy n rho de ae r1e r2e vd
           (denU chkf dec encTy n D a Exp.tLbl eta)
           (denU chkf dec encTy n D r1 Exp.tR eta)
           (denU chkf dec encTy n D r2 Exp.tR eta)
           hed
           (evalE_cast chkf dec encTy n rho ae va
             (RV.lbl (denU chkf dec encTy n D a Exp.tLbl eta)) hea hra)
           (evalE_cast chkf dec encTy n rho r1e v1
             (RV.cert (denU chkf dec encTy n D r1 Exp.tR eta)) he1 hr1)
           (evalE_cast chkf dec encTy n rho r2e v2
             (RV.cert (denU chkf dec encTy n D r2 Exp.tR eta)) he2 hr2)))
  (rfl))

(thm denU_bnil_app
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, k :- Nat,
   alpha :- (Car Sk.lbl),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    ((denU chkf dec encTy n D Exp.bnil (Exp.tBrs P k) eta) alpha)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) (dflt (skel P))))
  (exact (Eq.trans
    (denU_brs_at chkf dec encTy n D Exp.bnil P alpha eta)
    (congrArg
      (fn [x :- (Car (skel P))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
      (Eq.trans
        (congrArg (fn [f :- (Car (Sk.arr Sk.lbl (skel P)))] (f alpha))
          (den_bnil_at chkf dec encTy n (skels D)
            (Sk.arr Sk.lbl (skel P)) (henv_of_usk D eta)))
        (dflt_arr Sk.lbl (skel P) alpha))))))

(thm adeqE_bnil
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, k :- Nat,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D)))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho Exp.bnil v)
         (Erel chkf dec encTy n (usk (Exp.tBrs P k)) v
           (denU chkf dec encTy n D Exp.bnil (Exp.tBrs P k) eta)))))
  (constructor) (exact (RV.bnil (skel P)))
  (constructor) (exact (EvE.eBnil chkf dec encTy n rho (skel P)))
  (intro arg) (intro alpha) (intro _harg)
  (constructor) (exact (rdflt (skel P)))
  (constructor) (exact (EvE.apBnil chkf dec encTy n (skel P) arg))
  (exact (erel_car chkf dec encTy n (usk P) (rdflt (skel P))
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) (dflt (skel P)))
           ((denU chkf dec encTy n D Exp.bnil (Exp.tBrs P k) eta) alpha)
           (erdflt_ty chkf dec encTy n P)
           (Eq.symm (denU_bnil_app chkf dec encTy n D P k alpha eta)))))

(thm denU_bcons_zero
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), h :- Exp, t :- Exp, P :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) 0)
    (denU chkf dec encTy n D h P eta))
  (exact (Eq.trans
    (denU_brs_at chkf dec encTy n D (Exp.bcons h t) P 0 eta)
    (congrArg
      (fn [x :- (Car (skel P))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
      (bcons_zero chkf dec encTy n h t (skels D) (skel P) (henv_of_usk D eta))))))

(thm denU_bcons_succ
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), h :- Exp, t :- Exp, P :- Exp, j :- Nat,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) (Nat.succ j))
    ((denU chkf dec encTy n D t (Exp.tBrs P 0) eta) j))
  (exact (Eq.trans
    (denU_brs_at chkf dec encTy n D (Exp.bcons h t) P (Nat.succ j) eta)
    (Eq.trans
      (congrArg
        (fn [x :- (Car (skel P))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
        (bcons_succ chkf dec encTy n h t (skels D) (skel P) (henv_of_usk D eta) j))
      (Eq.symm (denU_brs_at chkf dec encTy n D t P j eta))))))

(thm adeqE_bcons_zero
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), h :- Exp, t :- Exp, P :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   vh :- RV, vt :- RV, arg :- RV,
   hrh :- (Erel chkf dec encTy n (usk P) vh (denU chkf dec encTy n D h P eta)),
   harg :- (Eq RV arg (RV.lbl 0))]
  (Exists (fn [w :- RV]
    (And (EvE chkf dec encTy n (EvSrc.ap (RV.bcons vh vt) arg) w)
         (Erel chkf dec encTy n (usk P) w
           ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) 0)))))
  (constructor) (exact vh)
  (constructor)
  (exact (Eq.mp
           (congrArg (fn [a :- RV]
                       (EvE chkf dec encTy n (EvSrc.ap (RV.bcons vh vt) a) vh))
             (Eq.symm harg))
           (EvE.apBconsZ chkf dec encTy n vh vt)))
  (exact (erel_car chkf dec encTy n (usk P) vh
           (denU chkf dec encTy n D h P eta)
           ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) 0)
           hrh
           (Eq.symm (denU_bcons_zero chkf dec encTy n D h t P eta)))))

(thm adeqE_bcons_succ
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), h :- Exp, t :- Exp, P :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   vh :- RV, vt :- RV, arg :- RV, j :- Nat,
   hrt :- (Erel chkf dec encTy n (usk (Exp.tBrs P 0)) vt
            (denU chkf dec encTy n D t (Exp.tBrs P 0) eta)),
   harg :- (Eq RV arg (RV.lbl (Nat.succ j)))]
  (Exists (fn [w :- RV]
    (And (EvE chkf dec encTy n (EvSrc.ap (RV.bcons vh vt) arg) w)
         (Erel chkf dec encTy n (usk P) w
           ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) (Nat.succ j))))))
  (refine' (exT RV _ _ (hrt (RV.lbl j) j rfl) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy n (EvSrc.ap vt (RV.lbl j)) w) (And.left hw))
  (have hrel (Erel chkf dec encTy n (usk P) w
               ((denU chkf dec encTy n D t (Exp.tBrs P 0) eta) j))
    (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (Eq.mp
           (congrArg (fn [a :- RV]
                       (EvE chkf dec encTy n (EvSrc.ap (RV.bcons vh vt) a) w))
             (Eq.symm harg))
           (EvE.apBconsS chkf dec encTy n vh vt j w hap)))
  (exact (erel_car chkf dec encTy n (usk P) w
           ((denU chkf dec encTy n D t (Exp.tBrs P 0) eta) j)
           ((denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P 0) eta) (Nat.succ j))
           hrel
           (Eq.symm (denU_bcons_succ chkf dec encTy n D h t P j eta)))))

(thm adeqE_bcons_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), h :- Exp, t :- Exp, P :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   vh :- RV, vt :- RV, arg :- RV, alpha :- Nat,
   hrh :- (Erel chkf dec encTy cap (usk P) vh (denU chkf dec encTy cap D h P eta)),
   hrt :- (Erel chkf dec encTy cap (usk (Exp.tBrs P 0)) vt
            (denU chkf dec encTy cap D t (Exp.tBrs P 0) eta)),
   harg :- (Erel chkf dec encTy cap (USk.base Sk.lbl) arg alpha)]
  (Exists (fn [w :- RV]
    (And (EvE chkf dec encTy cap (EvSrc.ap (RV.bcons vh vt) arg) w)
         (Erel chkf dec encTy cap (usk P) w
           ((denU chkf dec encTy cap D (Exp.bcons h t) (Exp.tBrs P 0) eta) alpha)))))
  (cases alpha)
  (have heq (Eq RV arg (RV.lbl 0)) harg)
  (exact (adeqE_bcons_zero chkf dec encTy cap D h t P eta vh vt arg hrh heq))
  (have heq (Eq RV arg (RV.lbl (Nat.succ n))) harg)
  (exact (adeqE_bcons_succ chkf dec encTy cap D h t P eta vh vt arg n hrt heq)))

(thm adeqE_bconsK
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, k :- Nat,
   h :- Exp, t :- Exp, he :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihh :- (Exists (fn [vh :- RV]
            (And (EvalE chkf dec encTy n rho he vh)
                 (Erel chkf dec encTy n (usk (subst1 (Exp.lbl k) P)) vh
                   (denU chkf dec encTy n D h (subst1 (Exp.lbl k) P) eta))))),
   iht :- (Exists (fn [vt :- RV]
            (And (EvalE chkf dec encTy n rho te vt)
                 (Erel chkf dec encTy n (usk (Exp.tBrs P (Nat.succ k))) vt
                   (denU chkf dec encTy n D t (Exp.tBrs P (Nat.succ k)) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.bcons he te) v)
         (Erel chkf dec encTy n (usk (Exp.tBrs P k)) v
           (denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P k) eta)))))
  (have hl (Eq Sk (skel (Exp.lbl k)) Sk.unit) (rfl))
  (refine' (exT RV _ _
    (adeqE_retarget chkf dec encTy n D h he
      (subst1 (Exp.lbl k) P) P rho eta
      (usk_subst1 (Exp.lbl k) P (usk_of_unit (Exp.lbl k) hl))
      (skel_subst1 (Exp.lbl k) P hl)
      ihh) _))
  (intro vh hh)
  (have heh (EvalE chkf dec encTy n rho he vh) (And.left hh))
  (have hrh (Erel chkf dec encTy n (usk P) vh (denU chkf dec encTy n D h P eta))
    (And.right hh))
  (refine' (exT RV _ _ iht _)) (intro vt ht)
  (have het (EvalE chkf dec encTy n rho te vt) (And.left ht))
  (have hrt (Erel chkf dec encTy n (usk (Exp.tBrs P (Nat.succ k))) vt
              (denU chkf dec encTy n D t (Exp.tBrs P (Nat.succ k)) eta))
    (And.right ht))
  (constructor) (exact (RV.bcons vh vt))
  (constructor)
  (exact (EvE.eBcons chkf dec encTy n rho he te vh vt heh het))
  (intro arg) (intro alpha) (intro harg)
  (exact (adeqE_bcons_at chkf dec encTy n D h t P eta vh vt arg alpha hrh hrt harg)))

(thm adeqE_bcons
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, k :- Nat,
   h :- Exp, t :- Exp, he :- Exp, te :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihh :- (Exists (fn [vh :- RV]
            (And (EvalE chkf dec encTy n rho he vh)
                 (Erel chkf dec encTy n (usk (subst1 (Exp.lbl k) P)) vh
                   (denU chkf dec encTy n D h (subst1 (Exp.lbl k) P) eta))))),
   iht :- (Exists (fn [vt :- RV]
            (And (EvalE chkf dec encTy n rho te vt)
                 (Erel chkf dec encTy n (usk (Exp.tBrs P (Nat.succ k))) vt
                   (denU chkf dec encTy n D t (Exp.tBrs P (Nat.succ k)) eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.bcons he te) v)
         (Erel chkf dec encTy n (usk (Exp.tBrs P k)) v
           (denU chkf dec encTy n D (Exp.bcons h t) (Exp.tBrs P k) eta)))))
  (exact (adeqE_bconsK chkf dec encTy n D P k h t he te rho eta ihh iht)))

;; --- recN (Theorem 4′) ------------------------------------------------------
;;
;; The inner induction is on the numeral.  Every accumulator is read at the
;; motive P: P[zero], stepTy and P[n] have that usage skeleton, because zero,
;; a successor and the numeral are terms (skeleton Unit).  denU of the recursor
;; is the Nat.rec of those denotations.  The conclusion of the judgment is
;; P[n], and adeqE_recN moves the witness there.


;; recN (Theorem 4′).  The motive instance P[succ x] and the substitutions
;; P[zero], P[n] have the usage skeleton of P: zero, succ and a numeral are
;; terms, so their skeleton is Unit.  The iterator then reads every
;; accumulator at that one skeleton.

(thm cast_nat_id [j :- Nat]
  (Eq Nat (Eq.mp (congrArg Car (usk_skel Exp.tNat)) j) j)
  (rfl))

(thm skels_rec_ctx [P :- Exp, D :- (List Exp)]
  (Eq (List Sk)
    (skels (List.cons Exp P (List.cons Exp Exp.tNat D)))
    (sk2 (skel P) Sk.nat (skels D)))
  (rfl))

(thm denU_nat_id
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), nv :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq Nat
    (denU chkf dec encTy n D nv Exp.tNat eta)
    (den chkf dec encTy n nv (skels D) Sk.nat (henv_of_usk D eta)))
  (rfl))

(thm sSucc_usk [i :- Nat]
  (Eq USk (usk (sSucc i)) (USk.base Sk.unit))
  (cases i)
  (rfl)
  (rfl))

(thm sSucc_skel [i :- Nat]
  (Eq Sk (skel (sSucc i)) Sk.unit)
  (cases i)
  (rfl)
  (rfl))

(thm stepTy_usk [P :- Exp]
  (Eq USk (usk (stepTy P)) (usk P))
  (exact (Eq.trans
    (usk_lift (subst (fn [i :- Nat] (sSucc i)) P) 1 0)
    (usk_subst P (fn [i :- Nat] (sSucc i))
      (fn [i :- Nat] (sSucc_usk i))))))

(thm stepTy_skel [P :- Exp]
  (Eq Sk (skel (stepTy P)) (skel P))
  (exact (Eq.trans
    (skel_lift (subst (fn [i :- Nat] (sSucc i)) P) 1 0)
    (skel_subst P (fn [i :- Nat] (sSucc i))
      (fn [i :- Nat] (sSucc_skel i))))))

;; Pointwise equal steps give equal iterates.  The zero case is the shared
;; base; at a successor the hypothesis rewrites the step and the induction
;; hypothesis rewrites the accumulator it receives.
(thm natrec_cong
  [s :- Sk, base :- (Car s),
   f :- (=> Nat (Car s) (Car s)),
   g :- (=> Nat (Car s) (Car s)),
   k :- Nat,
   h :- (forall [j Nat] (forall [a (Car s)]
          (Eq (Car s) (f j a) (g j a))))]
  (Eq (Car s)
    (Nat.rec$1 (fn [_ :- Nat] (Car s)) base f k)
    (Nat.rec$1 (fn [_ :- Nat] (Car s)) base g k))
  (induction k)
  (rfl)
  (exact (Eq.trans
    (h n (Nat.rec$1 (fn [_ :- Nat] (Car s)) base f n))
    (congrArg (fn [a :- (Car s)] (g n a)) ih_n))))

;; den_recN is a kernel definition.  Rewrite sees the folded application,
;; not the Nat.rec inside it, so the equation is stated on its own.
(thm den_recN_open
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   prev :- DenFn, n :- Nat,
   P :- Exp, z :- Exp, st :- Exp, nv :- Exp,
   iP :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   iz :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   is :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   in :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   G :- (List Sk), sk :- Sk, en :- (HEnv G)]
  (Eq (Car sk)
    (den_recN chkf dec encTy prev n P z st nv iP iz is in G sk en)
    (Nat.rec$1 (fn [_ :- Nat] (Car sk))
      (iz G sk en)
      (fn [k :- Nat, acc :- (Car sk)]
        (is (sk2 sk Sk.nat G) sk (Prod.mk acc (Prod.mk k en))))
      (in G Sk.nat en)))
  (rfl))

;; A cast stays outside the recursor.  Putting Eq.mp inside the step makes
;; the eliminator's motive fail to match the simple function; the induction
;; hypothesis carries the cast, and the step hypothesis applies it.
(thm cast_iter
  [s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2),
   base1 :- (Car s1), base2 :- (Car s2),
   step1 :- (=> Nat (Car s1) (Car s1)),
   step2 :- (=> Nat (Car s2) (Car s2)),
   k :- Nat,
   hbase :- (Eq (Car s1) base1
             (Eq.mp (congrArg Car (Eq.symm e)) base2)),
   hstep :- (forall [j Nat]
             (forall [a2 (Car s2)]
              (forall [a1 (Car s1)]
               (=> (Eq (Car s1) a1 (Eq.mp (congrArg Car (Eq.symm e)) a2))
                 (Eq (Car s1) (step1 j a1)
                   (Eq.mp (congrArg Car (Eq.symm e)) (step2 j a2)))))))]
  (Eq (Car s1)
    (Nat.rec$1 (fn [_ :- Nat] (Car s1)) base1 step1 k)
    (Eq.mp (congrArg Car (Eq.symm e))
      (Nat.rec$1 (fn [_ :- Nat] (Car s2)) base2 step2 k)))
  (induction k)
  (exact hbase)
  (change (Eq (Car s1)
    (step1 n (Nat.rec$1 (fn [_ :- Nat] (Car s1)) base1 step1 n))
    (Eq.mp (congrArg Car (Eq.symm e))
      (step2 n (Nat.rec$1 (fn [_ :- Nat] (Car s2)) base2 step2 n)))))
  (exact (hstep n
           (Nat.rec$1 (fn [_ :- Nat] (Car s2)) base2 step2 n)
           (Nat.rec$1 (fn [_ :- Nat] (Car s1)) base1 step1 n)
           ih_n)))

;; The step of ⟦recN⟧ reads the accumulator at skel P.  denU reads it at
;; usk P.  henv_ext2 is that environment once the accumulator is the cast,
;; which the hypothesis ha is.
(thm denU_rec_step
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, st :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   j :- Nat, a2 :- (Car (skel P)), a1 :- (Car (uskSk (usk P))),
   ha :- (Eq (Car (uskSk (usk P))) a1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) a2))]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n
      (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
      (Prod.mk a1 (Prod.mk j eta)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      (den chkf dec encTy n st
        (sk2 (skel P) Sk.nat (skels D)) (skel P)
        (Prod.mk a2 (Prod.mk j (henv_of_usk D eta))))))
  (have hback (Eq (Car (skel P)) a2
                (Eq.mp (congrArg Car (usk_skel P)) a1))
    (Eq.trans
      (Eq.symm (cast_back (uskSk (usk P)) (skel P) (usk_skel P) a2))
      (congrArg
        (fn [x :- (Car (uskSk (usk P)))]
          (Eq.mp (congrArg Car (usk_skel P)) x))
        (Eq.symm ha))))
  (rw [hback])
  (exact (Eq.symm
    (congrArg
      (fn [en :- (HEnv (skels (List.cons Exp P (List.cons Exp Exp.tNat D))))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
          (den chkf dec encTy n st
            (skels (List.cons Exp P (List.cons Exp Exp.tNat D)))
            (skel P) en)))
      (Eq.symm (henv_ext2 Exp.tNat P D j a1 eta))))))

;; ⟦recN⟧ at usk P is the Nat.rec of denU.  den_recN is folded, so the
;; equation is den_recN_open; the cast of that recursor is cast_iter.
(thm denU_recN
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, z :- Exp, st :- Exp, nv :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n D (Exp.recN P z st nv) P eta)
    (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P))))
      (denU chkf dec encTy n D z P eta)
      (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
        (denU chkf dec encTy n
          (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
          (Prod.mk acc (Prod.mk j eta))))
      (denU chkf dec encTy n D nv Exp.tNat eta)))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (skel P))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
      (Eq.trans
        (den_recN_at chkf dec encTy n P z st nv (skels D) (skel P) (henv_of_usk D eta))
        (den_recN_open chkf dec encTy (denPrev chkf dec encTy n) n P z st nv
          (den chkf dec encTy n P) (den chkf dec encTy n z)
          (den chkf dec encTy n st) (den chkf dec encTy n nv)
          (skels D) (skel P) (henv_of_usk D eta))))
    (Eq.symm
      (cast_iter (uskSk (usk P)) (skel P) (usk_skel P)
        (denU chkf dec encTy n D z P eta)
        (den chkf dec encTy n z (skels D) (skel P) (henv_of_usk D eta))
        (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
          (denU chkf dec encTy n
            (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
            (Prod.mk acc (Prod.mk j eta))))
        (fn [k :- Nat, acc :- (Car (skel P))]
          (den chkf dec encTy n st
            (sk2 (skel P) Sk.nat (skels D)) (skel P)
            (Prod.mk acc (Prod.mk k (henv_of_usk D eta)))))
        (den chkf dec encTy n nv (skels D) Sk.nat (henv_of_usk D eta))
        rfl
        (fn [j :- Nat, a2 :- (Car (skel P)), a1 :- (Car (uskSk (usk P))),
             ha :- (Eq (Car (uskSk (usk P))) a1
                     (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) a2))]
          (denU_rec_step chkf dec encTy n D P st eta j a2 a1 ha)))))))


;; E at Nat is equality with RV.nat.  The predecessor bound in the step is
;; that numeral, so the new environment entry is reflexivity.
(thm erel_nat_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, j :- Nat]
  (Erel chkf dec encTy cap (usk Exp.tNat) (RV.nat j) j)
  (rfl))

;; The recN iterator (Theorem 4′, inner induction on the numeral).  At 0 the
;; accumulator is the base.  At k + 1 the step runs in (mid, (k, ρ)).  That
;; environment is envE because mid is E-related to the denotational Nat.rec
;; at k, and k is related to itself at Nat.  The step's induction hypothesis
;; is already at the motive P: the caller moves it off stepTy.
(thm adeqE_iter
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, st :- Exp, se :- Exp,
   us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   z0 :- RV, alpha :- (Car (uskSk (usk P))),
   hz :- (Erel chkf dec encTy cap (usk P) z0 alpha),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
                    (List.cons U U.u1 (List.cons U U.uw us)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 se w)
                       (Erel chkf dec encTy cap (usk P) w
                         (denU chkf dec encTy cap
                           (List.cons Exp P (List.cons Exp Exp.tNat D))
                           st P eta2))))))))]
  (forall [k Nat]
    (Exists (fn [v :- RV]
      (And (EvE chkf dec encTy cap (EvSrc.iter rho se k z0) v)
           (Erel chkf dec encTy cap (usk P) v
             (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
               (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
                 (denU chkf dec encTy cap
                   (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                   (Prod.mk acc (Prod.mk j eta))))
               k))))))
  (intro k)
  (induction k)
  (constructor) (exact z0)
  (constructor) (exact (EvE.eIterZ chkf dec encTy cap rho se z0))
  (exact hz)
  (refine' (exT RV _ _ ih_n _)) (intro mid hm)
  (have hem (EvE chkf dec encTy cap (EvSrc.iter rho se n z0) mid) (And.left hm))
  (have hrm (Erel chkf dec encTy cap (usk P) mid
              (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
                (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
                  (denU chkf dec encTy cap
                    (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                    (Prod.mk acc (Prod.mk j eta))))
                n))
    (And.right hm))
  (have henv1 (envE chkf dec encTy cap
                (List.cons USk (usk Exp.tNat) (uskCtx D))
                (List.cons U U.uw us)
                (List.cons RV (RV.nat n) rho)
                (Prod.mk n eta))
    (envE_consw chkf dec encTy cap (usk Exp.tNat) (uskCtx D) us
      (RV.nat n) rho n eta (erel_nat_rfl chkf dec encTy cap n) hr))
  (have henv2 (envE chkf dec encTy cap
                (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
                (List.cons U U.u1 (List.cons U U.uw us))
                (List.cons RV mid (List.cons RV (RV.nat n) rho))
                (Prod.mk
                  (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
                    (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
                      (denU chkf dec encTy cap
                        (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                        (Prod.mk acc (Prod.mk j eta))))
                    n)
                  (Prod.mk n eta)))
    (envE_cons1 chkf dec encTy cap (usk P)
      (List.cons USk (usk Exp.tNat) (uskCtx D))
      (List.cons U U.uw us)
      mid (List.cons RV (RV.nat n) rho)
      (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
        (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
          (denU chkf dec encTy cap
            (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
            (Prod.mk acc (Prod.mk j eta))))
        n)
      (Prod.mk n eta) hrm henv1))
  (refine' (exT RV _ _
    (ihs (List.cons RV mid (List.cons RV (RV.nat n) rho))
         (Prod.mk
           (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
             (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
               (denU chkf dec encTy cap
                 (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                 (Prod.mk acc (Prod.mk j eta))))
             n)
           (Prod.mk n eta))
         henv2) _))
  (intro out ho)
  (have heo (EvalE chkf dec encTy cap
              (List.cons RV mid (List.cons RV (RV.nat n) rho)) se out)
    (And.left ho))
  (have hro (Erel chkf dec encTy cap (usk P) out
              (denU chkf dec encTy cap
                (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                (Prod.mk
                  (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P)))) alpha
                    (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
                      (denU chkf dec encTy cap
                        (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                        (Prod.mk acc (Prod.mk j eta))))
                    n)
                  (Prod.mk n eta))))
    (And.right ho))
  (constructor) (exact out)
  (constructor)
  (exact (EvE.eIterS chkf dec encTy cap rho se n z0 mid out hem heo))
  (exact hro))


;; The step is judged at stepTy, which is P[succ x] under the accumulator.
;; That type has the skeleton of P, so the iterator, which reads the step at
;; P, gets the hypothesis by adeqE_retarget.
(thm adeqE_step_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, st :- Exp, se :- Exp,
   us :- (List U),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
                    (List.cons U U.u1 (List.cons U U.uw us)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 se w)
                       (Erel chkf dec encTy n (usk (stepTy P)) w
                         (denU chkf dec encTy n
                           (List.cons Exp P (List.cons Exp Exp.tNat D))
                           st (stepTy P) eta2))))))))]
  (forall [rho2 (List RV)]
    (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))))]
      (=> (envE chkf dec encTy n
            (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
            (List.cons U U.u1 (List.cons U U.uw us)) rho2 eta2)
        (Exists (fn [w :- RV]
          (And (EvalE chkf dec encTy n rho2 se w)
               (Erel chkf dec encTy n (usk P) w
                 (denU chkf dec encTy n
                   (List.cons Exp P (List.cons Exp Exp.tNat D))
                   st P eta2))))))))
  (intro rho2) (intro eta2) (intro he)
  (exact (adeqE_retarget chkf dec encTy n
           (List.cons Exp P (List.cons Exp Exp.tNat D))
           st se (stepTy P) P rho2 eta2
           (stepTy_usk P) (stepTy_skel P)
           (ihs rho2 eta2 he))))

;; recN at the motive's own skeleton (Theorem 4′).  The base is judged at
;; P[zero] and moved to P, because zero has skeleton Unit.  The scrutinee
;; denotes the numeral the iterator runs for.  denU_recN is that iterate.
(thm adeqE_recN_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp,
   z :- Exp, st :- Exp, nv :- Exp,
   ze :- Exp, se :- Exp, ne :- Exp,
   us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hr :- (envE chkf dec encTy n (uskCtx D) us rho eta),
   ihn :- (Exists (fn [vn :- RV]
            (And (EvalE chkf dec encTy n rho ne vn)
                 (Erel chkf dec encTy n (usk Exp.tNat) vn
                   (denU chkf dec encTy n D nv Exp.tNat eta))))),
   ihz :- (Exists (fn [z0 :- RV]
            (And (EvalE chkf dec encTy n rho ze z0)
                 (Erel chkf dec encTy n (usk (subst1 Exp.zero P)) z0
                   (denU chkf dec encTy n D z (subst1 Exp.zero P) eta))))),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
                    (List.cons U U.u1 (List.cons U U.uw us)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 se w)
                       (Erel chkf dec encTy n (usk (stepTy P)) w
                         (denU chkf dec encTy n
                           (List.cons Exp P (List.cons Exp Exp.tNat D))
                           st (stepTy P) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.recN P ze se ne) v)
         (Erel chkf dec encTy n (usk P) v
           (denU chkf dec encTy n D (Exp.recN P z st nv) P eta)))))
  (rw [(denU_recN chkf dec encTy n D P z st nv eta)])
  (refine' (exT RV _ _ ihn _)) (intro vn hn)
  (have hen (EvalE chkf dec encTy n rho ne vn) (And.left hn))
  (have hrn (Eq RV vn (RV.nat (denU chkf dec encTy n D nv Exp.tNat eta)))
    (And.right hn))
  (have hek (EvalE chkf dec encTy n rho ne
              (RV.nat (denU chkf dec encTy n D nv Exp.tNat eta)))
    (evalE_cast chkf dec encTy n rho ne vn
      (RV.nat (denU chkf dec encTy n D nv Exp.tNat eta)) hen hrn))
  (refine' (exT RV _ _
    (adeqE_retarget chkf dec encTy n D z ze (subst1 Exp.zero P) P rho eta
      (usk_subst1 Exp.zero P (usk_of_unit Exp.zero rfl))
      (skel_subst1 Exp.zero P rfl)
      ihz) _))
  (intro z0 hz0)
  (have hez (EvalE chkf dec encTy n rho ze z0) (And.left hz0))
  (have hrz (Erel chkf dec encTy n (usk P) z0
              (denU chkf dec encTy n D z P eta))
    (And.right hz0))
  (refine' (exT RV _ _
    (adeqE_iter chkf dec encTy n D P st se us rho eta z0
      (denU chkf dec encTy n D z P eta) hrz hr
      (adeqE_step_at chkf dec encTy n D P st se us ihs)
      (denU chkf dec encTy n D nv Exp.tNat eta)) _))
  (intro v hv)
  (have hi (EvE chkf dec encTy n
             (EvSrc.iter rho se (denU chkf dec encTy n D nv Exp.tNat eta) z0) v)
    (And.left hv))
  (have hrel (Erel chkf dec encTy n (usk P) v
               (Nat.rec$1 (fn [_ :- Nat] (Car (uskSk (usk P))))
                 (denU chkf dec encTy n D z P eta)
                 (fn [j :- Nat, acc :- (Car (uskSk (usk P)))]
                   (denU chkf dec encTy n
                     (List.cons Exp P (List.cons Exp Exp.tNat D)) st P
                     (Prod.mk acc (Prod.mk j eta))))
                 (denU chkf dec encTy n D nv Exp.tNat eta)))
    (And.right hv))
  (constructor) (exact v)
  (constructor)
  (exact (EvE.eRecN chkf dec encTy n rho P ze se ne
           (denU chkf dec encTy n D nv Exp.tNat eta) z0 v hek hez hi))
  (exact hrel))

;; The conclusion of recN is P[n], not P.  A numeral has skeleton Unit, so
;; the two carriers agree and adeqE_recN_at transports.
(thm adeqE_recN
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp,
   z :- Exp, st :- Exp, nv :- Exp,
   ze :- Exp, se :- Exp, ne :- Exp,
   us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hub :- (Eq USk (usk nv) (USk.base Sk.unit)),
   hsn :- (Eq Sk (skel nv) Sk.unit),
   hr :- (envE chkf dec encTy n (uskCtx D) us rho eta),
   ihn :- (Exists (fn [vn :- RV]
            (And (EvalE chkf dec encTy n rho ne vn)
                 (Erel chkf dec encTy n (usk Exp.tNat) vn
                   (denU chkf dec encTy n D nv Exp.tNat eta))))),
   ihz :- (Exists (fn [z0 :- RV]
            (And (EvalE chkf dec encTy n rho ze z0)
                 (Erel chkf dec encTy n (usk (subst1 Exp.zero P)) z0
                   (denU chkf dec encTy n D z (subst1 Exp.zero P) eta))))),
   ihs :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))))]
              (=> (envE chkf dec encTy n
                    (uskCtx (List.cons Exp P (List.cons Exp Exp.tNat D)))
                    (List.cons U U.u1 (List.cons U U.uw us)) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy n rho2 se w)
                       (Erel chkf dec encTy n (usk (stepTy P)) w
                         (denU chkf dec encTy n
                           (List.cons Exp P (List.cons Exp Exp.tNat D))
                           st (stepTy P) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.recN P ze se ne) v)
         (Erel chkf dec encTy n (usk (subst1 nv P)) v
           (denU chkf dec encTy n D (Exp.recN P z st nv) (subst1 nv P) eta)))))
  (exact (adeqE_retarget chkf dec encTy n D
           (Exp.recN P z st nv) (Exp.recN P ze se ne)
           P (subst1 nv P) rho eta
           (Eq.symm (usk_subst1 nv P hub))
           (Eq.symm (skel_subst1 nv P hsn))
           (adeqE_recN_at chkf dec encTy n D P z st nv ze se ne us rho eta
             hr ihn ihz ihs))))

;; --- recSyn, the leaf (Theorem 4′) ------------------------------------------
;;
;; The leaf method runs with the label bound at usage ω.  leafTy, nodeTy,
;; y1Ty and y2Ty are substitutions of terms, so each has the usage skeleton
;; of the motive.  The node branch of Code.rec casts the two recursive
;; carriers into those binder types; a leaf does not run it.


;; recSyn (Theorem 4′).  The leaf method is P[sleaf a] under the label, and
;; the node method is P[snode a c1 c2] under five binders.  Each substituent
;; is a term, so those types have the usage skeleton of P.

(thm sLeaf_usk [i :- Nat]
  (Eq USk (usk (sLeafI i)) (USk.base Sk.unit))
  (cases i) (rfl) (rfl))

(thm sLeaf_skel [i :- Nat]
  (Eq Sk (skel (sLeafI i)) Sk.unit)
  (cases i) (rfl) (rfl))

(thm sNode_usk [i :- Nat]
  (Eq USk (usk (sNodeI i)) (USk.base Sk.unit))
  (cases i) (rfl) (rfl))

(thm sNode_skel [i :- Nat]
  (Eq Sk (skel (sNodeI i)) Sk.unit)
  (cases i) (rfl) (rfl))

(thm sAt_usk [k :- Nat, sh :- Nat, i :- Nat]
  (Eq USk (usk (sAt k sh i)) (USk.base Sk.unit))
  (cases i) (rfl) (rfl))

(thm sAt_skel [k :- Nat, sh :- Nat, i :- Nat]
  (Eq Sk (skel (sAt k sh i)) Sk.unit)
  (cases i) (rfl) (rfl))

(thm leafTy_usk [P :- Exp]
  (Eq USk (usk (leafTy P)) (usk P))
  (exact (usk_subst P (fn [i :- Nat] (sLeafI i))
           (fn [i :- Nat] (sLeaf_usk i)))))

(thm leafTy_skel [P :- Exp]
  (Eq Sk (skel (leafTy P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sLeafI i))
           (fn [i :- Nat] (sLeaf_skel i)))))

(thm nodeTy_usk [P :- Exp]
  (Eq USk (usk (nodeTy P)) (usk P))
  (exact (usk_subst P (fn [i :- Nat] (sNodeI i))
           (fn [i :- Nat] (sNode_usk i)))))

(thm nodeTy_skel [P :- Exp]
  (Eq Sk (skel (nodeTy P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sNodeI i))
           (fn [i :- Nat] (sNode_skel i)))))

(thm y1Ty_usk [P :- Exp]
  (Eq USk (usk (y1Ty P)) (usk P))
  (exact (usk_subst P (fn [i :- Nat] (sAt 1 3 i))
           (fn [i :- Nat] (sAt_usk 1 3 i)))))

(thm y1Ty_skel [P :- Exp]
  (Eq Sk (skel (y1Ty P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sAt 1 3 i))
           (fn [i :- Nat] (sAt_skel 1 3 i)))))

(thm y2Ty_usk [P :- Exp]
  (Eq USk (usk (y2Ty P)) (usk P))
  (exact (usk_subst P (fn [i :- Nat] (sAt 1 4 i))
           (fn [i :- Nat] (sAt_usk 1 4 i)))))

(thm y2Ty_skel [P :- Exp]
  (Eq Sk (skel (y2Ty P)) (skel P))
  (exact (skel_subst P (fn [i :- Nat] (sAt 1 4 i))
           (fn [i :- Nat] (sAt_skel 1 4 i)))))

(thm erel_lbl_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, j :- Nat]
  (Erel chkf dec encTy cap (usk Exp.tLbl) (RV.lbl j) j)
  (rfl))

(thm erel_code_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, c :- Code]
  (Erel chkf dec encTy cap (usk Exp.tSyn) (RV.code c) c)
  (rfl))

;; The leaf of recSyn.  The label is bound at usage ω and related to itself.
;; Code.rec at a leaf is the leaf method; the node method is part of the
;; recursor and is not run.  Its environment casts the two recursive carriers
;; from usk P to the binder types y1Ty and y2Ty, which have that skeleton.
(thm adeqE_recs_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp,
   tle :- Exp, tne :- Exp, us :- (List U), l :- Nat,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   ihl :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp Exp.tLbl D))
                    (List.cons U U.uw us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tle w)
                       (Erel chkf dec encTy cap (usk P) w
                         (denU chkf dec encTy cap
                           (List.cons Exp Exp.tLbl D) tl P eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvE chkf dec encTy cap (EvSrc.recs rho tle tne (Code.sl l)) v)
         (Erel chkf dec encTy cap (usk P) v
           (Code.rec$1 (fn [_ :- Code] (Car (uskSk (usk P))))
             (fn [j :- Nat]
               (denU chkf dec encTy cap (List.cons Exp Exp.tLbl D) tl P
                 (Prod.mk j eta)))
             (fn [j :- Nat, a :- Code, b :- Code,
                  ya :- (Car (uskSk (usk P))), yb :- (Car (uskSk (usk P)))]
               (denU chkf dec encTy cap
                 (List.cons Exp (y2Ty P)
                   (List.cons Exp (y1Ty P)
                     (List.cons Exp Exp.tSyn
                       (List.cons Exp Exp.tSyn
                         (List.cons Exp Exp.tLbl D)))))
                 tn P
                 (Prod.mk
                   (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                             (Eq.symm (y2Ty_usk P)))) yb)
                   (Prod.mk
                     (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                               (Eq.symm (y1Ty_usk P)))) ya)
                     (Prod.mk b (Prod.mk a (Prod.mk j eta)))))))
             (Code.sl l))))))
  (have henv (envE chkf dec encTy cap
               (uskCtx (List.cons Exp Exp.tLbl D))
               (List.cons U U.uw us)
               (List.cons RV (RV.lbl l) rho)
               (Prod.mk l eta))
    (envE_consw chkf dec encTy cap (usk Exp.tLbl) (uskCtx D) us
      (RV.lbl l) rho l eta (erel_lbl_rfl chkf dec encTy cap l) hr))
  (refine' (exT RV _ _
    (ihl (List.cons RV (RV.lbl l) rho) (Prod.mk l eta) henv) _))
  (intro w hw)
  (have he (EvalE chkf dec encTy cap (List.cons RV (RV.lbl l) rho) tle w)
    (And.left hw))
  (have hrel (Erel chkf dec encTy cap (usk P) w
               (denU chkf dec encTy cap (List.cons Exp Exp.tLbl D) tl P
                 (Prod.mk l eta)))
    (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eRecSL chkf dec encTy cap rho tle tne l w he))
  (exact hrel))

;; --- recSyn, the node (Theorem 4′) ------------------------------------------
;;
;; The node method's Code.rec is the same term the leaf states inline.
;; Naming it (recsDen) keeps the node proof inside the JVM method limit.
;; The two casts of a usage-skeleton equation are the same term: erel_at
;; writes the fused congruence, and the recursor writes congruence twice.

(thm cast_usk_car
  [u1 :- USk, u2 :- USk, e :- (Eq USk u1 u2), a :- (Car (uskSk u1))]
  (Eq (Car (uskSk u2))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) e) a)
    (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w)) e)) a))
  (rfl))

;; The five-binder context of the node method, innermost last: yb at y2Ty,
;; ya at y1Ty, the right code, the left code, the label.
(kdef nodeCtx
  (forall [P Exp] (forall [D (List Exp)] (List Exp)))
  (fn [P :- Exp, D :- (List Exp)]
    (List.cons Exp (y2Ty P)
      (List.cons Exp (y1Ty P)
        (List.cons Exp Exp.tSyn
          (List.cons Exp Exp.tSyn
            (List.cons Exp Exp.tLbl D)))))))

(thm nodeCtx_eq
  [P :- Exp, D :- (List Exp)]
  (Eq (List Exp) (nodeCtx P D)
    (List.cons Exp (y2Ty P)
      (List.cons Exp (y1Ty P)
        (List.cons Exp Exp.tSyn
          (List.cons Exp Exp.tSyn
            (List.cons Exp Exp.tLbl D))))))
  (rfl))

;; Cast a carrier at usk P across usk Q = usk P.
(kdef castYP
  (forall [P Exp] (forall [Q Exp]
    (forall [a (Car (uskSk (usk P)))]
      (=> (Eq USk (usk Q) (usk P)) (Car (uskSk (usk Q)))))))
  (fn [P :- Exp, Q :- Exp, a :- (Car (uskSk (usk P))),
       e :- (Eq USk (usk Q) (usk P))]
    (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w)) (Eq.symm e))) a)))

(thm castYP_eq
  [P :- Exp, Q :- Exp, a :- (Car (uskSk (usk P))),
   e :- (Eq USk (usk Q) (usk P))]
  (Eq (Car (uskSk (usk Q))) (castYP P Q a e)
    (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w)) (Eq.symm e))) a))
  (rfl))

;; The carrier of recSyn: Code.rec of the two methods.  The leaf theorem
;; states this recursor inline; recsDen_leaf is that they are one term.
(kdef recsDen
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [cap Nat] (forall [D (List Exp)] (forall [P Exp]
          (forall [tl Exp] (forall [tn Exp]
            (forall [eta (HEnv (usks (uskCtx D)))]
              (=> Code (Car (uskSk (usk P)))))))))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp,
       eta :- (HEnv (usks (uskCtx D))), c :- Code]
    (Code.rec$1 (fn [_ :- Code] (Car (uskSk (usk P))))
      (fn [j :- Nat]
        (denU chkf dec encTy cap (List.cons Exp Exp.tLbl D) tl P
          (Prod.mk j eta)))
      (fn [j :- Nat, a :- Code, b :- Code,
           ya :- (Car (uskSk (usk P))), yb :- (Car (uskSk (usk P)))]
        (denU chkf dec encTy cap
          (List.cons Exp (y2Ty P)
            (List.cons Exp (y1Ty P)
              (List.cons Exp Exp.tSyn
                (List.cons Exp Exp.tSyn
                  (List.cons Exp Exp.tLbl D)))))
          tn P
          (Prod.mk
            (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                      (Eq.symm (y2Ty_usk P)))) yb)
            (Prod.mk
              (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                        (Eq.symm (y1Ty_usk P)))) ya)
              (Prod.mk b (Prod.mk a (Prod.mk j eta)))))))
      c)))

(thm recsDen_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp,
   eta :- (HEnv (usks (uskCtx D))), l :- Nat]
  (Eq (Car (uskSk (usk P)))
    (recsDen chkf dec encTy cap D P tl tn eta (Code.sl l))
    (Code.rec$1 (fn [_ :- Code] (Car (uskSk (usk P))))
             (fn [j :- Nat]
               (denU chkf dec encTy cap (List.cons Exp Exp.tLbl D) tl P
                 (Prod.mk j eta)))
             (fn [j :- Nat, a :- Code, b :- Code,
                  ya :- (Car (uskSk (usk P))), yb :- (Car (uskSk (usk P)))]
               (denU chkf dec encTy cap
                 (List.cons Exp (y2Ty P)
                   (List.cons Exp (y1Ty P)
                     (List.cons Exp Exp.tSyn
                       (List.cons Exp Exp.tSyn
                         (List.cons Exp Exp.tLbl D)))))
                 tn P
                 (Prod.mk
                   (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                             (Eq.symm (y2Ty_usk P)))) yb)
                   (Prod.mk
                     (Eq.mp (congrArg Car (congrArg (fn [w :- USk] (uskSk w))
                               (Eq.symm (y1Ty_usk P)))) ya)
                     (Prod.mk b (Prod.mk a (Prod.mk j eta)))))))
             (Code.sl l)))
  (rfl))

;; The node method's carrier environment: the two recursive results cast
;; from usk P to y2Ty and y1Ty, then the right code, the left code, the label.
(kdef nodeEta
  (forall [P Exp] (forall [D (List Exp)]
    (forall [a Code] (forall [b Code] (forall [l Nat]
      (forall [ca (Car (uskSk (usk P)))]
        (forall [cb (Car (uskSk (usk P)))]
          (forall [eta (HEnv (usks (uskCtx D)))]
            (HEnv (usks (uskCtx (nodeCtx P D))))))))))))
  (fn [P :- Exp, D :- (List Exp), a :- Code, b :- Code, l :- Nat,
       ca :- (Car (uskSk (usk P))), cb :- (Car (uskSk (usk P))),
       eta :- (HEnv (usks (uskCtx D)))]
    (Prod.mk (castYP P (y2Ty P) cb (y2Ty_usk P))
      (Prod.mk (castYP P (y1Ty P) ca (y1Ty_usk P))
        (Prod.mk b (Prod.mk a (Prod.mk l eta)))))))

;; The node method's runtime environment, same order as nodeEta.
(kdef nodeRho
  (forall [yb RV] (forall [ya RV] (forall [b Code] (forall [a Code]
    (forall [l Nat] (forall [rho (List RV)] (List RV)))))))
  (fn [yb :- RV, ya :- RV, b :- Code, a :- Code, l :- Nat, rho :- (List RV)]
    (List.cons RV yb (List.cons RV ya
      (List.cons RV (RV.code b)
        (List.cons RV (RV.code a)
          (List.cons RV (RV.lbl l) rho)))))))

;; Usage vector of the node method: ya and yb at 1, the two codes and the
;; label at ω, then the tail.
(kdef nodeUs
  (forall [us (List U)] (List U))
  (fn [us :- (List U)]
    (List.cons U U.u1 (List.cons U U.u1
      (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us)))))))

(thm nodeEta_eq
  [P :- Exp, D :- (List Exp), a :- Code, b :- Code, l :- Nat,
   ca :- (Car (uskSk (usk P))), cb :- (Car (uskSk (usk P))),
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (HEnv (usks (uskCtx (nodeCtx P D))))
    (nodeEta P D a b l ca cb eta)
    (Prod.mk (castYP P (y2Ty P) cb (y2Ty_usk P))
      (Prod.mk (castYP P (y1Ty P) ca (y1Ty_usk P))
        (Prod.mk b (Prod.mk a (Prod.mk l eta))))))
  (rfl))

(thm nodeRho_eq
  [yb :- RV, ya :- RV, b :- Code, a :- Code, l :- Nat, rho :- (List RV)]
  (Eq (List RV) (nodeRho yb ya b a l rho)
    (List.cons RV yb (List.cons RV ya
      (List.cons RV (RV.code b)
        (List.cons RV (RV.code a)
          (List.cons RV (RV.lbl l) rho))))))
  (rfl))

(thm nodeUs_eq
  [us :- (List U)]
  (Eq (List U) (nodeUs us)
    (List.cons U U.u1 (List.cons U U.u1
      (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us))))))
  (rfl))

;; recSyn, the node (Theorem 4′).  The two recursive results are E-related
;; at usk P; the binders are y2Ty and y1Ty, which have that usage skeleton,
;; so the entries are the cast carriers.  recsDen is the Code.rec the leaf
;; states inline.

(thm adeqE_recs_node
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp,
   tle :- Exp, tne :- Exp, us :- (List U),
   l :- Nat, a :- Code, b :- Code,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   iha :- (Exists (fn [ya :- RV]
            (And (EvE chkf dec encTy cap (EvSrc.recs rho tle tne a) ya)
                 (Erel chkf dec encTy cap (usk P) ya (recsDen chkf dec encTy cap D P tl tn eta a))))),
   ihb :- (Exists (fn [yb :- RV]
            (And (EvE chkf dec encTy cap (EvSrc.recs rho tle tne b) yb)
                 (Erel chkf dec encTy cap (usk P) yb (recsDen chkf dec encTy cap D P tl tn eta b))))),
   ihn :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
              (=> (envE chkf dec encTy cap (uskCtx (nodeCtx P D)) (nodeUs us)
                    rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tne w)
                       (Erel chkf dec encTy cap (usk P) w
                         (denU chkf dec encTy cap (nodeCtx P D) tn P eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvE chkf dec encTy cap (EvSrc.recs rho tle tne (Code.sn l a b)) v)
         (Erel chkf dec encTy cap (usk P) v (recsDen chkf dec encTy cap D P tl tn eta (Code.sn l a b))))))
  (refine' (exT RV _ _ iha _)) (intro ya ha)
  (have hea (EvE chkf dec encTy cap (EvSrc.recs rho tle tne a) ya) (And.left ha))
  (have hra (Erel chkf dec encTy cap (usk P) ya (recsDen chkf dec encTy cap D P tl tn eta a)) (And.right ha))
  (refine' (exT RV _ _ ihb _)) (intro yb hb)
  (have heb (EvE chkf dec encTy cap (EvSrc.recs rho tle tne b) yb) (And.left hb))
  (have hrb (Erel chkf dec encTy cap (usk P) yb (recsDen chkf dec encTy cap D P tl tn eta b)) (And.right hb))
  (have eL (envE chkf dec encTy cap
             (uskCtx (List.cons Exp Exp.tLbl D))
             (List.cons U U.uw us)
             (List.cons RV (RV.lbl l) rho)
             (Prod.mk l eta))
    (envE_consw chkf dec encTy cap (usk Exp.tLbl) (uskCtx D) us
      (RV.lbl l) rho l eta (erel_lbl_rfl chkf dec encTy cap l) hr))
  (have eA (envE chkf dec encTy cap
             (uskCtx (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
             (List.cons U U.uw (List.cons U U.uw us))
             (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))
             (Prod.mk a (Prod.mk l eta)))
    (envE_consw chkf dec encTy cap (usk Exp.tSyn)
      (uskCtx (List.cons Exp Exp.tLbl D))
      (List.cons U U.uw us)
      (RV.code a) (List.cons RV (RV.lbl l) rho)
      a (Prod.mk l eta) (erel_code_rfl chkf dec encTy cap a) eL))
  (have eB (envE chkf dec encTy cap
             (uskCtx (List.cons Exp Exp.tSyn
                       (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
             (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us)))
             (List.cons RV (RV.code b)
               (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho)))
             (Prod.mk b (Prod.mk a (Prod.mk l eta))))
    (envE_consw chkf dec encTy cap (usk Exp.tSyn)
      (uskCtx (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
      (List.cons U U.uw (List.cons U U.uw us))
      (RV.code b)
      (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))
      b (Prod.mk a (Prod.mk l eta)) (erel_code_rfl chkf dec encTy cap b) eA))
  (have eYa (envE chkf dec encTy cap
              (uskCtx (List.cons Exp (y1Ty P)
                (List.cons Exp Exp.tSyn
                  (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))
              (List.cons U U.u1
                (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us))))
              (List.cons RV ya
                (List.cons RV (RV.code b)
                  (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))))
              (Prod.mk (castYP P (y1Ty P) (recsDen chkf dec encTy cap D P tl tn eta a) (y1Ty_usk P))
                (Prod.mk b (Prod.mk a (Prod.mk l eta)))))
    (envE_cons1 chkf dec encTy cap (usk (y1Ty P))
      (uskCtx (List.cons Exp Exp.tSyn
                (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
      (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us)))
      ya
      (List.cons RV (RV.code b)
        (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho)))
      (castYP P (y1Ty P) (recsDen chkf dec encTy cap D P tl tn eta a) (y1Ty_usk P))
      (Prod.mk b (Prod.mk a (Prod.mk l eta)))
      (erel_at chkf dec encTy cap (usk P) (usk (y1Ty P)) ya
        (recsDen chkf dec encTy cap D P tl tn eta a) hra (Eq.symm (y1Ty_usk P)))
      eB))
  (have eYb (envE chkf dec encTy cap
              (uskCtx (nodeCtx P D)) (nodeUs us)
              (nodeRho yb ya b a l rho)
              (nodeEta P D a b l (recsDen chkf dec encTy cap D P tl tn eta a) (recsDen chkf dec encTy cap D P tl tn eta b) eta))
    (envE_cons1 chkf dec encTy cap (usk (y2Ty P))
      (uskCtx (List.cons Exp (y1Ty P)
        (List.cons Exp Exp.tSyn
          (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))
      (List.cons U U.u1
        (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw us))))
      yb
      (List.cons RV ya
        (List.cons RV (RV.code b)
          (List.cons RV (RV.code a) (List.cons RV (RV.lbl l) rho))))
      (castYP P (y2Ty P) (recsDen chkf dec encTy cap D P tl tn eta b) (y2Ty_usk P))
      (Prod.mk (castYP P (y1Ty P) (recsDen chkf dec encTy cap D P tl tn eta a) (y1Ty_usk P))
        (Prod.mk b (Prod.mk a (Prod.mk l eta))))
      (erel_at chkf dec encTy cap (usk P) (usk (y2Ty P)) yb
        (recsDen chkf dec encTy cap D P tl tn eta b) hrb (Eq.symm (y2Ty_usk P)))
      eYa))
  (refine' (exT RV _ _
    (ihn (nodeRho yb ya b a l rho)
         (nodeEta P D a b l (recsDen chkf dec encTy cap D P tl tn eta a) (recsDen chkf dec encTy cap D P tl tn eta b) eta)
         eYb) _))
  (intro w hw)
  (have he (EvalE chkf dec encTy cap (nodeRho yb ya b a l rho) tne w)
    (And.left hw))
  (have hrel (Erel chkf dec encTy cap (usk P) w
               (denU chkf dec encTy cap (nodeCtx P D) tn P
                 (nodeEta P D a b l (recsDen chkf dec encTy cap D P tl tn eta a) (recsDen chkf dec encTy cap D P tl tn eta b) eta)))
    (And.right hw))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eRecSN chkf dec encTy cap rho tle tne l a b ya yb w hea heb he))
  (exact hrel))

;; recSyn's recursion (Theorem 4′).  Induction on the code.  The leaf and
;; the node share the tail environment; the leaf binds the label at ω and
;; the node binds yb, ya at 1 and the two codes and the label at ω.
(thm adeqE_recs
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp,
   tle :- Exp, tne :- Exp, us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   ihl :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp Exp.tLbl D))
                    (List.cons U U.uw us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tle w)
                       (Erel chkf dec encTy cap (usk P) w
                         (denU chkf dec encTy cap
                           (List.cons Exp Exp.tLbl D) tl P eta2)))))))),
   ihn :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
              (=> (envE chkf dec encTy cap (uskCtx (nodeCtx P D)) (nodeUs us)
                    rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tne w)
                       (Erel chkf dec encTy cap (usk P) w
                         (denU chkf dec encTy cap (nodeCtx P D) tn P eta2))))))))]
  (forall [c Code]
    (Exists (fn [v :- RV]
      (And (EvE chkf dec encTy cap (EvSrc.recs rho tle tne c) v)
           (Erel chkf dec encTy cap (usk P) v
             (recsDen chkf dec encTy cap D P tl tn eta c))))))
  (intro c) (induction c)
  (exact (adeqE_recs_leaf chkf dec encTy cap D P tl tn tle tne us l rho eta hr ihl))
  (exact (adeqE_recs_node chkf dec encTy cap D P tl tn tle tne us l a b rho eta hr ih_a ih_b ihn)))

;; --- recSyn, the outer judgment (Theorem 4′) --------------------------------
;;
;; ⟦recS⟧ is a Code.rec.  denU casts that whole recursor; the cast stays
;; outside it (cast_code), because a cast inside the node function makes
;; the eliminator's motive fail to match.  At Lbl and Syn the usage
;; skeleton is the base skeleton, so those entries are not cast.

(thm usk_skel_lbl []
  (Eq Sk (uskSk (usk Exp.tLbl)) Sk.lbl)
  (rfl))

(thm usk_skel_syn []
  (Eq Sk (uskSk (usk Exp.tSyn)) Sk.syn)
  (rfl))

(thm denU_syn_id
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq Code
    (denU chkf dec encTy n D t Exp.tSyn eta)
    (den chkf dec encTy n t (skels D) Sk.syn (henv_of_usk D eta)))
  (rfl))

(thm skels_lbl_ctx [D :- (List Exp)]
  (Eq (List Sk) (skels (List.cons Exp Exp.tLbl D))
      (List.cons Sk Sk.lbl (skels D)))
  (rfl))

(thm cast_lbl_id [j :- Nat]
  (Eq Nat (Eq.mp (congrArg Car (usk_skel Exp.tLbl)) j) j)
  (rfl))

(thm cast_syn_id [c :- Code]
  (Eq Code (Eq.mp (congrArg Car (usk_skel Exp.tSyn)) c) c)
  (rfl))

;; denU unfolds to the cast of ⟦·⟧ along usk_skel, at the transported
;; environment.  The equation is the definition.
(thm denU_open_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, tl :- Exp,
   eta :- (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n (List.cons Exp Exp.tLbl D) tl P eta)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      (den chkf dec encTy n tl (skels (List.cons Exp Exp.tLbl D)) (skel P)
        (henv_of_usk (List.cons Exp Exp.tLbl D) eta))))
  (rfl))

;; The leaf method reads the label.  henv_ext casts the label by usk_skel
;; of Lbl, and that cast is the label (cast_lbl_id), so the environment
;; ⟦·⟧ sees is (j, η).
(thm denU_rec_sleaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, tl :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n (List.cons Exp Exp.tLbl D) tl P (Prod.mk j eta))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      (den chkf dec encTy n tl (List.cons Sk Sk.lbl (skels D)) (skel P)
        (Prod.mk j (henv_of_usk D eta)))))
  (exact (congrArg
    (fn [en :- (HEnv (skels (List.cons Exp Exp.tLbl D)))]
      (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
        (den chkf dec encTy n tl
          (skels (List.cons Exp Exp.tLbl D)) (skel P) en)))
    (henv_ext Exp.tLbl D j eta))))

;; A cast stays outside Code.rec.  The induction hypothesis carries it,
;; and the leaf and node hypotheses apply it.  Same shape as cast_iter.
(thm cast_code
  [s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2),
   leaf1 :- (=> Nat (Car s1)),
   leaf2 :- (=> Nat (Car s2)),
   node1 :- (=> Nat Code Code (Car s1) (Car s1) (Car s1)),
   node2 :- (=> Nat Code Code (Car s2) (Car s2) (Car s2)),
   k :- Code,
   hleaf :- (forall [j Nat]
             (Eq (Car s1) (leaf1 j)
               (Eq.mp (congrArg Car (Eq.symm e)) (leaf2 j)))),
   hnode :- (forall [j Nat] (forall [a Code] (forall [b Code]
             (forall [ya2 (Car s2)] (forall [yb2 (Car s2)]
               (forall [ya1 (Car s1)] (forall [yb1 (Car s1)]
                 (=> (Eq (Car s1) ya1 (Eq.mp (congrArg Car (Eq.symm e)) ya2))
                   (=> (Eq (Car s1) yb1 (Eq.mp (congrArg Car (Eq.symm e)) yb2))
                     (Eq (Car s1) (node1 j a b ya1 yb1)
                       (Eq.mp (congrArg Car (Eq.symm e))
                         (node2 j a b ya2 yb2))))))))))))]
  (Eq (Car s1)
    (Code.rec$1 (fn [_ :- Code] (Car s1)) leaf1 node1 k)
    (Eq.mp (congrArg Car (Eq.symm e))
      (Code.rec$1 (fn [_ :- Code] (Car s2)) leaf2 node2 k)))
  (induction k)
  (change (Eq (Car s1) (leaf1 l)
            (Eq.mp (congrArg Car (Eq.symm e)) (leaf2 l))))
  (exact (hleaf l))
  (change (Eq (Car s1)
    (node1 l a b
      (Code.rec$1 (fn [_ :- Code] (Car s1)) leaf1 node1 a)
      (Code.rec$1 (fn [_ :- Code] (Car s1)) leaf1 node1 b))
    (Eq.mp (congrArg Car (Eq.symm e))
      (node2 l a b
        (Code.rec$1 (fn [_ :- Code] (Car s2)) leaf2 node2 a)
        (Code.rec$1 (fn [_ :- Code] (Car s2)) leaf2 node2 b)))))
  (exact (hnode l a b
    (Code.rec$1 (fn [_ :- Code] (Car s2)) leaf2 node2 a)
    (Code.rec$1 (fn [_ :- Code] (Car s2)) leaf2 node2 b)
    (Code.rec$1 (fn [_ :- Code] (Car s1)) leaf1 node1 a)
    (Code.rec$1 (fn [_ :- Code] (Car s1)) leaf1 node1 b)
    ih_a ih_b)))

;; Replacing the head skeleton of a context, and the head carrier with it.
(thm den_ctx_head
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, t :- Exp, s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2),
   G :- (List Sk), sk :- Sk, en :- (HEnv G), a :- (Car s2)]
  (Eq (Car sk)
    (den chkf dec encTy n t (List.cons Sk s1 G) sk
      (Prod.mk (Eq.mp (congrArg Car (Eq.symm e)) a) en))
    (den chkf dec encTy n t (List.cons Sk s2 G) sk (Prod.mk a en)))
  (cases e) (rfl))

;; The same, one binder down: the outer binder stays, the next one moves.
(thm den_ctx_next
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, t :- Exp, s0 :- Sk, s1 :- Sk, s2 :- Sk,
   e :- (Eq Sk s1 s2),
   G :- (List Sk), sk :- Sk, en :- (HEnv G),
   b :- (Car s0), a :- (Car s2)]
  (Eq (Car sk)
    (den chkf dec encTy n t (List.cons Sk s0 (List.cons Sk s1 G)) sk
      (Prod.mk b (Prod.mk (Eq.mp (congrArg Car (Eq.symm e)) a) en)))
    (den chkf dec encTy n t (List.cons Sk s0 (List.cons Sk s2 G)) sk
      (Prod.mk b (Prod.mk a en))))
  (cases e) (rfl))

;; The skeleton path from the motive, through y1Ty or y2Ty, and back.
;; Only skeleton equations: a usage-skeleton equation is cased by
;; cast_usk_path, and casing both at once was rejected.
(thm cast_y_simp
  [sU1 :- Sk, sU2 :- Sk, sP :- Sk, sQ :- Sk,
   eP :- (Eq Sk sU2 sP),
   eQ :- (Eq Sk sU1 sQ),
   eS :- (Eq Sk sQ sP),
   a :- (Car sP)]
  (Eq (Car sQ)
    (Eq.mp (congrArg Car eQ)
      (Eq.mp (congrArg Car (Eq.trans eP (Eq.trans (Eq.symm eS) (Eq.symm eQ))))
        (Eq.mp (congrArg Car (Eq.symm eP)) a)))
    (Eq.mp (congrArg Car (Eq.symm eS)) a))
  (cases eP) (cases eQ) (cases eS) (rfl))

;; A carrier at skel P, cast to usk P and then to usk Q, cast back by
;; usk_skel Q, is the carrier cast along skel Q = skel P.
(thm cast_y_head
  [uP :- USk, uQ :- USk, sP :- Sk, sQ :- Sk,
   eU :- (Eq USk uQ uP),
   eP :- (Eq Sk (uskSk uP) sP),
   eQ :- (Eq Sk (uskSk uQ) sQ),
   eS :- (Eq Sk sQ sP),
   a :- (Car sP)]
  (Eq (Car sQ)
    (Eq.mp (congrArg Car eQ)
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm eU))
        (Eq.mp (congrArg Car (Eq.symm eP)) a)))
    (Eq.mp (congrArg Car (Eq.symm eS)) a))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (uskSk uQ))] (Eq.mp (congrArg Car eQ) x))
      (cast_usk_path uQ uP eU sP sQ eP eQ eS a))
    (cast_y_simp (uskSk uQ) (uskSk uP) sP sQ eP eQ eS a))))

;; The two codes and the label.  Their usage skeletons are the base
;; skeletons, so henv_ext's casts are the values themselves.
(thm henv_code_tail
  [D :- (List Exp), j :- Nat, a :- Code, b :- Code,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (HEnv (skels (List.cons Exp Exp.tSyn
                     (List.cons Exp Exp.tSyn
                       (List.cons Exp Exp.tLbl D)))))
    (henv_of_usk (List.cons Exp Exp.tSyn
                   (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
      (Prod.mk b (Prod.mk a (Prod.mk j eta))))
    (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta)))))
  (exact (Eq.trans
    (henv_ext Exp.tSyn
      (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))
      b (Prod.mk a (Prod.mk j eta)))
    (congrArg
      (fn [en :- (HEnv (skels (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))]
        (Prod.mk (Eq.mp (congrArg Car (usk_skel Exp.tSyn)) b) en))
      (Eq.trans
        (henv_ext Exp.tSyn (List.cons Exp Exp.tLbl D) a (Prod.mk j eta))
        (congrArg
          (fn [en :- (HEnv (skels (List.cons Exp Exp.tLbl D)))]
            (Prod.mk (Eq.mp (congrArg Car (usk_skel Exp.tSyn)) a) en))
          (henv_ext Exp.tLbl D j eta)))))))

;; The node method's environment.  yb and ya arrive at usk P and are cast
;; into y2Ty and y1Ty; henv_of_usk casts them on to the skeletons, and
;; cast_y_head brings those back to skel P.  The codes and the label are
;; henv_code_tail.
(thm henv_recs
  [P :- Exp, D :- (List Exp), j :- Nat, a :- Code, b :- Code,
   ya2 :- (Car (skel P)), yb2 :- (Car (skel P)),
   ya1 :- (Car (uskSk (usk P))), yb1 :- (Car (uskSk (usk P))),
   eta :- (HEnv (usks (uskCtx D))),
   ha :- (Eq (Car (uskSk (usk P))) ya1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)),
   hb :- (Eq (Car (uskSk (usk P))) yb1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) yb2))]
  (Eq (HEnv (skels (List.cons Exp (y2Ty P)
                     (List.cons Exp (y1Ty P)
                       (List.cons Exp Exp.tSyn
                         (List.cons Exp Exp.tSyn
                           (List.cons Exp Exp.tLbl D)))))))
    (henv_of_usk (List.cons Exp (y2Ty P)
                   (List.cons Exp (y1Ty P)
                     (List.cons Exp Exp.tSyn
                       (List.cons Exp Exp.tSyn
                         (List.cons Exp Exp.tLbl D)))))
      (Prod.mk (castYP P (y2Ty P) yb1 (y2Ty_usk P))
        (Prod.mk (castYP P (y1Ty P) ya1 (y1Ty_usk P))
          (Prod.mk b (Prod.mk a (Prod.mk j eta))))))
    (Prod.mk (Eq.mp (congrArg Car (Eq.symm (y2Ty_skel P))) yb2)
      (Prod.mk (Eq.mp (congrArg Car (Eq.symm (y1Ty_skel P))) ya2)
        (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta)))))))
  (rw [hb]) (rw [ha])
  (exact (Eq.trans
    (henv_ext (y2Ty P)
      (List.cons Exp (y1Ty P)
        (List.cons Exp Exp.tSyn
          (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
      (castYP P (y2Ty P)
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) yb2)
        (y2Ty_usk P))
      (Prod.mk (castYP P (y1Ty P)
                 (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)
                 (y1Ty_usk P))
        (Prod.mk b (Prod.mk a (Prod.mk j eta)))))
    (Eq.trans
      (congrArg
        (fn [h :- (Car (skel (y2Ty P)))]
          (Prod.mk h
            (henv_of_usk (List.cons Exp (y1Ty P)
                           (List.cons Exp Exp.tSyn
                             (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
              (Prod.mk (castYP P (y1Ty P)
                         (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)
                         (y1Ty_usk P))
                (Prod.mk b (Prod.mk a (Prod.mk j eta)))))))
        (cast_y_head (usk P) (usk (y2Ty P)) (skel P) (skel (y2Ty P))
          (y2Ty_usk P) (usk_skel P) (usk_skel (y2Ty P)) (y2Ty_skel P) yb2))
      (congrArg
        (fn [en :- (HEnv (skels (List.cons Exp (y1Ty P)
                                (List.cons Exp Exp.tSyn
                                  (List.cons Exp Exp.tSyn
                                    (List.cons Exp Exp.tLbl D))))))]
          (Prod.mk (Eq.mp (congrArg Car (Eq.symm (y2Ty_skel P))) yb2) en))
        (Eq.trans
          (henv_ext (y1Ty P)
            (List.cons Exp Exp.tSyn
              (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))
            (castYP P (y1Ty P)
              (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)
              (y1Ty_usk P))
            (Prod.mk b (Prod.mk a (Prod.mk j eta))))
          (Eq.trans
            (congrArg
              (fn [h :- (Car (skel (y1Ty P)))]
                (Prod.mk h
                  (henv_of_usk (List.cons Exp Exp.tSyn
                                 (List.cons Exp Exp.tSyn
                                   (List.cons Exp Exp.tLbl D)))
                    (Prod.mk b (Prod.mk a (Prod.mk j eta))))))
              (cast_y_head (usk P) (usk (y1Ty P)) (skel P) (skel (y1Ty P))
                (y1Ty_usk P) (usk_skel P) (usk_skel (y1Ty P)) (y1Ty_skel P) ya2))
            (congrArg
              (fn [en :- (HEnv (skels (List.cons Exp Exp.tSyn
                                      (List.cons Exp Exp.tSyn
                                        (List.cons Exp Exp.tLbl D)))))]
                (Prod.mk (Eq.mp (congrArg Car (Eq.symm (y1Ty_skel P))) ya2) en))
              (henv_code_tail D j a b eta)))))))))

;; The node method of ⟦recS⟧ reads yb and ya at skel P.  denU reads them
;; at y2Ty and y1Ty.  henv_recs is that environment, and the two context
;; heads move from skel(y2Ty) and skel(y1Ty) to skel P.
(thm denU_rec_snode
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, tn :- Exp,
   eta :- (HEnv (usks (uskCtx D))),
   j :- Nat, a :- Code, b :- Code,
   ya2 :- (Car (skel P)), yb2 :- (Car (skel P)),
   ya1 :- (Car (uskSk (usk P))), yb1 :- (Car (uskSk (usk P))),
   ha :- (Eq (Car (uskSk (usk P))) ya1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)),
   hb :- (Eq (Car (uskSk (usk P))) yb1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) yb2))]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n
      (List.cons Exp (y2Ty P)
        (List.cons Exp (y1Ty P)
          (List.cons Exp Exp.tSyn
            (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))
      tn P
      (Prod.mk (castYP P (y2Ty P) yb1 (y2Ty_usk P))
        (Prod.mk (castYP P (y1Ty P) ya1 (y1Ty_usk P))
          (Prod.mk b (Prod.mk a (Prod.mk j eta))))))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
      (den chkf dec encTy n tn
        (sk2 (skel P) (skel P)
          (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D))))
        (skel P)
        (Prod.mk yb2 (Prod.mk ya2
          (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta)))))))))
  (exact (Eq.trans
    (congrArg
      (fn [en :- (HEnv (skels (List.cons Exp (y2Ty P)
                               (List.cons Exp (y1Ty P)
                                 (List.cons Exp Exp.tSyn
                                   (List.cons Exp Exp.tSyn
                                     (List.cons Exp Exp.tLbl D)))))))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P)))
          (den chkf dec encTy n tn
            (skels (List.cons Exp (y2Ty P)
                     (List.cons Exp (y1Ty P)
                       (List.cons Exp Exp.tSyn
                         (List.cons Exp Exp.tSyn
                           (List.cons Exp Exp.tLbl D))))))
            (skel P) en)))
      (henv_recs P D j a b ya2 yb2 ya1 yb1 eta ha hb))
    (Eq.trans
      (congrArg
        (fn [v :- (Car (skel P))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) v))
        (den_ctx_head chkf dec encTy n tn
          (skel (y2Ty P)) (skel P) (y2Ty_skel P)
          (skels (List.cons Exp (y1Ty P)
                   (List.cons Exp Exp.tSyn
                     (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))
          (skel P)
          (Prod.mk (Eq.mp (congrArg Car (Eq.symm (y1Ty_skel P))) ya2)
            (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta)))))
          yb2))
      (congrArg
        (fn [v :- (Car (skel P))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) v))
        (den_ctx_next chkf dec encTy n tn
          (skel P) (skel (y1Ty P)) (skel P) (y1Ty_skel P)
          (skels (List.cons Exp Exp.tSyn
                   (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))
          (skel P)
          (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta))))
          yb2 ya2))))))

;; den_recS is a kernel definition.  Rewrite sees the folded application,
;; not the Code.rec inside it.
(thm den_recS_open
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   prev :- DenFn, n :- Nat,
   P :- Exp, tl :- Exp, tn :- Exp, c :- Exp,
   iP :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   itl :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   itn :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   ic :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   G :- (List Sk), sk :- Sk, en :- (HEnv G)]
  (Eq (Car sk)
    (den_recS chkf dec encTy prev n P tl tn c iP itl itn ic G sk en)
    (Code.rec$1 (fn [_ :- Code] (Car sk))
      (fn [l :- Nat] (itl (List.cons Sk Sk.lbl G) sk (Prod.mk l en)))
      (fn [l :- Nat, a :- Code, b :- Code, ya :- (Car sk), yb :- (Car sk)]
        (itn (sk2 sk sk (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) sk
          (Prod.mk yb (Prod.mk ya (Prod.mk b (Prod.mk a (Prod.mk l en)))))))
      (ic G Sk.syn en)))
  (rfl))

;; ⟦recS⟧ at usk P is the Code.rec of denU.  den_recS is folded, so the
;; equation is den_recS_open; the cast of that recursor is cast_code.
(thm denU_recS
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tn :- Exp, c :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk P)))
    (denU chkf dec encTy n D (Exp.recS P tl tn c) P eta)
    (recsDen chkf dec encTy n D P tl tn eta
      (denU chkf dec encTy n D c Exp.tSyn eta)))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (skel P))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) x))
      (Eq.trans
        (den_recS_at chkf dec encTy n P tl tn c (skels D) (skel P)
          (henv_of_usk D eta))
        (den_recS_open chkf dec encTy (denPrev chkf dec encTy n) n P tl tn c
          (den chkf dec encTy n P) (den chkf dec encTy n tl)
          (den chkf dec encTy n tn) (den chkf dec encTy n c)
          (skels D) (skel P) (henv_of_usk D eta))))
    (Eq.symm
      (cast_code (uskSk (usk P)) (skel P) (usk_skel P)
        (fn [j :- Nat]
          (denU chkf dec encTy n (List.cons Exp Exp.tLbl D) tl P
            (Prod.mk j eta)))
        (fn [j :- Nat]
          (den chkf dec encTy n tl (List.cons Sk Sk.lbl (skels D)) (skel P)
            (Prod.mk j (henv_of_usk D eta))))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya :- (Car (uskSk (usk P))), yb :- (Car (uskSk (usk P)))]
          (denU chkf dec encTy n
            (List.cons Exp (y2Ty P)
              (List.cons Exp (y1Ty P)
                (List.cons Exp Exp.tSyn
                  (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))
            tn P
            (Prod.mk (castYP P (y2Ty P) yb (y2Ty_usk P))
              (Prod.mk (castYP P (y1Ty P) ya (y1Ty_usk P))
                (Prod.mk b (Prod.mk a (Prod.mk j eta)))))))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya :- (Car (skel P)), yb :- (Car (skel P))]
          (den chkf dec encTy n tn
            (sk2 (skel P) (skel P)
              (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D))))
            (skel P)
            (Prod.mk yb (Prod.mk ya
              (Prod.mk b (Prod.mk a (Prod.mk j (henv_of_usk D eta))))))))
        (den chkf dec encTy n c (skels D) Sk.syn (henv_of_usk D eta))
        (fn [j :- Nat]
          (denU_rec_sleaf chkf dec encTy n D P tl eta j))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya2 :- (Car (skel P)), yb2 :- (Car (skel P)),
             ya1 :- (Car (uskSk (usk P))), yb1 :- (Car (uskSk (usk P))),
             ha :- (Eq (Car (uskSk (usk P))) ya1
                     (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) ya2)),
             hb :- (Eq (Car (uskSk (usk P))) yb1
                     (Eq.mp (congrArg Car (Eq.symm (usk_skel P))) yb2))]
          (denU_rec_snode chkf dec encTy n D P tn eta j a b
            ya2 yb2 ya1 yb1 ha hb)))))))

;; The leaf method is judged at leafTy.  leafTy is a substitution of a
;; term, so it has the usage skeleton of P, and the witness moves.
(thm adeqE_leaf_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tl :- Exp, tle :- Exp,
   us :- (List U),
   ihl :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp Exp.tLbl D))
                    (List.cons U U.uw us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tle w)
                       (Erel chkf dec encTy cap (usk (leafTy P)) w
                         (denU chkf dec encTy cap
                           (List.cons Exp Exp.tLbl D) tl (leafTy P) eta2))))))))]
  (forall [rho2 (List RV)]
    (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
      (=> (envE chkf dec encTy cap
            (uskCtx (List.cons Exp Exp.tLbl D))
            (List.cons U U.uw us) rho2 eta2)
        (Exists (fn [w :- RV]
          (And (EvalE chkf dec encTy cap rho2 tle w)
               (Erel chkf dec encTy cap (usk P) w
                 (denU chkf dec encTy cap
                   (List.cons Exp Exp.tLbl D) tl P eta2))))))))
  (intro rho2) (intro eta2) (intro he)
  (exact (adeqE_retarget chkf dec encTy cap
           (List.cons Exp Exp.tLbl D) tl tle (leafTy P) P rho2 eta2
           (leafTy_usk P) (leafTy_skel P)
           (ihl rho2 eta2 he))))

;; The node method is judged at nodeTy, and nodeTy likewise has the
;; skeleton of P.
(thm adeqE_node_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp, tn :- Exp, tne :- Exp,
   us :- (List U),
   ihn :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (nodeCtx P D)) (nodeUs us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tne w)
                       (Erel chkf dec encTy cap (usk (nodeTy P)) w
                         (denU chkf dec encTy cap (nodeCtx P D) tn
                           (nodeTy P) eta2))))))))]
  (forall [rho2 (List RV)]
    (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
      (=> (envE chkf dec encTy cap
            (uskCtx (nodeCtx P D)) (nodeUs us) rho2 eta2)
        (Exists (fn [w :- RV]
          (And (EvalE chkf dec encTy cap rho2 tne w)
               (Erel chkf dec encTy cap (usk P) w
                 (denU chkf dec encTy cap (nodeCtx P D) tn P eta2))))))))
  (intro rho2) (intro eta2) (intro he)
  (exact (adeqE_retarget chkf dec encTy cap
           (nodeCtx P D) tn tne (nodeTy P) P rho2 eta2
           (nodeTy_usk P) (nodeTy_skel P)
           (ihn rho2 eta2 he))))

;; recS at the motive's own skeleton (Theorem 4′).  The methods are judged
;; at leafTy and nodeTy and moved to P.  The scrutinee denotes the code
;; the recursion runs on.  denU_recS is that Code.rec.
(thm adeqE_recS_at
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp,
   tl :- Exp, tn :- Exp, c :- Exp,
   tle :- Exp, tne :- Exp, ce :- Exp,
   us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   ihc :- (Exists (fn [vc :- RV]
            (And (EvalE chkf dec encTy cap rho ce vc)
                 (Erel chkf dec encTy cap (usk Exp.tSyn) vc
                   (denU chkf dec encTy cap D c Exp.tSyn eta))))),
   ihl :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp Exp.tLbl D))
                    (List.cons U U.uw us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tle w)
                       (Erel chkf dec encTy cap (usk (leafTy P)) w
                         (denU chkf dec encTy cap
                           (List.cons Exp Exp.tLbl D) tl (leafTy P) eta2)))))))),
   ihn :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (nodeCtx P D)) (nodeUs us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tne w)
                       (Erel chkf dec encTy cap (usk (nodeTy P)) w
                         (denU chkf dec encTy cap (nodeCtx P D) tn
                           (nodeTy P) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy cap rho (Exp.recS P tle tne ce) v)
         (Erel chkf dec encTy cap (usk P) v
           (denU chkf dec encTy cap D (Exp.recS P tl tn c) P eta)))))
  (rw [(denU_recS chkf dec encTy cap D P tl tn c eta)])
  (refine' (exT RV _ _ ihc _)) (intro vc hc)
  (have hec (EvalE chkf dec encTy cap rho ce vc) (And.left hc))
  (have hrc (Eq RV vc (RV.code (denU chkf dec encTy cap D c Exp.tSyn eta)))
    (And.right hc))
  (have hek (EvalE chkf dec encTy cap rho ce
              (RV.code (denU chkf dec encTy cap D c Exp.tSyn eta)))
    (evalE_cast chkf dec encTy cap rho ce vc
      (RV.code (denU chkf dec encTy cap D c Exp.tSyn eta)) hec hrc))
  (refine' (exT RV _ _
    (adeqE_recs chkf dec encTy cap D P tl tn tle tne us rho eta hr
      (adeqE_leaf_at chkf dec encTy cap D P tl tle us ihl)
      (adeqE_node_at chkf dec encTy cap D P tn tne us ihn)
      (denU chkf dec encTy cap D c Exp.tSyn eta)) _))
  (intro v hv)
  (have hi (EvE chkf dec encTy cap
             (EvSrc.recs rho tle tne (denU chkf dec encTy cap D c Exp.tSyn eta)) v)
    (And.left hv))
  (have hrel (Erel chkf dec encTy cap (usk P) v
               (recsDen chkf dec encTy cap D P tl tn eta
                 (denU chkf dec encTy cap D c Exp.tSyn eta)))
    (And.right hv))
  (constructor) (exact v)
  (constructor)
  (exact (EvE.eRecS chkf dec encTy cap rho P tle tne ce
           (denU chkf dec encTy cap D c Exp.tSyn eta) v hek hi))
  (exact hrel))

;; The conclusion of recS is P[c], not P.  A code term has skeleton Unit,
;; so the two carriers agree and adeqE_recS_at transports.
(thm adeqE_recS
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), P :- Exp,
   tl :- Exp, tn :- Exp, c :- Exp,
   tle :- Exp, tne :- Exp, ce :- Exp,
   us :- (List U),
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   hub :- (Eq USk (usk c) (USk.base Sk.unit)),
   hsc :- (Eq Sk (skel c) Sk.unit),
   hr :- (envE chkf dec encTy cap (uskCtx D) us rho eta),
   ihc :- (Exists (fn [vc :- RV]
            (And (EvalE chkf dec encTy cap rho ce vc)
                 (Erel chkf dec encTy cap (usk Exp.tSyn) vc
                   (denU chkf dec encTy cap D c Exp.tSyn eta))))),
   ihl :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (List.cons Exp Exp.tLbl D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (List.cons Exp Exp.tLbl D))
                    (List.cons U U.uw us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tle w)
                       (Erel chkf dec encTy cap (usk (leafTy P)) w
                         (denU chkf dec encTy cap
                           (List.cons Exp Exp.tLbl D) tl (leafTy P) eta2)))))))),
   ihn :- (forall [rho2 (List RV)]
            (forall [eta2 (HEnv (usks (uskCtx (nodeCtx P D))))]
              (=> (envE chkf dec encTy cap
                    (uskCtx (nodeCtx P D)) (nodeUs us) rho2 eta2)
                (Exists (fn [w :- RV]
                  (And (EvalE chkf dec encTy cap rho2 tne w)
                       (Erel chkf dec encTy cap (usk (nodeTy P)) w
                         (denU chkf dec encTy cap (nodeCtx P D) tn
                           (nodeTy P) eta2))))))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy cap rho (Exp.recS P tle tne ce) v)
         (Erel chkf dec encTy cap (usk (subst1 c P)) v
           (denU chkf dec encTy cap D (Exp.recS P tl tn c) (subst1 c P) eta)))))
  (exact (adeqE_retarget chkf dec encTy cap D
           (Exp.recS P tl tn c) (Exp.recS P tle tne ce)
           P (subst1 c P) rho eta
           (Eq.symm (usk_subst1 c P hub))
           (Eq.symm (skel_subst1 c P hsc))
           (adeqE_recS_at chkf dec encTy cap D P tl tn c tle tne ce us
             rho eta hr ihc ihl ihn))))

;; --- itR (Theorem 4′) -------------------------------------------------------
;;
;; g is applied to the label; h to the token, the label, and the two
;; recursive results.  gTy and hTy bind the motive under lifts, so the
;; carriers are cast back to usk X.  itrDen is that Code.rec.  denU_itR
;; equates it with ⟦itR⟧, and adeqE_itR is the judgment.

(kdef itrDen
  (forall [X Exp]
    (forall [fg (=> Nat (Car (uskSk (usk (lift 1 0 X)))))]
      (forall [fh (=> Unit (=> Nat
                    (=> (Car (uskSk (usk (lift 2 0 X))))
                      (=> (Car (uskSk (usk (lift 3 0 X))))
                        (Car (uskSk (usk (lift 4 0 X))))))))]
        (=> Code (Car (uskSk (usk X)))))))
  (fn [X :- Exp,
       fg :- (=> Nat (Car (uskSk (usk (lift 1 0 X))))),
       fh :- (=> Unit (=> Nat
               (=> (Car (uskSk (usk (lift 2 0 X))))
                 (=> (Car (uskSk (usk (lift 3 0 X))))
                   (Car (uskSk (usk (lift 4 0 X)))))))),
       c :- Code]
    (Code.rec$1 (fn [_ :- Code] (Car (uskSk (usk X))))
      (fn [j :- Nat]
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
          (fg j)))
      (fn [j :- Nat, a :- Code, b :- Code,
           ya :- (Car (uskSk (usk X))), yb :- (Car (uskSk (usk X)))]
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0))
          (((fh Unit.unit) j)
            (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                     (Eq.symm (usk_lift X 2 0))) ya)
            (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                     (Eq.symm (usk_lift X 3 0))) yb))))
      c)))

;; E at ◇ is equality with the token.  The carrier is unit and is ignored.
(thm erel_dia_rfl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat]
  (Erel chkf dec encTy cap (usk Exp.tDia) RV.token Unit.unit)
  (rfl))

;; itR, the leaf (Theorem 4′).  g is applied to the label.  The codomain
;; of gTy is lift 1 0 X, and that usage skeleton is usk X.
(thm adeqE_itr_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, X :- Exp, l :- Nat,
   vg :- RV, vh :- RV,
   fg :- (=> Nat (Car (uskSk (usk (lift 1 0 X))))),
   fh :- (=> Unit (=> Nat
           (=> (Car (uskSk (usk (lift 2 0 X))))
             (=> (Car (uskSk (usk (lift 3 0 X))))
               (Car (uskSk (usk (lift 4 0 X)))))))),
   hg :- (Erel chkf dec encTy cap (usk (gTy X)) vg fg)]
  (Exists (fn [w :- RV]
    (And (EvE chkf dec encTy cap (EvSrc.itr vg vh (Code.sl l)) w)
         (Erel chkf dec encTy cap (usk X) w
           (Code.rec$1 (fn [_ :- Code] (Car (uskSk (usk X))))
             (fn [j :- Nat]
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
                 (fg j)))
             (fn [j :- Nat, a :- Code, b :- Code,
                  ya :- (Car (uskSk (usk X))), yb :- (Car (uskSk (usk X)))]
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0))
                 (((fh Unit.unit) j)
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                            (Eq.symm (usk_lift X 2 0))) ya)
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                            (Eq.symm (usk_lift X 3 0))) yb))))
             (Code.sl l))))))
  (refine' (exT RV _ _ (hg (RV.lbl l) l (erel_lbl_rfl chkf dec encTy cap l)) _))
  (intro w hw)
  (have hap (EvE chkf dec encTy cap (EvSrc.ap vg (RV.lbl l)) w) (And.left hw))
  (have h0 (Erel chkf dec encTy cap (usk (lift 1 0 X)) w (fg l)) (And.right hw))
  (have hrel (Erel chkf dec encTy cap (usk X) w
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
                 (fg l)))
    (erel_at chkf dec encTy cap (usk (lift 1 0 X)) (usk X) w (fg l) h0
      (usk_lift X 1 0)))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eItL chkf dec encTy cap vg vh l w hap))
  (exact hrel))

;; itR, the node (Theorem 4′).  h is applied to the token, the label, and
;; the two recursive results.  The results are E-related at usk X and cast
;; into the lifted binders of hTy; the codomain lift 4 0 X comes back.
(thm adeqE_itr_node
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, X :- Exp, l :- Nat, a :- Code, b :- Code,
   vg :- RV, vh :- RV,
   fg :- (=> Nat (Car (uskSk (usk (lift 1 0 X))))),
   fh :- (=> Unit (=> Nat
           (=> (Car (uskSk (usk (lift 2 0 X))))
             (=> (Car (uskSk (usk (lift 3 0 X))))
               (Car (uskSk (usk (lift 4 0 X)))))))),
   hh :- (Erel chkf dec encTy cap (usk (hTy X)) vh fh),
   iha :- (Exists (fn [ya :- RV]
            (And (EvE chkf dec encTy cap (EvSrc.itr vg vh a) ya)
                 (Erel chkf dec encTy cap (usk X) ya (itrDen X fg fh a))))),
   ihb :- (Exists (fn [yb :- RV]
            (And (EvE chkf dec encTy cap (EvSrc.itr vg vh b) yb)
                 (Erel chkf dec encTy cap (usk X) yb (itrDen X fg fh b)))))]
  (Exists (fn [w :- RV]
    (And (EvE chkf dec encTy cap (EvSrc.itr vg vh (Code.sn l a b)) w)
         (Erel chkf dec encTy cap (usk X) w (itrDen X fg fh (Code.sn l a b))))))
  (refine' (exT RV _ _ iha _)) (intro ya ha)
  (have hea (EvE chkf dec encTy cap (EvSrc.itr vg vh a) ya) (And.left ha))
  (have hra (Erel chkf dec encTy cap (usk X) ya (itrDen X fg fh a)) (And.right ha))
  (refine' (exT RV _ _ ihb _)) (intro yb hb)
  (have heb (EvE chkf dec encTy cap (EvSrc.itr vg vh b) yb) (And.left hb))
  (have hrb (Erel chkf dec encTy cap (usk X) yb (itrDen X fg fh b)) (And.right hb))
  (refine' (exT RV _ _ (hh RV.token Unit.unit (erel_dia_rfl chkf dec encTy cap)) _))
  (intro v1 h1)
  (have he1 (EvE chkf dec encTy cap (EvSrc.ap vh RV.token) v1) (And.left h1))
  (have hr1 (Erel chkf dec encTy cap
              (usk (Exp.tPi U.uw Exp.tLbl
                     (Exp.tPi U.u1 (lift 2 0 X)
                       (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))))
              v1 (fh Unit.unit))
    (And.right h1))
  (refine' (exT RV _ _ (hr1 (RV.lbl l) l (erel_lbl_rfl chkf dec encTy cap l)) _))
  (intro v2 h2)
  (have he2 (EvE chkf dec encTy cap (EvSrc.ap v1 (RV.lbl l)) v2) (And.left h2))
  (have hr2 (Erel chkf dec encTy cap
              (usk (Exp.tPi U.u1 (lift 2 0 X)
                     (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))))
              v2 ((fh Unit.unit) l))
    (And.right h2))
  (have hya (Erel chkf dec encTy cap (usk (lift 2 0 X)) ya
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                       (Eq.symm (usk_lift X 2 0)))
                (itrDen X fg fh a)))
    (erel_at chkf dec encTy cap (usk X) (usk (lift 2 0 X)) ya
      (itrDen X fg fh a) hra (Eq.symm (usk_lift X 2 0))))
  (refine' (exT RV _ _ (hr2 ya
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                       (Eq.symm (usk_lift X 2 0)))
                (itrDen X fg fh a))
              hya) _))
  (intro v3 h3)
  (have he3 (EvE chkf dec encTy cap (EvSrc.ap v2 ya) v3) (And.left h3))
  (have hr3 (Erel chkf dec encTy cap
              (usk (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))
              v3 (((fh Unit.unit) l)
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                            (Eq.symm (usk_lift X 2 0)))
                     (itrDen X fg fh a))))
    (And.right h3))
  (have hyb (Erel chkf dec encTy cap (usk (lift 3 0 X)) yb
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                       (Eq.symm (usk_lift X 3 0)))
                (itrDen X fg fh b)))
    (erel_at chkf dec encTy cap (usk X) (usk (lift 3 0 X)) yb
      (itrDen X fg fh b) hrb (Eq.symm (usk_lift X 3 0))))
  (refine' (exT RV _ _ (hr3 yb
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                       (Eq.symm (usk_lift X 3 0)))
                (itrDen X fg fh b))
              hyb) _))
  (intro w hw)
  (have he4 (EvE chkf dec encTy cap (EvSrc.ap v3 yb) w) (And.left hw))
  (have h0 (Erel chkf dec encTy cap (usk (lift 4 0 X)) w
             (((fh Unit.unit) l)
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                        (Eq.symm (usk_lift X 2 0)))
                 (itrDen X fg fh a))
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                        (Eq.symm (usk_lift X 3 0)))
                 (itrDen X fg fh b))))
    (And.right hw))
  (have hrel (Erel chkf dec encTy cap (usk X) w
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0))
                 (((fh Unit.unit) l)
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                            (Eq.symm (usk_lift X 2 0)))
                     (itrDen X fg fh a))
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                            (Eq.symm (usk_lift X 3 0)))
                     (itrDen X fg fh b)))))
    (erel_at chkf dec encTy cap (usk (lift 4 0 X)) (usk X) w
      (((fh Unit.unit) l)
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                 (Eq.symm (usk_lift X 2 0)))
          (itrDen X fg fh a))
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                 (Eq.symm (usk_lift X 3 0)))
          (itrDen X fg fh b)))
      h0 (usk_lift X 4 0)))
  (constructor) (exact w)
  (constructor)
  (exact (EvE.eItN chkf dec encTy cap vg vh l a b ya yb v1 v2 v3 w
           hea heb he1 he2 he3 he4))
  (exact hrel))

;; itR's recursion (Theorem 4′).  Induction on the certificate.
(thm adeqE_itr
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, X :- Exp,
   vg :- RV, vh :- RV,
   fg :- (=> Nat (Car (uskSk (usk (lift 1 0 X))))),
   fh :- (=> Unit (=> Nat
           (=> (Car (uskSk (usk (lift 2 0 X))))
             (=> (Car (uskSk (usk (lift 3 0 X))))
               (Car (uskSk (usk (lift 4 0 X)))))))),
   hg :- (Erel chkf dec encTy cap (usk (gTy X)) vg fg),
   hh :- (Erel chkf dec encTy cap (usk (hTy X)) vh fh)]
  (forall [c Code]
    (Exists (fn [w :- RV]
      (And (EvE chkf dec encTy cap (EvSrc.itr vg vh c) w)
           (Erel chkf dec encTy cap (usk X) w (itrDen X fg fh c))))))
  (intro c) (induction c)
  (exact (adeqE_itr_leaf chkf dec encTy cap X l vg vh fg fh hg))
  (exact (adeqE_itr_node chkf dec encTy cap X l a b vg vh fg fh hh ih_a ih_b)))

;; --- the outer itR judgment (Theorem 4′) ------------------------------------
;;
;; ⟦itR⟧ is a Code.rec whose leaf applies ⟦g⟧ to the label and whose node
;; applies ⟦h⟧ to the token, the label, and the two recursive results.
;; itrDen is the same recursor at the usage-skeleton carriers.  The casts
;; stay outside Code.rec (cast_code).  adeqE_itR evaluates g, h, and the
;; certificate, then adeqE_itr folds them.

(thm den_itR_open
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   prev :- DenFn, n :- Nat,
   X :- Exp, g :- Exp, h :- Exp, r :- Exp,
   iX :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   ig :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   ih :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   ir :- (forall [G (List Sk)] (forall [sk Sk] (=> (HEnv G) (Car sk)))),
   G :- (List Sk), sk :- Sk, en :- (HEnv G)]
  (Eq (Car sk)
    (den_itR chkf dec encTy prev n X g h r iX ig ih ir G sk en)
    (Code.rec$1 (fn [_ :- Code] (Car sk))
      (fn [l :- Nat] ((ig G (Sk.arr Sk.lbl sk) en) l))
      (fn [l :- Nat, a :- Code, b :- Code, ya :- (Car sk), yb :- (Car sk)]
        ((ih G (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr sk (Sk.arr sk sk)))) en)
          Unit.unit l ya yb))
      (ir G Sk.cert en)))
  (rfl))

;; denU of g, applied to a label, is ⟦g⟧ at the arrow of the lifted motive.
;; The label cast is the identity.
(thm denU_itr_app
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, g :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (uskSk (usk (lift 1 0 X))))
    ((denU chkf dec encTy n D g (gTy X) eta) j)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 1 0 X))))
      ((den chkf dec encTy n g (skels D)
          (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta))
       j)))
  (exact (Eq.trans
    (denU_piw_at chkf dec encTy n D g Exp.tLbl (lift 1 0 X) j eta)
    (congrArg
      (fn [a :- Nat]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 1 0 X))))
          ((den chkf dec encTy n g (skels D)
              (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta))
           a)))
      (Eq.symm (cast_lbl_id j))))))

;; ⟦g⟧ at the lifted arrow, applied to the label, is ⟦g⟧ at the motive's arrow.
(thm denU_itr_arr2
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, g :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (skel X))
    ((den chkf dec encTy n g (skels D)
        (Sk.arr Sk.lbl (skel X)) (henv_of_usk D eta)) j)
    (Eq.mp (congrArg Car (skel_lift X 1 0))
      ((den chkf dec encTy n g (skels D)
          (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)) j)))
  (have eArr (Eq Sk (Sk.arr Sk.lbl (skel (lift 1 0 X))) (Sk.arr Sk.lbl (skel X)))
    (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) (skel_lift X 1 0)))
  (have hd (Eq (Car (Sk.arr Sk.lbl (skel X)))
              (Eq.mp (congrArg Car eArr)
                (den chkf dec encTy n g (skels D)
                  (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)))
              (den chkf dec encTy n g (skels D)
                (Sk.arr Sk.lbl (skel X)) (henv_of_usk D eta)))
    (den_at_eq chkf dec encTy n g (skels D)
      (Sk.arr Sk.lbl (skel (lift 1 0 X))) (Sk.arr Sk.lbl (skel X)) eArr
      (henv_of_usk D eta)))
  (have ha (Eq (Car (skel X))
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (lift 1 0 X))))
                      (Eq.refl$1 Sk Sk.lbl))
                    (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y))
                      (skel_lift X 1 0))))
                 (den chkf dec encTy n g (skels D)
                   (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)))
               j)
              (Eq.mp (congrArg Car (skel_lift X 1 0))
                ((den chkf dec encTy n g (skels D)
                    (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)) j)))
    (cast_app Sk.lbl (skel (lift 1 0 X)) Sk.lbl (skel X)
      (Eq.refl$1 Sk Sk.lbl) (skel_lift X 1 0)
      (den chkf dec encTy n g (skels D)
        (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta))
      j))
  (exact (Eq.trans
    (Eq.symm (congrArg
      (fn [f :- (Car (Sk.arr Sk.lbl (skel X)))] (f j))
      hd))
    ha)))

;; Symmetry of a symmetry is the equation.  cast_square writes the double
;; symmetry, and the lift equation is the single one.
(thm eq_symm_symm
  [s1 :- Sk, s2 :- Sk, e :- (Eq Sk s1 s2), a :- (Car s1)]
  (Eq (Car s2)
    (Eq.mp (congrArg Car (Eq.symm (Eq.symm e))) a)
    (Eq.mp (congrArg Car e) a))
  (cases e)
  (rfl))

;; The leaf of itrDen is the cast of ⟦g⟧ at the motive's arrow (Theorem 4′).
(thm denU_itr_leaf
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, g :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (uskSk (usk X)))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
      ((denU chkf dec encTy n D g (gTy X) eta) j))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
      ((den chkf dec encTy n g (skels D)
          (Sk.arr Sk.lbl (skel X)) (henv_of_usk D eta)) j)))
  (have happ (Eq (Car (uskSk (usk (lift 1 0 X))))
                ((denU chkf dec encTy n D g (gTy X) eta) j)
                (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 1 0 X))))
                  ((den chkf dec encTy n g (skels D)
                      (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta))
                   j)))
    (denU_itr_app chkf dec encTy n D X g eta j))
  (have hL (Eq (Car (uskSk (usk X)))
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
                ((denU chkf dec encTy n D g (gTy X) eta) j))
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
                (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 1 0 X))))
                  ((den chkf dec encTy n g (skels D)
                      (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta))
                   j))))
    (congrArg
      (fn [v :- (Car (uskSk (usk (lift 1 0 X))))]
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0)) v))
      happ))
  (rw [hL])
  (exact (Eq.trans
    (cast_usk_path (usk X) (usk (lift 1 0 X)) (Eq.symm (usk_lift X 1 0))
      (skel (lift 1 0 X)) (skel X)
      (usk_skel (lift 1 0 X)) (usk_skel X) (Eq.symm (skel_lift X 1 0))
      ((den chkf dec encTy n g (skels D)
          (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)) j))
    (Eq.trans
      (cast_square (uskSk (usk X)) (uskSk (usk (lift 1 0 X)))
        (skel (lift 1 0 X)) (skel X)
        (usk_skel (lift 1 0 X)) (Eq.symm (skel_lift X 1 0)) (usk_skel X)
        ((den chkf dec encTy n g (skels D)
            (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)) j))
      (congrArg
        (fn [w :- (Car (skel X))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) w))
        (Eq.trans
          (eq_symm_symm (skel (lift 1 0 X)) (skel X) (skel_lift X 1 0)
            ((den chkf dec encTy n g (skels D)
                (Sk.arr Sk.lbl (skel (lift 1 0 X))) (henv_of_usk D eta)) j))
          (Eq.symm (denU_itr_arr2 chkf dec encTy n D X g eta j))))))))

;; A carrier at the lifted skeleton, cast to the lift's usage skeleton and
;; then to the motive, is the carrier cast along the lift and then back by
;; usk_skel.  The same path, read backwards, sends a motive value into the lift.
(thm cast_lift
  [k :- Nat, X :- Exp, d :- (Car (skel (lift k 0 X)))]
  (Eq (Car (uskSk (usk X)))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X k 0))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift k 0 X)))) d))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
      (Eq.mp (congrArg Car (skel_lift X k 0)) d)))
  (exact (Eq.trans
    (cast_usk_path (usk X) (usk (lift k 0 X)) (Eq.symm (usk_lift X k 0))
      (skel (lift k 0 X)) (skel X)
      (usk_skel (lift k 0 X)) (usk_skel X) (Eq.symm (skel_lift X k 0))
      d)
    (Eq.trans
      (cast_square (uskSk (usk X)) (uskSk (usk (lift k 0 X)))
        (skel (lift k 0 X)) (skel X)
        (usk_skel (lift k 0 X)) (Eq.symm (skel_lift X k 0)) (usk_skel X)
        d)
      (congrArg
        (fn [w :- (Car (skel X))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) w))
        (eq_symm_symm (skel (lift k 0 X)) (skel X) (skel_lift X k 0) d))))))

(thm cast_back_u
  [u1 :- USk, u2 :- USk, eu :- (Eq USk u1 u2), a :- (Car (uskSk u2))]
  (Eq (Car (uskSk u2))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu)
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu)) a))
    a)
  (cases eu) (rfl))

(thm cast_back_u2
  [u1 :- USk, u2 :- USk, eu :- (Eq USk u1 u2), a :- (Car (uskSk u1))]
  (Eq (Car (uskSk u1))
    (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) (Eq.symm eu))
      (Eq.mp (congrArg (fn [w :- USk] (Car (uskSk w))) eu) a))
    a)
  (cases eu) (rfl))

(thm cast_arg_probe
  [k :- Nat, X :- Exp, ya2 :- (Car (skel X))]
  (Eq (Car (skel X))
    (Eq.mp (congrArg Car (skel_lift X k 0))
      (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))
    ya2)
  (exact (cast_back (skel (lift k 0 X)) (skel X) (skel_lift X k 0) ya2)))

(thm cast_arg_hL
  [k :- Nat, X :- Exp, ya2 :- (Car (skel X))]
  (Eq (Car (uskSk (usk X)))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X k 0))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift k 0 X))))
        (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
      (Eq.mp (congrArg Car (skel_lift X k 0))
        (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))))
  (exact (cast_lift k X
           (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))))

(thm cast_arg_h1b
  [k :- Nat, X :- Exp, ya2 :- (Car (skel X))]
  (Eq (Car (uskSk (usk X)))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X k 0))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift k 0 X))))
        (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2))
  (exact (Eq.trans
    (cast_arg_hL k X ya2)
    (congrArg
      (fn [w :- (Car (skel X))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) w))
      (cast_arg_probe k X ya2)))))

(thm cast_arg_h2b
  [k :- Nat, X :- Exp, ya2 :- (Car (skel X))]
  (Eq (Car (uskSk (usk (lift k 0 X))))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift k 0 X))))
      (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X k 0)))
      (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2)))
  (exact (Eq.trans
    (Eq.symm
      (cast_back_u2 (usk (lift k 0 X)) (usk X) (usk_lift X k 0)
        (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift k 0 X))))
          (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))))
    (congrArg
      (fn [v :- (Car (uskSk (usk X)))]
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X k 0))) v))
      (cast_arg_h1b k X ya2)))))

;; A value at the motive, cast to the lift's usage skeleton and back to the
;; lifted skeleton, is the value cast along the skeleton lift.
(thm cast_arg
  [k :- Nat, X :- Exp, ya2 :- (Car (skel X))]
  (Eq (Car (skel (lift k 0 X)))
    (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2)
    (Eq.mp (congrArg Car (usk_skel (lift k 0 X)))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X k 0)))
        (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2))))
  (exact (Eq.trans
    (Eq.symm
      (cast_back (uskSk (usk (lift k 0 X))) (skel (lift k 0 X))
        (usk_skel (lift k 0 X))
        (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2)))
    (congrArg
      (fn [v :- (Car (uskSk (usk (lift k 0 X))))]
        (Eq.mp (congrArg Car (usk_skel (lift k 0 X))) v))
      (cast_arg_h2b k X ya2)))))

;; usk_skel at Π₁ and Πω is the two component casts, as at Π₀.
(thm usk_skel_pi1 [A :- Exp, B :- Exp]
  (Eq (Eq Sk (uskSk (usk (Exp.tPi U.u1 A B))) (skel (Exp.tPi U.u1 A B)))
    (usk_skel (Exp.tPi U.u1 A B))
    (Eq.trans
      (congrArg (fn [x :- Sk] (Sk.arr x (uskSk (usk B)))) (usk_skel A))
      (congrArg (fn [y :- Sk] (Sk.arr (skel A) y)) (usk_skel B))))
  (rfl))

(thm usk_skel_piw [A :- Exp, B :- Exp]
  (Eq (Eq Sk (uskSk (usk (Exp.tPi U.uw A B))) (skel (Exp.tPi U.uw A B)))
    (usk_skel (Exp.tPi U.uw A B))
    (Eq.trans
      (congrArg (fn [x :- Sk] (Sk.arr x (uskSk (usk B)))) (usk_skel A))
      (congrArg (fn [y :- Sk] (Sk.arr (skel A) y)) (usk_skel B))))
  (rfl))

;; Applying a function cast along usk_skel of a Π is the cast of the application.
(thm app_cast_pi1
  [A :- Exp, B :- Exp,
   f :- (Car (Sk.arr (skel A) (skel B))),
   a :- (Car (uskSk (usk A)))]
  (Eq (Car (uskSk (usk B)))
    ((Eq.mp (congrArg Car (Eq.symm (usk_skel (Exp.tPi U.u1 A B)))) f) a)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (f (Eq.mp (congrArg Car (usk_skel A)) a))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B) f a)))

(thm app_cast_piw
  [A :- Exp, B :- Exp,
   f :- (Car (Sk.arr (skel A) (skel B))),
   a :- (Car (uskSk (usk A)))]
  (Eq (Car (uskSk (usk B)))
    ((Eq.mp (congrArg Car (Eq.symm (usk_skel (Exp.tPi U.uw A B)))) f) a)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel B)))
      (f (Eq.mp (congrArg Car (usk_skel A)) a))))
  (exact (cast_app_pi (uskSk (usk A)) (uskSk (usk B)) (skel A) (skel B)
           (usk_skel A) (usk_skel B) f a)))

;; The carrier of ◇ is unit, so the cast of the token's denotation is the unit.
(thm cast_dia_id [u :- Unit]
  (Eq Unit (Eq.mp (congrArg Car (usk_skel Exp.tDia)) u) u)
  (rfl))
;; The continuations of hTy, so each application of h names its codomain.
(kdef hCont (forall [X Exp] Exp)
  (fn [X :- Exp]
    (Exp.tPi U.uw Exp.tLbl
      (Exp.tPi U.u1 (lift 2 0 X)
        (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))))))

(kdef hPair (forall [X Exp] Exp)
  (fn [X :- Exp]
    (Exp.tPi U.u1 (lift 2 0 X)
      (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))))

(kdef hRight (forall [X Exp] Exp)
  (fn [X :- Exp]
    (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X))))

(thm hTy_cont [X :- Exp]
  (Eq Exp (hTy X) (Exp.tPi U.u1 Exp.tDia (hCont X)))
  (rfl))

(thm hCont_skel [X :- Exp]
  (Eq Sk (skel (hCont X)) (Sk.arr Sk.lbl (skel (hPair X))))
  (rfl))

(thm hPair_skel [X :- Exp]
  (Eq Sk (skel (hPair X))
    (Sk.arr (skel (lift 2 0 X)) (skel (hRight X))))
  (rfl))

(thm hRight_skel [X :- Exp]
  (Eq Sk (skel (hRight X))
    (Sk.arr (skel (lift 3 0 X)) (skel (lift 4 0 X))))
  (rfl))

(thm hCont_pair [X :- Exp]
  (Eq Exp (hCont X) (Exp.tPi U.uw Exp.tLbl (hPair X)))
  (rfl))

(thm hPair_right [X :- Exp]
  (Eq Exp (hPair X) (Exp.tPi U.u1 (lift 2 0 X) (hRight X)))
  (rfl))

(thm hRight_lift [X :- Exp]
  (Eq Exp (hRight X) (Exp.tPi U.u1 (lift 3 0 X) (lift 4 0 X)))
  (rfl))

;; h applied to the token.  The unit cast at ◇ is the identity.
(thm denU_itr_tok
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk (hCont X))))
    ((denU chkf dec encTy n D h (hTy X) eta) Unit.unit)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (hCont X))))
      ((den chkf dec encTy n h (skels D)
          (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
       Unit.unit)))
  (exact (Eq.trans
    (denU_pi1_at chkf dec encTy n D h Exp.tDia (hCont X) Unit.unit eta)
    (congrArg
      (fn [a :- Unit]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel (hCont X))))
          ((den chkf dec encTy n h (skels D)
              (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
           a)))
      (Eq.symm (cast_dia_id Unit.unit))))))

;; h applied to the label.  The label cast is the identity.
(thm denU_itr_lbl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (uskSk (usk (hPair X))))
    (((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (hPair X))))
      (((den chkf dec encTy n h (skels D)
           (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
        Unit.unit)
       j)))
  (have hf (Eq (Car (uskSk (usk (hCont X))))
              ((denU chkf dec encTy n D h (hTy X) eta) Unit.unit)
              (Eq.mp (congrArg Car (Eq.symm (usk_skel (hCont X))))
                ((den chkf dec encTy n h (skels D)
                    (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
                 Unit.unit)))
    (denU_itr_tok chkf dec encTy n D X h eta))
  (have ha (Eq (Car (uskSk (usk (hPair X))))
              ((Eq.mp (congrArg Car (Eq.symm (usk_skel (hCont X))))
                 ((den chkf dec encTy n h (skels D)
                     (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
                  Unit.unit))
               j)
              (Eq.mp (congrArg Car (Eq.symm (usk_skel (hPair X))))
                (((den chkf dec encTy n h (skels D)
                     (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
                  Unit.unit)
                 (Eq.mp (congrArg Car (usk_skel Exp.tLbl)) j))))
    (app_cast_piw Exp.tLbl (hPair X)
      ((den chkf dec encTy n h (skels D)
          (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
       Unit.unit)
      j))
  (exact (Eq.trans
    (congrArg (fn [f :- (Car (uskSk (usk (hCont X))))] (f j)) hf)
    (Eq.trans ha
      (congrArg
        (fn [a :- Nat]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel (hPair X))))
            (((den chkf dec encTy n h (skels D)
                 (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
              Unit.unit)
             a)))
        (cast_lbl_id j))))))

;; h applied to the left recursive result, still at the lift's usage skeleton.
(thm denU_itr_ya
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya1 :- (Car (uskSk (usk X)))]
  (Eq (Car (uskSk (usk (hRight X))))
    ((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (hRight X))))
      ((((den chkf dec encTy n h (skels D)
            (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
         Unit.unit)
        j)
       (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))))
  (have hf (Eq (Car (uskSk (usk (hPair X))))
              (((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
              (Eq.mp (congrArg Car (Eq.symm (usk_skel (hPair X))))
                (((den chkf dec encTy n h (skels D)
                     (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
                  Unit.unit)
                 j)))
    (denU_itr_lbl chkf dec encTy n D X h eta j))
  (exact (Eq.trans
    (congrArg
      (fn [f :- (Car (uskSk (usk (hPair X))))]
        (f (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))
      hf)
    (app_cast_pi1 (lift 2 0 X) (hRight X)
      (((den chkf dec encTy n h (skels D)
           (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
        Unit.unit)
       j)
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))))

;; h applied to the right recursive result.
(thm denU_itr_yb
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya1 :- (Car (uskSk (usk X))),
   yb1 :- (Car (uskSk (usk X)))]
  (Eq (Car (uskSk (usk (lift 4 0 X))))
    (((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
       (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 4 0 X))))
      (((((den chkf dec encTy n h (skels D)
             (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
          Unit.unit)
         j)
        (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
          (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))
       (Eq.mp (congrArg Car (usk_skel (lift 3 0 X)))
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1)))))
  (have hf (Eq (Car (uskSk (usk (hRight X))))
              ((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
                (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
              (Eq.mp (congrArg Car (Eq.symm (usk_skel (hRight X))))
                ((((den chkf dec encTy n h (skels D)
                      (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
                   Unit.unit)
                  j)
                 (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
                   (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))))
    (denU_itr_ya chkf dec encTy n D X h eta j ya1))
  (exact (Eq.trans
    (congrArg
      (fn [f :- (Car (uskSk (usk (hRight X))))]
        (f (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1)))
      hf)
    (app_cast_pi1 (lift 3 0 X) (lift 4 0 X)
      ((((den chkf dec encTy n h (skels D)
            (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
         Unit.unit)
        j)
       (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1)))))

;; ⟦h⟧ after the token and the label.  The two recursive arguments are still open.
(kdef hDen
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat] (forall [D (List Exp)] (forall [X Exp] (forall [h Exp]
          (forall [eta (HEnv (usks (uskCtx D)))] (forall [j Nat]
            (Car (skel (hPair X))))))))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
       eta :- (HEnv (usks (uskCtx D))), j :- Nat]
    (((den chkf dec encTy n h (skels D)
         (Sk.arr Sk.dia (skel (hCont X))) (henv_of_usk D eta))
      Unit.unit)
     j)))

(thm denU_itr_yb2
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya1 :- (Car (uskSk (usk X))),
   yb1 :- (Car (uskSk (usk X)))]
  (Eq (Car (uskSk (usk (lift 4 0 X))))
    (((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
       (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 4 0 X))))
      ((hDen chkf dec encTy n D X h eta j
         (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
           (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))
       (Eq.mp (congrArg Car (usk_skel (lift 3 0 X)))
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1)))))
  (exact (denU_itr_yb chkf dec encTy n D X h eta j ya1 yb1)))

;; A recursive result, related at the motive, is the argument at the lifted skeleton.
(thm arg_at
  [k :- Nat, X :- Exp,
   ya1 :- (Car (uskSk (usk X))),
   ya2 :- (Car (skel X)),
   ha :- (Eq (Car (uskSk (usk X))) ya1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2))]
  (Eq (Car (skel (lift k 0 X)))
    (Eq.mp (congrArg Car (usk_skel (lift k 0 X)))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X k 0))) ya1))
    (Eq.mp (congrArg Car (Eq.symm (skel_lift X k 0))) ya2))
  (exact (Eq.trans
    (congrArg
      (fn [v :- (Car (uskSk (usk X)))]
        (Eq.mp (congrArg Car (usk_skel (lift k 0 X)))
          (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X k 0))) v)))
      ha)
    (Eq.symm (cast_arg k X ya2)))))

(thm denU_itr_args
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya1 :- (Car (uskSk (usk X))), yb1 :- (Car (uskSk (usk X))),
   ya2 :- (Car (skel X)), yb2 :- (Car (skel X)),
   ha :- (Eq (Car (uskSk (usk X))) ya1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2)),
   hb :- (Eq (Car (uskSk (usk X))) yb1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) yb2))]
  (Eq (Car (uskSk (usk (lift 4 0 X))))
    (((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
       (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
      (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 4 0 X))))
      ((hDen chkf dec encTy n D X h eta j
         (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))
       (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2))))
  (exact (Eq.trans
    (denU_itr_yb2 chkf dec encTy n D X h eta j ya1 yb1)
    (congrArg
      (fn [v :- (Car (skel (lift 4 0 X)))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel (lift 4 0 X)))) v))
      (Eq.trans
        (congrArg
          (fn [b :- (Car (skel (lift 3 0 X)))]
            ((hDen chkf dec encTy n D X h eta j
               (Eq.mp (congrArg Car (usk_skel (lift 2 0 X)))
                 (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1)))
             b))
          (arg_at 3 X yb1 yb2 hb))
        (congrArg
          (fn [a :- (Car (skel (lift 2 0 X)))]
            ((hDen chkf dec encTy n D X h eta j a)
             (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2)))
          (arg_at 2 X ya1 ya2 ha)))))))
;; ⟦h⟧'s arrow at the lifted motive and at the motive itself.
(kdef hSkL (forall [X Exp] Sk)
  (fn [X :- Exp] (Sk.arr Sk.dia (skel (hCont X)))))

(kdef hSkX (forall [X Exp] Sk)
  (fn [X :- Exp]
    (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))))

(thm hYb_eq [X :- Exp]
  (Eq Sk (Sk.arr (skel (lift 3 0 X)) (skel (lift 4 0 X)))
        (Sk.arr (skel X) (skel X)))
  (exact (Eq.trans
    (congrArg (fn [x :- Sk] (Sk.arr x (skel (lift 4 0 X)))) (skel_lift X 3 0))
    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (skel_lift X 4 0)))))

(thm hYa_eq [X :- Exp]
  (Eq Sk (Sk.arr (skel (lift 2 0 X)) (skel (hRight X)))
        (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
  (exact (Eq.trans
    (congrArg (fn [x :- Sk] (Sk.arr x (skel (hRight X)))) (skel_lift X 2 0))
    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (hYb_eq X)))))

(thm hLbl_eq [X :- Exp]
  (Eq Sk (Sk.arr Sk.lbl (skel (hPair X)))
        (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
  (exact (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) (hYa_eq X))))

(thm hSk_eq [X :- Exp]
  (Eq Sk (hSkL X) (hSkX X))
  (exact (congrArg (fn [y :- Sk] (Sk.arr Sk.dia y))
    (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y))
      (Eq.trans
        (congrArg (fn [x :- Sk] (Sk.arr x (skel (hRight X)))) (skel_lift X 2 0))
        (congrArg (fn [y :- Sk] (Sk.arr (skel X) y))
          (Eq.trans
            (congrArg (fn [x :- Sk] (Sk.arr x (skel (lift 4 0 X)))) (skel_lift X 3 0))
            (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (skel_lift X 4 0)))))))))

;; Move ⟦h⟧ from the lifted arrow to the motive's arrow, one application at a time.
(thm denU_itr_mv_tok
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
    ((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit)
    (Eq.mp (congrArg Car (hLbl_eq X))
      ((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)) Unit.unit)))
  (have hd (Eq (Car (hSkX X))
              (Eq.mp (congrArg Car (hSk_eq X))
                (den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)))
              (den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)))
    (den_at_eq chkf dec encTy n h (skels D) (hSkL X) (hSkX X) (hSk_eq X)
      (henv_of_usk D eta)))
  (have ha (Eq (Car (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (hCont X))))
                      (Eq.refl$1 Sk Sk.dia))
                    (congrArg (fn [y :- Sk] (Sk.arr Sk.dia y)) (hLbl_eq X))))
                 (den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)))
               Unit.unit)
              (Eq.mp (congrArg Car (hLbl_eq X))
                ((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta))
                 Unit.unit)))
    (cast_app Sk.dia (skel (hCont X)) Sk.dia
      (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
      (Eq.refl$1 Sk Sk.dia) (hLbl_eq X)
      (den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta))
      Unit.unit))
  (exact (Eq.trans
    (Eq.symm (congrArg
      (fn [f :- (Car (hSkX X))] (f Unit.unit))
      hd))
    ha)))

(thm denU_itr_mv_lbl
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat]
  (Eq (Car (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
    (((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit) j)
    (Eq.mp (congrArg Car (hYa_eq X))
      (hDen chkf dec encTy n D X h eta j)))
  (have hd (Eq (Car (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
              ((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit)
              (Eq.mp (congrArg Car (hLbl_eq X))
                ((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)) Unit.unit)))
    (denU_itr_mv_tok chkf dec encTy n D X h eta))
  (have ha (Eq (Car (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk]
                                (Sk.arr x (Sk.arr (skel (lift 2 0 X)) (skel (hRight X)))))
                      (Eq.refl$1 Sk Sk.lbl))
                    (congrArg (fn [y :- Sk] (Sk.arr Sk.lbl y)) (hYa_eq X))))
                 ((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)) Unit.unit))
               j)
              (Eq.mp (congrArg Car (hYa_eq X))
                (((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)) Unit.unit) j)))
    (cast_app Sk.lbl (Sk.arr (skel (lift 2 0 X)) (skel (hRight X))) Sk.lbl
      (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))
      (Eq.refl$1 Sk Sk.lbl) (hYa_eq X)
      ((den chkf dec encTy n h (skels D) (hSkL X) (henv_of_usk D eta)) Unit.unit)
      j))
  (exact (Eq.trans
    (congrArg (fn [f :- (Car (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))] (f j)) hd)
    ha)))

(thm denU_itr_mv_ya
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya2 :- (Car (skel X))]
  (Eq (Car (Sk.arr (skel X) (skel X)))
    ((((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit) j) ya2)
    (Eq.mp (congrArg Car (hYb_eq X))
      ((hDen chkf dec encTy n D X h eta j)
       (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))))
  (have hd (Eq (Car (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
              (((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit) j)
              (Eq.mp (congrArg Car (hYa_eq X))
                (hDen chkf dec encTy n D X h eta j)))
    (denU_itr_mv_lbl chkf dec encTy n D X h eta j))
  (have ha (Eq (Car (Sk.arr (skel X) (skel X)))
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (hRight X)))) (skel_lift X 2 0))
                    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (hYb_eq X))))
                 (hDen chkf dec encTy n D X h eta j))
               (Eq.mp (congrArg Car (skel_lift X 2 0))
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
              (Eq.mp (congrArg Car (hYb_eq X))
                ((hDen chkf dec encTy n D X h eta j)
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))))
    (cast_app (skel (lift 2 0 X)) (skel (hRight X)) (skel X)
      (Sk.arr (skel X) (skel X))
      (skel_lift X 2 0) (hYb_eq X)
      (hDen chkf dec encTy n D X h eta j)
      (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
  (have hb (Eq (Car (Sk.arr (skel X) (skel X)))
              ((Eq.mp (congrArg Car (hYa_eq X))
                 (hDen chkf dec encTy n D X h eta j))
               ya2)
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (hRight X)))) (skel_lift X 2 0))
                    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (hYb_eq X))))
                 (hDen chkf dec encTy n D X h eta j))
               (Eq.mp (congrArg Car (skel_lift X 2 0))
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))))
    (congrArg
      (fn [a :- (Car (skel X))]
        ((Eq.mp (congrArg Car (hYa_eq X))
           (hDen chkf dec encTy n D X h eta j))
         a))
      (Eq.symm (cast_back (skel (lift 2 0 X)) (skel X) (skel_lift X 2 0) ya2))))
  (exact (Eq.trans
    (congrArg
      (fn [f :- (Car (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))] (f ya2))
      hd)
    (Eq.trans hb ha))))

(thm denU_itr_mv_yb
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya2 :- (Car (skel X)), yb2 :- (Car (skel X))]
  (Eq (Car (skel X))
    (((((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit) j) ya2) yb2)
    (Eq.mp (congrArg Car (skel_lift X 4 0))
      ((hDen chkf dec encTy n D X h eta j
         (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))
       (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2))))
  (have hd (Eq (Car (Sk.arr (skel X) (skel X)))
              ((((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta)) Unit.unit) j) ya2)
              (Eq.mp (congrArg Car (hYb_eq X))
                ((hDen chkf dec encTy n D X h eta j)
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))))
    (denU_itr_mv_ya chkf dec encTy n D X h eta j ya2))
  (have ha (Eq (Car (skel X))
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (lift 4 0 X)))) (skel_lift X 3 0))
                    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (skel_lift X 4 0))))
                 ((hDen chkf dec encTy n D X h eta j)
                  (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
               (Eq.mp (congrArg Car (skel_lift X 3 0))
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2)))
              (Eq.mp (congrArg Car (skel_lift X 4 0))
                (((hDen chkf dec encTy n D X h eta j)
                  (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2))))
    (cast_app (skel (lift 3 0 X)) (skel (lift 4 0 X)) (skel X) (skel X)
      (skel_lift X 3 0) (skel_lift X 4 0)
      ((hDen chkf dec encTy n D X h eta j)
       (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))
      (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2)))
  (have hb (Eq (Car (skel X))
              ((Eq.mp (congrArg Car (hYb_eq X))
                 ((hDen chkf dec encTy n D X h eta j)
                  (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
               yb2)
              ((Eq.mp (congrArg Car
                  (Eq.trans
                    (congrArg (fn [x :- Sk] (Sk.arr x (skel (lift 4 0 X)))) (skel_lift X 3 0))
                    (congrArg (fn [y :- Sk] (Sk.arr (skel X) y)) (skel_lift X 4 0))))
                 ((hDen chkf dec encTy n D X h eta j)
                  (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
               (Eq.mp (congrArg Car (skel_lift X 3 0))
                 (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2))))
    (congrArg
      (fn [b :- (Car (skel X))]
        ((Eq.mp (congrArg Car (hYb_eq X))
           ((hDen chkf dec encTy n D X h eta j)
            (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2)))
         b))
      (Eq.symm (cast_back (skel (lift 3 0 X)) (skel X) (skel_lift X 3 0) yb2))))
  (exact (Eq.trans
    (congrArg
      (fn [f :- (Car (Sk.arr (skel X) (skel X)))] (f yb2))
      hd)
    (Eq.trans hb ha))))

;; The node of itrDen is the cast of ⟦h⟧ at the motive's arrow (Theorem 4′).
(thm denU_itr_node
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, h :- Exp,
   eta :- (HEnv (usks (uskCtx D))), j :- Nat,
   ya1 :- (Car (uskSk (usk X))), yb1 :- (Car (uskSk (usk X))),
   ya2 :- (Car (skel X)), yb2 :- (Car (skel X)),
   ha :- (Eq (Car (uskSk (usk X))) ya1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2)),
   hb :- (Eq (Car (uskSk (usk X))) yb1
           (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) yb2))]
  (Eq (Car (uskSk (usk X)))
    (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0))
      (((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
         (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 2 0))) ya1))
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (Eq.symm (usk_lift X 3 0))) yb1)))
    (Eq.mp (congrArg Car (Eq.symm (usk_skel X)))
      (((((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta))
          Unit.unit) j) ya2) yb2)))
  (exact (Eq.trans
    (congrArg
      (fn [v :- (Car (uskSk (usk (lift 4 0 X))))]
        (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0)) v))
      (denU_itr_args chkf dec encTy n D X h eta j ya1 yb1 ya2 yb2 ha hb))
    (Eq.trans
      (cast_lift 4 X
        ((hDen chkf dec encTy n D X h eta j
           (Eq.mp (congrArg Car (Eq.symm (skel_lift X 2 0))) ya2))
         (Eq.mp (congrArg Car (Eq.symm (skel_lift X 3 0))) yb2)))
      (congrArg
        (fn [w :- (Car (skel X))]
          (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) w))
        (Eq.symm (denU_itr_mv_yb chkf dec encTy n D X h eta j ya2 yb2)))))))

;; denU at a certificate is ⟦·⟧ at Sk.cert.
(thm denU_cert_id
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), t :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq Code
    (denU chkf dec encTy n D t Exp.tR eta)
    (den chkf dec encTy n t (skels D) Sk.cert (henv_of_usk D eta)))
  (rfl))

;; denU of itR is itrDen of the denotations (Theorem 4′).  The cast stays
;; outside Code.rec.
(thm denU_itR
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), X :- Exp, g :- Exp, h :- Exp, r :- Exp,
   eta :- (HEnv (usks (uskCtx D)))]
  (Eq (Car (uskSk (usk X)))
    (denU chkf dec encTy n D (Exp.itR X g h r) X eta)
    (itrDen X
      (denU chkf dec encTy n D g (gTy X) eta)
      (denU chkf dec encTy n D h (hTy X) eta)
      (denU chkf dec encTy n D r Exp.tR eta)))
  (exact (Eq.trans
    (congrArg
      (fn [x :- (Car (skel X))]
        (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) x))
      (Eq.trans
        (den_itR_at chkf dec encTy n X g h r (skels D) (skel X)
          (henv_of_usk D eta))
        (den_itR_open chkf dec encTy (denPrev chkf dec encTy n) n
          X g h r
          (den chkf dec encTy n X)
          (den chkf dec encTy n g)
          (den chkf dec encTy n h)
          (den chkf dec encTy n r)
          (skels D) (skel X) (henv_of_usk D eta))))
    (Eq.symm
      (cast_code (uskSk (usk X)) (skel X) (usk_skel X)
        (fn [j :- Nat]
          (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 1 0))
            ((denU chkf dec encTy n D g (gTy X) eta) j)))
        (fn [j :- Nat]
          ((den chkf dec encTy n g (skels D)
              (Sk.arr Sk.lbl (skel X)) (henv_of_usk D eta))
           j))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya :- (Car (uskSk (usk X))), yb :- (Car (uskSk (usk X)))]
          (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u))) (usk_lift X 4 0))
            (((((denU chkf dec encTy n D h (hTy X) eta) Unit.unit) j)
               (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                        (Eq.symm (usk_lift X 2 0))) ya))
              (Eq.mp (congrArg (fn [u :- USk] (Car (uskSk u)))
                       (Eq.symm (usk_lift X 3 0))) yb))))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya :- (Car (skel X)), yb :- (Car (skel X))]
          (((((den chkf dec encTy n h (skels D) (hSkX X) (henv_of_usk D eta))
              Unit.unit) j) ya) yb))
        (den chkf dec encTy n r (skels D) Sk.cert (henv_of_usk D eta))
        (fn [j :- Nat]
          (denU_itr_leaf chkf dec encTy n D X g eta j))
        (fn [j :- Nat, a :- Code, b :- Code,
             ya2 :- (Car (skel X)), yb2 :- (Car (skel X)),
             ya1 :- (Car (uskSk (usk X))), yb1 :- (Car (uskSk (usk X))),
             ha :- (Eq (Car (uskSk (usk X))) ya1
                     (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) ya2)),
             hb :- (Eq (Car (uskSk (usk X))) yb1
                     (Eq.mp (congrArg Car (Eq.symm (usk_skel X))) yb2))]
          (denU_itr_node chkf dec encTy n D X h eta j
            ya1 yb1 ya2 yb2 ha hb)))))))

;; itR (Theorem 4′).  g, h, and the certificate are evaluated, then adeqE_itr
;; folds them the way Code.rec folds the denotation.
(thm adeqE_itR
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   cap :- Nat, D :- (List Exp), X :- Exp,
   g :- Exp, h :- Exp, r :- Exp,
   ge :- Exp, he :- Exp, re :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihg :- (Exists (fn [vg :- RV]
            (And (EvalE chkf dec encTy cap rho ge vg)
                 (Erel chkf dec encTy cap (usk (gTy X)) vg
                   (denU chkf dec encTy cap D g (gTy X) eta))))),
   ihh :- (Exists (fn [vh :- RV]
            (And (EvalE chkf dec encTy cap rho he vh)
                 (Erel chkf dec encTy cap (usk (hTy X)) vh
                   (denU chkf dec encTy cap D h (hTy X) eta))))),
   ihr :- (Exists (fn [vr :- RV]
            (And (EvalE chkf dec encTy cap rho re vr)
                 (Erel chkf dec encTy cap (usk Exp.tR) vr
                   (denU chkf dec encTy cap D r Exp.tR eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy cap rho (Exp.itR X ge he re) v)
         (Erel chkf dec encTy cap (usk X) v
           (denU chkf dec encTy cap D (Exp.itR X g h r) X eta)))))
  (rw [(denU_itR chkf dec encTy cap D X g h r eta)])
  (refine' (exT RV _ _ ihg _)) (intro vg hg0)
  (have heg (EvalE chkf dec encTy cap rho ge vg) (And.left hg0))
  (have hrg (Erel chkf dec encTy cap (usk (gTy X)) vg
              (denU chkf dec encTy cap D g (gTy X) eta))
    (And.right hg0))
  (refine' (exT RV _ _ ihh _)) (intro vh hh0)
  (have heh (EvalE chkf dec encTy cap rho he vh) (And.left hh0))
  (have hrh (Erel chkf dec encTy cap (usk (hTy X)) vh
              (denU chkf dec encTy cap D h (hTy X) eta))
    (And.right hh0))
  (refine' (exT RV _ _ ihr _)) (intro vr hr0)
  (have her0 (EvalE chkf dec encTy cap rho re vr) (And.left hr0))
  (have hrr (Eq RV vr (RV.cert (denU chkf dec encTy cap D r Exp.tR eta)))
    (And.right hr0))
  (have her (EvalE chkf dec encTy cap rho re
              (RV.cert (denU chkf dec encTy cap D r Exp.tR eta)))
    (evalE_cast chkf dec encTy cap rho re vr
      (RV.cert (denU chkf dec encTy cap D r Exp.tR eta)) her0 hrr))
  (refine' (exT RV _ _
    (adeqE_itr chkf dec encTy cap X vg vh
      (denU chkf dec encTy cap D g (gTy X) eta)
      (denU chkf dec encTy cap D h (hTy X) eta)
      hrg hrh
      (denU chkf dec encTy cap D r Exp.tR eta)) _))
  (intro v hv)
  (have hi (EvE chkf dec encTy cap
             (EvSrc.itr vg vh (denU chkf dec encTy cap D r Exp.tR eta)) v)
    (And.left hv))
  (have hrel (Erel chkf dec encTy cap (usk X) v
               (itrDen X
                 (denU chkf dec encTy cap D g (gTy X) eta)
                 (denU chkf dec encTy cap D h (hTy X) eta)
                 (denU chkf dec encTy cap D r Exp.tR eta)))
    (And.right hv))
  (constructor) (exact v)
  (constructor)
  (exact (EvE.eItR chkf dec encTy cap rho X ge he re vg vh
           (denU chkf dec encTy cap D r Exp.tR eta) v heg heh her hi))
  (exact hrel))

;; --- print and chk′ (Theorem 4′) --------------------------------------------
;;
;; print forgets the token: the certificate's code is the Syn value.
;; chk′ agrees because Check is a total function of the two codes.

(thm adeqE_prn
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), r :- Exp, re :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ih :- (Exists (fn [vr :- RV]
           (And (EvalE chkf dec encTy n rho re vr)
                (Erel chkf dec encTy n (usk Exp.tR) vr
                  (denU chkf dec encTy n D r Exp.tR eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.prn re) v)
         (Erel chkf dec encTy n (usk Exp.tSyn) v
           (denU chkf dec encTy n D (Exp.prn r) Exp.tSyn eta)))))
  (rw [(den_prn_at chkf dec encTy n r (skels D) (skel Exp.tSyn) (henv_of_usk D eta))])
  (rw [(coe_self Sk.syn (den chkf dec encTy n r (skels D) Sk.cert (henv_of_usk D eta)))])
  (refine' (exT RV _ _ ih _)) (intro vr hr0)
  (have he (EvalE chkf dec encTy n rho re vr) (And.left hr0))
  (have hr (Eq RV vr (RV.cert (denU chkf dec encTy n D r Exp.tR eta))) (And.right hr0))
  (constructor)
  (exact (RV.code (denU chkf dec encTy n D r Exp.tR eta)))
  (constructor)
  (exact (EvE.ePrn chkf dec encTy n rho re
           (denU chkf dec encTy n D r Exp.tR eta)
           (evalE_cast chkf dec encTy n rho re vr
             (RV.cert (denU chkf dec encTy n D r Exp.tR eta)) he hr)))
  (rfl))

(thm adeqE_chk
  [chkf :- (=> Code Code Bool),
   dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   n :- Nat, D :- (List Exp), c :- Exp, d :- Exp, ce :- Exp, de :- Exp,
   rho :- (List RV), eta :- (HEnv (usks (uskCtx D))),
   ihc :- (Exists (fn [vc :- RV]
            (And (EvalE chkf dec encTy n rho ce vc)
                 (Erel chkf dec encTy n (usk Exp.tSyn) vc
                   (denU chkf dec encTy n D c Exp.tSyn eta))))),
   ihd :- (Exists (fn [vd :- RV]
            (And (EvalE chkf dec encTy n rho de vd)
                 (Erel chkf dec encTy n (usk Exp.tSyn) vd
                   (denU chkf dec encTy n D d Exp.tSyn eta)))))]
  (Exists (fn [v :- RV]
    (And (EvalE chkf dec encTy n rho (Exp.chk ce de) v)
         (Erel chkf dec encTy n (usk Exp.tBool) v
           (denU chkf dec encTy n D (Exp.chk c d) Exp.tBool eta)))))
  (rw [(den_chk_at chkf dec encTy n c d (skels D) (skel Exp.tBool) (henv_of_usk D eta))])
  (rw [(coe_self Sk.bool
         (chkf (den chkf dec encTy n c (skels D) Sk.syn (henv_of_usk D eta))
               (den chkf dec encTy n d (skels D) Sk.syn (henv_of_usk D eta))))])
  (refine' (exT RV _ _ ihc _)) (intro vc hc)
  (have hec (EvalE chkf dec encTy n rho ce vc) (And.left hc))
  (have hrc (Eq RV vc (RV.code (denU chkf dec encTy n D c Exp.tSyn eta))) (And.right hc))
  (refine' (exT RV _ _ ihd _)) (intro vd hd)
  (have hed (EvalE chkf dec encTy n rho de vd) (And.left hd))
  (have hrd (Eq RV vd (RV.code (denU chkf dec encTy n D d Exp.tSyn eta))) (And.right hd))
  (constructor)
  (exact (RV.bool (chkf (denU chkf dec encTy n D c Exp.tSyn eta)
                        (denU chkf dec encTy n D d Exp.tSyn eta))))
  (constructor)
  (exact (EvE.eChk chkf dec encTy n rho ce de
           (denU chkf dec encTy n D c Exp.tSyn eta)
           (denU chkf dec encTy n D d Exp.tSyn eta)
           (evalE_cast chkf dec encTy n rho ce vc
             (RV.code (denU chkf dec encTy n D c Exp.tSyn eta)) hec hrc)
           (evalE_cast chkf dec encTy n rho de vd
             (RV.code (denU chkf dec encTy n D d Exp.tSyn eta)) hed hrd)))
  (rfl))
