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
  the second component's type B[x/y] is brought back to B by denU_unsubst."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
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
