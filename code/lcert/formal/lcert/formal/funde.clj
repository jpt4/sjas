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
  plus coe_self are the same rewrites Theorem 4 uses."
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
