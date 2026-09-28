(ns lcert.formal.model
  "F3e — environments, the checker's specification, and the statements of
  §3's theorems (R4-metatheory.md §§3.4–3.9).

  EnvSat: η ⊨ⁿₖ Γ (§3.4).  An entry of usage 1 has its value in Vⱼ with a
  footprint j; the footprints of all entries sum to at most k.  An entry of
  usage ω has its value in V₀.  An entry of usage 0 is unconstrained.

  CheckSpec chkf dec encTy — the trust base of ADR-0006.  The metatheory uses
  Check only through these properties, which the concrete Check has by
  construction (§1.6; proved for it in phase F7):
  - Lemmas 2.6–2.7: if chkf c d, then c decodes (dec) to a derivable,
    closed-typed judgment Θₘ ⊢ t :¹ A with ⌜A⌝ = d and m < nodes(c);
  - the base-type codes used by the rules are the encodings of those types;
  - E5: ⌜A ⊸ 0⌝ = snode arrow₁ ⌜A⌝ ⌜0⌝ for closed A;
  - E1: the encoding is injective on closed types.

  The statements of Lemma 3.6, Theorem 1, Corollary 3.7 and Theorem 3 are
  kernel-checked Prop constants here; their proofs are separate theorems."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]))

;; Every variable index below c: closedness of a type is closedBelow 0.
(a/defn closedF [e :- Exp] (=> Nat Bool)
  (match e
    [tEmpty (fn [c :- Nat] true)]
    [tUnit (fn [c :- Nat] true)]
    [tBool (fn [c :- Nat] true)]
    [tNat (fn [c :- Nat] true)]
    [tLbl (fn [c :- Nat] true)]
    [tSyn (fn [c :- Nat] true)]
    [tDia (fn [c :- Nat] true)]
    [tR (fn [c :- Nat] true)]
    [(tT b) (fn [c :- Nat] ((closedF b) c))]
    [(tPi r A B) (fn [c :- Nat] (Bool.and ((closedF A) c) ((closedF B) (+ c 1))))]
    [(tSig r A B) (fn [c :- Nat] (Bool.and ((closedF A) c) ((closedF B) (+ c 1))))]
    [(var i) (fn [c :- Nat] (Nat.blt i c))]
    [star (fn [c :- Nat] true)]
    [(abort A t) (fn [c :- Nat] (Bool.and ((closedF A) c) ((closedF t) c)))]
    [tt (fn [c :- Nat] true)]
    [ff (fn [c :- Nat] true)]
    [(ite b t e) (fn [c :- Nat] (Bool.and ((closedF b) c) (Bool.and ((closedF t) c) ((closedF e) c))))]
    [(elimB P b t e) (fn [c :- Nat] (Bool.and ((closedF P) (+ c 1)) (Bool.and ((closedF b) c) (Bool.and ((closedF t) c) ((closedF e) c)))))]
    [zero (fn [c :- Nat] true)]
    [(succ n) (fn [c :- Nat] ((closedF n) c))]
    [(recN P z s n) (fn [c :- Nat] (Bool.and ((closedF P) (+ c 1)) (Bool.and ((closedF z) c) (Bool.and ((closedF s) (+ c 2)) ((closedF n) c)))))]
    [(lbl l) (fn [c :- Nat] true)]
    [(caseL P x bs) (fn [c :- Nat] (Bool.and ((closedF P) (+ c 1)) (Bool.and ((closedF x) c) ((closedF bs) c))))]
    [bnil (fn [c :- Nat] true)]
    [(bcons h t) (fn [c :- Nat] (Bool.and ((closedF h) c) ((closedF t) c)))]
    [(sleaf x) (fn [c :- Nat] ((closedF x) c))]
    [(snode x c1 c2) (fn [c :- Nat] (Bool.and ((closedF x) c) (Bool.and ((closedF c1) c) ((closedF c2) c))))]
    [(recS P tl tn x) (fn [c :- Nat] (Bool.and ((closedF P) (+ c 1)) (Bool.and ((closedF tl) (+ c 1)) (Bool.and ((closedF tn) (+ c 5)) ((closedF x) c)))))]
    [(leaf x) (fn [c :- Nat] ((closedF x) c))]
    [(node d x r1 r2) (fn [c :- Nat] (Bool.and ((closedF d) c) (Bool.and ((closedF x) c) (Bool.and ((closedF r1) c) ((closedF r2) c)))))]
    [(itR X g h r) (fn [c :- Nat] (Bool.and ((closedF X) c) (Bool.and ((closedF g) c) (Bool.and ((closedF h) c) ((closedF r) c)))))]
    [(prn r) (fn [c :- Nat] ((closedF r) c))]
    [(lam r A t) (fn [c :- Nat] (Bool.and ((closedF A) c) ((closedF t) (+ c 1))))]
    [(app f u) (fn [c :- Nat] (Bool.and ((closedF f) c) ((closedF u) c)))]
    [(pair S x y) (fn [c :- Nat] (Bool.and ((closedF S) c) (Bool.and ((closedF x) c) ((closedF y) c))))]
    [(letp C p t) (fn [c :- Nat] (Bool.and ((closedF C) c) (Bool.and ((closedF p) c) ((closedF t) (+ c 2)))))]
    [(chk x d) (fn [c :- Nat] (Bool.and ((closedF x) c) ((closedF d) c)))]
    [(h1 r s x e1 e2) (fn [c :- Nat] (Bool.and ((closedF r) c) (Bool.and ((closedF s) c) (Bool.and ((closedF x) c) (Bool.and ((closedF e1) c) ((closedF e2) c))))))]
    [(refl D r e) (fn [c :- Nat] (Bool.and ((closedF D) c) (Bool.and ((closedF r) c) ((closedF e) c))))]
    [(insp X r x t1 t2) (fn [c :- Nat] (Bool.and ((closedF X) c) (Bool.and ((closedF r) c) (Bool.and ((closedF x) c) (Bool.and ((closedF t1) (+ c 2)) ((closedF t2) (+ c 2)))))))]
    [(tBrs P j) (fn [c :- Nat] ((closedF P) (+ c 1)))]))

(a/defn closedTy [A :- Exp] Bool ((closedF A) 0))

;; --- environments (§3.4) -------------------------------------------------------

;; The condition on one entry of usage r whose value, at footprint j, satisfies P.
(kdef EntryOK (=> U Nat (=> Nat Prop) Prop)
  (fn [r :- U, j :- Nat, P :- (=> Nat Prop)]
    (U.rec$1 (fn [_ :- U] Prop) (Eq Nat j 0) (P j) (And (Eq Nat j 0) (P 0)) r)))

;; EnvSat n D us η k: the environment η satisfies the context (D, us) with
;; total footprint at most k, at cap n.
(kdef EnvSat
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (forall [n Nat] (forall [D (List Exp)] (=> (List U) (HEnv (skels D)) Nat Prop))))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), n :- Nat, D :- (List Exp)]
    (List.rec$1$0 Exp (fn [D :- (List Exp)] (=> (List U) (HEnv (skels D)) Nat Prop))
      (fn [us :- (List U), en :- Unit, k :- Nat] True)
      (fn [A :- Exp, rest :- (List Exp), ih :- (=> (List U) (HEnv (skels rest)) Nat Prop)]
        (fn [us :- (List U), en :- (HEnv (skels (List.cons Exp A rest))), k :- Nat]
          (List.rec$1$0 U (fn [_ :- (List U)] Prop) False
            (fn [r :- U, us2 :- (List U), _ :- Prop]
              (Exists (fn [j :- Nat] (Exists (fn [k2 :- Nat]
                (And (Nat.le (+ j k2) k)
                  (And (ih us2 (Prod.snd en) k2)
                       (EntryOK r j (fn [jj :- Nat] (V chkf dec encTy n A (skels rest) (Prod.snd en) jj (skel A) (Prod.fst en)))))))))))
            us)))
      D)))

;; --- the checker's specification (the trust base) ------------------------------

(kdef CheckSpec
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (=> (=> Exp Code) Prop)))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)]
    (And
      ;; Lemmas 2.6–2.7: an accepted code decodes to a derivable closed-typed
      ;; judgment at its declared budget, which is below its node count.
      (forall [c Code] (forall [d Code] (=> (Eq Bool (chkf c d) Bool.true)
        (Exists (fn [m :- Nat] (Exists (fn [t :- Exp] (Exists (fn [A :- Exp]
          (And (Eq (Option (Prod Nat (Prod Exp Exp))) (dec c) (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t A))))
          (And (Rt chkf (thetaD m) (thetaU m) t A)
          (And (Eq Bool (closedTy A) Bool.true)
          (And (Eq Code (encTy A) d)
               (Nat.lt m (cnodes c)))))))))))))))
    (And
      ;; the base-type codes of the rules are the encodings of those types
      (forall [D Exp] (forall [cd Exp] (=> (Eq (Option Exp) (baseCode D) (Option.some Exp cd))
        (Eq (Option Code) (codeOf cd) (Option.some Code (encTy D))))))
    (And
      ;; E5, for closed A
      (forall [A Exp] (=> (Eq Bool (closedTy A) Bool.true)
        (Eq Code (encTy (Exp.tPi U.u1 A Exp.tEmpty)) (Code.sn 25 (encTy A) (Code.sl 15)))))
      ;; E1, on closed types
      (forall [A Exp] (forall [B Exp] (=> (Eq Bool (closedTy A) Bool.true) (Eq Bool (closedTy B) Bool.true)
        (Eq Code (encTy A) (encTy B)) (Eq Exp A B)))))))))

;; --- statements (§3) ---------------------------------------------------------------

;; Lemma 3.6 (soundness at budget n), for well-formed contexts (the paper's
;; presupposition; WFCtx, judgment.clj).
(kdef Lemma_3_6 Prop
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> (CheckSpec chkf dec encTy)
      (forall [n Nat] (forall [D (List Exp)] (forall [us (List U)] (forall [t Exp] (forall [A Exp]
        (=> (WFCtx chkf D) (Rt chkf D us t A)
          (forall [en (HEnv (skels D))] (forall [k Nat]
            (=> (Nat.le k n) (EnvSat chkf dec encTy n D us en k)
              (V chkf dec encTy n A (skels D) en k (skel A) (den chkf dec encTy n t (skels D) (skel A) en))))))))))))))))

;; Theorem 1 (consistency): for no n is Θₙ ⊢ t :¹ 0 derivable.
(kdef Theorem_1 Prop
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> (CheckSpec chkf dec encTy)
      (forall [n Nat] (forall [t Exp] (Not (Rt chkf (thetaD n) (thetaU n) t Exp.tEmpty)))))))))

;; Corollary 3.7: Check accepts no refutation and no contradictory pair.
(kdef Corollary_3_7 Prop
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))] (forall [encTy (=> Exp Code)]
    (=> (CheckSpec chkf dec encTy)
      (And (forall [c Code] (Not (Eq Bool (chkf c (encTy Exp.tEmpty)) Bool.true)))
           (forall [c1 Code] (forall [c2 Code] (forall [d Code]
             (Not (And (Eq Bool (chkf c1 d) Bool.true) (Eq Bool (chkf c2 (Code.sn 25 d (Code.sl 15))) Bool.true))))))))))))

;; --- Theorem 2 (self-justification): closed inhabitants of H° and H₁° ----------

;; H° = Π(r :₁ R). T(chk′ (print r) c⊥) ⊸ 0.
(a/defn Hcirc [] Exp (Exp.tPi U.u1 Exp.tR (Exp.tPi U.u1 (chkT (Exp.var 0) (cbot)) Exp.tEmpty)))
(a/defn Hterm [] Exp (Exp.lam U.u1 Exp.tR (Exp.lam U.u1 (chkT (Exp.var 0) (cbot)) (Exp.refl Exp.tEmpty (Exp.var 1) (Exp.var 0)))))

;; ⊢ λ(r :₁ R). λ(e :₁ T(chk′ (print r) c⊥)). H r e :¹ H°, at budget 0.
(thm Theorem_2_H [chkf :- (=> Code Code Bool)]
  (Rt chkf (List.nil Exp) (List.nil U) (Hterm) (Hcirc))
  (exact
    (Rt.rLam chkf (List.nil Exp) (List.nil U) U.u1 Exp.tR
       (Exp.lam U.u1 (chkT (Exp.var 0) (cbot)) (Exp.refl Exp.tEmpty (Exp.var 1) (Exp.var 0)))
       (Exp.tPi U.u1 (chkT (Exp.var 0) (cbot)) Exp.tEmpty)
       (Tl.fBase chkf (List.nil Exp) Exp.tR rfl)
       (Rt.rLam chkf (consE Exp.tR (List.nil Exp)) (consU U.u1 (List.nil U)) U.u1 (chkT (Exp.var 0) (cbot))
          (Exp.refl Exp.tEmpty (Exp.var 1) (Exp.var 0)) Exp.tEmpty
          (Tl.fT chkf (consE Exp.tR (List.nil Exp)) (Exp.chk (Exp.prn (Exp.var 0)) (cbot))
             (Tl.zChk chkf (consE Exp.tR (List.nil Exp)) (Exp.prn (Exp.var 0)) (cbot)
                (Tl.zPrn chkf (consE Exp.tR (List.nil Exp)) (Exp.var 0) (Tl.zVar chkf (consE Exp.tR (List.nil Exp)) 0 Exp.tR rfl))
                (Tl.zSleaf chkf (consE Exp.tR (List.nil Exp)) (Exp.lbl 15)
                   (Tl.zConst chkf (consE Exp.tR (List.nil Exp)) (Exp.lbl 15) Exp.tLbl rfl))))
          (Rt.rRefl chkf (consE (chkT (Exp.var 0) (cbot)) (consE Exp.tR (List.nil Exp)))
             (consU U.u0 (consU U.u1 (List.nil U))) (consU U.u1 (consU U.u0 (List.nil U)))
             Exp.tEmpty (cLeaf 15) (Exp.var 1) (Exp.var 0) rfl
             (Rt.rVar chkf (consE (chkT (Exp.var 0) (cbot)) (consE Exp.tR (List.nil Exp)))
                (consU U.u0 (consU U.u1 (List.nil U))) 1 Exp.tR U.u1 rfl rfl rfl rfl)
             (Rt.rVar chkf (consE (chkT (Exp.var 0) (cbot)) (consE Exp.tR (List.nil Exp)))
                (consU U.u1 (consU U.u0 (List.nil U))) 0 (chkT (Exp.var 0) (cbot)) U.u1 rfl rfl rfl rfl))))))
