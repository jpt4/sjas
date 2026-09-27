(ns lcert.formal.subst
  "F3f — substitution into the model (R4-metatheory.md Lemmas 3.1 and 3.3).

  coe s s is the identity in both directions (coe_self, coe_rev): the
  denotation's variable clause is lookup followed by coe, and a typed
  variable's skeleton already matches, so the coercion disappears.

  envOf n σ Gp G η is the environment Lemma 3.1 reads the source term in: the
  value of variable i is ⟦σ i⟧ⁿη.  Reflect's recursive call does not use it,
  since it runs a closed decoded program in tokenEnv.

  Written by the Grok agent (review F3s, 2026-09-27).  That run also found
  that Lemmas 3.1 and 3.3 failed for skeleton typing as it then stood, which
  admitted reflect at an open type; SkJ now requires a base data type, as the
  rule table does, and the counterexample no longer applies."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]))

;; ---------------------------------------------------------------------------
;; coe s s = id, both directions (carrier.clj's coe / coePair).
;; One induction: the arrow case needs the domain's reverse map and the
;; codomain's forward map, and the product case needs both components.
;; ---------------------------------------------------------------------------

(thm coe_both [s :- Sk]
  (And (forall [v (Car s)] (= (coe s s v) v))
       (forall [v (Car s)] (= ((Prod.snd (coePair s s)) v) v)))
  ;; Bases are the identity pair, so both conjuncts are rfl. all_goals prepends
  ;; each constructor's goals, and what remains — product forward, product
  ;; reverse, arrow forward, arrow reverse — is closed by congruence with the
  ;; two induction hypotheses. (rewrite would park the rewritten goal at the
  ;; end of the list, so the next tactic would see a different skeleton.)
  (induction s)
  (all_goals (constructor))
  (all_goals (intro v))
  (all_goals (try (rfl)))
  (cases v)
  (exact (Eq.trans
           (congrArg (fn [x :- (Car s)] (Prod.mk x (coe t t snd))) ((And.left ih_s) fst))
           (congrArg (fn [y :- (Car t)] (Prod.mk fst y)) ((And.left ih_t) snd))))
  (cases v)
  (exact (Eq.trans
           (congrArg (fn [x :- (Car s)] (Prod.mk x ((Prod.snd (coePair t t)) snd))) ((And.right ih_s) fst))
           (congrArg (fn [y :- (Car t)] (Prod.mk fst y)) ((And.right ih_t) snd))))
  (funext w)
  (exact (Eq.trans
           (congrArg (fn [z :- (Car s)] (coe t t (v z))) ((And.right ih_s) w))
           ((And.left ih_t) (v w))))
  (funext w)
  (exact (Eq.trans
           (congrArg (fn [z :- (Car s)] ((Prod.snd (coePair t t)) (v z))) ((And.left ih_s) w))
           ((And.right ih_t) (v w)))))

;; Lemma used by every variable case of denotation: coe s s v = v.
(thm coe_self [s :- Sk, v :- (Car s)] (= (coe s s v) v)
  (exact ((And.left (coe_both s)) v)))

;; The reverse half of coePair s s, needed wherever a domain is precomposed
;; (the arrow clause of coeFrom_arr).
(thm coe_rev [s :- Sk, v :- (Car s)] (= ((Prod.snd (coePair s s)) v) v)
  (exact ((And.right (coe_both s)) v)))

;; ---------------------------------------------------------------------------
;; envOf n σ Gp G η : HEnv Gp.
;; nil ↦ ⋆; (s :: rest) ↦ (⟦σ 0⟧ⁿ_G,s η, envOf n (i ↦ σ (i+1)) rest G η).
;; The list recursor is the structural recursion; σ and η stay in the
;; returned function, as with liftF / substF.
;; ---------------------------------------------------------------------------

(kdef envOf
  (forall [chkf (=> Code Code Bool)]
    (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
      (forall [encTy (=> Exp Code)]
        (forall [n Nat]
          (forall [Gp (List Sk)]
            (=> (=> Nat Exp) (forall [G (List Sk)] (=> (HEnv G) (HEnv Gp)))))))))
  (fn [chkf :- (=> Code Code Bool),
       dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
       encTy :- (=> Exp Code),
       n :- Nat,
       Gp :- (List Sk)]
    (List.rec$1$0 Sk
      (fn [Gp :- (List Sk)] (=> (=> Nat Exp) (forall [G (List Sk)] (=> (HEnv G) (HEnv Gp)))))
      (fn [sg :- (=> Nat Exp), G :- (List Sk), eta :- (HEnv G)] Unit.unit)
      (fn [s :- Sk, rest :- (List Sk),
           ih :- (=> (=> Nat Exp) (forall [G (List Sk)] (=> (HEnv G) (HEnv rest))))]
        (fn [sg :- (=> Nat Exp), G :- (List Sk), eta :- (HEnv G)]
          (Prod.mk (den chkf dec encTy n (sg 0) G s eta)
                   (ih (fn [i :- Nat] (sg (+ i 1))) G eta))))
      Gp)))

(thm envOf_nil [chkf :- (=> Code Code Bool),
                dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
                encTy :- (=> Exp Code),
                n :- Nat, sg :- (=> Nat Exp), G :- (List Sk), eta :- (HEnv G)]
  (= (envOf chkf dec encTy n (List.nil Sk) sg G eta) Unit.unit)
  (rfl))

