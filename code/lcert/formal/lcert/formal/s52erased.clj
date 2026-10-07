(ns lcert.formal.s52erased
  "Theorem 5.2: induction on every erasure, followed by budget induction.

  The minor premises below are ordinary kernel terms, one per Er rule, each
  using the corresponding s52_* case lemma. They are produced by the same
  generator as the Tl and Rt inductions (s52assembly.clj); only the mode
  differs: premises carry erasures, and a premise's environment is the
  conclusion's, restricted along SubU (env52_sub, with theorem4e.clj's
  usage-vector bookkeeping). Lam dispatches on its usage (s52e_lam), since
  the usage-zero clause quantifies over the whole carrier."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef kdef! lv]]
            [lcert.formal.s52fund :refer [prove! pars ps ctx den rel trace result]]
            [lcert.formal.s52assembly :refer :all]))

;; S52JudgE: an erased runtime derivation, in every environment related at
;; its own usage vector (usage-zero entries unconstrained), evaluates safely
;; under evalᴱₙ, to a value related to the denotation of the source term.
(kdef-fn! 'S52JudgE (concat NH '[D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, e :- Exp])
  '(forall [rho (List RV)] (forall [en (HEnv (skels D))]
     (=> (WFS D) (Env52 chkf dec encTy cap Bool.true D us rho en)
       (Result52 chkf dec encTy cap Bool.true (skels D) en rho t A e)))))

;; Lam, by cases on the usage. At 0 the closure quantifies over the whole
;; carrier and the body runs with ⋆ for the bound variable (the entry has
;; usage 0, so it is unconstrained); at 1 and ω it is the ordinary clause.
(thm* 's52e_lam
  (concat NH '[D :- (List Exp), us :- (List U), A :- Exp, B :- Exp, t :- Exp, te :- Exp,
               hA :- (SkJ Bool.true (skels D) A Sk.unit), hw :- (WFS D),
               rho :- (List RV), en :- (HEnv (skels D)),
               he :- (Env52 chkf dec encTy cap Bool.true D us rho en)])
  '(forall [r U]
     (=> (forall [rho2 (List RV)] (forall [en2 (HEnv (skels (List.cons Exp A D)))]
           (=> (WFS (List.cons Exp A D))
               (Env52 chkf dec encTy cap Bool.true (List.cons Exp A D) (List.cons U r us) rho2 en2)
               (Result52 chkf dec encTy cap Bool.true (skels (List.cons Exp A D)) en2 rho2 t B te))))
         (Result52 chkf dec encTy cap Bool.true (skels D) en rho (Exp.lam r A t) (Exp.tPi r A B)
                   (Exp.lam r A te))))
  '[(intro r) (cases r)
    (intro ih)
    (exact (s52_lam0e chkf dec encTy cap Bool.true (skels D) en rho A B t te (Eq.refl Bool.true)
      (fn [alpha :- (Car (skel A))]
        (ih (List.cons RV RV.star rho) (Prod.mk alpha en) (wfs_cons A D hA hw)
          (env52_cons chkf dec encTy cap Bool.true A D U.u0 us RV.star rho alpha en
            (entry52_zero (S52 chkf dec encTy cap Bool.true A (skels D) en (skel A) RV.star alpha))
            he)))))
    (intro ih)
    (exact (s52_lam chkf dec encTy cap Bool.true (skels D) en rho U.u1 A B t te
      (Or.inr (Eq.refl Bool.true))
      (fn [av :- RV, alpha :- (Car (skel A)),
           ha :- (S52 chkf dec encTy cap Bool.true A (skels D) en (skel A) av alpha)]
        (ih (List.cons RV av rho) (Prod.mk alpha en) (wfs_cons A D hA hw)
          (env52_cons_rel chkf dec encTy cap Bool.true A D U.u1 us av rho alpha en
            (Or.inr (Eq.refl Bool.true)) ha he)))))
    (intro ih)
    (exact (s52_lam chkf dec encTy cap Bool.true (skels D) en rho U.uw A B t te
      (Or.inr (Eq.refl Bool.true))
      (fn [av :- RV, alpha :- (Car (skel A)),
           ha :- (S52 chkf dec encTy cap Bool.true A (skels D) en (skel A) av alpha)]
        (ih (List.cons RV av rho) (Prod.mk alpha en) (wfs_cons A D hA hw)
          (env52_cons_rel chkf dec encTy cap Bool.true A D U.uw us av rho alpha en
            (Or.inr (Eq.refl Bool.true)) ha he)))))])

(def STEP-E '[hcs :- (CheckSpec chkf dec encTy),
              belowE :- (forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m Bool.true)))])

;; The induction on Er at one budget (Theorem 5.2 for evalᴱₙ).
(thm* 's52_er_step
  (concat NH STEP-E '[D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, e0 :- Exp,
                      der :- (Er chkf D0 us0 t0 A0 e0)])
  '(S52JudgE chkf dec encTy cap D0 us0 t0 A0 e0)
  [(L 'exact (concat
     (rec-term 'Er "lcert/formal/erase.clj"
       '(fn [D :- (List Exp), us :- (List U), t :- Exp, A :- Exp, e :- Exp, hd :- (Er chkf D us t A e)]
          (S52JudgE chkf dec encTy cap D us t A e))
       'S52JudgE er-fams :er)
     '[D0 us0 t0 A0 e0 der]))])

;; --- one budget, from the budgets below it -------------------------------------------
;; Closed52 (s52reflect.clj) is the fundamental property at the closed
;; token context Θ_cap. Its environment is the token environment, all of whose
;; entries S relates (tok52), in the well-formed context Θ_cap (wfs_theta).

(thm* 's52_closed_step_false
  (concat NH '[hcs :- (CheckSpec chkf dec encTy),
               belowN :- (forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m Bool.false)))])
  '(Closed52 chkf dec encTy cap Bool.false)
  '[(intro t A hd e hs)
    (have hs2 (Eq Exp e t) hs)
    (exact (Eq.mpr (congrArg (fn [z :- Exp]
        (Result52 chkf dec encTy cap Bool.false (skels (thetaD cap)) (tokEnvD cap) (rtokens cap) t A z)) hs2)
      (s52_rt_step chkf dec encTy cap hcs belowN (thetaD cap) (thetaU cap) t A hd
        (thetaU cap) (rtokens cap) (tokEnvD cap) (wfs_theta cap)
        (tok52 chkf dec encTy cap Bool.false cap))))])

(thm* 's52_closed_step_true
  (concat NH '[hcs :- (CheckSpec chkf dec encTy),
               belowE :- (forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m Bool.true)))])
  '(Closed52 chkf dec encTy cap Bool.true)
  '[(intro t A hd e hs)
    (exact (s52_er_step chkf dec encTy cap hcs belowE (thetaD cap) (thetaU cap) t A e hs
      (rtokens cap) (tokEnvD cap) (wfs_theta cap)
      (tok52 chkf dec encTy cap Bool.true cap)))])

(thm* 's52_closed_step
  (concat NH '[hcs :- (CheckSpec chkf dec encTy)])
  '(forall [erasing Bool]
     (=> (forall [m Nat] (=> (LT.lt m cap) (Closed52 chkf dec encTy m erasing)))
         (Closed52 chkf dec encTy cap erasing)))
  '[(intro erasing) (cases erasing)
    (intro below) (exact (s52_closed_step_false chkf dec encTy cap hcs below))
    (intro below) (exact (s52_closed_step_true chkf dec encTy cap hcs below))])

;; --- the strong induction on the budget (as theorem4e.clj) ------------------------------

(thm* 's52_closed_below_succ
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), erasing :- Bool]
  '(forall [n Nat]
     (=> (forall [m Nat] (=> (LT.lt m n) (Closed52 chkf dec encTy m erasing)))
         (forall [m Nat] (=> (LT.lt m (Nat.succ n)) (Closed52 chkf dec encTy m erasing)))))
  '[(intro n ih m hm)
    (have hor (Or (Eq Nat m n) (LT.lt m n)) (Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hm)))
    (cases hor)
    (subst h)
    (exact (s52_closed_step chkf dec encTy n hcs erasing ih))
    (exact (ih m h))])

(thm* 's52_closed_below_all
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), erasing :- Bool]
  '(forall [n Nat] (forall [m Nat] (=> (LT.lt m n) (Closed52 chkf dec encTy m erasing))))
  '[(intro n)
    (induction n)
    (intro m hm) (exact (False.elim (Nat.not_lt_zero m hm)))
    (exact (s52_closed_below_succ chkf dec encTy hcs erasing n ih_n))])

;; The fundamental property of the closed token context, at every budget and
;; for both evaluators (R4 §5, Theorem 5.2's proof).
(thm* 's52_closed_all
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), hcs :- (CheckSpec chkf dec encTy), erasing :- Bool]
  '(forall [n Nat] (Closed52 chkf dec encTy n erasing))
  '[(intro n)
    (exact (s52_closed_step chkf dec encTy n hcs erasing
      (s52_closed_below_all chkf dec encTy hcs erasing n)))])
