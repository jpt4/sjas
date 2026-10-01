(ns lcert.formal.theorem4
  "F5 — assembling Theorem 4 (R4-metatheory.md §5) from the adequacy lemmas.

  eval.clj states Adeq and Theorem_4 and proves one lemma per SkJ term
  constructor (adeq_*).  Two obligations were left open there, and this
  namespace discharges the first of them.

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
            [lcert.formal.substitution]))

;; --- the clauses of skOf that inspect a recursive result ----------------------

;; arrCod (A → B) = some B.  skOf of an application is arrCod of the
;; function's skeleton, and sApp concludes at the codomain.
(thm arrCod_arr [a :- Sk, b :- Sk]
  (Eq (Option Sk) (arrCod (Sk.arr a b)) (Option.some Sk b))
  (rfl))

;; The none branch of skOf's λ clause, stated on an arbitrary option and
;; then read back at skOf (the some branch is lamOpt_some / skOf_lam).
(thm lamOpt_none [a :- Sk, o :- (Option Sk), h :- (Eq (Option Sk) o (Option.none Sk))]
  (Eq (Option Sk)
    (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Option Sk)) (Option.none Sk)
      (fn [x :- Sk] (Option.some Sk (Sk.arr a x))) o)
    (Option.none Sk))
  (subst h)
  (rfl))

(thm skOf_lam_none [G :- (List Sk), r :- U, A :- Exp, t :- Exp,
                    h :- (Eq (Option Sk) (skOf (List.cons Sk (skel A) G) t) (Option.none Sk))]
  (Eq (Option Sk) (skOf G (Exp.lam r A t)) (Option.none Sk))
  (exact (lamOpt_none (skel A) (skOf (List.cons Sk (skel A) G) t) h)))

(thm appOpt_none [o :- (Option Sk), h :- (Eq (Option Sk) o (Option.none Sk))]
  (Eq (Option Sk)
    (Option.rec$1$0 Sk (fn [_ :- (Option Sk)] (Option Sk)) (Option.none Sk)
      (fn [x :- Sk] (arrCod x)) o)
    (Option.none Sk))
  (subst h)
  (rfl))

(thm skOf_app_none [G :- (List Sk), f :- Exp, u :- Exp,
                    h :- (Eq (Option Sk) (skOf G f) (Option.none Sk))]
  (Eq (Option Sk) (skOf G (Exp.app f u)) (Option.none Sk))
  (exact (appOpt_none (skOf G f) h)))

;; bnil is the witness that skOf can be none of a simply typed term.
(thm skOf_bnil_none [G :- (List Sk)]
  (Eq (Option Sk) (skOf G Exp.bnil) (Option.none Sk))
  (rfl))

;; --- brHead is the spine along which skOf returns none -----------------------

(thm brHead_bnil [] (Eq Bool (brHead Exp.bnil) Bool.true) (rfl))
(thm brHead_bcons [h :- Exp, t :- Exp] (Eq Bool (brHead (Exp.bcons h t)) Bool.true) (rfl))
(thm brHead_ite [b :- Exp, t :- Exp, e :- Exp]
  (Eq Bool (brHead (Exp.ite b t e)) (brHead t))
  (rfl))
(thm brHead_lam [r :- U, A :- Exp, t :- Exp]
  (Eq Bool (brHead (Exp.lam r A t)) (brHead t))
  (rfl))
(thm brHead_app [f :- Exp, u :- Exp]
  (Eq Bool (brHead (Exp.app f u)) (brHead f))
  (rfl))

;; argsOK unfolds on the constructors Theorem 4 splits.  rfl, so the
;; conjunction in an induction hypothesis is the unfolded one.
(thm argsOK_bnil_true [G :- (List Sk)]
  (Eq Bool (argsOK G Exp.bnil) Bool.true)
  (rfl))

(thm argsOK_ite_eq [G :- (List Sk), b :- Exp, t :- Exp, e :- Exp]
  (Eq Bool (argsOK G (Exp.ite b t e))
    (Bool.and (argsOK G b) (Bool.and (argsOK G t) (argsOK G e))))
  (rfl))

(thm argsOK_lam_eq [G :- (List Sk), r :- U, A :- Exp, t :- Exp]
  (Eq Bool (argsOK G (Exp.lam r A t)) (argsOK (List.cons Sk (skel A) G) t))
  (rfl))

(thm argsOK_app_eq [G :- (List Sk), f :- Exp, u :- Exp]
  (Eq Bool (argsOK G (Exp.app f u))
    (Bool.and (argsOK G f)
      (Bool.and (argsOK G u)
        (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))))
  (rfl))

;; If the skOf-spine ends in a branch list, skOf is none in every context.
(thm skOf_brHead [e0 :- Exp]
  (=> (Eq Bool (brHead e0) Bool.true)
    (forall [G (List Sk)] (Eq (Option Sk) (skOf G e0) (Option.none Sk))))
  (induction e0)
  (all_goals (intro hb))
  (all_goals (first (exact (Bool.noConfusion hb)) (skip)))
  (all_goals (intro G))
  (all_goals (try (rfl)))
  (exact (ih_t hb G))
  (exact (skOf_lam_none G r A t (ih_t hb (List.cons Sk (skel A) G))))
  (exact (skOf_app_none G f u (ih_f hb G))))

;; A Boolean that cannot be true is false (the same fact as bool_false_of
;; in lemma36.clj, restated so this file does not depend on that proof).
(thm not_true_is_false [b :- Bool]
  (=> (=> (Eq Bool b Bool.true) False) (Eq Bool b Bool.false))
  (cases b)
  (all_goals (intro h))
  (exact (False.elim (h (Eq.refl$1 Bool.true))))
  (exact (Eq.refl$1 Bool.false)))

;; The converse direction used by application and let: a defined skOf
;; rules the branch-list spine out, which is skOf_agree's third hypothesis.
(thm br_of_skSome [e :- Exp, G :- (List Sk), s :- Sk,
                   h :- (Eq (Option Sk) (skOf G e) (Option.some Sk s))]
  (Eq Bool (brHead e) Bool.false)
  (exact (not_true_is_false (brHead e)
           (fn [hb :- (Eq Bool (brHead e) Bool.true)]
             (none_ne_someS s (Eq.trans (Eq.symm (skOf_brHead e hb G)) h))))))

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
(thm skOf_agree [w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (=> (Eq Bool w0 Bool.false)
    (=> (Eq Bool (argsOK G0 e0) Bool.true)
      (=> (Eq Bool (brHead e0) Bool.false)
        (Eq (Option Sk) (skOf G0 e0) (Option.some Sk s0)))))
  (induction der)
  (all_goals (intro hw hok hbr))
  (all_goals (first (exact (Bool.noConfusion hw)) (skip)))
  (all_goals (try (rfl)))
  (exact (Eq.trans
           (skOf_app G f u (Sk.arr s t)
             (ih_hf rfl
               (andb_left (argsOK G f)
                 (Bool.and (argsOK G u)
                   (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
                 hok)
               hbr))
           (arrCod_arr s t)))
  (exact (skOf_lam G r A t s (ih_ht rfl hok hbr)))
  (exact (Bool.noConfusion hbr))
  (exact (Bool.noConfusion hbr))
  (exact (ih_ht rfl
           (andb_left (argsOK G t) (argsOK G e)
             (andb_right (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok))
           hbr))
  (exact h))
