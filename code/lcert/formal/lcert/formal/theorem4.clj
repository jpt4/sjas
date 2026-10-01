(ns lcert.formal.theorem4
  "F5 — assembling Theorem 4 (R4-metatheory.md §5) from the adequacy lemmas.

  eval.clj states Adeq and Theorem_4 and proves one lemma per SkJ term
  constructor (adeq_*).  Two obligations were left open there.  This
  namespace discharges both, then proves Theorem 4.  skOf agrees with
  simple typing off the branch-list spine, and a derivable term has argsOK.
  adeq_skj inducts on SkJ, feeding the adequacy lemmas; the budget induction
  is strong because reflect decodes a certificate at a smaller budget.
  theorem4_spec inhabits the constant Theorem_4.  corollary51 is Corollary
  5.1, with ConvCase kept as a hypothesis.

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
            [lcert.formal.lemma36]))

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

(thm argsOK_base [G :- (List Sk), X :- Exp, h :- (Eq Bool (isBaseOrDia X) Bool.true)]
  (Eq Bool (argsOK G X) Bool.true)
  (cases X)
  (all_goals (first (rfl) (exact (Bool.noConfusion h)))))

(thm argsOK_const [G :- (List Sk)]
  (forall [t Exp] (forall [A Exp]
    (=> (Eq Bool (constTyped t A) Bool.true) (Eq Bool (argsOK G t) Bool.true))))
  (intro t)
  (cases t)
  (all_goals (intro A hc))
  (all_goals (first (rfl) (exact (Bool.noConfusion hc)))))

(thm match_sk_true [o :- (Option Sk), s :- Sk, h :- (Eq (Option Sk) o (Option.some Sk s))]
  (Eq Bool (match o [none Bool.false] [(some sf) Bool.true]) Bool.true)
  (subst h)
  (rfl))

(thm argsOK_app_asm [G :- (List Sk), f :- Exp, u :- Exp,
                     hf :- (Eq Bool (argsOK G f) Bool.true),
                     hu :- (Eq Bool (argsOK G u) Bool.true),
                     hs :- (Eq Bool (match (skOf G u) [none Bool.false] [(some sf) Bool.true]) Bool.true)]
  (Eq Bool (argsOK G (Exp.app f u)) Bool.true)
  (exact (andb_intro (argsOK G f)
           (Bool.and (argsOK G u) (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
           hf
           (andb_intro (argsOK G u) (match (skOf G u) [none Bool.false] [(some sf) Bool.true]) hu hs))))

(thm argsOK_let_eq [G :- (List Sk), C :- Exp, p :- Exp, t :- Exp]
  (Eq Bool (argsOK G (Exp.letp C p t))
    (Bool.and (argsOK G p)
      (match (skOf G p)
        [none Bool.false]
        [(some sp) (match sp [(prod a b) (argsOK (sk2 b a G) t)] [_ Bool.false])])))
  (rfl))

(thm letOpt_dep [pOK :- Bool, sp :- (Option Sk), a :- Sk, b :- Sk, t :- Exp, G :- (List Sk),
                 hs :- (Eq (Option Sk) sp (Option.some Sk (Sk.prod a b))),
                 hp :- (Eq Bool pOK Bool.true),
                 ht :- (Eq Bool (argsOK (sk2 b a G) t) Bool.true)]
  (Eq Bool
    (Bool.and pOK
      (match sp
        [none Bool.false]
        [(some s0) (match s0 [(prod x y) (argsOK (sk2 y x G) t)] [_ Bool.false])]))
    Bool.true)
  (subst hs)
  (exact (andb_intro pOK (argsOK (sk2 b a G) t) hp ht)))

(thm let_ok_prod [G :- (List Sk), C :- Exp, p :- Exp, t :- Exp, a :- Sk, b :- Sk,
                  hp :- (Eq Bool (argsOK G p) Bool.true),
                  hs :- (Eq (Option Sk) (skOf G p) (Option.some Sk (Sk.prod a b))),
                  ht :- (Eq Bool (argsOK (sk2 b a G) t) Bool.true)]
  (Eq Bool (argsOK G (Exp.letp C p t)) Bool.true)
  (exact (Eq.trans (argsOK_let_eq G C p t)
           (letOpt_dep (argsOK G p) (skOf G p) a b t G hs hp ht))))

(println :let-ok)

(thm argsOK_ctx [t :- Exp, G1 :- (List Sk), G2 :- (List Sk), h :- (Eq (List Sk) G1 G2)]
  (Eq Bool (argsOK G1 t) (argsOK G2 t))
  (exact (congrArg (fn [G :- (List Sk)] (argsOK G t)) h)))

(println :args-helpers-ok)

(thm recs_ctx_eq [P :- Exp, D :- (List Exp)]
  (Eq (List Sk)
    (skels (List.cons Exp (y2Ty P) (List.cons Exp (y1Ty P)
             (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))))
    (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))))
  (exact (Eq.trans
           (congrArg (fn [s :- Sk]
                       (List.cons Sk s (List.cons Sk (skel (y1Ty P))
                         (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D))))))
             (skel_y2Ty P))
           (congrArg (fn [s :- Sk]
                       (List.cons Sk (skel P) (List.cons Sk s
                         (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D))))))
             (skel_y1Ty P)))))

(thm argsOK_recs_tn [P :- Exp, t :- Exp, D :- (List Exp),
                     h :- (Eq Bool
                            (argsOK (skels (List.cons Exp (y2Ty P) (List.cons Exp (y1Ty P)
                                     (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D)))))) t)
                            Bool.true)]
  (Eq Bool (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))) t) Bool.true)
  (exact (Eq.trans (Eq.symm (argsOK_ctx t
                     (skels (List.cons Exp (y2Ty P) (List.cons Exp (y1Ty P)
                              (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl D))))))
                     (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D))))
                     (recs_ctx_eq P D)))
           h)))

(println :recs-ok)

(thm tl_argsOK [chkf :- (=> Code Code Bool), w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp,
                der :- (Tl chkf w0 D0 t0 A0)]
  (Eq Bool (argsOK (skels D0) t0) Bool.true)
  (induction der)
  ;; fBase
  (exact (argsOK_base (skels D) X h))
  ;; fT, fPi, fSig: argsOK does not enter a type former
  (rfl) (rfl) (rfl)
  ;; zVar
  (rfl)
  ;; zConst
  (exact (argsOK_const (skels D) t A h))
  ;; zConv, zLam
  (exact ih_ht)
  (exact ih_ht)
  ;; zApp: the argument is formed, hence clean, so skOf reads its skeleton
  (exact (argsOK_app_asm (skels D) f u ih_hf ih_hu
           (match_sk_true (skOf (skels D) u) (skel A)
             (skOf_tl chkf Bool.false D u A hu rfl
               (skj_clean Bool.true (skels D) A Sk.unit (lemma25_tl_type chkf D A hA))))))
  ;; zPair
  (exact (andb_intro (argsOK (skels D) x) (argsOK (skels D) y) ih_hx ih_hy))
  ;; zLet
  (exact (let_ok_prod (skels D) C p t (skel A) (skel B) ih_hp
           (skOf_tl chkf Bool.false D p (Exp.tSig r A B) hp rfl
             (skj_clean Bool.true (skels D) (Exp.tSig r A B) Sk.unit
               (SkJ.wSig (skels D) r A B
                 (lemma25_tl_type chkf D A hA)
                 (lemma25_tl_type chkf (List.cons Exp A D) B hB))))
           ih_ht))
  ;; zAbort
  (exact ih_ht)
  ;; zIte
  (exact (andb_intro (argsOK (skels D) b)
           (Bool.and (argsOK (skels D) t) (argsOK (skels D) e))
           ih_hb
           (andb_intro (argsOK (skels D) t) (argsOK (skels D) e) ih_ht ih_he)))
  ;; zElimB
  (exact (andb_intro (argsOK (skels D) b)
           (Bool.and (argsOK (skels D) t) (argsOK (skels D) e))
           ih_hb
           (andb_intro (argsOK (skels D) t) (argsOK (skels D) e) ih_ht ih_he)))
  ;; zSucc
  (exact ih_h)
  ;; zRecN
  (exact (andb_intro (argsOK (skels D) z)
           (Bool.and (argsOK (skels D) n) (argsOK (sk2 (skel P) Sk.nat (skels D)) s))
           ih_hz
           (andb_intro (argsOK (skels D) n) (argsOK (sk2 (skel P) Sk.nat (skels D)) s) ih_hn ih_hs)))
  ;; zCaseL
  (exact (andb_intro (argsOK (skels D) x) (argsOK (skels D) bs) ih_hx ih_hb))
  ;; zBnil
  (rfl)
  ;; zBcons
  (exact (andb_intro (argsOK (skels D) h) (argsOK (skels D) t) ih_hh ih_ht))
  ;; zSleaf
  (exact ih_h)
  ;; zSnode
  (exact (andb_intro (argsOK (skels D) x)
           (Bool.and (argsOK (skels D) c1) (argsOK (skels D) c2))
           ih_hx
           (andb_intro (argsOK (skels D) c1) (argsOK (skels D) c2) ih_h1 ih_h2)))
  ;; zRecS
  (exact (andb_intro (argsOK (skels D) c)
           (Bool.and (argsOK (List.cons Sk Sk.lbl (skels D)) tl)
             (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))) tn))
           ih_hc
           (andb_intro (argsOK (List.cons Sk Sk.lbl (skels D)) tl)
             (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))) tn)
             ih_hl
             (argsOK_recs_tn P tn D ih_hn))))
  ;; zLeaf
  (exact ih_h)
  ;; zNode
  (exact (andb_intro (argsOK (skels D) d)
           (Bool.and (argsOK (skels D) x)
             (Bool.and (argsOK (skels D) r1) (argsOK (skels D) r2)))
           ih_hd
           (andb_intro (argsOK (skels D) x)
             (Bool.and (argsOK (skels D) r1) (argsOK (skels D) r2))
             ih_hx
             (andb_intro (argsOK (skels D) r1) (argsOK (skels D) r2) ih_h1 ih_h2))))
  ;; zItR
  (exact (andb_intro (argsOK (skels D) g)
           (Bool.and (argsOK (skels D) h) (argsOK (skels D) r))
           ih_hg
           (andb_intro (argsOK (skels D) h) (argsOK (skels D) r) ih_hh ih_hr)))
  ;; zPrn
  (exact ih_h)
  ;; zChk
  (exact (andb_intro (argsOK (skels D) c) (argsOK (skels D) d) ih_hc ih_hd))
  ;; zH1
  (exact (andb_intro (argsOK (skels D) r)
           (Bool.and (argsOK (skels D) s)
             (Bool.and (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2))))
           ih_hr
           (andb_intro (argsOK (skels D) s)
             (Bool.and (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2)))
             ih_hs
             (andb_intro (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2))
               ih_hc
               (andb_intro (argsOK (skels D) e1) (argsOK (skels D) e2) ih_h1 ih_h2)))))
  ;; zRefl
  (exact (andb_intro (argsOK (skels D) r) (argsOK (skels D) e) ih_hr ih_he))
  ;; zInsp
  (exact (andb_intro (argsOK (skels D) r)
           (Bool.and (argsOK (skels D) c)
             (Bool.and (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2)))
           ih_hr
           (andb_intro (argsOK (skels D) c)
             (Bool.and (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2))
             ih_hc
             (andb_intro (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2)
               ih_h1 ih_h2)))))

(println :tl-args-ok)

(thm rt_argsOK [chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp,
                der :- (Rt chkf D0 us0 t0 A0)]
  (Eq Bool (argsOK (skels D0) t0) Bool.true)
  (induction der)
  ;; rVar
  (rfl)
  ;; rConst
  (exact (argsOK_const (skels D) t A h))
  ;; rLam
  (exact ih_ht)
  ;; rApp0: u is a type-level term
  (exact (argsOK_app_asm (skels D) f u ih_hf
           (tl_argsOK chkf Bool.false D u A hu)
           (match_sk_true (skOf (skels D) u) (skel A)
             (skOf_tl chkf Bool.false D u A hu rfl
               (skj_clean Bool.true (skels D) A Sk.unit (lemma25_tl_type chkf D A hA))))))
  ;; rApp
  (exact (argsOK_app_asm (skels D) f u ih_hf ih_hu
           (match_sk_true (skOf (skels D) u) (skel A)
             (skOf_rt chkf D us2 u A hu
               (skj_clean Bool.true (skels D) A Sk.unit (lemma25_tl_type chkf D A hA))))))
  ;; rPair0: the first component is type-level
  (exact (andb_intro (argsOK (skels D) x) (argsOK (skels D) y)
           (tl_argsOK chkf Bool.false D x A hx) ih_hy))
  ;; rPair
  (exact (andb_intro (argsOK (skels D) x) (argsOK (skels D) y) ih_hx ih_hy))
  ;; rLet: the scrutinee is typed at a formed Σ, so skOf is its product skeleton
  (exact (let_ok_prod (skels D) C p t (skel A) (skel B) ih_hp
           (skOf_rt chkf D us1 p (Exp.tSig r A B) hp
             (skj_clean Bool.true (skels D) (Exp.tSig r A B) Sk.unit
               (SkJ.wSig (skels D) r A B
                 (lemma25_tl_type chkf D A hA)
                 (lemma25_tl_type chkf (List.cons Exp A D) B hB))))
           ih_ht))
  ;; rAbort, rConv
  (exact ih_ht)
  (exact ih_ht)
  ;; rIte
  (exact (andb_intro (argsOK (skels D) b)
           (Bool.and (argsOK (skels D) t) (argsOK (skels D) e))
           ih_hb
           (andb_intro (argsOK (skels D) t) (argsOK (skels D) e) ih_ht ih_he)))
  ;; rElimB
  (exact (andb_intro (argsOK (skels D) b)
           (Bool.and (argsOK (skels D) t) (argsOK (skels D) e))
           ih_hb
           (andb_intro (argsOK (skels D) t) (argsOK (skels D) e) ih_ht ih_he)))
  ;; rSucc
  (exact ih_h)
  ;; rRecN
  (exact (andb_intro (argsOK (skels D) z)
           (Bool.and (argsOK (skels D) n) (argsOK (sk2 (skel P) Sk.nat (skels D)) s))
           ih_hz
           (andb_intro (argsOK (skels D) n) (argsOK (sk2 (skel P) Sk.nat (skels D)) s) ih_hn ih_hs)))
  ;; rCaseL
  (exact (andb_intro (argsOK (skels D) x) (argsOK (skels D) bs) ih_hx ih_hb))
  ;; rBnil
  (rfl)
  ;; rBcons
  (exact (andb_intro (argsOK (skels D) h) (argsOK (skels D) t) ih_hh ih_ht))
  ;; rSleaf
  (exact ih_h)
  ;; rSnode
  (exact (andb_intro (argsOK (skels D) x)
           (Bool.and (argsOK (skels D) c1) (argsOK (skels D) c2))
           ih_hx
           (andb_intro (argsOK (skels D) c1) (argsOK (skels D) c2) ih_h1 ih_h2)))
  ;; rRecS
  (exact (andb_intro (argsOK (skels D) c)
           (Bool.and (argsOK (List.cons Sk Sk.lbl (skels D)) tl)
             (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))) tn))
           ih_hc
           (andb_intro (argsOK (List.cons Sk Sk.lbl (skels D)) tl)
             (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl (skels D)))) tn)
             ih_hl
             (argsOK_recs_tn P tn D ih_hn))))
  ;; rLeaf
  (exact ih_h)
  ;; rNode
  (exact (andb_intro (argsOK (skels D) d)
           (Bool.and (argsOK (skels D) x)
             (Bool.and (argsOK (skels D) r1) (argsOK (skels D) r2)))
           ih_hd
           (andb_intro (argsOK (skels D) x)
             (Bool.and (argsOK (skels D) r1) (argsOK (skels D) r2))
             ih_hx
             (andb_intro (argsOK (skels D) r1) (argsOK (skels D) r2) ih_h1 ih_h2))))
  ;; rItR
  (exact (andb_intro (argsOK (skels D) g)
           (Bool.and (argsOK (skels D) h) (argsOK (skels D) r))
           ih_hg
           (andb_intro (argsOK (skels D) h) (argsOK (skels D) r) ih_hh ih_hr)))
  ;; rPrn
  (exact ih_h)
  ;; rChk
  (exact (andb_intro (argsOK (skels D) c) (argsOK (skels D) d) ih_hc ih_hd))
  ;; rH1
  (exact (andb_intro (argsOK (skels D) r)
           (Bool.and (argsOK (skels D) s)
             (Bool.and (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2))))
           ih_hr
           (andb_intro (argsOK (skels D) s)
             (Bool.and (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2)))
             ih_hs
             (andb_intro (argsOK (skels D) c)
               (Bool.and (argsOK (skels D) e1) (argsOK (skels D) e2))
               ih_hc
               (andb_intro (argsOK (skels D) e1) (argsOK (skels D) e2) ih_h1 ih_h2)))))
  ;; rRefl
  (exact (andb_intro (argsOK (skels D) r) (argsOK (skels D) e) ih_hr ih_he))
  ;; rInsp
  (exact (andb_intro (argsOK (skels D) r)
           (Bool.and (argsOK (skels D) c)
             (Bool.and (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2)))
           ih_hr
           (andb_intro (argsOK (skels D) c)
             (Bool.and (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2))
             ih_hc
             (andb_intro (argsOK (sk2 Sk.unit Sk.cert (skels D)) t1)
               (argsOK (sk2 Sk.unit Sk.cert (skels D)) t2)
               ih_h1 ih_h2)))))

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
(thm sk_witness [o :- (Option Sk),
                 h :- (Eq Bool (match o [none Bool.false] [(some sf) Bool.true]) Bool.true)]
  (Exists (fn [s :- Sk] (Eq (Option Sk) o (Option.some Sk s))))
  (cases o)
  (exact (Bool.noConfusion h))
  (constructor)
  (exact val)
  (rfl))

(thm skels_theta [m :- Nat]
  (Eq (List Sk) (skels (thetaD m)) (thetaSk m))
  (induction m)
  (rfl)
  (exact (congrArg (fn [g :- (List Sk)] (List.cons Sk Sk.dia g)) ih_n)))

;; isBaseTy D picks the seven data types reflect may target.  Each of them
;; has a base skeleton, so rel there is equality, and each is closed, so E1
;; of CheckSpec applies.
(thm baseSk_base [D :- Exp]
  (=> (Eq Bool (isBaseTy D) Bool.true) (Eq Bool (baseSk (skel D)) Bool.true))
  (cases D)
  (all_goals (intro hb))
  (all_goals (first (rfl) (exact (Bool.noConfusion hb)))))

(thm closed_of_base [D :- Exp]
  (=> (Eq Bool (isBaseTy D) Bool.true) (Eq Bool (closedTy D) Bool.true))
  (cases D)
  (all_goals (intro hb))
  (all_goals (first (rfl) (exact (Bool.noConfusion hb)))))

(thm skj_along [w :- Bool, e :- Exp, s :- Sk, G1 :- (List Sk), G2 :- (List Sk),
                hG :- (Eq (List Sk) G1 G2), h :- (SkJ w G1 e s)]
  (SkJ w G2 e s)
  (exact (Eq.mp (congrArg (fn [G :- (List Sk)] (SkJ w G e s)) hG) h)))

;; Drop the relation from an adequacy witness.  adeq_node's token premise,
;; and reflect's evidence, are evaluation only.
(thm eval_only
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), n :- Nat, rho :- (List RV), t :- Exp, s :- Sk,
   G :- (List Sk), eta :- (HEnv G),
   h :- (Exists (fn [v :- RV]
          (And (Eval chkf dec encTy n rho t v)
               (rel chkf dec encTy n s v (den chkf dec encTy n t G s eta)))))]
  (Exists (fn [v :- RV] (Eval chkf dec encTy n rho t v)))
  (refine' (exT RV _ _ h _))
  (intro v hv)
  (exact (Exists.intro v (And.left hv))))

;; An argument whose skOf-match succeeded is typed at the skeleton SkJ gave
;; it.  sk_witness recovers the option; br_of_skSome turns it into the
;; brHead hypothesis skOf_agree needs.
(thm app_arg_sk [G :- (List Sk), u :- Exp, s :- Sk, der :- (SkJ Bool.false G u s),
                 hok :- (Eq Bool (argsOK G u) Bool.true),
                 hm :- (Eq Bool (match (skOf G u) [none Bool.false] [(some sf) Bool.true]) Bool.true)]
  (Eq (Option Sk) (skOf G u) (Option.some Sk s))
  (refine' (exT Sk _ _ (sk_witness (skOf G u) hm) _))
  (intro s0 hs0)
  (exact (skOf_agree Bool.false G u s der rfl hok (br_of_skSome u G s0 hs0))))

;; The outer match of let's argsOK clause is true whenever the inner one is:
;; the none branch is false, so the scrutinee's skOf is some.
(thm let_match_some [sp :- (Option Sk), inner :- (=> Sk Bool),
                     h :- (Eq Bool (match sp [none Bool.false] [(some s0) (inner s0)]) Bool.true)]
  (Eq Bool (match sp [none Bool.false] [(some sf) Bool.true]) Bool.true)
  (cases sp)
  (exact (Bool.noConfusion h))
  (rfl))

;; Once skOf of the scrutinee is the product, the inner clause of argsOK is
;; the body's argsOK in the extended context.  subst rewrites the option
;; variable; the product is not a variable, so the equation is a hypothesis.
(thm let_body_of [pOK :- Bool, sp :- (Option Sk), a :- Sk, b :- Sk, t :- Exp, G :- (List Sk),
                  hs :- (Eq (Option Sk) sp (Option.some Sk (Sk.prod a b))),
                  h :- (Eq Bool
                        (Bool.and pOK
                          (match sp
                            [none Bool.false]
                            [(some s0) (match s0 [(prod x y) (argsOK (sk2 y x G) t)] [_ Bool.false])]))
                        Bool.true)]
  (Eq Bool (argsOK (sk2 b a G) t) Bool.true)
  (subst hs)
  (exact (andb_right pOK (argsOK (sk2 b a G) t) h)))

;; m < b and b ≤ c give m < c.  omega is a tactic, not a proof term, so the
;; step is its own lemma.  Nat.le_of_ble_eq_true returns Nat.le, which omega
;; accepts.
(thm lt_le_omega [a :- Nat, b :- Nat, c :- Nat, h1 :- (LT.lt a b), h2 :- (Nat.le b c)]
  (LT.lt a c)
  (omega))

;; Theorem 4, reflect (R4-metatheory §5, the sRefl clause).
;;
;; r and e are adequate by the induction hypothesis.  The guard is
;; cnodes(⟦r⟧) ≤ cap ∧ chkf(⟦r⟧, ⌜D⌝).  Failure returns the error token
;; (adeq_refl_no).  Success: CheckSpec decodes ⟦r⟧ to Θₘ ⊢ t₂ : A with
;; m < cnodes(⟦r⟧), hence m < cap; E1 and closed_of_base give A = D; the
;; outer hypothesis at m evaluates t₂ in the token environment.  adeq_refl_ok
;; assembles the certificate.
(thm adeq_refl_asm
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   D :- Exp, r :- Exp, e :- Exp,
   rho :- (List RV), eta :- (HEnv G),
   hcs :- (CheckSpec chkf dec encTy),
   below :- (forall [m Nat] (=> (LT.lt m cap) (Adeq chkf dec encTy m))),
   hb :- (Eq Bool (isBaseTy D) Bool.true),
   ihr :- (Exists (fn [vr :- RV]
            (And (Eval chkf dec encTy cap rho r vr)
                 (rel chkf dec encTy cap Sk.cert vr (den chkf dec encTy cap r G Sk.cert eta))))),
   ihe :- (Exists (fn [ve :- RV]
            (And (Eval chkf dec encTy cap rho e ve)
                 (rel chkf dec encTy cap Sk.unit ve (den chkf dec encTy cap e G Sk.unit eta)))))]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.refl D r e) v)
         (rel chkf dec encTy cap (skel D) v (den chkf dec encTy cap (Exp.refl D r e) G (skel D) eta)))))
  (have ie (Exists (fn [ve :- RV] (Eval chkf dec encTy cap rho e ve)))
    (eval_only chkf dec encTy cap rho e Sk.unit G eta ihe))
  (by_cases (Bool.and (Nat.ble (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap)
              (chkf (den chkf dec encTy cap r G Sk.cert eta) (encTy D))))
  (exact (adeq_refl_no chkf dec encTy cap G D r e rho eta ihr ie hc))
  (have hchk (Eq Bool (chkf (den chkf dec encTy cap r G Sk.cert eta) (encTy D)) Bool.true)
    (andb_right (Nat.ble (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap)
      (chkf (den chkf dec encTy cap r G Sk.cert eta) (encTy D)) hc))
  (refine' (exT Nat _ _ ((And.left hcs) (den chkf dec encTy cap r G Sk.cert eta) (encTy D) hchk) _))
  (intro m hm)
  (refine' (exT Exp _ _ hm _))
  (intro t2 ht2)
  (refine' (exT Exp _ _ ht2 _))
  (intro A hA)
  (have hd (Eq (Option (Prod Nat (Prod Exp Exp)))
            (dec (den chkf dec encTy cap r G Sk.cert eta))
            (Option.some (Prod Nat (Prod Exp Exp)) (Prod.mk m (Prod.mk t2 A))))
    (And.left hA))
  (have hrt (Rt chkf (thetaD m) (thetaU m) t2 A) (And.left (And.right hA)))
  (have hclA (Eq Bool (closedTy A) Bool.true)
    (And.left (And.right (And.right (And.right hA)))))
  (have henc (Eq Code (encTy A) (encTy D))
    (And.left (And.right (And.right (And.right (And.right hA))))))
  (have hlt0 (LT.lt m (cnodes (den chkf dec encTy cap r G Sk.cert eta)))
    (And.right (And.right (And.right (And.right (And.right hA))))))
  (have hble (Eq Bool (Nat.ble (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap) Bool.true)
    (andb_left (Nat.ble (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap)
      (chkf (den chkf dec encTy cap r G Sk.cert eta) (encTy D)) hc))
  (have hle (Nat.le (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap) (Nat.le_of_ble_eq_true hble))
  (have hlt (LT.lt m cap) (lt_le_omega m (cnodes (den chkf dec encTy cap r G Sk.cert eta)) cap hlt0 hle))
  (have eAD (Eq Exp A D)
    ((And.right (And.right (And.right hcs))) A D hclA (closed_of_base D hb) henc))
  (have hrtD (Rt chkf (thetaD m) (thetaU m) t2 D)
    (Eq.mp (congrArg (fn [Z :- Exp] (Rt chkf (thetaD m) (thetaU m) t2 Z)) eAD) hrt))
  (have hsk (SkJ Bool.false (thetaSk m) t2 (skel D))
    (skj_along Bool.false t2 (skel D) (skels (thetaD m)) (thetaSk m) (skels_theta m)
      (lemma25_rt chkf (thetaD m) (thetaU m) t2 D hrtD)))
  (have hok (Eq Bool (argsOK (thetaSk m) t2) Bool.true)
    (Eq.trans (Eq.symm (argsOK_ctx t2 (skels (thetaD m)) (thetaSk m) (skels_theta m)))
      (rt_argsOK chkf (thetaD m) (thetaU m) t2 D hrtD)))
  (have hdec := (below m hlt (thetaSk m) t2 (skel D) hsk hok (rtokens m) (tokenEnv m)
                  (tokens_rel chkf dec encTy m m)))
  (exact (adeq_refl_ok chkf dec encTy cap G D r e rho eta m t2 A
           (baseSk_base D hb) ihr ie hc hd hlt hdec)))

(println :refl-asm-ok)

;; --- Theorem 4 ----------------------------------------------------------------
;;
;; Each SkJ term constructor is one lemma (case_*).  adeq_skj is the inner
;; induction; adeq_from_below / below_succ / adeq_below_all are the strong
;; induction on the budget (reflect is the only appeal to a smaller budget).
;; theorem4 is that statement; theorem4_spec is the constant Theorem_4 in
;; eval.clj, which reduces to it.  corollary51 is Corollary 5.1: at a base
;; data type the value is the canonical form of an element of V(D).

(thm case_abort
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), A :- Exp, t :- Exp,
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap Sk.unit v (den chkf dec encTy cap t G Sk.unit eta2)))))))))),
   hok :- (Eq Bool (argsOK G t) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.abort A t) v)
         (rel chkf dec encTy cap (skel A) v (den chkf dec encTy cap (Exp.abort A t) G (skel A) eta)))))
  (have ie := (iht rfl hok rho eta henv))
  (refine' (exT RV _ _ ie _))
  (intro vt hv)
  (exact (adeq_abort chkf dec encTy cap G A t rho eta vt (And.left hv))))

(thm case_ite
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   b :- Exp, t :- Exp, e :- Exp, s :- Sk,
   ihb :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G b) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 b v)
                         (rel chkf dec encTy cap Sk.bool v (den chkf dec encTy cap b G Sk.bool eta2)))))))))),
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap s v (den chkf dec encTy cap t G s eta2)))))))))),
   ihe :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G e) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 e v)
                         (rel chkf dec encTy cap s v (den chkf dec encTy cap e G s eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G b) (Bool.and (argsOK G t) (argsOK G e))) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.ite b t e) v)
         (rel chkf dec encTy cap s v (den chkf dec encTy cap (Exp.ite b t e) G s eta)))))
  (exact (adeq_ite_den chkf dec encTy cap G b t e s rho eta
           (ihb rfl (andb_left (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok) rho eta henv)
           (iht rfl (andb_left (argsOK G t) (argsOK G e)
                      (andb_right (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok)) rho eta henv)
           (ihe rfl (andb_right (argsOK G t) (argsOK G e)
                      (andb_right (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok)) rho eta henv))))

(thm case_elim
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   P :- Exp, b :- Exp, t :- Exp, e :- Exp,
   ihb :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G b) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 b v)
                         (rel chkf dec encTy cap Sk.bool v (den chkf dec encTy cap b G Sk.bool eta2)))))))))),
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap t G (skel P) eta2)))))))))),
   ihe :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G e) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 e v)
                         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap e G (skel P) eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G b) (Bool.and (argsOK G t) (argsOK G e))) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.elimB P b t e) v)
         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap (Exp.elimB P b t e) G (skel P) eta)))))
  (exact (adeq_elim_den chkf dec encTy cap G P b t e rho eta
           (ihb rfl (andb_left (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok) rho eta henv)
           (iht rfl (andb_left (argsOK G t) (argsOK G e)
                      (andb_right (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok)) rho eta henv)
           (ihe rfl (andb_right (argsOK G t) (argsOK G e)
                      (andb_right (argsOK G b) (Bool.and (argsOK G t) (argsOK G e)) hok)) rho eta henv))))

(thm case_succ
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), n :- Exp,
   ih :- (=> (Eq Bool Bool.false Bool.false)
           (=> (Eq Bool (argsOK G n) Bool.true)
             (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
               (=> (envRel chkf dec encTy cap G rho2 eta2)
                 (Exists (fn [v :- RV]
                   (And (Eval chkf dec encTy cap rho2 n v)
                        (rel chkf dec encTy cap Sk.nat v (den chkf dec encTy cap n G Sk.nat eta2)))))))))),
   hok :- (Eq Bool (argsOK G n) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.succ n) v)
         (rel chkf dec encTy cap Sk.nat v (den chkf dec encTy cap (Exp.succ n) G Sk.nat eta)))))
  (have ie := (ih rfl hok rho eta henv))
  (refine' (exT RV _ _ ie _))
  (intro vt hv)
  (exact (adeq_succ chkf dec encTy cap G n rho eta vt (And.left hv) (And.right hv))))

(thm case_recN
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   P :- Exp, z :- Exp, st :- Exp, n :- Exp,
   ihz :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G z) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 z v)
                         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap z G (skel P) eta2)))))))))),
   ihs :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (sk2 (skel P) Sk.nat G) st) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (sk2 (skel P) Sk.nat G))]
                (=> (envRel chkf dec encTy cap (sk2 (skel P) Sk.nat G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 st v)
                         (rel chkf dec encTy cap (skel P) v
                           (den chkf dec encTy cap st (sk2 (skel P) Sk.nat G) (skel P) eta2)))))))))),
   ihn :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G n) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 n v)
                         (rel chkf dec encTy cap Sk.nat v (den chkf dec encTy cap n G Sk.nat eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G z)
              (Bool.and (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.recN P z st n) v)
         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap (Exp.recN P z st n) G (skel P) eta)))))
  (exact (adeq_recN chkf dec encTy cap G P z st n (skel P) rho eta henv
           (ihn rfl
             (andb_left (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)
               (andb_right (argsOK G z)
                 (Bool.and (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)) hok))
             rho eta henv)
           (ihz rfl
             (andb_left (argsOK G z)
               (Bool.and (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)) hok)
             rho eta henv)
           (ihs rfl
             (andb_right (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)
               (andb_right (argsOK G z)
                 (Bool.and (argsOK G n) (argsOK (sk2 (skel P) Sk.nat G) st)) hok))))))

(thm case_caseL
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   P :- Exp, x :- Exp, bs :- Exp,
   ihx :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G x) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 x v)
                         (rel chkf dec encTy cap Sk.lbl v (den chkf dec encTy cap x G Sk.lbl eta2)))))))))),
   ihb :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G bs) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 bs v)
                         (rel chkf dec encTy cap (Sk.arr Sk.lbl (skel P)) v
                           (den chkf dec encTy cap bs G (Sk.arr Sk.lbl (skel P)) eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G x) (argsOK G bs)) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.caseL P x bs) v)
         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap (Exp.caseL P x bs) G (skel P) eta)))))
  (exact (adeq_caseL chkf dec encTy cap G P x bs (skel P) rho eta
           (ihx rfl (andb_left (argsOK G x) (argsOK G bs) hok) rho eta henv)
           (ihb rfl (andb_right (argsOK G x) (argsOK G bs) hok) rho eta henv))))

(thm case_bcons
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   h :- Exp, t :- Exp, s :- Sk,
   ihh :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G h) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 h v)
                         (rel chkf dec encTy cap s v (den chkf dec encTy cap h G s eta2)))))))))),
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap (Sk.arr Sk.lbl s) v
                           (den chkf dec encTy cap t G (Sk.arr Sk.lbl s) eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G h) (argsOK G t)) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.bcons h t) v)
         (rel chkf dec encTy cap (Sk.arr Sk.lbl s) v
           (den chkf dec encTy cap (Exp.bcons h t) G (Sk.arr Sk.lbl s) eta)))))
  (exact (adeq_bcons chkf dec encTy cap G h t s rho eta
           (ihh rfl (andb_left (argsOK G h) (argsOK G t) hok) rho eta henv)
           (iht rfl (andb_right (argsOK G h) (argsOK G t) hok) rho eta henv))))

(thm case_snode
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   x :- Exp, c1 :- Exp, c2 :- Exp,
   ihx :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G x) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 x v)
                         (rel chkf dec encTy cap Sk.lbl v (den chkf dec encTy cap x G Sk.lbl eta2)))))))))),
   ih1 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c1) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c1 v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c1 G Sk.syn eta2)))))))))),
   ih2 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c2) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c2 v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c2 G Sk.syn eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G x) (Bool.and (argsOK G c1) (argsOK G c2))) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.snode x c1 c2) v)
         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap (Exp.snode x c1 c2) G Sk.syn eta)))))
  (exact (adeq_snode chkf dec encTy cap G x c1 c2 rho eta
           (ihx rfl (andb_left (argsOK G x) (Bool.and (argsOK G c1) (argsOK G c2)) hok) rho eta henv)
           (ih1 rfl (andb_left (argsOK G c1) (argsOK G c2)
                      (andb_right (argsOK G x) (Bool.and (argsOK G c1) (argsOK G c2)) hok)) rho eta henv)
           (ih2 rfl (andb_right (argsOK G c1) (argsOK G c2)
                      (andb_right (argsOK G x) (Bool.and (argsOK G c1) (argsOK G c2)) hok)) rho eta henv))))

(thm case_recS
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   P :- Exp, tl :- Exp, tn :- Exp, c :- Exp,
   ihl :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (List.cons Sk Sk.lbl G) tl) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (List.cons Sk Sk.lbl G))]
                (=> (envRel chkf dec encTy cap (List.cons Sk Sk.lbl G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 tl v)
                         (rel chkf dec encTy cap (skel P) v
                           (den chkf dec encTy cap tl (List.cons Sk Sk.lbl G) (skel P) eta2)))))))))),
   ihn :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn) Bool.true)
              (forall [rho2 (List RV)]
                (forall [eta2 (HEnv (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))))]
                  (=> (envRel chkf dec encTy cap
                        (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) rho2 eta2)
                    (Exists (fn [v :- RV]
                      (And (Eval chkf dec encTy cap rho2 tn v)
                           (rel chkf dec encTy cap (skel P) v
                             (den chkf dec encTy cap tn
                               (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G)))
                               (skel P) eta2)))))))))),
   ihc :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c G Sk.syn eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G c)
              (Bool.and (argsOK (List.cons Sk Sk.lbl G) tl)
                (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn)))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.recS P tl tn c) v)
         (rel chkf dec encTy cap (skel P) v (den chkf dec encTy cap (Exp.recS P tl tn c) G (skel P) eta)))))
  (exact (adeq_recS chkf dec encTy cap G P tl tn c (skel P) rho eta henv
           (ihc rfl
             (andb_left (argsOK G c)
               (Bool.and (argsOK (List.cons Sk Sk.lbl G) tl)
                 (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn))
               hok)
             rho eta henv)
           (ihl rfl
             (andb_left (argsOK (List.cons Sk Sk.lbl G) tl)
               (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn)
               (andb_right (argsOK G c)
                 (Bool.and (argsOK (List.cons Sk Sk.lbl G) tl)
                   (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn))
                 hok)))
           (ihn rfl
             (andb_right (argsOK (List.cons Sk Sk.lbl G) tl)
               (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn)
               (andb_right (argsOK G c)
                 (Bool.and (argsOK (List.cons Sk Sk.lbl G) tl)
                   (argsOK (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn))
                 hok))))))

(thm case_node
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp,
   ihd :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G d) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 d v)
                         (rel chkf dec encTy cap Sk.dia v (den chkf dec encTy cap d G Sk.dia eta2)))))))))),
   ihx :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G x) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 x v)
                         (rel chkf dec encTy cap Sk.lbl v (den chkf dec encTy cap x G Sk.lbl eta2)))))))))),
   ih1 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r1) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r1 v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r1 G Sk.cert eta2)))))))))),
   ih2 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r2) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r2 v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r2 G Sk.cert eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G d)
              (Bool.and (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.node d x r1 r2) v)
         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap (Exp.node d x r1 r2) G Sk.cert eta)))))
  (exact (adeq_node chkf dec encTy cap G d x r1 r2 rho eta
           (eval_only chkf dec encTy cap rho d Sk.dia G eta
             (ihd rfl
               (andb_left (argsOK G d)
                 (Bool.and (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))) hok)
               rho eta henv))
           (ihx rfl
             (andb_left (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))
               (andb_right (argsOK G d)
                 (Bool.and (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))) hok))
             rho eta henv)
           (ih1 rfl
             (andb_left (argsOK G r1) (argsOK G r2)
               (andb_right (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))
                 (andb_right (argsOK G d)
                   (Bool.and (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))) hok)))
             rho eta henv)
           (ih2 rfl
             (andb_right (argsOK G r1) (argsOK G r2)
               (andb_right (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))
                 (andb_right (argsOK G d)
                   (Bool.and (argsOK G x) (Bool.and (argsOK G r1) (argsOK G r2))) hok)))
             rho eta henv))))

(thm case_itR
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   X :- Exp, g :- Exp, h :- Exp, r :- Exp,
   ihg :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G g) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 g v)
                         (rel chkf dec encTy cap (Sk.arr Sk.lbl (skel X)) v
                           (den chkf dec encTy cap g G (Sk.arr Sk.lbl (skel X)) eta2)))))))))),
   ihh :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G h) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 h v)
                         (rel chkf dec encTy cap
                           (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))) v
                           (den chkf dec encTy cap h G
                             (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
                             eta2)))))))))),
   ihr :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r G Sk.cert eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G g) (Bool.and (argsOK G h) (argsOK G r))) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.itR X g h r) v)
         (rel chkf dec encTy cap (skel X) v (den chkf dec encTy cap (Exp.itR X g h r) G (skel X) eta)))))
  (exact (adeq_itR chkf dec encTy cap G X g h r (skel X) rho eta
           (ihg rfl (andb_left (argsOK G g) (Bool.and (argsOK G h) (argsOK G r)) hok) rho eta henv)
           (ihh rfl (andb_left (argsOK G h) (argsOK G r)
                      (andb_right (argsOK G g) (Bool.and (argsOK G h) (argsOK G r)) hok)) rho eta henv)
           (ihr rfl (andb_right (argsOK G h) (argsOK G r)
                      (andb_right (argsOK G g) (Bool.and (argsOK G h) (argsOK G r)) hok)) rho eta henv))))

(thm case_sleaf
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), x :- Exp,
   ih :- (=> (Eq Bool Bool.false Bool.false)
           (=> (Eq Bool (argsOK G x) Bool.true)
             (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
               (=> (envRel chkf dec encTy cap G rho2 eta2)
                 (Exists (fn [v :- RV]
                   (And (Eval chkf dec encTy cap rho2 x v)
                        (rel chkf dec encTy cap Sk.lbl v (den chkf dec encTy cap x G Sk.lbl eta2)))))))))),
   hok :- (Eq Bool (argsOK G x) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.sleaf x) v)
         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap (Exp.sleaf x) G Sk.syn eta)))))
  (exact (adeq_sleaf chkf dec encTy cap G x rho eta (ih rfl hok rho eta henv))))

(thm case_leaf
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), x :- Exp,
   ih :- (=> (Eq Bool Bool.false Bool.false)
           (=> (Eq Bool (argsOK G x) Bool.true)
             (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
               (=> (envRel chkf dec encTy cap G rho2 eta2)
                 (Exists (fn [v :- RV]
                   (And (Eval chkf dec encTy cap rho2 x v)
                        (rel chkf dec encTy cap Sk.lbl v (den chkf dec encTy cap x G Sk.lbl eta2)))))))))),
   hok :- (Eq Bool (argsOK G x) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.leaf x) v)
         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap (Exp.leaf x) G Sk.cert eta)))))
  (exact (adeq_leaf chkf dec encTy cap G x rho eta (ih rfl hok rho eta henv))))

(thm case_prn
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), r :- Exp,
   ih :- (=> (Eq Bool Bool.false Bool.false)
           (=> (Eq Bool (argsOK G r) Bool.true)
             (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
               (=> (envRel chkf dec encTy cap G rho2 eta2)
                 (Exists (fn [v :- RV]
                   (And (Eval chkf dec encTy cap rho2 r v)
                        (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r G Sk.cert eta2)))))))))),
   hok :- (Eq Bool (argsOK G r) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.prn r) v)
         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap (Exp.prn r) G Sk.syn eta)))))
  (exact (adeq_prn chkf dec encTy cap G r rho eta (ih rfl hok rho eta henv))))

(thm case_lam
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   r :- U, A :- Exp, t :- Exp, s :- Sk,
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (List.cons Sk (skel A) G) t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (List.cons Sk (skel A) G))]
                (=> (envRel chkf dec encTy cap (List.cons Sk (skel A) G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap s v
                           (den chkf dec encTy cap t (List.cons Sk (skel A) G) s eta2)))))))))),
   hok :- (Eq Bool (argsOK (List.cons Sk (skel A) G) t) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.lam r A t) v)
         (rel chkf dec encTy cap (Sk.arr (skel A) s) v
           (den chkf dec encTy cap (Exp.lam r A t) G (Sk.arr (skel A) s) eta)))))
  (exact (adeq_lam chkf dec encTy cap G r A t s rho eta henv (iht rfl hok))))

(thm case_pair
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   r :- U, A :- Exp, B :- Exp, x :- Exp, y :- Exp,
   ihx :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G x) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 x v)
                         (rel chkf dec encTy cap (skel A) v (den chkf dec encTy cap x G (skel A) eta2)))))))))),
   ihy :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G y) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 y v)
                         (rel chkf dec encTy cap (skel B) v (den chkf dec encTy cap y G (skel B) eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G x) (argsOK G y)) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.pair (Exp.tSig r A B) x y) v)
         (rel chkf dec encTy cap (Sk.prod (skel A) (skel B)) v
           (den chkf dec encTy cap (Exp.pair (Exp.tSig r A B) x y) G (Sk.prod (skel A) (skel B)) eta)))))
  (exact (adeq_pair chkf dec encTy cap G r A B x y rho eta
           (ihx rfl (andb_left (argsOK G x) (argsOK G y) hok) rho eta henv)
           (ihy rfl (andb_right (argsOK G x) (argsOK G y) hok) rho eta henv))))

(thm case_chk
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk), c :- Exp, d :- Exp,
   ihc :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c G Sk.syn eta2)))))))))),
   ihd :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G d) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 d v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap d G Sk.syn eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G c) (argsOK G d)) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.chk c d) v)
         (rel chkf dec encTy cap Sk.bool v (den chkf dec encTy cap (Exp.chk c d) G Sk.bool eta)))))
  (exact (adeq_chk chkf dec encTy cap G c d rho eta
           (ihc rfl (andb_left (argsOK G c) (argsOK G d) hok) rho eta henv)
           (ihd rfl (andb_right (argsOK G c) (argsOK G d) hok) rho eta henv))))

(thm case_app
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   f :- Exp, u :- Exp, s :- Sk, t :- Sk,
   hf :- (SkJ Bool.false G f (Sk.arr s t)),
   hu :- (SkJ Bool.false G u s),
   ihf :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G f) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 f v)
                         (rel chkf dec encTy cap (Sk.arr s t) v
                           (den chkf dec encTy cap f G (Sk.arr s t) eta2)))))))))),
   ihu :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G u) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 u v)
                         (rel chkf dec encTy cap s v (den chkf dec encTy cap u G s eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G f)
              (Bool.and (argsOK G u)
                (match (skOf G u) [none Bool.false] [(some sf) Bool.true])))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.app f u) v)
         (rel chkf dec encTy cap t v (den chkf dec encTy cap (Exp.app f u) G t eta)))))
  (have hsu := (app_arg_sk G u s hu
                 (andb_left (argsOK G u)
                   (match (skOf G u) [none Bool.false] [(some sf) Bool.true])
                   (andb_right (argsOK G f)
                     (Bool.and (argsOK G u)
                       (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
                     hok))
                 (andb_right (argsOK G u)
                   (match (skOf G u) [none Bool.false] [(some sf) Bool.true])
                   (andb_right (argsOK G f)
                     (Bool.and (argsOK G u)
                       (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
                     hok))))
  (exact (adeq_app chkf dec encTy cap G f u s t rho eta hsu
           (ihf rfl
             (andb_left (argsOK G f)
               (Bool.and (argsOK G u)
                 (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
               hok)
             rho eta henv)
           (ihu rfl
             (andb_left (argsOK G u)
               (match (skOf G u) [none Bool.false] [(some sf) Bool.true])
               (andb_right (argsOK G f)
                 (Bool.and (argsOK G u)
                   (match (skOf G u) [none Bool.false] [(some sf) Bool.true]))
                 hok))
             rho eta henv))))

(thm case_let
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   C :- Exp, p :- Exp, t :- Exp, s1 :- Sk, s2 :- Sk,
   hp :- (SkJ Bool.false G p (Sk.prod s1 s2)),
   ihp :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G p) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 p v)
                         (rel chkf dec encTy cap (Sk.prod s1 s2) v
                           (den chkf dec encTy cap p G (Sk.prod s1 s2) eta2)))))))))),
   iht :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (sk2 s2 s1 G) t) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (sk2 s2 s1 G))]
                (=> (envRel chkf dec encTy cap (sk2 s2 s1 G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t v)
                         (rel chkf dec encTy cap (skel C) v
                           (den chkf dec encTy cap t (sk2 s2 s1 G) (skel C) eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G p)
              (match (skOf G p)
                [none Bool.false]
                [(some s0) (match s0 [(prod xa ya) (argsOK (sk2 ya xa G) t)] [_ Bool.false])]))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.letp C p t) v)
         (rel chkf dec encTy cap (skel C) v (den chkf dec encTy cap (Exp.letp C p t) G (skel C) eta)))))
  (have hsu := (app_arg_sk G p (Sk.prod s1 s2) hp
                 (andb_left (argsOK G p)
                   (match (skOf G p)
                     [none Bool.false]
                     [(some s0) (match s0 [(prod xa ya) (argsOK (sk2 ya xa G) t)] [_ Bool.false])])
                   hok)
                 (let_match_some (skOf G p)
                   (fn [s0 :- Sk] (match s0 [(prod xa ya) (argsOK (sk2 ya xa G) t)] [_ Bool.false]))
                   (andb_right (argsOK G p)
                     (match (skOf G p)
                       [none Bool.false]
                       [(some s0) (match s0 [(prod xa ya) (argsOK (sk2 ya xa G) t)] [_ Bool.false])])
                     hok))))
  (have hbody := (let_body_of (argsOK G p) (skOf G p) s1 s2 t G hsu hok))
  (exact (adeq_let chkf dec encTy cap G C p t s1 s2 (skel C) rho eta henv hsu
           (ihp rfl
             (andb_left (argsOK G p)
               (match (skOf G p)
                 [none Bool.false]
                 [(some s0) (match s0 [(prod xa ya) (argsOK (sk2 ya xa G) t)] [_ Bool.false])])
               hok)
             rho eta henv)
           (iht rfl hbody))))

(thm case_refl
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   hcs :- (CheckSpec chkf dec encTy),
   below :- (forall [m Nat] (=> (LT.lt m cap) (Adeq chkf dec encTy m))),
   D :- Exp, r :- Exp, e :- Exp,
   hb :- (Eq Bool (isBaseTy D) Bool.true),
   ihr :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r G Sk.cert eta2)))))))))),
   ihe :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G e) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 e v)
                         (rel chkf dec encTy cap Sk.unit v (den chkf dec encTy cap e G Sk.unit eta2)))))))))),
   hok :- (Eq Bool (Bool.and (argsOK G r) (argsOK G e)) Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.refl D r e) v)
         (rel chkf dec encTy cap (skel D) v (den chkf dec encTy cap (Exp.refl D r e) G (skel D) eta)))))
  (exact (adeq_refl_asm chkf dec encTy cap G D r e rho eta hcs below hb
           (ihr rfl (andb_left (argsOK G r) (argsOK G e) hok) rho eta henv)
           (ihe rfl (andb_right (argsOK G r) (argsOK G e) hok) rho eta henv))))

(thm case_insp
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   X :- Exp, r :- Exp, c :- Exp, t1 :- Exp, t2 :- Exp,
   ihr :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r G Sk.cert eta2)))))))))),
   ihc :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c G Sk.syn eta2)))))))))),
   ih1 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (sk2 Sk.unit Sk.cert G) t1) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (sk2 Sk.unit Sk.cert G))]
                (=> (envRel chkf dec encTy cap (sk2 Sk.unit Sk.cert G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t1 v)
                         (rel chkf dec encTy cap (skel X) v
                           (den chkf dec encTy cap t1 (sk2 Sk.unit Sk.cert G) (skel X) eta2)))))))))),
   ih2 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK (sk2 Sk.unit Sk.cert G) t2) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv (sk2 Sk.unit Sk.cert G))]
                (=> (envRel chkf dec encTy cap (sk2 Sk.unit Sk.cert G) rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 t2 v)
                         (rel chkf dec encTy cap (skel X) v
                           (den chkf dec encTy cap t2 (sk2 Sk.unit Sk.cert G) (skel X) eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G r)
              (Bool.and (argsOK G c)
                (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1)
                  (argsOK (sk2 Sk.unit Sk.cert G) t2))))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.insp X r c t1 t2) v)
         (rel chkf dec encTy cap (skel X) v (den chkf dec encTy cap (Exp.insp X r c t1 t2) G (skel X) eta)))))
  (exact (adeq_insp chkf dec encTy cap G X r c t1 t2 (skel X) rho eta henv
           (ihr rfl
             (andb_left (argsOK G r)
               (Bool.and (argsOK G c)
                 (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)))
               hok)
             rho eta henv)
           (ihc rfl
             (andb_left (argsOK G c)
               (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2))
               (andb_right (argsOK G r)
                 (Bool.and (argsOK G c)
                   (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)))
                 hok))
             rho eta henv)
           (ih1 rfl
             (andb_left (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)
               (andb_right (argsOK G c)
                 (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2))
                 (andb_right (argsOK G r)
                   (Bool.and (argsOK G c)
                     (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)))
                   hok))))
           (ih2 rfl
             (andb_right (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)
               (andb_right (argsOK G c)
                 (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2))
                 (andb_right (argsOK G r)
                   (Bool.and (argsOK G c)
                     (Bool.and (argsOK (sk2 Sk.unit Sk.cert G) t1) (argsOK (sk2 Sk.unit Sk.cert G) t2)))
                   hok)))))))

(thm h1_oks [G :- (List Sk), r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
             hok :- (Eq Bool
                      (Bool.and (argsOK G r)
                        (Bool.and (argsOK G s)
                          (Bool.and (argsOK G c)
                            (Bool.and (argsOK G e1) (argsOK G e2)))))
                      Bool.true)]
  (And (Eq Bool (argsOK G r) Bool.true)
    (And (Eq Bool (argsOK G s) Bool.true)
      (And (Eq Bool (argsOK G c) Bool.true)
        (And (Eq Bool (argsOK G e1) Bool.true)
             (Eq Bool (argsOK G e2) Bool.true)))))
  (constructor)
  (exact (andb_left (argsOK G r)
           (Bool.and (argsOK G s)
             (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))))
           hok))
  (constructor)
  (exact (andb_left (argsOK G s)
           (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2)))
           (andb_right (argsOK G r)
             (Bool.and (argsOK G s)
               (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))))
             hok)))
  (constructor)
  (exact (andb_left (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))
           (andb_right (argsOK G s)
             (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2)))
             (andb_right (argsOK G r)
               (Bool.and (argsOK G s)
                 (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))))
               hok))))
  (constructor)
  (exact (andb_left (argsOK G e1) (argsOK G e2)
           (andb_right (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))
             (andb_right (argsOK G s)
               (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2)))
               (andb_right (argsOK G r)
                 (Bool.and (argsOK G s)
                   (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))))
                 hok)))))
  (exact (andb_right (argsOK G e1) (argsOK G e2)
           (andb_right (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))
             (andb_right (argsOK G s)
               (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2)))
               (andb_right (argsOK G r)
                 (Bool.and (argsOK G s)
                   (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2))))
                 hok))))))

(thm case_h1
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat, G :- (List Sk),
   r :- Exp, s :- Exp, c :- Exp, e1 :- Exp, e2 :- Exp,
   ihr :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G r) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 r v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap r G Sk.cert eta2)))))))))),
   ihs :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G s) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 s v)
                         (rel chkf dec encTy cap Sk.cert v (den chkf dec encTy cap s G Sk.cert eta2)))))))))),
   ihc :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G c) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 c v)
                         (rel chkf dec encTy cap Sk.syn v (den chkf dec encTy cap c G Sk.syn eta2)))))))))),
   ih1 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G e1) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 e1 v)
                         (rel chkf dec encTy cap Sk.unit v (den chkf dec encTy cap e1 G Sk.unit eta2)))))))))),
   ih2 :- (=> (Eq Bool Bool.false Bool.false)
            (=> (Eq Bool (argsOK G e2) Bool.true)
              (forall [rho2 (List RV)] (forall [eta2 (HEnv G)]
                (=> (envRel chkf dec encTy cap G rho2 eta2)
                  (Exists (fn [v :- RV]
                    (And (Eval chkf dec encTy cap rho2 e2 v)
                         (rel chkf dec encTy cap Sk.unit v (den chkf dec encTy cap e2 G Sk.unit eta2)))))))))),
   hok :- (Eq Bool
            (Bool.and (argsOK G r)
              (Bool.and (argsOK G s)
                (Bool.and (argsOK G c) (Bool.and (argsOK G e1) (argsOK G e2)))))
            Bool.true),
   rho :- (List RV), eta :- (HEnv G),
   henv :- (envRel chkf dec encTy cap G rho eta)]
  (Exists (fn [v :- RV]
    (And (Eval chkf dec encTy cap rho (Exp.h1 r s c e1 e2) v)
         (rel chkf dec encTy cap Sk.unit v (den chkf dec encTy cap (Exp.h1 r s c e1 e2) G Sk.unit eta)))))
  (have oks := (h1_oks G r s c e1 e2 hok))
  (have er := (ihr rfl (And.left oks) rho eta henv))
  (refine' (exT RV _ _ er _)) (intro vr pvr)
  (have es := (ihs rfl (And.left (And.right oks)) rho eta henv))
  (refine' (exT RV _ _ es _)) (intro vs pvs)
  (have ec := (ihc rfl (And.left (And.right (And.right oks))) rho eta henv))
  (refine' (exT RV _ _ ec _)) (intro vc pvc)
  (have ee1 := (ih1 rfl (And.left (And.right (And.right (And.right oks)))) rho eta henv))
  (refine' (exT RV _ _ ee1 _)) (intro v1 p1)
  (have ee2 := (ih2 rfl (And.right (And.right (And.right (And.right oks)))) rho eta henv))
  (refine' (exT RV _ _ ee2 _)) (intro v2 p2)
  (exact (adeq_h1 chkf dec encTy cap G r s c e1 e2 rho eta vr vs vc v1 v2
           (And.left pvr) (And.left pvs) (And.left pvc) (And.left p1) (And.left p2))))

;; Theorem 4, inner induction on the simple-typing derivation
;; (R4-metatheory §5).  Each term constructor is one adequacy lemma.
(thm adeq_skj
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat,
   hcs :- (CheckSpec chkf dec encTy),
   below :- (forall [m Nat] (=> (LT.lt m cap) (Adeq chkf dec encTy m))),
   w0 :- Bool, G0 :- (List Sk), e0 :- Exp, s0 :- Sk, der :- (SkJ w0 G0 e0 s0)]
  (=> (Eq Bool w0 Bool.false)
    (=> (Eq Bool (argsOK G0 e0) Bool.true)
      (forall [rho (List RV)] (forall [eta (HEnv G0)]
        (=> (envRel chkf dec encTy cap G0 rho eta)
          (Exists (fn [v :- RV]
            (And (Eval chkf dec encTy cap rho e0 v)
                 (rel chkf dec encTy cap s0 v (den chkf dec encTy cap e0 G0 s0 eta))))))))))
  (induction der)
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw) (exact (Bool.noConfusion hw))
  (intro hw hok rho eta henv)
  (exact (adeq_var chkf dec encTy cap G i s h rho eta henv))
  (intro hw hok rho eta henv)
  (exact (adeq_star chkf dec encTy cap G rho eta))
  (intro hw hok rho eta henv)
  (exact (case_abort chkf dec encTy cap G A t ih_ht hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (adeq_tt chkf dec encTy cap G rho eta))
  (intro hw hok rho eta henv)
  (exact (adeq_ff chkf dec encTy cap G rho eta))
  (intro hw hok rho eta henv)
  (exact (case_ite chkf dec encTy cap G b t e s ih_hb ih_ht ih_he hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_elim chkf dec encTy cap G P b t e ih_hb ih_ht ih_he hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (adeq_zero chkf dec encTy cap G rho eta))
  (intro hw hok rho eta henv)
  (exact (case_succ chkf dec encTy cap G n ih_h hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_recN chkf dec encTy cap G P z st n ih_hz ih_hs ih_hn hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (adeq_lbl chkf dec encTy cap l G rho eta))
  (intro hw hok rho eta henv)
  (exact (case_caseL chkf dec encTy cap G P x bs ih_hx ih_hb hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (adeq_bnil chkf dec encTy cap G s rho eta))
  (intro hw hok rho eta henv)
  (exact (case_bcons chkf dec encTy cap G h t s ih_hh ih_ht hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_sleaf chkf dec encTy cap G x ih_h hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_snode chkf dec encTy cap G x c1 c2 ih_hx ih_h1 ih_h2 hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_recS chkf dec encTy cap G P tl tn c ih_hl ih_hn ih_hc hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_leaf chkf dec encTy cap G x ih_h hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_node chkf dec encTy cap G d x r1 r2 ih_hd ih_hx ih_h1 ih_h2 hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_itR chkf dec encTy cap G X g h r ih_hg ih_hh ih_hr hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_prn chkf dec encTy cap G r ih_h hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_lam chkf dec encTy cap G r A t s ih_ht hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_app chkf dec encTy cap G f u s t hf hu ih_hf ih_hu hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_pair chkf dec encTy cap G r A B x y ih_hx ih_hy hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_let chkf dec encTy cap G C p t s1 s2 hp ih_hp ih_ht hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_chk chkf dec encTy cap G c d ih_hc ih_hd hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_h1 chkf dec encTy cap G r s c e1 e2 ih_hr ih_hs ih_hc ih_h1 ih_h2 hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_refl chkf dec encTy cap G hcs below D r e hb ih_hr ih_he hok rho eta henv))
  (intro hw hok rho eta henv)
  (exact (case_insp chkf dec encTy cap G X r c t1 t2 ih_hr ih_hc ih_h1 ih_h2 hok rho eta henv)))

;; Adeq at cap, given Adeq at every smaller budget.  The SkJ induction is
;; adeq_skj; reflect is the only constructor that uses the smaller budgets.
(thm adeq_from_below
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code), cap :- Nat,
   hcs :- (CheckSpec chkf dec encTy),
   below :- (forall [m Nat] (=> (LT.lt m cap) (Adeq chkf dec encTy m)))]
  (Adeq chkf dec encTy cap)
  (intro G t s hsk hok rho eta hr)
  (exact (adeq_skj chkf dec encTy cap hcs below Bool.false G t s hsk rfl hok rho eta hr)))

(thm below_zero
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code)]
  (forall [m Nat] (=> (LT.lt m 0) (Adeq chkf dec encTy m)))
  (intro m hm)
  (exact (False.elim (Nat.not_lt_zero m hm))))

;; Strong step: m < n + 1 is m = n or m < n.  The equal case is Adeq at n
;; from the hypothesis for everything smaller; the rest is the hypothesis.
(thm below_succ
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat]
    (=> (forall [m Nat] (=> (LT.lt m n) (Adeq chkf dec encTy m)))
        (forall [m Nat] (=> (LT.lt m (Nat.succ n)) (Adeq chkf dec encTy m)))))
  (intro n ih m hm)
  (have hor (Or (Eq Nat m n) (LT.lt m n)) (Nat.eq_or_lt_of_le (Nat.le_of_lt_succ hm)))
  (cases hor)
  (subst h)
  (exact (adeq_from_below chkf dec encTy n hcs ih))
  (exact (ih m h)))

;; Theorem 4 (R4-metatheory §5): under CheckSpec, evalₙ terminates and is
;; adequate for every simply typed term with argsOK, at every budget.
;; The constant Theorem_4 in eval.clj is this statement.  Induction on the
;; budget needs the index in context first (induction: hypothesis not in
;; context, otherwise).
(thm adeq_below_all
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (forall [m Nat] (=> (LT.lt m n) (Adeq chkf dec encTy m))))
  (intro n)
  (induction n)
  (exact (below_zero chkf dec encTy))
  (exact (below_succ chkf dec encTy hcs n ih_n)))

(thm theorem4
  [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
   encTy :- (=> Exp Code),
   hcs :- (CheckSpec chkf dec encTy)]
  (forall [n Nat] (Adeq chkf dec encTy n))
  (intro n)
  (exact (adeq_from_below chkf dec encTy n hcs (adeq_below_all chkf dec encTy hcs n))))

(thm theorem4_spec [] Theorem_4
  (exact theorem4))

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
  (have hsk (SkJ Bool.false (thetaSk n) t (skel D))
    (skj_along Bool.false t (skel D) (skels (thetaD n)) (thetaSk n) (skels_theta n)
      (lemma25_rt chkf (thetaD n) (thetaU n) t D hd)))
  (have hok (Eq Bool (argsOK (thetaSk n) t) Bool.true)
    (Eq.trans (Eq.symm (argsOK_ctx t (skels (thetaD n)) (thetaSk n) (skels_theta n)))
      (rt_argsOK chkf (thetaD n) (thetaU n) t D hd)))
  (have hrun := (theorem4 chkf dec encTy hcs n (thetaSk n) t (skel D) hsk hok
                  (rtokens n) (tokenEnv n) (tokens_rel chkf dec encTy n n)))
  (have hv := (lemma36 chkf dec encTy hcs hconv n (thetaD n) (thetaU n) t D hd
                (wf_theta chkf n) (tokEnvD n) n (Nat.le_refl n) (tok_sat chkf dec encTy n n)))
  (have heq := (tok_transfer n (den chkf dec encTy n t) (skel D)))
  (refine' (exT RV _ _ hrun _))
  (intro v hvand)
  (constructor) (exact v)
  (constructor) (exact (And.left hvand))
  (constructor) (exact (And.right hvand))
  (constructor) (exact hv)
  (exact heq))
