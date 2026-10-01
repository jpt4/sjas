(ns lcert.formal.convcase
  "F3m, continued — Lemma 3.2 at every position, and the Conv case of
  Lemma 3.6 (R4-metatheory.md §3.2, §3.3, §3.6).

  conversion.clj proves a head step of an nbr skeleton-typed term preserves
  the denotation (hd_den) and the empty-path case (den_step_nil).  This
  namespace finishes the argument.

  §1  nbr is preserved by substitution.  A substitution whose every value is
      nbr at flag false (no stray branch list) preserves nbrF at every flag.
      Branch lists are the only constructors that read the flag (bnil returns
      it, bcons demands it); nbrF_anyflag says a term that is nbr at false is
      nbr at every flag, so it may be copied into a branch-list position.
      Under a binder the substitution is upn, and lifting preserves nbrF
      (nbrF_lift, syntactic.clj).  nbr_subst1 and nbr_substL are the
      instances the head steps use.

  §2  The contractum of a head step is skeleton-typed at the redex's
      skeleton, and nbr.  Substituting steps (β, β-let, recNS, recSL, recSN)
      do not need a new skOf lemma: skj_term_unit puts the substituted term
      at skeleton Unit, skOf_complete (nbr) gives skOf = some s, subOK_cons /
      subOK_list build SubOK, and skj_subst (vweaken.clj) types the
      contractum.  subst1_consSub / substL_instLS transport the typing onto
      subst1 / substL.

  Later sections (path congruence, the V chain, F_conv, conv_all) are added
  as they are checked.  conv_all is ConvAll (lemma36.clj); Lemma_3_6_holds,
  Theorem_1_holds and Corollary_3_7_holds are the paper statements."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.carrier :refer :all]
            [lcert.formal.den :refer :all]
            [lcert.formal.sem :refer :all]
            [lcert.formal.model :refer :all]
            [lcert.formal.syntactic]
            [lcert.formal.substitution]
            [lcert.formal.skeletons]
            [lcert.formal.skof]
            [lcert.formal.vweaken]
            [lcert.formal.fundamental]
            [lcert.formal.conversion]
            [lcert.formal.lemma36]))

(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private congruence @#'lcert.formal.syntactic/congruence)

;; ===========================================================================
;; §1  nbr through substitution
;; ===========================================================================

;; A variable is nbr at every flag: it is not a branch list.
(thm nbrF_var [i :- Nat, fl :- Bool]
  (Eq Bool ((nbrF (Exp.var i)) fl) Bool.true)
  (rfl))

;; nbr at flag false implies nbr at every flag.  Only bnil and bcons read the
;; flag, and both fail at false, so the premise is contradictory there.  Every
;; other constructor ignores the flag, so the two applications are the same
;; Boolean.
(a/prove-theorem 'nbrF_anyflag '[e :- Exp]
  '(=> (Eq Bool ((nbrF e) Bool.false) Bool.true)
       (forall [fl Bool] (Eq Bool ((nbrF e) fl) Bool.true)))
  (lv (into ['(induction e)]
            (mapcat (fn [[ctor _]]
                      (cond
                        (= ctor 'bnil)
                        '[(intro hn fl) (exact (Bool.noConfusion hn))]
                        (= ctor 'bcons)
                        '[(intro hn fl)
                          (exact (Bool.noConfusion
                                   (band_left Bool.false
                                     (Bool.and ((nbrF h) Bool.false) ((nbrF t) Bool.true)) hn)))]
                        :else
                        '[(intro hn fl) (exact hn)]))
                    exp-fields))))

;; up σ replaces 0 by a variable and i+1 by a lift of σ i.  Both are nbr at
;; false when every σ i is.
(thm up_pres [sg :- (=> Nat Exp),
              hs :- (forall [i Nat] (Eq Bool ((nbrF (sg i)) Bool.false) Bool.true))]
  (forall [i Nat] (Eq Bool ((nbrF (up sg i)) Bool.false) Bool.true))
  (intro i)
  (cases i)
  (exact (nbrF_var 0 Bool.false))
  (exact (Eq.trans (nbrF_lift (sg n) 1 0 Bool.false) (hs n))))

;; upn k σ, by induction on the number of binders.
(thm upn_pres [k :- Nat, sg :- (=> Nat Exp),
               hs :- (forall [i Nat] (Eq Bool ((nbrF (sg i)) Bool.false) Bool.true))]
  (forall [i Nat] (Eq Bool ((nbrF (upn k sg i)) Bool.false) Bool.true))
  (induction k)
  (intro i)
  (exact (hs i))
  (intro i)
  (exact (up_pres (upn n sg) ih_n i)))

(defn- nbr-and-chain [xs]
  (if (= 1 (count xs)) (first xs)
      (list 'Bool.and (first xs) (nbr-and-chain (rest xs)))))

(defn- nbr-subst-conjunction [fields]
  (if (= 1 (count fields))
    (nth (first fields) 3)
    (let [[_ l r pf] (first fields)
          rest-fields (rest fields)]
      (congruence 'Bool.and
        [['Bool l r pf]
         ['Bool (nbr-and-chain (map second rest-fields))
          (nbr-and-chain (map #(nth % 2) rest-fields))
          (nbr-subst-conjunction rest-fields)]]))))

;; subst σ (var i) is σ i (subst_var).  Do not rewrite first: rw closes a goal
;; by reflexivity and would skip the exact that uses the nbr hypothesis.
;; nbrF of the variable is true; nbrF_anyflag supplies the same for σ i.
(thm nbrF_subst_var [sg :- (=> Nat Exp),
                     hs :- (forall [j Nat] (Eq Bool ((nbrF (sg j)) Bool.false) Bool.true)),
                     i :- Nat, fl :- Bool]
  (Eq Bool ((nbrF (subst sg (Exp.var i))) fl) ((nbrF (Exp.var i)) fl))
  (exact (Eq.trans (nbrF_anyflag (sg i) (hs i) fl)
                   (Eq.symm (nbrF_var i fl)))))

;; nbrF (e[σ]) fl = nbrF e fl, when every σ i is nbr at false.  The variable
;; case is nbrF_subst_var.  A field under `depth` binders is substituted by
;; upn depth σ (substF), whose values are nbr by upn_pres.  The and-chain is
;; the same one nbrF builds, so the congruence is the goal.  rfl closes an
;; empty constructor: subst and nbrF reduce at the kernel even when the goal
;; printer still shows the folded application.
(a/prove-theorem 'nbrF_subst '[e :- Exp]
  '(forall [sg (=> Nat Exp)]
     (=> (forall [i Nat] (Eq Bool ((nbrF (sg i)) Bool.false) Bool.true))
         (forall [fl Bool]
           (Eq Bool ((nbrF (subst sg e)) fl) ((nbrF e) fl)))))
  (lv (into ['(induction e)]
            (mapcat (fn [[ctor fields]]
                      (cons '(intro sg hs fl)
                            (cond
                              (= ctor 'var)
                              '[(exact (nbrF_subst_var sg hs i fl))]
                              :else
                              (let [fs (vec (concat
                                             (when (= ctor 'bcons) [['Bool 'fl 'fl 'rfl]])
                                             (for [[f ty depth] fields
                                                   :when (= ty 'Exp)
                                                   :let [flag (if (or (and (= ctor 'caseL) (= f 'bs))
                                                                      (and (= ctor 'bcons) (= f 't)))
                                                                'Bool.true 'Bool.false)]]
                                               ['Bool
                                                (list (list 'nbrF (list 'subst (list 'upn depth 'sg) f)) flag)
                                                (list (list 'nbrF f) flag)
                                                (list (symbol (str "ih_" f))
                                                      (list 'upn depth 'sg)
                                                      (list 'upn_pres depth 'sg 'hs)
                                                      flag)])))]
                                (if (empty? fs)
                                  '[(rfl)]
                                  [(list 'exact (nbr-subst-conjunction fs))])))))
                    exp-fields))))

;; inst1 u: 0 ↦ u, i+1 ↦ var i.  nbr when u is.
(thm inst1_nbr [u :- Exp, hu :- (Eq Bool (nbr u) Bool.true)]
  (forall [i Nat] (Eq Bool ((nbrF (inst1 u i)) Bool.false) Bool.true))
  (intro i)
  (cases i)
  (exact hu)
  (exact (nbrF_var n Bool.false)))

;; Lemma 3.2's substituting steps: β and recSL.  nbr of both sides of the
;; substitution gives nbr of the contractum.
(thm nbr_subst1 [u :- Exp, e :- Exp, hu :- (Eq Bool (nbr u) Bool.true), he :- (Eq Bool (nbr e) Bool.true)]
  (Eq Bool (nbr (subst1 u e)) Bool.true)
  (exact (Eq.trans (nbrF_subst e (fn [i :- Nat] (inst1 u i)) (inst1_nbr u hu) Bool.false) he)))

;; Every element of a list is nbr.  The nil case is True, so a concrete list
;; of nbr proofs is an And-chain ending in True (Eq.refl of the empty list is
;; not involved: the base is the proposition True).
(kdef NbrAll (=> (List Exp) Prop)
  (fn [us :- (List Exp)]
    (List.rec$1$0 Exp (fn [_ :- (List Exp)] Prop) True
      (fn [u :- Exp, _ :- (List Exp), ih :- Prop] (And (Eq Bool (nbr u) Bool.true) ih))
      us)))

;; instL us i is either a list element or a variable past the end.  Both are
;; nbr when NbrAll us holds.  instL.eq_* because instL does not reduce on a
;; symbolic list (it is well-founded in the same way nthS is; the equations
;; are the computation).
(thm instL_nbr [us :- (List Exp)]
  (=> (NbrAll us) (forall [i Nat] (Eq Bool ((nbrF (instL us i)) Bool.false) Bool.true)))
  (induction us)
  (intro hus i)
  (exact (Eq.trans (congrArg (fn [v :- Exp] ((nbrF v) Bool.false)) (instL.eq_1 i)) (nbrF_var i Bool.false)))
  (intro hus i)
  (cases i)
  (exact (Eq.trans (congrArg (fn [v :- Exp] ((nbrF v) Bool.false)) (instL.eq_2 head tail)) (And.left hus)))
  (exact (Eq.trans (congrArg (fn [v :- Exp] ((nbrF v) Bool.false)) (instL.eq_3 head tail n))
                   (ih_tail (And.right hus) n))))

;; instLS is instL (instL_instLS).  The σ of substL is therefore nbr.
(thm instLS_nbr [us :- (List Exp), hus :- (NbrAll us)]
  (forall [i Nat] (Eq Bool ((nbrF (instLS us i)) Bool.false) Bool.true))
  (intro i)
  (exact (Eq.trans (congrArg (fn [v :- Exp] ((nbrF v) Bool.false)) (Eq.symm (instL_instLS us i)))
                   (instL_nbr us hus i))))

;; β-let, recNS, recSN: substL of an nbr list into an nbr body.
(thm nbr_substL [us :- (List Exp), e :- Exp, hus :- (NbrAll us), he :- (Eq Bool (nbr e) Bool.true)]
  (Eq Bool (nbr (substL us e)) Bool.true)
  (rw [(substL_instLS us e)])
  (exact (Eq.trans (nbrF_subst e (instLS us) (instLS_nbr us hus) Bool.false) he)))

;; ===========================================================================
;; §2  Skeleton typing of a head contractum
;; ===========================================================================

;; β / recSL: SkJ of t[u/x] at the body's skeleton.  SubOK is subOK_cons of
;; the argument (typed, skOf-faithful) with the identity; skj_subst types the
;; substituted body; subst1_consSub renames the substitution to subst1.
(thm skj_subst1 [G :- (List Sk), sdom :- Sk, t :- Exp, sb :- Sk,
                 ht :- (SkJ Bool.false (List.cons Sk sdom G) t sb),
                 u :- Exp, hu :- (SkJ Bool.false G u sdom),
                 hk :- (Eq (Option Sk) (skOf G u) (Option.some Sk sdom))]
  (SkJ Bool.false G (subst1 u t) sb)
  (have hsub (SkJ Bool.false G (subst (consSub u (fn [j :- Nat] (Exp.var j))) t) sb)
    (skj_subst Bool.false (List.cons Sk sdom G) t sb ht G
      (consSub u (fn [j :- Nat] (Exp.var j)))
      (subOK_cons u sdom G (fn [j :- Nat] (Exp.var j)) G hu hk (subOK_id G))))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ Bool.false G e sb)) (Eq.symm (subst1_consSub u t))) hsub)))

;; A list substitution, the same transport for substL (β-let, recNS, recSN).
(thm skj_substL [w0 :- Bool, G :- (List Sk), us :- (List Exp), ss :- (List Sk), hty :- (TyL G us ss),
                 t :- Exp, s2 :- Sk, der :- (SkJ w0 (appS ss G) t s2)]
  (SkJ w0 G (substL us t) s2)
  (have hsub (SkJ w0 G (subst (instLS us) t) s2)
    (skj_subst w0 (appS ss G) t s2 der G (instLS us) (subOK_list G us ss hty)))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ w0 G e s2)) (Eq.symm (substL_instLS us t))) hsub)))

;; β.  Inversion is the same unpacking as den_beta; the conclusion is typing
;; and nbr of the contractum rather than the denotation equation.
(thm skj_beta_core [r :- U, A :- Exp, t :- Exp, u :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, sb :- Sk,
                    harr :- (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb)),
                    ht :- (SkJ Bool.false (List.cons Sk (skel A) G) t sb),
                    hu :- (SkJ Bool.false G u s1),
                    hn :- (Eq Bool (nbr (Exp.app (Exp.lam r A t) u)) Bool.true)]
  (And (SkJ Bool.false G (subst1 u t) s) (Eq Bool (nbr (subst1 u t)) Bool.true))
  (have hai (And (Eq Sk s1 (skel A)) (Eq Sk s sb)) (arr_inj s1 s (skel A) sb harr))
  (have h1 (Eq Sk s1 (skel A)) (And.left hai))
  (have h2 (Eq Sk s sb) (And.right hai))
  (have hnu (Eq Bool ((nbrF u) Bool.false) Bool.true)
        (nbr_app_u (Exp.lam r A t) u Bool.false hn))
  (have hnf (Eq Bool ((nbrF (Exp.lam r A t)) Bool.false) Bool.true)
        (nbr_app_f (Exp.lam r A t) u Bool.false hn))
  (have hnt (Eq Bool ((nbrF t) Bool.false) Bool.true)
        (nbr_lam_t r A t Bool.false hnf))
  (have hku (Eq (Option Sk) (skOf G u) (Option.some Sk s1)) (skOf_ok G u s1 hu hnu))
  (subst h1)
  (subst h2)
  (exact (And.intro
           (skj_subst1 G (skel A) t sb ht u hu hku)
           (nbr_subst1 u t hnu hnt))))

(thm skj_beta [r :- U, A :- Exp, t :- Exp, u :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.app (Exp.lam r A t) u) s),
               hn :- (Eq Bool (nbr (Exp.app (Exp.lam r A t) u)) Bool.true)]
  (And (SkJ Bool.false G (subst1 u t) s) (Eq Bool (nbr (subst1 u t)) Bool.true))
  (have happ (And (Eq Bool Bool.false Bool.false)
                  (Exists (fn [s1 :- Sk]
                    (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s))
                         (SkJ Bool.false G u s1)))))
        (inv_app Bool.false G (Exp.lam r A t) u s hj))
  (exact (exSk (fn [s1 :- Sk] (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s)) (SkJ Bool.false G u s1)))
               (And (SkJ Bool.false G (subst1 u t) s) (Eq Bool (nbr (subst1 u t)) Bool.true))
               (And.right happ)
               (fn [s1 :- Sk, hs1 :- (And (SkJ Bool.false G (Exp.lam r A t) (Sk.arr s1 s)) (SkJ Bool.false G u s1))]
                 (exSk (fn [sb :- Sk] (And (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb))
                                           (And (SkJ Bool.true G A Sk.unit)
                                                (SkJ Bool.false (List.cons Sk (skel A) G) t sb))))
                       (And (SkJ Bool.false G (subst1 u t) s) (Eq Bool (nbr (subst1 u t)) Bool.true))
                       (And.right (inv_lam Bool.false G r A t (Sk.arr s1 s) (And.left hs1)))
                       (fn [sb :- Sk, hsb :- (And (Eq Sk (Sk.arr s1 s) (Sk.arr (skel A) sb))
                                                  (And (SkJ Bool.true G A Sk.unit)
                                                       (SkJ Bool.false (List.cons Sk (skel A) G) t sb)))]
                         (skj_beta_core r A t u G s s1 sb
                           (And.left hsb) (And.right (And.right hsb)) (And.right hs1) hn)))))))
