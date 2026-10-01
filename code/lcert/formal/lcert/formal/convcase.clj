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
      subst1 / substL.  hd_skj assembles every Hd constructor: the
      contractum stays skeleton-typed at the redex's skeleton, and nbr.
      caseLb is lookup (skj_nthB, nbr_nthB); tTT and tTF are impossible.

  §3  A step at a path preserves ⟦·⟧ and skOf (Lemma 3.2).  StepPack is the
      motive.  The empty path is step_nil_fl.  A type former is not a
      skeleton-typed term, and a childless term has no cons-path.  At a
      child, the index is split by Nat.rec (skeletons.clj's
      child-index-split): a child the denotation ignores is den_ig, and a
      child it reads at a fixed skeleton is a congruence (den_succ_cong,
      den_coe_*, den_ite_*).  skOf is Eq.refl when the constructor does not
      read that child, the induction hypothesis when it reads skOf of the
      child (ite's then-branch), and sk_abort_A when it reads the child's
      skeleton.  Bare rfl inside the recursor leaves a universe hole;
      Eq.refl$1 does not.

  The V chain, F_conv and conv_all are added as they are checked.
  conv_all is ConvAll (lemma36.clj); Lemma_3_6_holds, Theorem_1_holds and
  Corollary_3_7_holds are the paper statements."
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

;; ite tt t e ⇝ t, and ite ff t e ⇝ e.  Both branches are typed at the
;; redex's skeleton (inv_ite); nbr of the redex projects onto the branch.
(thm skj_iteT [t :- Exp, e :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.ite Exp.tt t e) s),
               hn :- (Eq Bool (nbr (Exp.ite Exp.tt t e)) Bool.true)]
  (And (SkJ Bool.false G t s) (Eq Bool (nbr t) Bool.true))
  (exact (And.intro
           (And.left (And.right (And.right (inv_ite Bool.false G Exp.tt t e s hj))))
           (nbr_ite_t Exp.tt t e Bool.false hn))))

(thm skj_iteF [t :- Exp, e :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.ite Exp.ff t e) s),
               hn :- (Eq Bool (nbr (Exp.ite Exp.ff t e)) Bool.true)]
  (And (SkJ Bool.false G e s) (Eq Bool (nbr e) Bool.true))
  (exact (And.intro
           (And.right (And.right (And.right (inv_ite Bool.false G Exp.ff t e s hj))))
           (nbr_ite_e Exp.ff t e Bool.false hn))))

;; elimB selects a branch typed at skel P, and the redex is typed at that
;; same skeleton.  The equality is substituted so the contractum is typed at s.
(thm skj_elimT [P :- Exp, t :- Exp, e :- Exp, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.elimB P Exp.tt t e) s),
                hn :- (Eq Bool (nbr (Exp.elimB P Exp.tt t e)) Bool.true)]
  (And (SkJ Bool.false G t s) (Eq Bool (nbr t) Bool.true))
  (have hi (And (Eq Bool Bool.false Bool.false)
                (And (Eq Sk s (skel P))
                  (And (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit)
                    (And (SkJ Bool.false G Exp.tt Sk.bool)
                      (And (SkJ Bool.false G t (skel P))
                        (SkJ Bool.false G e (skel P)))))))
        (inv_elimB Bool.false G P Exp.tt t e s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (subst he)
  (exact (And.intro (And.left (And.right (And.right (And.right (And.right hi)))))
                    (nbr_elimB_t P Exp.tt t e Bool.false hn))))

(thm skj_elimF [P :- Exp, t :- Exp, e :- Exp, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.elimB P Exp.ff t e) s),
                hn :- (Eq Bool (nbr (Exp.elimB P Exp.ff t e)) Bool.true)]
  (And (SkJ Bool.false G e s) (Eq Bool (nbr e) Bool.true))
  (have hi (And (Eq Bool Bool.false Bool.false)
                (And (Eq Sk s (skel P))
                  (And (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit)
                    (And (SkJ Bool.false G Exp.ff Sk.bool)
                      (And (SkJ Bool.false G t (skel P))
                        (SkJ Bool.false G e (skel P)))))))
        (inv_elimB Bool.false G P Exp.ff t e s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (subst he)
  (exact (And.intro (And.right (And.right (And.right (And.right (And.right hi)))))
                    (nbr_elimB_e P Exp.ff t e Bool.false hn))))

;; recN at zero returns the base, typed at skel P.
(thm skj_recNZ [P :- Exp, z :- Exp, st :- Exp, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.recN P z st Exp.zero) s),
                hn :- (Eq Bool (nbr (Exp.recN P z st Exp.zero)) Bool.true)]
  (And (SkJ Bool.false G z s) (Eq Bool (nbr z) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.recN P z st Exp.zero) s)
        (inv_recN Bool.false G P z st Exp.zero s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (subst he)
  (exact (And.intro (And.left (And.right (And.right (And.right hi))))
                    (nbr_recN_z P z st Exp.zero Bool.false hn))))

;; recN at a successor substitutes the recursive result and the predecessor
;; into the step.  The smaller recursor is typed by re-applying sRecN; nbr of
;; its four children is the redex's nbr with the successor peeled.
(thm skj_recNS [P :- Exp, z :- Exp, st :- Exp, m :- Exp, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.recN P z st (Exp.succ m)) s),
                hn :- (Eq Bool (nbr (Exp.recN P z st (Exp.succ m))) Bool.true)]
  (And (SkJ Bool.false G (substL (List.cons Exp (Exp.recN P z st m) (List.cons Exp m (List.nil Exp))) st) s)
       (Eq Bool (nbr (substL (List.cons Exp (Exp.recN P z st m) (List.cons Exp m (List.nil Exp))) st)) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.recN P z st (Exp.succ m)) s)
        (inv_recN Bool.false G P z st (Exp.succ m) s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (have hm (SkJ Bool.false G m Sk.nat)
        (And.right (And.right (inv_succ Bool.false G m Sk.nat
                    (And.right (And.right (And.right (And.right (And.right hi)))))))))
  (have hnm (Eq Bool (nbr m) Bool.true)
        (nbr_succ_n m Bool.false (nbr_recN_n P z st (Exp.succ m) Bool.false hn)))
  (have hkm (Eq (Option Sk) (skOf G m) (Option.some Sk Sk.nat))
        (skOf_ok G m Sk.nat hm hnm))
  (have hP (SkJ Bool.true (List.cons Sk Sk.nat G) P Sk.unit)
        (And.left (And.right (And.right hi))))
  (have hz (SkJ Bool.false G z (skel P))
        (And.left (And.right (And.right (And.right hi)))))
  (have hst (SkJ Bool.false (sk2 (skel P) Sk.nat G) st (skel P))
        (And.left (And.right (And.right (And.right (And.right hi))))))
  (have hr (SkJ Bool.false G (Exp.recN P z st m) (skel P))
        (SkJ.sRecN G P z st m hP hz hst hm))
  (have hnr (Eq Bool (nbr (Exp.recN P z st m)) Bool.true)
        (andb_intro ((nbrF P) Bool.false)
          (Bool.and ((nbrF z) Bool.false) (Bool.and ((nbrF st) Bool.false) ((nbrF m) Bool.false)))
          (nbr_recN_P P z st (Exp.succ m) Bool.false hn)
          (andb_intro ((nbrF z) Bool.false)
            (Bool.and ((nbrF st) Bool.false) ((nbrF m) Bool.false))
            (nbr_recN_z P z st (Exp.succ m) Bool.false hn)
            (andb_intro ((nbrF st) Bool.false) ((nbrF m) Bool.false)
              (nbr_recN_s P z st (Exp.succ m) Bool.false hn) hnm))))
  (have hkr (Eq (Option Sk) (skOf G (Exp.recN P z st m)) (Option.some Sk (skel P)))
        (skOf_ok G (Exp.recN P z st m) (skel P) hr hnr))
  (subst he)
  (exact (And.intro
           (skj_substL Bool.false G
             (List.cons Exp (Exp.recN P z st m) (List.cons Exp m (List.nil Exp)))
             (List.cons Sk (skel P) (List.cons Sk Sk.nat (List.nil Sk)))
             (And.intro hr (And.intro hkr (And.intro hm (And.intro hkm (Eq.refl$1 (List.nil Sk))))))
             st (skel P) hst)
           (nbr_substL (List.cons Exp (Exp.recN P z st m) (List.cons Exp m (List.nil Exp))) st
             (And.intro hnr (And.intro hnm True.intro))
             (nbr_recN_s P z st (Exp.succ m) Bool.false hn)))))

;; recS at a leaf substitutes the label into the leaf branch.
(thm skj_recSL [P :- Exp, tl :- Exp, tn :- Exp, x :- Exp, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.recS P tl tn (Exp.sleaf x)) s),
                hn :- (Eq Bool (nbr (Exp.recS P tl tn (Exp.sleaf x))) Bool.true)]
  (And (SkJ Bool.false G (subst1 x tl) s) (Eq Bool (nbr (subst1 x tl)) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.recS P tl tn (Exp.sleaf x)) s)
        (inv_recS Bool.false G P tl tn (Exp.sleaf x) s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (have hx (SkJ Bool.false G x Sk.lbl)
        (And.right (And.right (inv_sleaf Bool.false G x Sk.syn
                    (And.right (And.right (And.right (And.right (And.right hi)))))))))
  (have hnx (Eq Bool (nbr x) Bool.true)
        (nbr_sleaf_a x Bool.false (nbr_recS_c P tl tn (Exp.sleaf x) Bool.false hn)))
  (have hk (Eq (Option Sk) (skOf G x) (Option.some Sk Sk.lbl))
        (skOf_ok G x Sk.lbl hx hnx))
  (have htl (SkJ Bool.false (List.cons Sk Sk.lbl G) tl (skel P))
        (And.left (And.right (And.right (And.right hi)))))
  (subst he)
  (exact (And.intro
           (skj_subst1 G Sk.lbl tl (skel P) htl x hx hk)
           (nbr_subst1 x tl hnx (nbr_recS_tl P tl tn (Exp.sleaf x) Bool.false hn)))))

;; prn (leaf x) ⇝ sleaf x.  The redex is typed at Syn; the label is typed at Lbl.
(thm skj_prnL [x :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.prn (Exp.leaf x)) s),
               hn :- (Eq Bool (nbr (Exp.prn (Exp.leaf x))) Bool.true)]
  (And (SkJ Bool.false G (Exp.sleaf x) s) (Eq Bool (nbr (Exp.sleaf x)) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.prn (Exp.leaf x)) s) (inv_prn Bool.false G (Exp.leaf x) s hj))
  (have he (Eq Sk s Sk.syn) (And.left (And.right hi)))
  (have hleaf (InvSkJ Bool.false G (Exp.leaf x) Sk.cert) (inv_leaf Bool.false G x Sk.cert (And.right (And.right hi))))
  (have hx (SkJ Bool.false G x Sk.lbl) (And.right (And.right hleaf)))
  (have hnx (Eq Bool (nbr x) Bool.true)
        (nbr_leaf_a x Bool.false (nbr_prn_r (Exp.leaf x) Bool.false hn)))
  (subst he)
  (exact (And.intro (SkJ.sSleaf G x hx) hnx)))

;; prn (node …) ⇝ snode x (prn r1) (prn r2).  Each recursive print is a code
;; because its argument is a certificate (sPrn).
(thm skj_prnN [d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.prn (Exp.node d x r1 r2)) s),
               hn :- (Eq Bool (nbr (Exp.prn (Exp.node d x r1 r2))) Bool.true)]
  (And (SkJ Bool.false G (Exp.snode x (Exp.prn r1) (Exp.prn r2)) s)
       (Eq Bool (nbr (Exp.snode x (Exp.prn r1) (Exp.prn r2))) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.prn (Exp.node d x r1 r2)) s) (inv_prn Bool.false G (Exp.node d x r1 r2) s hj))
  (have he (Eq Sk s Sk.syn) (And.left (And.right hi)))
  (have hc (InvSkJ Bool.false G (Exp.node d x r1 r2) Sk.cert)
        (inv_node Bool.false G d x r1 r2 Sk.cert (And.right (And.right hi))))
  (have hnn (Eq Bool (nbr (Exp.node d x r1 r2)) Bool.true)
        (nbr_prn_r (Exp.node d x r1 r2) Bool.false hn))
  (have hx (SkJ Bool.false G x Sk.lbl) (And.left (And.right (And.right (And.right hc)))))
  (have h1 (SkJ Bool.false G r1 Sk.cert) (And.left (And.right (And.right (And.right (And.right hc))))))
  (have h2 (SkJ Bool.false G r2 Sk.cert) (And.right (And.right (And.right (And.right (And.right hc))))))
  (have hnx (Eq Bool (nbr x) Bool.true) (nbr_node_a d x r1 r2 Bool.false hnn))
  (have hn1 (Eq Bool (nbr r1) Bool.true) (nbr_node_r1 d x r1 r2 Bool.false hnn))
  (have hn2 (Eq Bool (nbr r2) Bool.true) (nbr_node_r2 d x r1 r2 Bool.false hnn))
  (subst he)
  (exact (And.intro
           (SkJ.sSnode G x (Exp.prn r1) (Exp.prn r2) hx (SkJ.sPrn G r1 h1) (SkJ.sPrn G r2 h2))
           (andb_intro ((nbrF x) Bool.false)
             (Bool.and ((nbrF (Exp.prn r1)) Bool.false) ((nbrF (Exp.prn r2)) Bool.false))
             hnx
             (andb_intro ((nbrF (Exp.prn r1)) Bool.false) ((nbrF (Exp.prn r2)) Bool.false) hn1 hn2)))))

;; δ's contractum is tt or ff, both typed at Bool.  chk is typed at Bool.
(thm skj_boolExp [b :- Bool, G :- (List Sk)]
  (And (SkJ Bool.false G (boolExp b) Sk.bool) (Eq Bool (nbr (boolExp b)) Bool.true))
  (cases b)
  (exact (And.intro (SkJ.sFF G) rfl))
  (exact (And.intro (SkJ.sTT G) rfl)))

(thm skj_delta [c :- Exp, d :- Exp, b :- Bool, G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.chk c d) s)]
  (And (SkJ Bool.false G (boolExp b) s) (Eq Bool (nbr (boolExp b)) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.chk c d) s) (inv_chk Bool.false G c d s hj))
  (have he (Eq Sk s Sk.bool) (And.left (And.right hi)))
  (subst he)
  (exact (skj_boolExp b G)))

;; β-let, once the pair has been inverted.  var 0 is y and var 1 is x
;; (substL's list order), matching sk2 s2 s1 G.
(thm skj_betaLet_at [C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, s2 :- Sk,
                     rr :- U, AA :- Exp, BB :- Exp,
                     hs :- (Eq Sk s (skel C)),
                     ht :- (SkJ Bool.false (sk2 s2 s1 G) t (skel C)),
                     hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true),
                     hBB :- (And (Eq Exp S (Exp.tSig rr AA BB))
                              (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                (And (SkJ Bool.true G S Sk.unit)
                                  (And (SkJ Bool.false G x (skel AA))
                                    (SkJ Bool.false G y (skel BB))))))]
  (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
       (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
  (have hx (SkJ Bool.false G x (skel AA)) (And.left (And.right (And.right (And.right hBB)))))
  (have hy (SkJ Bool.false G y (skel BB)) (And.right (And.right (And.right (And.right hBB)))))
  (have hsk (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB))) (And.left (And.right hBB)))
  (have hpi (And (Eq Sk s1 (skel AA)) (Eq Sk s2 (skel BB))) (prod_inj s1 s2 (skel AA) (skel BB) hsk))
  (have h1 (Eq Sk s1 (skel AA)) (And.left hpi))
  (have h2 (Eq Sk s2 (skel BB)) (And.right hpi))
  (have hrest (Eq Bool (Bool.and ((nbrF (Exp.pair S x y)) Bool.false) ((nbrF t) Bool.false)) Bool.true)
        (band_right ((nbrF C) Bool.false) (Bool.and ((nbrF (Exp.pair S x y)) Bool.false) ((nbrF t) Bool.false)) hn))
  (have hpn (Eq Bool ((nbrF (Exp.pair S x y)) Bool.false) Bool.true)
        (band_left ((nbrF (Exp.pair S x y)) Bool.false) ((nbrF t) Bool.false) hrest))
  (have hxy (Eq Bool (Bool.and ((nbrF x) Bool.false) ((nbrF y) Bool.false)) Bool.true)
        (band_right ((nbrF S) Bool.false) (Bool.and ((nbrF x) Bool.false) ((nbrF y) Bool.false)) hpn))
  (have hnx (Eq Bool ((nbrF x) Bool.false) Bool.true) (band_left ((nbrF x) Bool.false) ((nbrF y) Bool.false) hxy))
  (have hny (Eq Bool ((nbrF y) Bool.false) Bool.true) (band_right ((nbrF x) Bool.false) ((nbrF y) Bool.false) hxy))
  (have hkx (Eq (Option Sk) (skOf G x) (Option.some Sk (skel AA))) (skOf_ok G x (skel AA) hx hnx))
  (have hky (Eq (Option Sk) (skOf G y) (Option.some Sk (skel BB))) (skOf_ok G y (skel BB) hy hny))
  (have hnt (Eq Bool (nbr t) Bool.true) (band_right ((nbrF (Exp.pair S x y)) Bool.false) ((nbrF t) Bool.false) hrest))
  (subst h1)
  (subst h2)
  (subst hs)
  (exact (And.intro
           (skj_substL Bool.false G
             (List.cons Exp y (List.cons Exp x (List.nil Exp)))
             (List.cons Sk (skel BB) (List.cons Sk (skel AA) (List.nil Sk)))
             (And.intro hy (And.intro hky (And.intro hx (And.intro hkx (Eq.refl$1 (List.nil Sk))))))
             t (skel C) ht)
           (nbr_substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t
             (And.intro hny (And.intro hnx True.intro)) hnt))))

;; The pair inside a β-let is a Σ, so its components are typed at skel AA and
;; skel BB.  The three exists are the universe and the two components.
(thm skj_betaLet_core [C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk, s1 :- Sk, s2 :- Sk,
                       hs :- (Eq Sk s (skel C)),
                       hp :- (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2)),
                       ht :- (SkJ Bool.false (sk2 s2 s1 G) t (skel C)),
                       hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true)]
  (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
       (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
  (have hpair (And (Eq Bool Bool.false Bool.false)
                   (Exists (fn [rr :- U]
                     (Exists (fn [AA :- Exp]
                       (Exists (fn [BB :- Exp]
                         (And (Eq Exp S (Exp.tSig rr AA BB))
                           (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                             (And (SkJ Bool.true G S Sk.unit)
                               (And (SkJ Bool.false G x (skel AA))
                                 (SkJ Bool.false G y (skel BB)))))))))))))
        (inv_pair Bool.false G S x y (Sk.prod s1 s2) hp))
  (exact (exU (fn [rr :- U] (Exists (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                   (And (Eq Exp S (Exp.tSig rr AA BB))
                     (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                       (And (SkJ Bool.true G S Sk.unit)
                         (And (SkJ Bool.false G x (skel AA))
                           (SkJ Bool.false G y (skel BB)))))))))))
               (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
                    (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
               (And.right hpair)
               (fn [rr :- U, hrr :- (Exists (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                      (And (Eq Exp S (Exp.tSig rr AA BB))
                        (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                          (And (SkJ Bool.true G S Sk.unit)
                            (And (SkJ Bool.false G x (skel AA))
                              (SkJ Bool.false G y (skel BB))))))))))]
                 (exExpC (fn [AA :- Exp] (Exists (fn [BB :- Exp]
                          (And (Eq Exp S (Exp.tSig rr AA BB))
                            (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                              (And (SkJ Bool.true G S Sk.unit)
                                (And (SkJ Bool.false G x (skel AA))
                                  (SkJ Bool.false G y (skel BB)))))))))
                       (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
                            (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
                       hrr
                       (fn [AA :- Exp, hAA :- (Exists (fn [BB :- Exp]
                              (And (Eq Exp S (Exp.tSig rr AA BB))
                                (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                  (And (SkJ Bool.true G S Sk.unit)
                                    (And (SkJ Bool.false G x (skel AA))
                                      (SkJ Bool.false G y (skel BB))))))))]
                         (exExpC (fn [BB :- Exp]
                                 (And (Eq Exp S (Exp.tSig rr AA BB))
                                   (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                     (And (SkJ Bool.true G S Sk.unit)
                                       (And (SkJ Bool.false G x (skel AA))
                                         (SkJ Bool.false G y (skel BB)))))))
                               (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
                                    (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
                               hAA
                               (fn [BB :- Exp, hBB :- (And (Eq Exp S (Exp.tSig rr AA BB))
                                                      (And (Eq Sk (Sk.prod s1 s2) (Sk.prod (skel AA) (skel BB)))
                                                        (And (SkJ Bool.true G S Sk.unit)
                                                          (And (SkJ Bool.false G x (skel AA))
                                                            (SkJ Bool.false G y (skel BB))))))]
                                 (skj_betaLet_at C S x y t G s s1 s2 rr AA BB hs ht hn hBB)))))))))

(thm skj_betaLet [C :- Exp, S :- Exp, x :- Exp, y :- Exp, t :- Exp, G :- (List Sk), s :- Sk,
                  hj :- (SkJ Bool.false G (Exp.letp C (Exp.pair S x y) t) s),
                  hn :- (Eq Bool (nbr (Exp.letp C (Exp.pair S x y) t)) Bool.true)]
  (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
       (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
  (have hlet (And (Eq Bool Bool.false Bool.false)
                  (And (Eq Sk s (skel C))
                    (Exists (fn [s1 :- Sk]
                      (Exists (fn [s2 :- Sk]
                        (And (SkJ Bool.true G C Sk.unit)
                          (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                            (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))))))))
        (inv_letp Bool.false G C (Exp.pair S x y) t s hj))
  (have hs (Eq Sk s (skel C)) (And.left (And.right hlet)))
  (exact (exSk (fn [s1 :- Sk] (Exists (fn [s2 :- Sk]
                   (And (SkJ Bool.true G C Sk.unit)
                     (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                       (SkJ Bool.false (sk2 s2 s1 G) t (skel C)))))))
               (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
                    (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
               (And.right (And.right hlet))
               (fn [s1 :- Sk, hs1 :- (Exists (fn [s2 :- Sk]
                      (And (SkJ Bool.true G C Sk.unit)
                        (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                          (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))))]
                 (exSk (fn [s2 :- Sk]
                         (And (SkJ Bool.true G C Sk.unit)
                           (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                             (SkJ Bool.false (sk2 s2 s1 G) t (skel C)))))
                       (And (SkJ Bool.false G (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t) s)
                            (Eq Bool (nbr (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t)) Bool.true))
                       hs1
                       (fn [s2 :- Sk, hs2 :- (And (SkJ Bool.true G C Sk.unit)
                                             (And (SkJ Bool.false G (Exp.pair S x y) (Sk.prod s1 s2))
                                               (SkJ Bool.false (sk2 s2 s1 G) t (skel C))))]
                         (skj_betaLet_core C S x y t G s s1 s2 hs
                           (And.left (And.right hs2)) (And.right (And.right hs2)) hn)))))))

;; recS at a node substitutes five terms, de Bruijn order: recursive right
;; result, recursive left result, right code, left code, label.
(thm skj_recSN [P :- Exp, tl :- Exp, tn :- Exp, x :- Exp, c1 :- Exp, c2 :- Exp,
                G :- (List Sk), s :- Sk,
                hj :- (SkJ Bool.false G (Exp.recS P tl tn (Exp.snode x c1 c2)) s),
                hn :- (Eq Bool (nbr (Exp.recS P tl tn (Exp.snode x c1 c2))) Bool.true)]
  (And (SkJ Bool.false G
         (substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
            (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn) s)
       (Eq Bool (nbr (substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
            (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn)) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.recS P tl tn (Exp.snode x c1 c2)) s)
        (inv_recS Bool.false G P tl tn (Exp.snode x c1 c2) s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (have hP (SkJ Bool.true (List.cons Sk Sk.syn G) P Sk.unit) (And.left (And.right (And.right hi))))
  (have htl (SkJ Bool.false (List.cons Sk Sk.lbl G) tl (skel P))
        (And.left (And.right (And.right (And.right hi)))))
  (have htn (SkJ Bool.false (sk2 (skel P) (skel P) (sk2 Sk.syn Sk.syn (List.cons Sk Sk.lbl G))) tn (skel P))
        (And.left (And.right (And.right (And.right (And.right hi))))))
  (have hc (InvSkJ Bool.false G (Exp.snode x c1 c2) Sk.syn)
        (inv_snode Bool.false G x c1 c2 Sk.syn (And.right (And.right (And.right (And.right (And.right hi)))))))
  (have hnc (Eq Bool (nbr (Exp.snode x c1 c2)) Bool.true)
        (nbr_recS_c P tl tn (Exp.snode x c1 c2) Bool.false hn))
  (have hx (SkJ Bool.false G x Sk.lbl) (And.left (And.right (And.right hc))))
  (have h1 (SkJ Bool.false G c1 Sk.syn) (And.left (And.right (And.right (And.right hc)))))
  (have h2 (SkJ Bool.false G c2 Sk.syn) (And.right (And.right (And.right (And.right hc)))))
  (have hnx (Eq Bool (nbr x) Bool.true) (nbr_snode_a x c1 c2 Bool.false hnc))
  (have hn1 (Eq Bool (nbr c1) Bool.true) (nbr_snode_c1 x c1 c2 Bool.false hnc))
  (have hn2 (Eq Bool (nbr c2) Bool.true) (nbr_snode_c2 x c1 c2 Bool.false hnc))
  (have hkx (Eq (Option Sk) (skOf G x) (Option.some Sk Sk.lbl)) (skOf_ok G x Sk.lbl hx hnx))
  (have hk1 (Eq (Option Sk) (skOf G c1) (Option.some Sk Sk.syn)) (skOf_ok G c1 Sk.syn h1 hn1))
  (have hk2 (Eq (Option Sk) (skOf G c2) (Option.some Sk Sk.syn)) (skOf_ok G c2 Sk.syn h2 hn2))
  (have hr1 (SkJ Bool.false G (Exp.recS P tl tn c1) (skel P)) (SkJ.sRecS G P tl tn c1 hP htl htn h1))
  (have hr2 (SkJ Bool.false G (Exp.recS P tl tn c2) (skel P)) (SkJ.sRecS G P tl tn c2 hP htl htn h2))
  (have hfour (Eq Bool (Bool.and ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false))) Bool.true)
        (band_right ((nbrF P) Bool.false)
          (Bool.and ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false))) hn))
  (have hnt (Eq Bool (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false)) Bool.true)
        (band_right ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false)) hfour))
  (have hnp (Eq Bool (nbr P) Bool.true) (band_left ((nbrF P) Bool.false)
        (Bool.and ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false))) hn))
  (have hntl (Eq Bool (nbr tl) Bool.true)
        (band_left ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false)) hfour))
  (have hntn (Eq Bool (nbr tn) Bool.true)
        (band_left ((nbrF tn) Bool.false) ((nbrF (Exp.snode x c1 c2)) Bool.false) hnt))
  (have hnr1 (Eq Bool (nbr (Exp.recS P tl tn c1)) Bool.true)
        (andb_intro ((nbrF P) Bool.false)
          (Bool.and ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF c1) Bool.false)))
          hnp (andb_intro ((nbrF tl) Bool.false)
            (Bool.and ((nbrF tn) Bool.false) ((nbrF c1) Bool.false))
            hntl (andb_intro ((nbrF tn) Bool.false) ((nbrF c1) Bool.false) hntn hn1))))
  (have hnr2 (Eq Bool (nbr (Exp.recS P tl tn c2)) Bool.true)
        (andb_intro ((nbrF P) Bool.false)
          (Bool.and ((nbrF tl) Bool.false) (Bool.and ((nbrF tn) Bool.false) ((nbrF c2) Bool.false)))
          hnp (andb_intro ((nbrF tl) Bool.false)
            (Bool.and ((nbrF tn) Bool.false) ((nbrF c2) Bool.false))
            hntl (andb_intro ((nbrF tn) Bool.false) ((nbrF c2) Bool.false) hntn hn2))))
  (subst he)
  (exact (And.intro
           (skj_substL Bool.false G
             (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
               (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp))))))
             (List.cons Sk (skel P) (List.cons Sk (skel P)
               (List.cons Sk Sk.syn (List.cons Sk Sk.syn (List.cons Sk Sk.lbl (List.nil Sk))))))
             (And.intro hr2 (And.intro rfl (And.intro hr1 (And.intro rfl
               (And.intro h2 (And.intro hk2 (And.intro h1 (And.intro hk1
                 (And.intro hx (And.intro hkx (Eq.refl$1 (List.nil Sk))))))))))))
             tn (skel P) htn)
           (nbr_substL
             (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
               (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp))))))
             tn
             (And.intro hnr2 (And.intro hnr1 (And.intro hn2 (And.intro hn1 (And.intro hnx True.intro)))))
             hntn))))

;; itR on a leaf is application of the leaf step to the label.
(thm skj_itRL [X :- Exp, g :- Exp, h :- Exp, x :- Exp, G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.itR X g h (Exp.leaf x)) s),
               hn :- (Eq Bool (nbr (Exp.itR X g h (Exp.leaf x))) Bool.true)]
  (And (SkJ Bool.false G (Exp.app g x) s) (Eq Bool (nbr (Exp.app g x)) Bool.true))
  (have hit (And (Eq Bool Bool.false Bool.false)
                 (And (Eq Sk s (skel X))
                   (And (SkJ Bool.true G X Sk.unit)
                     (And (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))
                       (And (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
                         (SkJ Bool.false G (Exp.leaf x) Sk.cert))))))
        (inv_itR Bool.false G X g h (Exp.leaf x) s hj))
  (have he (Eq Sk s (skel X)) (And.left (And.right hit)))
  (have hg (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))
        (And.left (And.right (And.right (And.right hit)))))
  (have hleaf (InvSkJ Bool.false G (Exp.leaf x) Sk.cert)
        (inv_leaf Bool.false G x Sk.cert (And.right (And.right (And.right (And.right (And.right hit)))))))
  (have hx (SkJ Bool.false G x Sk.lbl) (And.right (And.right hleaf)))
  (have hnx (Eq Bool (nbr x) Bool.true)
        (nbr_leaf_a x Bool.false (nbr_itR_r X g h (Exp.leaf x) Bool.false hn)))
  (have hng (Eq Bool (nbr g) Bool.true) (nbr_itR_g X g h (Exp.leaf x) Bool.false hn))
  (subst he)
  (exact (And.intro
           (SkJ.sApp G g x Sk.lbl (skel X) hg hx)
           (andb_intro ((nbrF g) Bool.false) ((nbrF x) Bool.false) hng hnx))))

;; itR on a node is the node step applied to the token, the label, and the
;; two recursive results.  Each application peels one arrow off h's skeleton.
(thm skj_itRN [X :- Exp, g :- Exp, h :- Exp, d :- Exp, x :- Exp, r1 :- Exp, r2 :- Exp,
               G :- (List Sk), s :- Sk,
               hj :- (SkJ Bool.false G (Exp.itR X g h (Exp.node d x r1 r2)) s),
               hn :- (Eq Bool (nbr (Exp.itR X g h (Exp.node d x r1 r2))) Bool.true)]
  (And (SkJ Bool.false G (Exp.app (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Exp.itR X g h r2)) s)
       (Eq Bool (nbr (Exp.app (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Exp.itR X g h r2))) Bool.true))
  (have hit (And (Eq Bool Bool.false Bool.false)
                 (And (Eq Sk s (skel X))
                   (And (SkJ Bool.true G X Sk.unit)
                     (And (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))
                       (And (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
                         (SkJ Bool.false G (Exp.node d x r1 r2) Sk.cert))))))
        (inv_itR Bool.false G X g h (Exp.node d x r1 r2) s hj))
  (have he (Eq Sk s (skel X)) (And.left (And.right hit)))
  (have hX (SkJ Bool.true G X Sk.unit) (And.left (And.right (And.right hit))))
  (have hg (SkJ Bool.false G g (Sk.arr Sk.lbl (skel X)))
        (And.left (And.right (And.right (And.right hit)))))
  (have hh (SkJ Bool.false G h (Sk.arr Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))))
        (And.left (And.right (And.right (And.right (And.right hit))))))
  (have hc (InvSkJ Bool.false G (Exp.node d x r1 r2) Sk.cert)
        (inv_node Bool.false G d x r1 r2 Sk.cert (And.right (And.right (And.right (And.right (And.right hit)))))))
  (have hnn (Eq Bool (nbr (Exp.node d x r1 r2)) Bool.true)
        (nbr_itR_r X g h (Exp.node d x r1 r2) Bool.false hn))
  (have hd (SkJ Bool.false G d Sk.dia) (And.left (And.right (And.right hc))))
  (have hx (SkJ Bool.false G x Sk.lbl) (And.left (And.right (And.right (And.right hc)))))
  (have h1 (SkJ Bool.false G r1 Sk.cert) (And.left (And.right (And.right (And.right (And.right hc))))))
  (have h2 (SkJ Bool.false G r2 Sk.cert) (And.right (And.right (And.right (And.right (And.right hc))))))
  (have hnd (Eq Bool (nbr d) Bool.true) (nbr_node_d d x r1 r2 Bool.false hnn))
  (have hnx (Eq Bool (nbr x) Bool.true) (nbr_node_a d x r1 r2 Bool.false hnn))
  (have hn1 (Eq Bool (nbr r1) Bool.true) (nbr_node_r1 d x r1 r2 Bool.false hnn))
  (have hn2 (Eq Bool (nbr r2) Bool.true) (nbr_node_r2 d x r1 r2 Bool.false hnn))
  (have hnh (Eq Bool (nbr h) Bool.true) (nbr_itR_h X g h (Exp.node d x r1 r2) Bool.false hn))
  (have hng (Eq Bool (nbr g) Bool.true) (nbr_itR_g X g h (Exp.node d x r1 r2) Bool.false hn))
  (have hnX (Eq Bool (nbr X) Bool.true) (nbr_itR_X X g h (Exp.node d x r1 r2) Bool.false hn))
  (have hcod (SkJ Bool.false G (Exp.app h d) (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))))
        (SkJ.sApp G h d Sk.dia (Sk.arr Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X)))) hh hd))
  (have hcod2 (SkJ Bool.false G (Exp.app (Exp.app h d) x) (Sk.arr (skel X) (Sk.arr (skel X) (skel X))))
        (SkJ.sApp G (Exp.app h d) x Sk.lbl (Sk.arr (skel X) (Sk.arr (skel X) (skel X))) hcod hx))
  (have ir1 (SkJ Bool.false G (Exp.itR X g h r1) (skel X)) (SkJ.sItR G X g h r1 hX hg hh h1))
  (have ir2 (SkJ Bool.false G (Exp.itR X g h r2) (skel X)) (SkJ.sItR G X g h r2 hX hg hh h2))
  (have hcod3 (SkJ Bool.false G (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Sk.arr (skel X) (skel X)))
        (SkJ.sApp G (Exp.app (Exp.app h d) x) (Exp.itR X g h r1) (skel X) (Sk.arr (skel X) (skel X)) hcod2 ir1))
  (have n4 (Eq Bool (nbr (Exp.itR X g h r1)) Bool.true)
        (andb_intro ((nbrF X) Bool.false)
          (Bool.and ((nbrF g) Bool.false) (Bool.and ((nbrF h) Bool.false) ((nbrF r1) Bool.false)))
          hnX (andb_intro ((nbrF g) Bool.false)
            (Bool.and ((nbrF h) Bool.false) ((nbrF r1) Bool.false))
            hng (andb_intro ((nbrF h) Bool.false) ((nbrF r1) Bool.false) hnh hn1))))
  (have n5 (Eq Bool (nbr (Exp.itR X g h r2)) Bool.true)
        (andb_intro ((nbrF X) Bool.false)
          (Bool.and ((nbrF g) Bool.false) (Bool.and ((nbrF h) Bool.false) ((nbrF r2) Bool.false)))
          hnX (andb_intro ((nbrF g) Bool.false)
            (Bool.and ((nbrF h) Bool.false) ((nbrF r2) Bool.false))
            hng (andb_intro ((nbrF h) Bool.false) ((nbrF r2) Bool.false) hnh hn2))))
  (have na1 (Eq Bool (nbr (Exp.app h d)) Bool.true)
        (andb_intro ((nbrF h) Bool.false) ((nbrF d) Bool.false) hnh hnd))
  (have na2 (Eq Bool (nbr (Exp.app (Exp.app h d) x)) Bool.true)
        (andb_intro ((nbrF (Exp.app h d)) Bool.false) ((nbrF x) Bool.false) na1 hnx))
  (have na3 (Eq Bool (nbr (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1))) Bool.true)
        (andb_intro ((nbrF (Exp.app (Exp.app h d) x)) Bool.false) ((nbrF (Exp.itR X g h r1)) Bool.false) na2 n4))
  (subst he)
  (exact (And.intro
           (SkJ.sApp G (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1)) (Exp.itR X g h r2)
             (skel X) (skel X) hcod3 ir2)
           (andb_intro ((nbrF (Exp.app (Exp.app (Exp.app h d) x) (Exp.itR X g h r1))) Bool.false)
             ((nbrF (Exp.itR X g h r2)) Bool.false) na3 n5))))

;; A looked-up branch of a skeleton-typed bcons is typed at the list's
;; codomain.  s1 is the skeleton the inversion of sBcons gives the head; the
;; conclusion skeleton s is the codomain of the list's arrow, so arr_inj
;; identifies them.  The zero index is the head; a successor is the tail's
;; induction hypothesis.
(thm skj_bcons_pick [h :- Exp, t :- Exp, G :- (List Sk), s :- Sk, l :- Nat, b :- Exp, s1 :- Sk,
                     heq :- (Eq Sk (Sk.arr Sk.lbl s) (Sk.arr Sk.lbl s1)),
                     hh :- (SkJ Bool.false G h s1),
                     ht :- (SkJ Bool.false G t (Sk.arr Sk.lbl s1)),
                     hq :- (Eq (Option Exp) (nthB (Exp.bcons h t) l) (Option.some Exp b)),
                     ih :- (forall [lq Nat] (forall [bq Exp]
                             (=> (Eq (Option Exp) (nthB t lq) (Option.some Exp bq))
                                 (=> (SkJ Bool.false G t (Sk.arr Sk.lbl s))
                                     (SkJ Bool.false G bq s)))))]
  (SkJ Bool.false G b s)
  (have hs (Eq Sk s s1) (And.right (arr_inj Sk.lbl s Sk.lbl s1 heq)))
  (cases l)
  (have hb (Eq Exp h b) (some_inj h b (Eq.trans (Eq.symm (nthB.eq_25 h t)) hq)))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (SkJ Bool.false G e s)) hb)
                (Eq.mp (congrArg (fn [sk :- Sk] (SkJ Bool.false G h sk)) (Eq.symm hs)) hh)))
  (exact (ih n b (Eq.trans (Eq.symm (nthB.eq_26 h t n)) hq)
             (Eq.mp (congrArg (fn [sk :- Sk] (SkJ Bool.false G t (Sk.arr Sk.lbl sk))) (Eq.symm hs)) ht))))

;; The same lookup, for nbr at flag true.  A bcons is nbr at true only when
;; its head is nbr at false and its tail is nbr at true (conv.clj), which is
;; exactly what the zero and successor cases need.
(thm nbr_bcons_pick [h :- Exp, t :- Exp, l :- Nat, b :- Exp,
                     hq :- (Eq (Option Exp) (nthB (Exp.bcons h t) l) (Option.some Exp b)),
                     hn :- (Eq Bool ((nbrF (Exp.bcons h t)) Bool.true) Bool.true),
                     ih :- (forall [lq Nat] (forall [bq Exp]
                             (=> (Eq (Option Exp) (nthB t lq) (Option.some Exp bq))
                                 (=> (Eq Bool ((nbrF t) Bool.true) Bool.true)
                                     (Eq Bool (nbr bq) Bool.true)))))]
  (Eq Bool (nbr b) Bool.true)
  (cases l)
  (have hb (Eq Exp h b) (some_inj h b (Eq.trans (Eq.symm (nthB.eq_25 h t)) hq)))
  (exact (Eq.mp (congrArg (fn [e :- Exp] (Eq Bool (nbr e) Bool.true)) hb)
                (nbr_bcons_h h t Bool.true hn)))
  (exact (ih n b (Eq.trans (Eq.symm (nthB.eq_26 h t n)) hq) (nbr_bcons_t h t Bool.true hn))))

;; Lookup typing. nthB of a non-bcons is none, so a successful lookup is a
;; bcons branch, typed at the codomain.
(let [refute (fn [idx fields]
               (let [eqn (symbol (str "nthB.eq_" (if (< idx 24) (inc idx) (+ idx 2))))]
                 ['(intro lq bq hq hj)
                  (list 'exact
                    (list 'False.elim$0
                      (list 'none_ne_someE 'bq
                        (list 'Eq.trans
                          (list 'Eq.symm (apply list eqn (cons 'lq (map first fields))))
                          'hq))))]))]
  (a/prove-theorem 'skj_nthB
    '[G :- (List Sk), outsk :- Sk, bs :- Exp]
    '(forall [l Nat] (forall [b Exp]
       (=> (Eq (Option Exp) (nthB bs l) (Option.some Exp b))
           (=> (SkJ Bool.false G bs (Sk.arr Sk.lbl outsk))
               (SkJ Bool.false G b outsk)))))
    (lv (into ['(induction bs)]
          (mapcat (fn [[idx [ctor fields]]]
                    (if (= ctor 'bcons)
                      '[(intro l b hq hj)
                        (have hc (InvSkJ Bool.false G (Exp.bcons h t) (Sk.arr Sk.lbl outsk))
                              (inv_bcons Bool.false G h t (Sk.arr Sk.lbl outsk) hj))
                        (exact (exSk
                          (fn [s1 :- Sk]
                            (And (Eq Sk (Sk.arr Sk.lbl outsk) (Sk.arr Sk.lbl s1))
                                 (And (SkJ Bool.false G h s1)
                                      (SkJ Bool.false G t (Sk.arr Sk.lbl s1)))))
                          (SkJ Bool.false G b outsk)
                          (And.right hc)
                          (fn [s1 :- Sk, hs1 :- (And (Eq Sk (Sk.arr Sk.lbl outsk) (Sk.arr Sk.lbl s1))
                                                  (And (SkJ Bool.false G h s1)
                                                       (SkJ Bool.false G t (Sk.arr Sk.lbl s1))))]
                            (skj_bcons_pick h t G outsk l b s1 (And.left hs1)
                              (And.left (And.right hs1)) (And.right (And.right hs1)) hq ih_t))))]
                      (refute idx fields)))
                  (map-indexed vector exp-fields))))))

;; And the looked-up branch is nbr, when the list is nbr at flag true.
(let [refute (fn [idx fields]
               (let [eqn (symbol (str "nthB.eq_" (if (< idx 24) (inc idx) (+ idx 2))))]
                 ['(intro lq bq hq hn)
                  (list 'exact
                    (list 'False.elim$0
                      (list 'none_ne_someE 'bq
                        (list 'Eq.trans
                          (list 'Eq.symm (apply list eqn (cons 'lq (map first fields))))
                          'hq))))]))]
  (a/prove-theorem 'nbr_nthB
    '[bs :- Exp]
    '(forall [l Nat] (forall [b Exp]
       (=> (Eq (Option Exp) (nthB bs l) (Option.some Exp b))
           (=> (Eq Bool ((nbrF bs) Bool.true) Bool.true)
               (Eq Bool (nbr b) Bool.true)))))
    (lv (into ['(induction bs)]
          (mapcat (fn [[idx [ctor fields]]]
                    (if (= ctor 'bcons)
                      '[(intro l b hq hn)
                        (exact (nbr_bcons_pick h t l b hq hn ih_t))]
                      (refute idx fields)))
                  (map-indexed vector exp-fields))))))

;; Lemma 3.2, Hd.caseLb, for skeleton typing and nbr.  The branch list is
;; typed at Lbl → skel P (inv_caseL) and is nbr at flag true (nbr_caseL_bs);
;; skj_nthB / nbr_nthB read the same branch nthB does.
(thm skj_caseLb [P :- Exp, l :- Nat, bs :- Exp, b :- Exp,
                 h :- (Eq (Option Exp) (nthB bs l) (Option.some Exp b)),
                 G :- (List Sk), s :- Sk,
                 hj :- (SkJ Bool.false G (Exp.caseL P (Exp.lbl l) bs) s),
                 hn :- (Eq Bool (nbr (Exp.caseL P (Exp.lbl l) bs)) Bool.true)]
  (And (SkJ Bool.false G b s) (Eq Bool (nbr b) Bool.true))
  (have hi (InvSkJ Bool.false G (Exp.caseL P (Exp.lbl l) bs) s)
        (inv_caseL Bool.false G P (Exp.lbl l) bs s hj))
  (have he (Eq Sk s (skel P)) (And.left (And.right hi)))
  (have hbs (SkJ Bool.false G bs (Sk.arr Sk.lbl (skel P)))
        (And.right (And.right (And.right (And.right hi)))))
  (have hnbs (Eq Bool ((nbrF bs) Bool.true) Bool.true)
        (nbr_caseL_bs P (Exp.lbl l) bs Bool.false hn))
  (subst he)
  (exact (And.intro (skj_nthB G (skel P) bs l b h hbs) (nbr_nthB bs l b h hnbs))))

;; Every head step preserves skeleton typing and nbr.  The skeleton parameter
;; is `out`, not `s`: Hd.recNZ and Hd.recNS bind a field named s.  tTT and
;; tTF are impossible for a term derivation (skj_not_tT).
(thm hd_skj [chkf :- (=> Code Code Bool),
             re :- Exp, co :- Exp, hder :- (Hd chkf re co),
             G :- (List Sk), out :- Sk,
             hj :- (SkJ Bool.false G re out),
             hn :- (Eq Bool (nbr re) Bool.true)]
  (And (SkJ Bool.false G co out) (Eq Bool (nbr co) Bool.true))
  (cases hder)
  (exact (skj_beta r A t u G out hj hn))
  (exact (skj_betaLet C S x y t G out hj hn))
  (exact (skj_iteT t e G out hj hn))
  (exact (skj_iteF t e G out hj hn))
  (exact (skj_elimT P t e G out hj hn))
  (exact (skj_elimF P t e G out hj hn))
  (exact (skj_recNZ P z s G out hj hn))
  (exact (skj_recNS P z s n G out hj hn))
  (exact (skj_caseLb P l bs b h G out hj hn))
  (exact (skj_recSL P tl tn x G out hj hn))
  (exact (skj_recSN P tl tn x c1 c2 G out hj hn))
  (exact (skj_itRL X g h x G out hj hn))
  (exact (skj_itRN X g h d x r1 r2 G out hj hn))
  (exact (skj_prnL x G out hj hn))
  (exact (skj_prnN d x r1 r2 G out hj hn))
  (exact (skj_delta c d (chkf cc dc) G out hj))
  (exact (False.elim$0 (skj_not_tT Exp.tt G out hj)))
  (exact (False.elim$0 (skj_not_tT Exp.ff G out hj))))

;; ===========================================================================
;; §3  A step at a path preserves ⟦·⟧ and skOf (Lemma 3.2)
;; ===========================================================================
;;
;; The empty path is a head step: hd_den and hd_skj, packaged by
;; step_nil_pack.  A head redex is never a branch list, so it is nbr at
;; false whenever it is nbr at the flag the position demands (nbr_hd_flag);
;; and it is never a base type (hd_not_base), so reflect's type annotation
;; cannot be the redex.

(thm step_nil_pack [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                    e :- Exp, G :- (List Sk), s :- Sk,
                    hj :- (SkJ Bool.false G e s),
                    hn :- (Eq Bool (nbr e) Bool.true),
                    r :- Exp, co :- Exp,
                    hg :- (Eq (Option Exp) (getP (List.nil Nat) e) (Option.some Exp r)),
                    hder :- (Hd chkf r co)]
  (And (Eq (Option Sk) (skOf G (setP (List.nil Nat) e co)) (skOf G e))
       (forall [cap Nat] (forall [en (HEnv G)]
         (Eq (Car s) (den chkf dec encTy cap (setP (List.nil Nat) e co) G s en)
                     (den chkf dec encTy cap e G s en)))))
  (have he (Eq Exp e r) (some_inj e r (Eq.trans (Eq.symm (getP_nil e)) hg)))
  (subst he)
  (have hco (And (SkJ Bool.false G co s) (Eq Bool (nbr co) Bool.true))
        (hd_skj chkf r co hder G s hj hn))
  (exact (And.intro
           (Eq.trans (congrArg (fn [t :- Exp] (skOf G t)) (setP_nil r co))
                     (Eq.trans (skOf_ok G co s (And.left hco) (And.right hco))
                               (Eq.symm (skOf_ok G r s hj hn))))
           (fn [cap :- Nat, en :- (HEnv G)]
             (den_step_nil chkf dec encTy cap r G s hj hn r co (getP_nil r) hder en)))))

(thm hd_not_base [chkf :- (=> Code Code Bool), re :- Exp, r2 :- Exp, hder :- (Hd chkf re r2),
                  hb :- (Eq Bool (isBaseTy re) Bool.true)]
  False
  (cases hder)
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb))
  (exact (Bool.noConfusion hb)))

(thm nbr_hd_flag [chkf :- (=> Code Code Bool), re :- Exp, r2 :- Exp, hder :- (Hd chkf re r2), fl :- Bool]
  (Eq Bool ((nbrF re) fl) ((nbrF re) Bool.false))
  (cases hder)
  (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl)
  (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl) (rfl))

;; The same pack at whatever flag the position is nbr.  The redex ignores
;; the flag, so nbr at fl is nbr at false, which step_nil_pack needs.
(thm step_nil_fl [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code),
                  fl :- Bool, e :- Exp, G :- (List Sk), s :- Sk,
                  hj :- (SkJ Bool.false G e s),
                  hn :- (Eq Bool ((nbrF e) fl) Bool.true),
                  r :- Exp, co :- Exp,
                  hg :- (Eq (Option Exp) (getP (List.nil Nat) e) (Option.some Exp r)),
                  hder :- (Hd chkf r co)]
  (And (Eq (Option Sk) (skOf G (setP (List.nil Nat) e co)) (skOf G e))
       (forall [cap Nat] (forall [en (HEnv G)]
         (Eq (Car s) (den chkf dec encTy cap (setP (List.nil Nat) e co) G s en)
                     (den chkf dec encTy cap e G s en)))))
  (have he (Eq Exp e r) (some_inj e r (Eq.trans (Eq.symm (getP_nil e)) hg)))
  (subst he)
  (have hn0 (Eq Bool (nbr r) Bool.true)
        (Eq.trans (Eq.symm (nbr_hd_flag chkf r co hder fl)) hn))
  (exact (step_nil_pack chkf dec encTy r G s hj hn0 r co (getP_nil r) hder)))

;; StepPack chkf dec encTy e: a step anywhere in an nbr skeleton-typed term
;; preserves skOf and the denotation at the typed skeleton.  The flag is the
;; one the position is nbr at (false everywhere except a branch-list tail).
(kdef StepPack
  (forall [chkf (=> Code Code Bool)] (forall [dec (=> Code (Option (Prod Nat (Prod Exp Exp))))]
    (forall [encTy (=> Exp Code)] (=> Exp Prop))))
  (fn [chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code), e :- Exp]
    (forall [fl Bool] (forall [G (List Sk)] (forall [s Sk]
      (=> (SkJ Bool.false G e s)
          (=> (Eq Bool ((nbrF e) fl) Bool.true)
              (forall [p (List Nat)] (forall [r Exp] (forall [r2 Exp]
                (=> (Eq (Option Exp) (getP p e) (Option.some Exp r))
                    (=> (Hd chkf r r2)
                        (And (Eq (Option Sk) (skOf G (setP p e r2)) (skOf G e))
                             (forall [cap Nat] (forall [en (HEnv G)]
                               (Eq (Car s) (den chkf dec encTy cap (setP p e r2) G s en)
                                           (den chkf dec encTy cap e G s en)))))))))))))))))

(def ^:private step-params
  '[chkf :- (=> Code Code Bool), dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))), encTy :- (=> Exp Code)])

(defn- ctor-term [ctor fields]
  (let [fs (map first fields)]
    (if (seq fs) (apply list (symbol (str "Exp." ctor)) fs) (symbol (str "Exp." ctor)))))

;; A type former is never skeleton-typed as a term (its rule has w = true;
;; tBrs has no rule at all).  A childless term has no cons-path.
(def ^:private type-formers
  '#{tEmpty tUnit tBool tNat tLbl tSyn tDia tR tT tPi tSig})

(defn- prove-step! [nm params concl tactics]
  (when-not (lcert.formal.base/has? nm)
    (a/prove-theorem nm (lv params) (lv concl) (lv tactics))))

(doseq [[ctor fields] exp-fields]
  (let [term (ctor-term ctor fields)
        params (into step-params (mapcat (fn [[f ty]] [f :- ty]) fields))
        nm (symbol (str "step_" ctor))]
    (cond
      (= ctor 'tBrs)
      (prove-step! nm params (list 'StepPack 'chkf 'dec 'encTy term)
        ['(intro fl G s hj hn)
         (list 'exact (list 'False.elim$0 (list 'skj_inv 'Bool.false 'G term 's 'hj)))])
      (type-formers ctor)
      (prove-step! nm params (list 'StepPack 'chkf 'dec 'encTy term)
        ['(intro fl G s hj hn)
         (list 'exact (list 'False.elim$0
                       (list 'Bool.noConfusion
                         (list 'And.left (list 'skj_inv 'Bool.false 'G term 's 'hj)))))])
      (not-any? #(= 'Exp (second %)) fields)
      (prove-step! nm params (list 'StepPack 'chkf 'dec 'encTy term)
        ['(intro fl G s hj hn p)
         '(cases p)
         '(intro r r2 hg hd)
         (list 'exact (list 'step_nil_fl 'chkf 'dec 'encTy 'fl term 'G 's 'hj 'hn 'r 'r2 'hg 'hd))
         '(intro r r2 hg hd)
         '(exact (False.elim$0 (none_ne_someE r hg)))]))))


;; A cons-path is one child index, then a path inside that child.  Nat.rec
;; peels the index: 0, 1, …, and every larger index is not a child.  The
;; skeleton is `out` and the redex is `re`, so the binders do not shadow a
;; constructor field (h1 has a field s, prn has a field r).
(defn- path-and [term idx]
  (list 'And
    (list 'Eq '(Option Sk)
      (list 'skOf 'G (list 'setP (list 'List.cons 'Nat idx 'tail) term 'co))
      (list 'skOf 'G term))
    (list 'forall '[cap Nat]
      (list 'forall '[en (HEnv G)]
        (list 'Eq '(Car out)
          (list 'den 'chkf 'dec 'encTy 'cap
            (list 'setP (list 'List.cons 'Nat idx 'tail) term 'co) 'G 'out 'en)
          (list 'den 'chkf 'dec 'encTy 'cap term 'G 'out 'en))))))

(defn- path-hyp [term idx]
  (list 'Eq '(Option Exp)
    (list 'getP (list 'List.cons 'Nat idx 'tail) term)
    '(Option.some Exp re)))

(defn- path-mot [term idx] (list '=> (path-hyp term idx) (path-and term idx)))

(defn- sucn [n v]
  (if (zero? n) v (list 'Nat.succ (sucn (dec n) v))))

(defn- index-split [term proofs]
  (let [n (count proofs)]
    (letfn [(build [i]
              (let [idx-var (if (zero? i) 'j (sucn i 'j))
                    idx0 (sucn i 0)
                    overflow (sucn (inc i) 'k)]
                (list 'Nat.rec$0
                  (list 'fn '[j :- Nat] (path-mot term idx-var))
                  (list 'fn ['hgc ':- (path-hyp term idx0)] (nth proofs i))
                  (if (< (inc i) n)
                    (list 'fn ['j ':- 'Nat, 'ihj ':- (path-mot term idx-var)]
                      (build (inc i)))
                    (list 'fn ['k ':- 'Nat,
                               'ihk ':- (path-mot term (sucn i 'k)),
                               'hgc ':- (path-hyp term overflow)]
                      (list 'False.rec$0
                        (list 'fn '[_ :- False] (path-and term overflow))
                        '(none_ne_someE re hgc))))
                  (if (zero? i) 'head 'j))))]
      (list (build 0) 'hg))))

(defn- prove! [nm params concl tactics]
  (when-not (lcert.formal.base/has? nm)
    (a/prove-theorem nm (lv params) (lv concl) (lv tactics))))

(def ^:private Pcap
  '[chkf :- (=> Code Code Bool),
    dec :- (=> Code (Option (Prod Nat (Prod Exp Exp)))),
    encTy :- (=> Exp Code), cap :- Nat])

;; Fields whose denotation clause never applies the field's own denotation.
;; Replacing one of them leaves ⟦·⟧ unchanged at every skeleton.
(def ^:private den-unread
  '{abort #{A t} elimB #{P} recN #{P} caseL #{P} recS #{P}
    node #{d} itR #{X} lam #{A} pair #{S} letp #{C}
    h1 #{r s c e1 e2} refl #{e} insp #{X}})

(doseq [[ctor fields] exp-fields
        :let [dead (get den-unread ctor)]
        [f ty] fields
        :when (and dead (= ty 'Exp) (contains? dead f))]
  (let [fq (symbol (str f "q"))
        fs (mapv first fields)
        left (apply list (symbol (str "Exp." ctor)) (replace {f fq} fs))
        right (ctor-term ctor fields)
        at (symbol (str "den_" ctor "_at"))
        params (into Pcap
                 (concat
                   (mapcat (fn [[v t]]
                             (if (= v f) [v :- t, fq :- 'Exp] [v :- t]))
                           fields)
                   '[G :- (List Sk), sk :- Sk, en :- (HEnv G)]))
        concl (list 'Eq '(Car sk)
                (list 'den 'chkf 'dec 'encTy 'cap left 'G 'sk 'en)
                (list 'den 'chkf 'dec 'encTy 'cap right 'G 'sk 'en))]
    (prove! (symbol (str "den_ig_" ctor "_" f)) params concl
      [(list 'rw [(apply list at 'chkf 'dec 'encTy 'cap (concat (replace {f fq} fs) ['G 'sk 'en]))])
       (list 'rw [(apply list at 'chkf 'dec 'encTy 'cap (concat fs ['G 'sk 'en]))])])))

;; Successor reads its argument at Nat.  The clause is coe (succ ⟦n⟧).
(prove! 'den_succ_cong
  (into Pcap '[n :- Exp, n2 :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car Sk.nat) (den chkf dec encTy cap n2 G Sk.nat en)
                                     (den chkf dec encTy cap n G Sk.nat en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.succ n2) G sk en)
                (den chkf dec encTy cap (Exp.succ n) G sk en))
  ['(rw [(den_succ_at chkf dec encTy cap n2 G sk en)])
   '(rw [(den_succ_at chkf dec encTy cap n G sk en)])
   '(change (Eq (Car sk)
      (coe Sk.nat sk (Nat.succ (den chkf dec encTy cap n2 G Sk.nat en)))
      (coe Sk.nat sk (Nat.succ (den chkf dec encTy cap n G Sk.nat en)))))
   '(exact (congrArg (fn [v :- (Car Sk.nat)] (coe Sk.nat sk (Nat.succ v))) h))])

(prove! 'den_coe_sleaf
  (into Pcap '[a :- Exp, aq :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car Sk.lbl) (den chkf dec encTy cap aq G Sk.lbl en)
                                     (den chkf dec encTy cap a G Sk.lbl en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.sleaf aq) G sk en)
                (den chkf dec encTy cap (Exp.sleaf a) G sk en))
  ['(rw [(den_sleaf_at chkf dec encTy cap aq G sk en)])
   '(rw [(den_sleaf_at chkf dec encTy cap a G sk en)])
   '(change (Eq (Car sk)
      (coe Sk.syn sk (Code.sl (den chkf dec encTy cap aq G Sk.lbl en)))
      (coe Sk.syn sk (Code.sl (den chkf dec encTy cap a G Sk.lbl en)))))
   '(exact (congrArg (fn [v :- (Car Sk.lbl)] (coe Sk.syn sk (Code.sl v))) h))])

(prove! 'den_coe_leaf
  (into Pcap '[a :- Exp, aq :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car Sk.lbl) (den chkf dec encTy cap aq G Sk.lbl en)
                                     (den chkf dec encTy cap a G Sk.lbl en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.leaf aq) G sk en)
                (den chkf dec encTy cap (Exp.leaf a) G sk en))
  ['(rw [(den_leaf_at chkf dec encTy cap aq G sk en)])
   '(rw [(den_leaf_at chkf dec encTy cap a G sk en)])
   '(change (Eq (Car sk)
      (coe Sk.cert sk (Code.sl (den chkf dec encTy cap aq G Sk.lbl en)))
      (coe Sk.cert sk (Code.sl (den chkf dec encTy cap a G Sk.lbl en)))))
   '(exact (congrArg (fn [v :- (Car Sk.lbl)] (coe Sk.cert sk (Code.sl v))) h))])

(prove! 'den_coe_prn
  (into Pcap '[r :- Exp, rq :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car Sk.cert) (den chkf dec encTy cap rq G Sk.cert en)
                                      (den chkf dec encTy cap r G Sk.cert en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.prn rq) G sk en)
                (den chkf dec encTy cap (Exp.prn r) G sk en))
  ['(rw [(den_prn_at chkf dec encTy cap rq G sk en)])
   '(rw [(den_prn_at chkf dec encTy cap r G sk en)])
   '(change (Eq (Car sk)
      (coe Sk.syn sk (den chkf dec encTy cap rq G Sk.cert en))
      (coe Sk.syn sk (den chkf dec encTy cap r G Sk.cert en))))
   '(exact (congrArg (fn [v :- (Car Sk.cert)] (coe Sk.syn sk v)) h))])

;; ite's branches are denoted at the output skeleton, which is the skeleton
;; they are typed at.  The scrutinee is den_ite_b (conversion.clj).
(prove! 'den_ite_t
  (into Pcap '[b :- Exp, t :- Exp, tq :- Exp, e :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car sk) (den chkf dec encTy cap tq G sk en)
                                  (den chkf dec encTy cap t G sk en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.ite b tq e) G sk en)
                (den chkf dec encTy cap (Exp.ite b t e) G sk en))
  ['(rw [(den_ite_at chkf dec encTy cap b tq e G sk en)])
   '(rw [(den_ite_at chkf dec encTy cap b t e G sk en)])
   '(change (Eq (Car sk)
      (Bool.rec$1 (fn [_ :- Bool] (Car sk))
        (den chkf dec encTy cap e G sk en) (den chkf dec encTy cap tq G sk en)
        (den chkf dec encTy cap b G Sk.bool en))
      (Bool.rec$1 (fn [_ :- Bool] (Car sk))
        (den chkf dec encTy cap e G sk en) (den chkf dec encTy cap t G sk en)
        (den chkf dec encTy cap b G Sk.bool en))))
   '(exact (congrArg (fn [v :- (Car sk)]
             (Bool.rec$1 (fn [_ :- Bool] (Car sk))
               (den chkf dec encTy cap e G sk en) v
               (den chkf dec encTy cap b G Sk.bool en))) h))])

(prove! 'den_ite_e
  (into Pcap '[b :- Exp, t :- Exp, e :- Exp, eq :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
               h :- (Eq (Car sk) (den chkf dec encTy cap eq G sk en)
                                  (den chkf dec encTy cap e G sk en))])
  '(Eq (Car sk) (den chkf dec encTy cap (Exp.ite b t eq) G sk en)
                (den chkf dec encTy cap (Exp.ite b t e) G sk en))
  ['(rw [(den_ite_at chkf dec encTy cap b t eq G sk en)])
   '(rw [(den_ite_at chkf dec encTy cap b t e G sk en)])
   '(change (Eq (Car sk)
      (Bool.rec$1 (fn [_ :- Bool] (Car sk))
        (den chkf dec encTy cap eq G sk en) (den chkf dec encTy cap t G sk en)
        (den chkf dec encTy cap b G Sk.bool en))
      (Bool.rec$1 (fn [_ :- Bool] (Car sk))
        (den chkf dec encTy cap e G sk en) (den chkf dec encTy cap t G sk en)
        (den chkf dec encTy cap b G Sk.bool en))))
   '(exact (congrArg (fn [v :- (Car sk)]
             (Bool.rec$1 (fn [_ :- Bool] (Car sk))
               v (den chkf dec encTy cap t G sk en)
               (den chkf dec encTy cap b G Sk.bool en))) h))])

;; abort's type is a skeleton annotation: a step inside it preserves skel
;; (step_skel_path), and skOf of abort is some of that skeleton.
(prove! 'sk_abort_A
  '[chkf :- (=> Code Code Bool), A :- Exp, t :- Exp, G :- (List Sk), q :- (List Nat),
    r :- Exp, r2 :- Exp,
    hA :- (SkJ Bool.true G A Sk.unit),
    hg :- (Eq (Option Exp) (getP q A) (Option.some Exp r)),
    hd :- (Hd chkf r r2)]
  '(Eq (Option Sk) (skOf G (Exp.abort (setP q A r2) t)) (skOf G (Exp.abort A t)))
  ['(have hs (Eq Sk (skel (setP q A r2)) (skel A))
      (step_skel_path chkf q A r r2 (skj_isTy Bool.true G A Sk.unit hA rfl) hg hd))
   '(exact (congrArg (fn [sk :- Sk] (Option.some Sk sk)) hs))])

(defn- pack-step! [nm extra term proofs]
  (prove! nm (into step-params extra)
    (list 'StepPack 'chkf 'dec 'encTy term)
    ['(intro fl G out hj hn pth)
     '(cases pth)
     '(intro re co hg hd)
     (list 'exact (list 'step_nil_fl 'chkf 'dec 'encTy 'fl term 'G 'out 'hj 'hn 're 'co 'hg 'hd))
     '(intro re co hg hd)
     (list 'exact (index-split term proofs))]))

;; h1 denotes the default carrier.  Every child is invisible to ⟦·⟧ and to skOf.
(let [fields '[[r Exp] [s Exp] [c Exp] [e1 Exp] [e2 Exp]]
      term '(Exp.h1 r s c e1 e2)]
  (pack-step! 'step_h1 (mapcat (fn [[f]] [f :- 'Exp]) fields) term
    (vec (for [[f] fields]
           (list 'And.intro '(Eq.refl$1 (skOf G (Exp.h1 r s c e1 e2)))
             (list 'fn '[cap :- Nat, en :- (HEnv G)]
               (apply list (concat
                 [(symbol (str "den_ig_h1_" f)) 'chkf 'dec 'encTy 'cap]
                 (mapcat (fn [[g]] (if (= g f) [g (list 'setP 'tail g 'co)] [g])) fields)
                 ['G 'out 'en]))))))))

(pack-step! 'step_succ
  '[n :- Exp, ih_n :- (StepPack chkf dec encTy n)]
  '(Exp.succ n)
  ['(And.intro (Eq.refl$1 (skOf G (Exp.succ n)))
      (fn [cap :- Nat, en :- (HEnv G)]
        (den_succ_cong chkf dec encTy cap n (setP tail n co) G out en
          ((And.right (ih_n Bool.false G Sk.nat
             (And.right (And.right (inv_succ Bool.false G n out hj)))
             (nbr_succ_n n fl hn)
             tail re co hgc hd)) cap en))))])

(pack-step! 'step_abort '[A :- Exp, t :- Exp] '(Exp.abort A t)
  [(list 'And.intro
     '(sk_abort_A chkf A t G tail re co
        (And.left (And.right (And.right (inv_abort Bool.false G A t out hj)))) hgc hd)
     '(fn [cap :- Nat, en :- (HEnv G)]
        (den_ig_abort_A chkf dec encTy cap A (setP tail A co) t G out en)))
   '(And.intro (Eq.refl$1 (skOf G (Exp.abort A t)))
      (fn [cap :- Nat, en :- (HEnv G)]
        (den_ig_abort_t chkf dec encTy cap A t (setP tail t co) G out en)))])

(pack-step! 'step_sleaf
  '[a :- Exp, ih_a :- (StepPack chkf dec encTy a)]
  '(Exp.sleaf a)
  ['(And.intro (Eq.refl$1 (skOf G (Exp.sleaf a)))
      (fn [cap :- Nat, en :- (HEnv G)]
        (den_coe_sleaf chkf dec encTy cap a (setP tail a co) G out en
          ((And.right (ih_a Bool.false G Sk.lbl
             (And.right (And.right (inv_sleaf Bool.false G a out hj)))
             (nbr_sleaf_a a fl hn)
             tail re co hgc hd)) cap en))))])

(pack-step! 'step_leaf
  '[a :- Exp, ih_a :- (StepPack chkf dec encTy a)]
  '(Exp.leaf a)
  ['(And.intro (Eq.refl$1 (skOf G (Exp.leaf a)))
      (fn [cap :- Nat, en :- (HEnv G)]
        (den_coe_leaf chkf dec encTy cap a (setP tail a co) G out en
          ((And.right (ih_a Bool.false G Sk.lbl
             (And.right (And.right (inv_leaf Bool.false G a out hj)))
             (nbr_leaf_a a fl hn)
             tail re co hgc hd)) cap en))))])

(pack-step! 'step_prn
  '[r :- Exp, ih_r :- (StepPack chkf dec encTy r)]
  '(Exp.prn r)
  ['(And.intro (Eq.refl$1 (skOf G (Exp.prn r)))
      (fn [cap :- Nat, en :- (HEnv G)]
        (den_coe_prn chkf dec encTy cap r (setP tail r co) G out en
          ((And.right (ih_r Bool.false G Sk.cert
             (And.right (And.right (inv_prn Bool.false G r out hj)))
             (nbr_prn_r r fl hn)
             tail re co hgc hd)) cap en))))])

;; ite's then-branch is the only child skOf reads.  Its induction hypothesis
;; is already the skOf equation, because skOf (ite b t e) reduces to skOf t.
(let [term '(Exp.ite b t e)
      inv '(inv_ite Bool.false G b t e out hj)
      hb (list 'And.left (list 'And.right inv))
      ht (list 'And.left (list 'And.right (list 'And.right inv)))
      he (list 'And.right (list 'And.right (list 'And.right inv)))
      nbrb '(nbr_ite_b b t e fl hn)
      nbrt '(nbr_ite_t b t e fl hn)
      nbre '(nbr_ite_e b t e fl hn)
      den-of (fn [ih csk sj nb]
               (list 'And.right (list ih 'Bool.false 'G csk sj nb 'tail 're 'co 'hgc 'hd)))]
  (pack-step! 'step_ite
    '[b :- Exp, t :- Exp, e :- Exp,
      ih_b :- (StepPack chkf dec encTy b),
      ih_t :- (StepPack chkf dec encTy t),
      ih_e :- (StepPack chkf dec encTy e)]
    term
    [(list 'And.intro '(Eq.refl$1 (skOf G (Exp.ite b t e)))
       (list 'fn '[cap :- Nat, en :- (HEnv G)]
         (list 'den_ite_b 'chkf 'dec 'encTy 'cap 'b '(setP tail b co) 't 'e 'G 'out 'en
           (list (den-of 'ih_b 'Sk.bool hb nbrb) 'cap 'en))))
     (list 'And.intro
       (list 'And.left (list 'ih_t 'Bool.false 'G 'out ht nbrt 'tail 're 'co 'hgc 'hd))
       (list 'fn '[cap :- Nat, en :- (HEnv G)]
         (list 'den_ite_t 'chkf 'dec 'encTy 'cap 'b 't '(setP tail t co) 'e 'G 'out 'en
           (list (den-of 'ih_t 'out ht nbrt) 'cap 'en))))
     (list 'And.intro '(Eq.refl$1 (skOf G (Exp.ite b t e)))
       (list 'fn '[cap :- Nat, en :- (HEnv G)]
         (list 'den_ite_e 'chkf 'dec 'encTy 'cap 'b 't 'e '(setP tail e co) 'G 'out 'en
           (list (den-of 'ih_e 'out he nbre) 'cap 'en))))]))


;; snode and node denote a code node of the children the clause reads.
;; node.d is ignored (den_ig_node_d).  chk is chkf of the two code denotations.
(defn- rights [n t] (if (zero? n) t (list 'And.right (rights (dec n) t))))
(defn- ih-den [ihname csk sj nbr]
  (list 'And.right (list ihname 'Bool.false 'G csk sj nbr 'tail 're 'co 'hgc 'hd)))
(defn- pack! [nm extra term proofs]
  (prove! nm (into step-params extra) (list 'StepPack 'chkf 'dec 'encTy term)
    ['(intro fl G out hj hn pth) '(cases pth) '(intro re co hg hd)
     (list 'exact (list 'step_nil_fl 'chkf 'dec 'encTy 'fl term 'G 'out 'hj 'hn 're 'co 'hg 'hd))
     '(intro re co hg hd) (list 'exact (index-split term proofs))]))

(defn- sn-lemma [nm ctor outsk slots changed]
    ;; slots: vector of [field child-skel]. changed is the index.
    (let [fs (mapv first slots)
          f (nth fs changed)
          fq (symbol (str f "q"))
          csk (second (nth slots changed))
          dens (fn [useq]
                 (map-indexed (fn [i [g sk]]
                                (list 'den 'chkf 'dec 'encTy 'cap
                                  (if (and useq (= i changed)) fq g) 'G sk 'en))
                              slots))
          left (apply list (symbol (str "Exp." ctor)) (map-indexed (fn [i g] (if (= i changed) fq g)) fs))
          right (apply list (symbol (str "Exp." ctor)) fs)
          at (symbol (str "den_" ctor "_at"))
          clause (fn [useq]
                   (list 'coe outsk 'sk (apply list 'Code.sn (dens useq))))
          params (into Pcap (concat
                    (mapcat (fn [[i [g _]]]
                              (if (= i changed) [g :- 'Exp, fq :- 'Exp] [g :- 'Exp]))
                            (map-indexed vector slots))
                    ['G :- '(List Sk), 'sk :- 'Sk, 'en :- '(HEnv G),
                     'h :- (list 'Eq (list 'Car csk)
                             (list 'den 'chkf 'dec 'encTy 'cap fq 'G csk 'en)
                             (list 'den 'chkf 'dec 'encTy 'cap f 'G csk 'en))]))]
      (prove! nm params
        (list 'Eq '(Car sk)
          (list 'den 'chkf 'dec 'encTy 'cap left 'G 'sk 'en)
          (list 'den 'chkf 'dec 'encTy 'cap right 'G 'sk 'en))
        [(list 'rw [(apply list at 'chkf 'dec 'encTy 'cap (concat (map-indexed (fn [i g] (if (= i changed) fq g)) fs) ['G 'sk 'en]))])
         (list 'rw [(apply list at 'chkf 'dec 'encTy 'cap (concat fs ['G 'sk 'en]))])
         (list 'change (list 'Eq '(Car sk) (clause true) (clause false)))
         (list 'exact (list 'congrArg
                       (list 'fn ['v ':- (list 'Car csk)]
                         (list 'coe outsk 'sk
                           (apply list 'Code.sn
                             (map-indexed (fn [i [g sk]]
                                            (if (= i changed) 'v
                                              (list 'den 'chkf 'dec 'encTy 'cap g 'G sk 'en)))
                                          slots))))
                       'h))])))

(def ^:private snode-slots '[[a Sk.lbl] [c1 Sk.syn] [c2 Sk.syn]])
(sn-lemma 'den_sn_a 'snode 'Sk.syn snode-slots 0)
(sn-lemma 'den_sn_c1 'snode 'Sk.syn snode-slots 1)
(sn-lemma 'den_sn_c2 'snode 'Sk.syn snode-slots 2)

(defn- nd [nm which]
    (let [fq (symbol (str which "q"))
          fields '[d a r1 r2]
          csk ({'a 'Sk.lbl 'r1 'Sk.cert 'r2 'Sk.cert} which)
          left (apply list 'Exp.node (map #(if (= % which) fq %) fields))
          right '(Exp.node d a r1 r2)
          denf (fn [g sk] (list 'den 'chkf 'dec 'encTy 'cap g 'G sk 'en))
          hole (fn [usev]
                 (list 'coe 'Sk.cert 'sk
                   (list 'Code.sn
                     (if (= which 'a) (if usev 'v (denf (if usev fq 'a) 'Sk.lbl)) (denf 'a 'Sk.lbl))
                     (if (= which 'r1) (if usev 'v (denf 'r1 'Sk.cert)) (denf 'r1 'Sk.cert))
                     (if (= which 'r2) (if usev 'v (denf 'r2 'Sk.cert)) (denf 'r2 'Sk.cert)))))
          at-args (fn [new?] (map #(if (and new? (= % which)) fq %) fields))]
      (prove! nm
        (into Pcap (concat
                     (mapcat (fn [g] (if (= g which) [g :- 'Exp, fq :- 'Exp] [g :- 'Exp])) fields)
                     ['G :- '(List Sk), 'sk :- 'Sk, 'en :- '(HEnv G),
                      'h :- (list 'Eq (list 'Car csk)
                              (list 'den 'chkf 'dec 'encTy 'cap fq 'G csk 'en)
                              (list 'den 'chkf 'dec 'encTy 'cap which 'G csk 'en))]))
        (list 'Eq '(Car sk)
          (list 'den 'chkf 'dec 'encTy 'cap left 'G 'sk 'en)
          (list 'den 'chkf 'dec 'encTy 'cap right 'G 'sk 'en))
        [(list 'rw [(apply list 'den_node_at 'chkf 'dec 'encTy 'cap (concat (at-args true) ['G 'sk 'en]))])
         (list 'rw [(apply list 'den_node_at 'chkf 'dec 'encTy 'cap (concat (at-args false) ['G 'sk 'en]))])
         (list 'change (list 'Eq '(Car sk)
                        (list 'coe 'Sk.cert 'sk
                          (list 'Code.sn
                            (denf (if (= which 'a) fq 'a) 'Sk.lbl)
                            (denf (if (= which 'r1) fq 'r1) 'Sk.cert)
                            (denf (if (= which 'r2) fq 'r2) 'Sk.cert)))
                        (list 'coe 'Sk.cert 'sk
                          (list 'Code.sn (denf 'a 'Sk.lbl) (denf 'r1 'Sk.cert) (denf 'r2 'Sk.cert)))))
         (list 'exact (list 'congrArg
                       (list 'fn ['v ':- (list 'Car csk)] (hole true))
                       'h))])))
  (nd 'den_nd_a 'a)
  (nd 'den_nd_r1 'r1)
  (nd 'den_nd_r2 'r2)

  (nd 'den_nd_a 'a)
(nd 'den_nd_r1 'r1)
(nd 'den_nd_r2 'r2)

(let [term '(Exp.snode a c1 c2)
        inv '(inv_snode Bool.false G a c1 c2 out hj)
        mk (fn [ihname field csk nproj den last?]
             (let [sj (if last? (rights nproj inv) (list 'And.left (rights nproj inv)))
                   nbr (list (symbol (str "nbr_snode_" field)) 'a 'c1 'c2 'fl 'hn)
                   args (mapcat (fn [g]
                                  (if (= g field) [g (list 'setP 'tail g 'co)] [g]))
                                '[a c1 c2])]
               (list 'And.intro '(Eq.refl$1 (skOf G (Exp.snode a c1 c2)))
                 (list 'fn '[cap :- Nat, en :- (HEnv G)]
                   (apply list (concat [den 'chkf 'dec 'encTy 'cap] args
                     ['G 'out 'en
                      (list (ih-den ihname csk sj nbr) 'cap 'en)]))))))]
    (pack! 'step_snode
      '[a :- Exp, c1 :- Exp, c2 :- Exp,
        ih_a :- (StepPack chkf dec encTy a),
        ih_c1 :- (StepPack chkf dec encTy c1),
        ih_c2 :- (StepPack chkf dec encTy c2)]
      term
      [(mk 'ih_a 'a 'Sk.lbl 2 'den_sn_a false)
       (mk 'ih_c1 'c1 'Sk.syn 3 'den_sn_c1 false)
       (mk 'ih_c2 'c2 'Sk.syn 4 'den_sn_c2 true)]))
  
(let [term '(Exp.node d a r1 r2)
        inv '(inv_node Bool.false G d a r1 r2 out hj)
        mk (fn [ihname field csk nproj den last?]
             (let [sj (if last? (rights nproj inv) (list 'And.left (rights nproj inv)))
                   nbr (list (symbol (str "nbr_node_" field)) 'd 'a 'r1 'r2 'fl 'hn)
                   args (mapcat (fn [g] (if (= g field) [g (list 'setP 'tail g 'co)] [g]))
                                '[d a r1 r2])]
               (list 'And.intro '(Eq.refl$1 (skOf G (Exp.node d a r1 r2)))
                 (list 'fn '[cap :- Nat, en :- (HEnv G)]
                   (apply list (concat [den 'chkf 'dec 'encTy 'cap] args
                     ['G 'out 'en (list (ih-den ihname csk sj nbr) 'cap 'en)]))))))]
    (pack! 'step_node
      '[d :- Exp, a :- Exp, r1 :- Exp, r2 :- Exp,
        ih_a :- (StepPack chkf dec encTy a),
        ih_r1 :- (StepPack chkf dec encTy r1),
        ih_r2 :- (StepPack chkf dec encTy r2)]
      term
      ['(And.intro (Eq.refl$1 (skOf G (Exp.node d a r1 r2)))
          (fn [cap :- Nat, en :- (HEnv G)]
            (den_ig_node_d chkf dec encTy cap d (setP tail d co) a r1 r2 G out en)))
       (mk 'ih_a 'a 'Sk.lbl 3 'den_nd_a false)
       (mk 'ih_r1 'r1 'Sk.cert 4 'den_nd_r1 false)
       (mk 'ih_r2 'r2 'Sk.cert 5 'den_nd_r2 true)]))

  
(prove! 'den_chk_c
    (into Pcap '[c :- Exp, cq :- Exp, d :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
                 h :- (Eq (Car Sk.syn) (den chkf dec encTy cap cq G Sk.syn en)
                                       (den chkf dec encTy cap c G Sk.syn en))])
    '(Eq (Car sk) (den chkf dec encTy cap (Exp.chk cq d) G sk en)
                  (den chkf dec encTy cap (Exp.chk c d) G sk en))
    ['(rw [(den_chk_at chkf dec encTy cap cq d G sk en)])
     '(rw [(den_chk_at chkf dec encTy cap c d G sk en)])
     '(change (Eq (Car sk)
        (coe Sk.bool sk (chkf (den chkf dec encTy cap cq G Sk.syn en) (den chkf dec encTy cap d G Sk.syn en)))
        (coe Sk.bool sk (chkf (den chkf dec encTy cap c G Sk.syn en) (den chkf dec encTy cap d G Sk.syn en)))))
     '(exact (congrArg (fn [v :- (Car Sk.syn)]
               (coe Sk.bool sk (chkf v (den chkf dec encTy cap d G Sk.syn en)))) h))])
  (prove! 'den_chk_d
    (into Pcap '[c :- Exp, d :- Exp, dq :- Exp, G :- (List Sk), sk :- Sk, en :- (HEnv G),
                 h :- (Eq (Car Sk.syn) (den chkf dec encTy cap dq G Sk.syn en)
                                       (den chkf dec encTy cap d G Sk.syn en))])
    '(Eq (Car sk) (den chkf dec encTy cap (Exp.chk c dq) G sk en)
                  (den chkf dec encTy cap (Exp.chk c d) G sk en))
    ['(rw [(den_chk_at chkf dec encTy cap c dq G sk en)])
     '(rw [(den_chk_at chkf dec encTy cap c d G sk en)])
     '(change (Eq (Car sk)
        (coe Sk.bool sk (chkf (den chkf dec encTy cap c G Sk.syn en) (den chkf dec encTy cap dq G Sk.syn en)))
        (coe Sk.bool sk (chkf (den chkf dec encTy cap c G Sk.syn en) (den chkf dec encTy cap d G Sk.syn en)))))
     '(exact (congrArg (fn [v :- (Car Sk.syn)]
               (coe Sk.bool sk (chkf (den chkf dec encTy cap c G Sk.syn en) v))) h))])

  (let [term '(Exp.chk c d)
        inv '(inv_chk Bool.false G c d out hj)
        mk (fn [ihname field nproj den last?]
             (let [sj (if last? (rights nproj inv) (list 'And.left (rights nproj inv)))
                   nbr (list (symbol (str "nbr_chk_" field)) 'c 'd 'fl 'hn)
                   args (mapcat (fn [g] (if (= g field) [g (list 'setP 'tail g 'co)] [g])) '[c d])]
               (list 'And.intro '(Eq.refl$1 (skOf G (Exp.chk c d)))
                 (list 'fn '[cap :- Nat, en :- (HEnv G)]
                   (apply list (concat [den 'chkf 'dec 'encTy 'cap] args
                     ['G 'out 'en (list (ih-den ihname 'Sk.syn sj nbr) 'cap 'en)]))))))]
    (pack! 'step_chk
      '[c :- Exp, d :- Exp,
        ih_c :- (StepPack chkf dec encTy c),
        ih_d :- (StepPack chkf dec encTy d)]
      term
      [(mk 'ih_c 'c 2 'den_chk_c false)
       (mk 'ih_d 'd 3 'den_chk_d true)]))
  
;; elimB: the motive is a skeleton annotation (sk_elimB_P).  The branches
;; are typed at skel P and denoted at the output skeleton, which inversion
;; equates; Eq.mp moves the branch's skeleton typing across that equation.
(prove! 'sk_elimB_P
    '[chkf :- (=> Code Code Bool), P :- Exp, b :- Exp, t :- Exp, e :- Exp, G :- (List Sk),
      q :- (List Nat), r :- Exp, r2 :- Exp,
      hP :- (SkJ Bool.true (List.cons Sk Sk.bool G) P Sk.unit),
      hg :- (Eq (Option Exp) (getP q P) (Option.some Exp r)),
      hd :- (Hd chkf r r2)]
    '(Eq (Option Sk) (skOf G (Exp.elimB (setP q P r2) b t e)) (skOf G (Exp.elimB P b t e)))
    ['(have hs (Eq Sk (skel (setP q P r2)) (skel P))
        (step_skel_path chkf q P r r2
          (skj_isTy Bool.true (List.cons Sk Sk.bool G) P Sk.unit hP rfl) hg hd))
     '(exact (congrArg (fn [k :- Sk] (Option.some Sk k)) hs))])

  
(defn- el [nm which]
    (let [fq (symbol (str which "q"))
          order '[P b t e]
          csk (if (= which 'b) 'Sk.bool 'sk)
          args-new (map #(if (= % which) fq %) order)
          left (apply list 'Exp.elimB args-new)
          denb '(den chkf dec encTy cap b G Sk.bool en)
          dent (fn [x] (list 'den 'chkf 'dec 'encTy 'cap x 'G 'sk 'en))
          dene (fn [x] (list 'den 'chkf 'dec 'encTy 'cap x 'G 'sk 'en))
          rec (fn [bv tv ev]
                (list 'Bool.rec$1 '(fn [_ :- Bool] (Car sk)) ev tv bv))
          newv (fn [pos v]
                 (cond (= which 'b) (rec v (dent 't) (dene 'e))
                       (= which 't) (rec denb v (dene 'e))
                       :else (rec denb (dent 't) v)))]
      (prove! nm
        (into Pcap (concat
                     (mapcat (fn [g] (if (= g which) [g :- 'Exp, fq :- 'Exp] [g :- 'Exp])) order)
                     ['G :- '(List Sk), 'sk :- 'Sk, 'en :- '(HEnv G),
                      'h :- (list 'Eq (list 'Car (if (= which 'b) 'Sk.bool 'sk))
                              (list 'den 'chkf 'dec 'encTy 'cap fq 'G (if (= which 'b) 'Sk.bool 'sk) 'en)
                              (list 'den 'chkf 'dec 'encTy 'cap which 'G (if (= which 'b) 'Sk.bool 'sk) 'en))]))
        (list 'Eq '(Car sk)
          (list 'den 'chkf 'dec 'encTy 'cap left 'G 'sk 'en)
          '(den chkf dec encTy cap (Exp.elimB P b t e) G sk en))
        [(list 'rw [(apply list 'den_elimB_at 'chkf 'dec 'encTy 'cap (concat args-new ['G 'sk 'en]))])
         '(rw [(den_elimB_at chkf dec encTy cap P b t e G sk en)])
         (list 'change (list 'Eq '(Car sk)
                        (newv which (list 'den 'chkf 'dec 'encTy 'cap fq 'G (if (= which 'b) 'Sk.bool 'sk) 'en))
                        (newv which (list 'den 'chkf 'dec 'encTy 'cap which 'G (if (= which 'b) 'Sk.bool 'sk) 'en))))
         (list 'exact (list 'congrArg
                       (list 'fn ['v ':- (list 'Car (if (= which 'b) 'Sk.bool 'sk))]
                         (newv which 'v))
                       'h))])))
  (el 'den_el_b 'b)
  (el 'den_el_t 't)
  (el 'den_el_e 'e)
  
(defn at-out [field sj-at-skel hs]
    (list 'Eq.mp
      (list 'congrArg (list 'fn ['k ':- 'Sk] (list 'SkJ 'Bool.false 'G field 'k))
            (list 'Eq.symm hs))
      sj-at-skel))
  (let [term '(Exp.elimB P b t e)
        inv '(inv_elimB Bool.false G P b t e out hj)
        hs (list 'And.left (rights 1 inv))
        hP (list 'And.left (rights 2 inv))
        hb (list 'And.left (rights 3 inv))
        ht (list 'And.left (rights 4 inv))
        he (rights 5 inv)
        nbr (fn [f] (list (symbol (str "nbr_elimB_" f)) 'P 'b 't 'e 'fl 'hn))
        denih (fn [ih csk sj nb]
                (list 'And.right (list ih 'Bool.false 'G csk sj nb 'tail 're 'co 'hgc 'hd)))
        args (fn [field]
               (mapcat (fn [g] (if (= g field) [g (list 'setP 'tail g 'co)] [g])) '[P b t e]))
        call (fn [den field csk sj nb]
               (list 'fn '[cap :- Nat, en :- (HEnv G)]
                 (apply list (concat [den 'chkf 'dec 'encTy 'cap] (args field)
                   ['G 'out 'en (list (denih (symbol (str "ih_" field)) csk sj nb) 'cap 'en)]))))
        pP (list 'And.intro
             (list 'sk_elimB_P 'chkf 'P 'b 't 'e 'G 'tail 're 'co hP 'hgc 'hd)
             '(fn [cap :- Nat, en :- (HEnv G)]
                (den_ig_elimB_P chkf dec encTy cap P (setP tail P co) b t e G out en)))
        pb (list 'And.intro '(Eq.refl$1 (skOf G (Exp.elimB P b t e)))
             (call 'den_el_b 'b 'Sk.bool hb (nbr 'b)))
        pt (list 'And.intro '(Eq.refl$1 (skOf G (Exp.elimB P b t e)))
             (call 'den_el_t 't 'out (at-out 't ht hs) (nbr 't)))
        pe (list 'And.intro '(Eq.refl$1 (skOf G (Exp.elimB P b t e)))
             (call 'den_el_e 'e 'out (at-out 'e he hs) (nbr 'e)))
        split (index-split term [pP pb pt pe])]
    (prove! 'step_elimB
      (into step-params '[P :- Exp, b :- Exp, t :- Exp, e :- Exp,
                 ih_b :- (StepPack chkf dec encTy b),
                 ih_t :- (StepPack chkf dec encTy t),
                 ih_e :- (StepPack chkf dec encTy e)])
      '(StepPack chkf dec encTy (Exp.elimB P b t e))
      ['(intro fl G out hj hn pth) '(cases pth) '(intro re co hg hd)
       '(exact (step_nil_fl chkf dec encTy fl (Exp.elimB P b t e) G out hj hn re co hg hd))
       '(intro re co hg hd) (list 'exact split)]))
  