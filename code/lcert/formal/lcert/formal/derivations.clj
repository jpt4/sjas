(ns lcert.formal.derivations
  "F2f — Lemmas 2.1 (weakening) and 2.4 (composition) of R4-metatheory.md §2,
  for the formal judgments Tl and Rt of lcert.formal.judgment (ADR-0006).

  Contexts are telescopes, innermost entry first; variables are de Bruijn
  indices; `lift k c e` adds k to every index ≥ c (lcert.formal.syntax).

  Weakening inserts one entry X at position c of a context D.  An entry at
  position j < c lives under the entries after it, and after the insertion X
  sits c − j − 1 places into its tail, so that entry is lifted at cutoff
  c − j − 1 (insD, §2).  The statements:
    Tl chkf w D t A      ⟹  Tl chkf w (insD c X D) (lift 1 c t) (lift 1 c A)
    Rt chkf D us t A     ⟹  Rt chkf (insD c X D) (insU c 0 us) (lift 1 c t) (lift 1 c A)
  (tl_weaken, rt_weaken).  X is arbitrary: no rule inspects a context entry
  except through a variable, and the new variable occurs nowhere.  At runtime
  the new entry has usage 0, as the paper's Lemma 2.1 has it.

  Contents.
  §1  Lift algebra: commutation of two lifts (lift_lift_comm), composition at
      nested cutoffs (lift_comp_ge), closed expressions are fixed by lift
      (closed_lift).
  §2  Lift commutes with substitution (lift_subst), under a pointwise
      condition on the two substitutions that is stable under binders; its
      instances for subst1, substL and the motive substitutions of the rule
      table (stepTy, leafTy, nodeTy, y1Ty, y2Ty, gTy, hTy).
  §3  Steps and conversion under lift: a head step, a step at a position,
      and a conversion chain (cv_lift), using skj_weaken
      (lcert.formal.substitution) for the chain's well-formedness side
      conditions.
  §4  Context and usage-vector insertion (insD, insU) and their lookups.
  §5  Lemma 2.1 for Tl (tl_weaken) and for Rt (rt_weaken).
  §6  Lemma 2.4 (composition) for all-token contexts (lemma24).

  Proof technique, as in lcert.formal.skeletons: inductions over Exp are
  generated as explicit Eq.trans/congrArg terms by the table-driven
  generator of lcert.formal.syntactic; inductions over derivations use
  `induction` with one explicit proof term per rule.  Every term is checked
  by the kernel; the generators are not trusted."
  (:require [ansatz.core :as a]
            [clojure.walk :as walk]
            [lcert.formal.base :refer [thm kdef lv]]
            [lcert.formal.usage :refer :all]
            [lcert.formal.syntax :refer :all]
            [lcert.formal.skel :refer :all]
            [lcert.formal.conv :refer :all]
            [lcert.formal.judgment :refer :all]
            [lcert.formal.syntactic]
            [lcert.formal.skeletons :refer :all]
            [lcert.formal.substitution :refer :all]
            [lcert.formal.model :refer :all]))

;; The constructor table and generators of lcert.formal.syntactic
;; (referenced, not copied, so there is one table).
(def ^:private exp-fields @#'lcert.formal.syntactic/exp-fields)
(def ^:private congruence @#'lcert.formal.syntactic/congruence)
(def ^:private prove-exp! @#'lcert.formal.syntactic/prove-exp!)
(def ^:private under @#'lcert.formal.syntactic/under)
(def ^:private ih @#'lcert.formal.syntactic/ih)

;; prove-exp! with the case tactics passed through lv, so that they may use
;; explicit universe levels (Foo$1) and explicit application (AT_Foo).
(defn- prove-exp-lv! [nm prop intros clause]
  (prove-exp! nm prop intros (fn [ctor fields] (lv (clause ctor fields)))))

;; ===========================================================================
;; §1  Lift algebra
;; ===========================================================================

;; Arithmetic side conditions of the variable cases (omega needs LE.le/LT.lt).
(thm dv_lt_a [i :- Nat, c :- Nat, k :- Nat, d :- Nat, h :- (LT.lt i d)] (LT.lt i (+ (+ c k) d)) (omega))
(thm dv_lt_b [i :- Nat, c :- Nat, d :- Nat, h :- (LT.lt i d)] (LT.lt i (+ c d)) (omega))
(thm dv_lt_c [i :- Nat, c :- Nat, k :- Nat, d :- Nat, h :- (LT.lt i (+ c d))] (LT.lt (+ i k) (+ (+ c k) d)) (omega))
(thm dv_le_a [i :- Nat, c :- Nat, k :- Nat, d :- Nat, h :- (Not (LT.lt i (+ c d)))] (LE.le (+ (+ c k) d) (+ i k)) (omega))
(thm dv_le_b [i :- Nat, c :- Nat, d :- Nat, h :- (Not (LT.lt i (+ c d)))] (LE.le d (+ i 1)) (omega))
(thm dv_le_c [i :- Nat, c :- Nat, d :- Nat, h :- (LE.le d i)] (LE.le d i) (omega))
(thm dv_eq_a [i :- Nat, k :- Nat] (= (+ (+ i k) 1) (+ (+ i 1) k)) (omega))

;; The variable case of lift_lift_comm, by the position of i relative to the
;; two cutoffs d ≤ c + d: below d, between, and at or above c + d.
(thm llc_var_ge [i :- Nat, k :- Nat, c :- Nat, d :- Nat, hd :- (LE.le d i)]
  (= (lift 1 (+ (+ c k) d) (lift k d (Exp.var i))) (lift k d (lift 1 (+ c d) (Exp.var i))))
  (have hc (Decidable (Nat.lt i (+ c d))) (Nat.decLt i (+ c d)))
  (cases hc)
  ;; i ≥ c + d: both sides are var (i + k + 1)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift 1 (+ (+ c k) d) v)) (lift_var_above k d i hd))
    (Eq.trans (lift_var_above 1 (+ (+ c k) d) (+ i k) (dv_le_a i c k d h))
      (Eq.trans (congrArg Exp.var (dv_eq_a i k))
        (Eq.symm (Eq.trans (congrArg (fn [v :- Exp] (lift k d v)) (lift_var_above 1 (+ c d) i (Nat.le_of_not_lt h)))
                           (lift_var_above k d (+ i 1) (dv_le_b i c d h))))))))
  ;; d ≤ i < c + d: both sides are var (i + k)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift 1 (+ (+ c k) d) v)) (lift_var_above k d i hd))
    (Eq.trans (lift_var_below 1 (+ (+ c k) d) (+ i k) (dv_lt_c i c k d h))
      (Eq.symm (Eq.trans (congrArg (fn [v :- Exp] (lift k d v)) (lift_var_below 1 (+ c d) i h))
                         (lift_var_above k d i hd)))))))

(thm llc_var [i :- Nat, k :- Nat, c :- Nat, d :- Nat]
  (= (lift 1 (+ (+ c k) d) (lift k d (Exp.var i))) (lift k d (lift 1 (+ c d) (Exp.var i))))
  (have hc (Decidable (Nat.lt i d)) (Nat.decLt i d))
  (cases hc)
  (exact (llc_var_ge i k c d (Nat.le_of_not_lt h)))
  ;; i < d: both sides are var i
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift 1 (+ (+ c k) d) v)) (lift_var_below k d i h))
    (Eq.trans (lift_var_below 1 (+ (+ c k) d) i (dv_lt_a i c k d h))
      (Eq.symm (Eq.trans (congrArg (fn [v :- Exp] (lift k d v)) (lift_var_below 1 (+ c d) i (dv_lt_b i c d h)))
                         (lift_var_below k d i h)))))))

;; lift_lift_comm: a one-place lift above a k-place lift at cutoff d moves
;; inside it:  lift 1 (c+k+d) (lift k d e) = lift k d (lift 1 (c+d) e).
;; The binder-growing cutoff d is written last, so that under a binder
;; (d ↦ d + n) the cutoffs stay definitionally equal to liftF's (x + n).
;; The motive's variables are named kk cc dd: Exp's fields include c and d
;; (recS, node, chk, h1, insp), and a clash makes the case terms ill-typed.
(prove-exp! 'lift_lift_comm
  '(forall [kk Nat] (forall [cc Nat] (forall [dd Nat]
     (= (lift 1 (+ (+ cc kk) dd) (lift kk dd e)) (lift kk dd (lift 1 (+ cc dd) e))))))
  '[kk cc dd]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(exact (llc_var i kk cc dd))]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty depth]]
                   (let [dd (under 'dd depth)]
                     (if (= ty 'Exp)
                       [ty (list 'lift 1 (list '+ '(+ cc kk) dd) (list 'lift 'kk dd f))
                        (list 'lift 'kk dd (list 'lift 1 (list '+ 'cc dd) f))
                        (list (ih f) 'kk 'cc dd)]
                       [ty f f nil]))) fields)))])))

;; lift_comp_ge: two lifts at nested cutoffs compose when the outer cutoff
;; lies within the range the inner one opened:  for m ≤ b,
;;   lift a (m+d) (lift b d e) = lift (b+a) d e.
;; (lift_comp of lcert.formal.syntactic is the case m = 0.)
(thm lcg_lt [i :- Nat, m :- Nat, d :- Nat, h :- (LT.lt i d)] (LT.lt i (+ m d)) (omega))
(thm lcg_le [i :- Nat, b :- Nat, m :- Nat, d :- Nat, hm :- (LE.le m b), h :- (LE.le d i)] (LE.le (+ m d) (+ i b)) (omega))

(thm lcg_var [i :- Nat, a :- Nat, b :- Nat, m :- Nat, hm :- (LE.le m b), d :- Nat]
  (= (lift a (+ m d) (lift b d (Exp.var i))) (lift (+ b a) d (Exp.var i)))
  (have hc (Decidable (Nat.lt i d)) (Nat.decLt i d))
  (cases hc)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift a (+ m d) v)) (lift_var_above b d i (Nat.le_of_not_lt h)))
    (Eq.trans (lift_var_above a (+ m d) (+ i b) (lcg_le i b m d hm (Nat.le_of_not_lt h)))
      (Eq.trans (congrArg Exp.var (Nat.add_assoc i b a))
        (Eq.symm (lift_var_above (+ b a) d i (Nat.le_of_not_lt h)))))))
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift a (+ m d) v)) (lift_var_below b d i h))
    (Eq.trans (lift_var_below a (+ m d) i (lcg_lt i m d h))
      (Eq.symm (lift_var_below (+ b a) d i h))))))

(prove-exp! 'lift_comp_ge
  '(forall [aa Nat] (forall [bb Nat] (forall [mm Nat] (=> (LE.le mm bb) (forall [dd Nat]
     (= (lift aa (+ mm dd) (lift bb dd e)) (lift (+ bb aa) dd e)))))))
  '[aa bb mm hm dd]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(exact (lcg_var i aa bb mm hm dd))]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty depth]]
                   (let [dd (under 'dd depth)]
                     (if (= ty 'Exp)
                       [ty (list 'lift 'aa (list '+ 'mm dd) (list 'lift 'bb dd f))
                        (list 'lift '(+ bb aa) dd f)
                        (list (ih f) 'aa 'bb 'mm 'hm dd)]
                       [ty f f nil]))) fields)))])))

;; closed_lift: an expression whose free variables all lie below d is fixed
;; by every lift at a cutoff c ≥ d.  closedF (model.clj) computes the
;; conjunction of its fields' closedness, right-nested with Bool.and; the
;; j-th conjunct is extracted with andb_left / andb_right (skeletons.clj).
(defn- conj-proof
  "A proof that the j-th of the Boolean conjuncts xs is true, from h, a
  proof that their right-nested conjunction is true."
  [xs j h]
  (if (= 1 (count xs))
    h
    (let [rest-conj (reduce (fn [acc x] (list 'Bool.and x acc)) (last xs) (reverse (butlast (rest xs))))]
      (if (zero? j)
        (list 'andb_left (first xs) rest-conj h)
        (conj-proof (vec (rest xs)) (dec j) (list 'andb_right (first xs) rest-conj h))))))

(thm closed_var [i :- Nat, d :- Nat, h :- (= (Nat.blt i d) true), k :- Nat, c :- Nat, hle :- (LE.le d c)]
  (= (lift k c (Exp.var i)) (Exp.var i))
  (exact (lift_var_below k c i (Nat.lt_of_lt_of_le (Nat.le_of_ble_eq_true h) hle))))

(prove-exp! 'closed_lift
  '(forall [dd Nat] (=> (= (closedF e dd) true)
     (forall [kk Nat] (forall [cc Nat] (=> (LE.le dd cc) (= (lift kk cc e) e))))))
  '[dd hcl kk cc hle]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(exact (closed_var i dd hcl kk cc hle))]
      (let [efs (filterv #(= 'Exp (second %)) fields)
            xs (mapv (fn [[f _ depth]] (list 'closedF f (under 'dd depth))) efs)]
        [(list 'exact
           (congruence (symbol (str "Exp." ctor))
             (mapv (fn [[f ty depth]]
                     (if (= ty 'Exp)
                       (let [j (.indexOf (mapv first efs) f)]
                         [ty (list 'lift 'kk (under 'cc depth) f) f
                          (list (ih f) (under 'dd depth) (conj-proof xs j 'hcl) 'kk (under 'cc depth)
                                (if (zero? depth) 'hle (list 'Nat.add_le_add_right 'hle depth)))])
                       [ty f f nil])) fields)))]))))

;; A closed type (closedTy A = closedF A 0) is fixed by every lift.
(thm closedTy_lift [A :- Exp, h :- (= (closedTy A) true), k :- Nat, c :- Nat] (= (lift k c A) A)
  (exact (closed_lift A 0 h k c (Nat.zero_le c))))

;; ===========================================================================
;; §2  Lift commutes with substitution
;; ===========================================================================

;; LSC σ σ' c d: lifting at c after σ is σ' after lifting at d, on every
;; variable.  lift_subst extends it to every expression.  (A substitution
;; that sends variables to terms under c binders of the target, with d the
;; matching cutoff in the source.)
(kdef LSC (=> (=> Nat Exp) (=> Nat Exp) Nat Nat Prop)
  (fn [sg :- (=> Nat Exp), sg2 :- (=> Nat Exp), c :- Nat, d :- Nat]
    (forall [i Nat] (Eq Exp (lift 1 c (sg i)) (subst sg2 (lift 1 d (Exp.var i)))))))

(thm ls_lt [j :- Nat, d :- Nat, h :- (LT.lt j d)] (LT.lt (+ j 1) (+ d 1)) (omega))
(thm ls_le [j :- Nat, d :- Nat, h :- (LE.le d j)] (LE.le (+ d 1) (+ j 1)) (omega))

;; Under a binder, variable j + 1 of the source is variable j shifted, on
;; both sides of the cutoff: the up-shifted substitution (up) and the
;; cons-extended one (consSub, lcert.formal.substitution) read it the same.
(thm up_var_succ [sg2 :- (=> Nat Exp), d :- Nat, j :- Nat]
  (= (subst (upn 1 sg2) (lift 1 (+ d 1) (Exp.var (+ j 1)))) (lift 1 0 (subst sg2 (lift 1 d (Exp.var j)))))
  (have hc (Decidable (Nat.lt j d)) (Nat.decLt j d))
  (cases hc)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (subst (upn 1 sg2) v)) (lift_var_above 1 (+ d 1) (+ j 1) (ls_le j d (Nat.le_of_not_lt h))))
    (congrArg (fn [v :- Exp] (lift 1 0 (subst sg2 v))) (Eq.symm (lift_var_above 1 d j (Nat.le_of_not_lt h))))))
  (exact (Eq.trans (congrArg (fn [v :- Exp] (subst (upn 1 sg2) v)) (lift_var_below 1 (+ d 1) (+ j 1) (ls_lt j d h)))
    (congrArg (fn [v :- Exp] (lift 1 0 (subst sg2 v))) (Eq.symm (lift_var_below 1 d j h))))))

(thm cons_var_succ [a0 :- Exp, sg2 :- (=> Nat Exp), d :- Nat, j :- Nat]
  (= (subst (consSub a0 sg2) (lift 1 (+ d 1) (Exp.var (+ j 1)))) (subst sg2 (lift 1 d (Exp.var j))))
  (have hc (Decidable (Nat.lt j d)) (Nat.decLt j d))
  (cases hc)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (subst (consSub a0 sg2) v)) (lift_var_above 1 (+ d 1) (+ j 1) (ls_le j d (Nat.le_of_not_lt h))))
    (congrArg (fn [v :- Exp] (subst sg2 v)) (Eq.symm (lift_var_above 1 d j (Nat.le_of_not_lt h))))))
  (exact (Eq.trans (congrArg (fn [v :- Exp] (subst (consSub a0 sg2) v)) (lift_var_below 1 (+ d 1) (+ j 1) (ls_lt j d h)))
    (congrArg (fn [v :- Exp] (subst sg2 v)) (Eq.symm (lift_var_below 1 d j h))))))

;; LSC is stable under one binder (up on both sides, both cutoffs + 1) ...
(thm lsc_up [sg :- (=> Nat Exp), sg2 :- (=> Nat Exp), c :- Nat, d :- Nat, h :- (LSC sg sg2 c d)]
  (LSC (upn 1 sg) (upn 1 sg2) (+ c 1) (+ d 1))
  (unfold LSC)
  (intro i)
  (cases i)
  (rfl)
  ;; variable n + 1: lift_lift_comm moves the outer lift inside the shift
  (exact (Eq.trans (lift_lift_comm (sg n) 1 c 0)
    (Eq.trans (congrArg (fn [v :- Exp] (lift 1 0 v)) (h n)) (Eq.symm (up_var_succ sg2 d n))))))

;; ... and so under n binders.
(thm lsc_upn [n :- Nat]
  (forall [sg (=> Nat Exp)] (forall [sg2 (=> Nat Exp)] (forall [c Nat] (forall [d Nat]
    (=> (LSC sg sg2 c d) (LSC (upn n sg) (upn n sg2) (+ c n) (+ d n)))))))
  (induction n)
  (intro sg sg2 c d h)
  (exact h)
  (intro sg sg2 c d h)
  (exact (lsc_up (upn n sg) (upn n sg2) (+ c n) (+ d n) (ih_n sg sg2 c d h))))

;; lift_subst: lift 1 c (subst σ e) = subst σ' (lift 1 d e) whenever LSC σ σ' c d.
(prove-exp! 'lift_subst
  '(forall [sg (=> Nat Exp)] (forall [sg2 (=> Nat Exp)] (forall [cc Nat] (forall [dd Nat]
     (=> (LSC sg sg2 cc dd) (= (lift 1 cc (subst sg e)) (subst sg2 (lift 1 dd e))))))))
  '[sg sg2 cc dd hl]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(exact (hl i))]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty depth]]
                   (if (= ty 'Exp)
                     (let [s1 (if (zero? depth) 'sg (list 'upn depth 'sg))
                           s2 (if (zero? depth) 'sg2 (list 'upn depth 'sg2))
                           hh (if (zero? depth) 'hl (list 'lsc_upn depth 'sg 'sg2 'cc 'dd 'hl))]
                       [ty (list 'lift 1 (under 'cc depth) (list 'subst s1 f))
                        (list 'subst s2 (list 'lift 1 (under 'dd depth) f))
                        (list (ih f) s1 s2 (under 'cc depth) (under 'dd depth) hh)])
                     [ty f f nil])) fields)))])))

;; --- the substitutions of the rule table ------------------------------------
;; Each sends variable 0 to a term u and variable j + 1 to var (j + m): the
;; shape of subst1 (m = 0) and of the motive instances (sSucc, sLeafI: m = 1;
;; sNodeI: m = 5; sAt 1 3 / sAt 1 4 of y1Ty / y2Ty: m = 3, 4).  For such σ,
;; σ' (σ' 0 the lifted head, the same tail), LSC σ σ' (c + m) (c + 1).
(thm lm_lt [j :- Nat, c :- Nat, m :- Nat, h :- (LT.lt j c)] (LT.lt (+ j m) (+ c m)) (omega))
(thm lm_le [j :- Nat, c :- Nat, m :- Nat, h :- (LE.le c j)] (LE.le (+ c m) (+ j m)) (omega))
(thm lm_eq [j :- Nat, m :- Nat] (= (+ (+ j 1) m) (+ (+ j m) 1)) (omega))

(thm lsc_tail [sg2 :- (=> Nat Exp), m :- Nat, c :- Nat, ht :- (forall [j Nat] (= (sg2 (+ j 1)) (Exp.var (+ j m)))), j :- Nat]
  (= (lift 1 (+ c m) (Exp.var (+ j m))) (subst sg2 (lift 1 (+ c 1) (Exp.var (+ j 1)))))
  (have hc (Decidable (Nat.lt j c)) (Nat.decLt j c))
  (cases hc)
  (exact (Eq.trans (lift_var_above 1 (+ c m) (+ j m) (lm_le j c m (Nat.le_of_not_lt h)))
    (Eq.trans (congrArg Exp.var (Eq.symm (lm_eq j m)))
      (Eq.trans (Eq.symm (ht (+ j 1)))
        (congrArg (fn [v :- Exp] (subst sg2 v)) (Eq.symm (lift_var_above 1 (+ c 1) (+ j 1) (ls_le j c (Nat.le_of_not_lt h)))))))))
  (exact (Eq.trans (lift_var_below 1 (+ c m) (+ j m) (lm_lt j c m h))
    (Eq.trans (Eq.symm (ht j))
      (congrArg (fn [v :- Exp] (subst sg2 v)) (Eq.symm (lift_var_below 1 (+ c 1) (+ j 1) (ls_lt j c h))))))))

(thm lsc_mk [sg :- (=> Nat Exp), sg2 :- (=> Nat Exp), m :- Nat, c :- Nat,
             h0 :- (= (lift 1 (+ c m) (sg 0)) (sg2 0)),
             hs :- (forall [j Nat] (= (sg (+ j 1)) (Exp.var (+ j m)))),
             ht :- (forall [j Nat] (= (sg2 (+ j 1)) (Exp.var (+ j m))))]
  (LSC sg sg2 (+ c m) (+ c 1))
  (unfold LSC)
  (intro i)
  (cases i)
  (exact h0)
  (exact (Eq.trans (congrArg (fn [v :- Exp] (lift 1 (+ c m) v)) (hs n)) (lsc_tail sg2 m c ht n))))

;; subst1 (β, B[u/x]):  lift 1 c (t[u/x]) = (lift 1 (c+1) t)[lift 1 c u / x].
(thm lift_subst1 [u :- Exp, t :- Exp, c :- Nat]
  (= (lift 1 c (subst1 u t)) (subst1 (lift 1 c u) (lift 1 (+ c 1) t)))
  (exact (lift_subst t (fn [i :- Nat] (inst1 u i)) (fn [i :- Nat] (inst1 (lift 1 c u) i)) c (+ c 1)
    (lsc_mk (fn [i :- Nat] (inst1 u i)) (fn [i :- Nat] (inst1 (lift 1 c u) i)) 0 c
      (Eq.refl$1 (lift 1 c u)) (fn [j :- Nat] (Eq.refl$1 (Exp.var j))) (fn [j :- Nat] (Eq.refl$1 (Exp.var j)))))))

;; The motive instances.  P lives under one binder x, at cutoff c + 1.
(thm lift_sSucc [P :- Exp, c :- Nat]
  (= (lift 1 (+ c 1) (subst (fn [i :- Nat] (sSucc i)) P)) (subst (fn [i :- Nat] (sSucc i)) (lift 1 (+ c 1) P)))
  (exact (lift_subst P (fn [i :- Nat] (sSucc i)) (fn [i :- Nat] (sSucc i)) (+ c 1) (+ c 1)
    (lsc_mk (fn [i :- Nat] (sSucc i)) (fn [i :- Nat] (sSucc i)) 1 c
      (Eq.refl$1 (Exp.succ (Exp.var 0))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 1)))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 1))))))))

;; stepTy P = (P[succ x/x]) under y: at cutoff c + 2.
(thm lift_stepTy [P :- Exp, c :- Nat] (= (lift 1 (+ c 2) (stepTy P)) (stepTy (lift 1 (+ c 1) P)))
  (exact (Eq.trans (lift_lift_comm (subst (fn [i :- Nat] (sSucc i)) P) 1 (+ c 1) 0)
    (congrArg (fn [v :- Exp] (lift 1 0 v)) (lift_sSucc P c)))))

(thm lift_leafTy [P :- Exp, c :- Nat] (= (lift 1 (+ c 1) (leafTy P)) (leafTy (lift 1 (+ c 1) P)))
  (exact (lift_subst P (fn [i :- Nat] (sLeafI i)) (fn [i :- Nat] (sLeafI i)) (+ c 1) (+ c 1)
    (lsc_mk (fn [i :- Nat] (sLeafI i)) (fn [i :- Nat] (sLeafI i)) 1 c
      (Eq.refl$1 (Exp.sleaf (Exp.var 0))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 1)))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 1))))))))

(thm lift_nodeTy [P :- Exp, c :- Nat] (= (lift 1 (+ c 5) (nodeTy P)) (nodeTy (lift 1 (+ c 1) P)))
  (exact (lift_subst P (fn [i :- Nat] (sNodeI i)) (fn [i :- Nat] (sNodeI i)) (+ c 5) (+ c 1)
    (lsc_mk (fn [i :- Nat] (sNodeI i)) (fn [i :- Nat] (sNodeI i)) 5 c
      (Eq.refl$1 (Exp.snode (Exp.var 4) (Exp.var 3) (Exp.var 2))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 5)))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 5))))))))

(thm lift_y1Ty [P :- Exp, c :- Nat] (= (lift 1 (+ c 3) (y1Ty P)) (y1Ty (lift 1 (+ c 1) P)))
  (exact (lift_subst P (fn [i :- Nat] (sAt 1 3 i)) (fn [i :- Nat] (sAt 1 3 i)) (+ c 3) (+ c 1)
    (lsc_mk (fn [i :- Nat] (sAt 1 3 i)) (fn [i :- Nat] (sAt 1 3 i)) 3 c
      (Eq.refl$1 (Exp.var 1)) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 3)))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 3))))))))

(thm lift_y2Ty [P :- Exp, c :- Nat] (= (lift 1 (+ c 4) (y2Ty P)) (y2Ty (lift 1 (+ c 1) P)))
  (exact (lift_subst P (fn [i :- Nat] (sAt 1 4 i)) (fn [i :- Nat] (sAt 1 4 i)) (+ c 4) (+ c 1)
    (lsc_mk (fn [i :- Nat] (sAt 1 4 i)) (fn [i :- Nat] (sAt 1 4 i)) 4 c
      (Eq.refl$1 (Exp.var 1)) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 4)))) (fn [j :- Nat] (Eq.refl$1 (Exp.var (+ j 4))))))))

;; The types lifted past binders: Let's C (by 2), itR's gTy and hTy.
(thm lift_lift2 [C :- Exp, c :- Nat] (= (lift 1 (+ c 2) (lift 2 0 C)) (lift 2 0 (lift 1 c C)))
  (exact (lift_lift_comm C 2 c 0)))

(thm lift_gTy [X :- Exp, c :- Nat] (= (lift 1 c (gTy X)) (gTy (lift 1 c X)))
  (exact (congrArg (fn [v :- Exp] (Exp.tPi U.uw Exp.tLbl v)) (lift_lift_comm X 1 c 0))))

(thm lift_hTy [X :- Exp, c :- Nat] (= (lift 1 c (hTy X)) (hTy (lift 1 c X)))
  (exact (Eq.trans
    (congrArg (fn [v :- Exp] (Exp.tPi U.u1 Exp.tDia (Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 v
              (Exp.tPi U.u1 (lift 1 (+ c 3) (lift 3 0 X)) (lift 1 (+ c 4) (lift 4 0 X)))))))
              (lift_lift_comm X 2 c 0))
    (Eq.trans
      (congrArg (fn [v :- Exp] (Exp.tPi U.u1 Exp.tDia (Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 (lift 1 c X))
                (Exp.tPi U.u1 v (lift 1 (+ c 4) (lift 4 0 X)))))))
                (lift_lift_comm X 3 c 0))
      (congrArg (fn [v :- Exp] (Exp.tPi U.u1 Exp.tDia (Exp.tPi U.uw Exp.tLbl (Exp.tPi U.u1 (lift 2 0 (lift 1 c X))
                (Exp.tPi U.u1 (lift 3 0 (lift 1 c X)) v)))))
                (lift_lift_comm X 4 c 0))))))

;; substL (β for let, recN, recSyn): list substitutions, through their
;; structural form instLS (substitution.clj).  liftL c us lifts every entry.
(kdef liftL (=> Nat (List Exp) (List Exp))
  (fn [c :- Nat, us :- (List Exp)]
    (List.rec$1$0 Exp (fn [_ :- (List Exp)] (List Exp)) (List.nil Exp)
      (fn [u :- Exp, rest :- (List Exp), ih :- (List Exp)] (List.cons Exp (lift 1 c u) ih)) us)))

(thm lsc_id_var [c :- Nat, i :- Nat]
  (= (lift 1 c (Exp.var i)) (subst (fn [j :- Nat] (Exp.var j)) (lift 1 c (Exp.var i))))
  (have hc (Decidable (Nat.lt i c)) (Nat.decLt i c))
  (cases hc)
  (exact (Eq.trans (lift_var_above 1 c i (Nat.le_of_not_lt h))
    (Eq.symm (congrArg (fn [v :- Exp] (subst (fn [j :- Nat] (Exp.var j)) v)) (lift_var_above 1 c i (Nat.le_of_not_lt h))))))
  (exact (Eq.trans (lift_var_below 1 c i h)
    (Eq.symm (congrArg (fn [v :- Exp] (subst (fn [j :- Nat] (Exp.var j)) v)) (lift_var_below 1 c i h))))))

(thm lsc_list [us :- (List Exp)]
  (forall [c Nat] (LSC (instLS us) (instLS (liftL c us)) c (+ c (lenE us))))
  (induction us)
  (intro c)
  (unfold LSC)
  (intro i)
  (exact (lsc_id_var c i))
  (intro c)
  (unfold LSC)
  (intro i)
  (cases i)
  (rfl)
  (exact (Eq.trans (ih_tail c n) (Eq.symm (cons_var_succ (lift 1 c head) (instLS (liftL c tail)) (+ c (lenE tail)) n)))))

(thm lift_substL [us :- (List Exp), t :- Exp, c :- Nat]
  (= (lift 1 c (substL us t)) (substL (liftL c us) (lift 1 (+ c (lenE us)) t)))
  (rw [(substL_instLS us t)])
  (rw [(substL_instLS (liftL c us) (lift 1 (+ c (lenE us)) t))])
  (exact (lift_subst t (instLS us) (instLS (liftL c us)) c (+ c (lenE us)) (lsc_list us c))))

;; ===========================================================================
;; §3  Steps and conversion under lift
;; ===========================================================================

;; ∃-elimination into any Prop, at Nat and at Exp (as exN of
;; lcert.formal.splitting): (refine' (exNat _ _ h _)) then (intro x hx).
(thm exNat [P :- (=> Nat Prop), Q :- Prop, h :- (Exists P), f :- (forall [x Nat] (=> (P x) Q))] Q
  (exact (AT_Exists.rec Nat P (fn [_ :- (Exists P)] Q) f h)))
(thm exExp [P :- (=> Exp Prop), Q :- Prop, h :- (Exists P), f :- (forall [x Exp] (=> (P x) Q))] Q
  (exact (AT_Exists.rec Exp P (fn [_ :- (Exists P)] Q) f h)))
(thm exPath [P :- (=> (List Nat) Prop), Q :- Prop, h :- (Exists P), f :- (forall [x (List Nat)] (=> (P x) Q))] Q
  (exact (AT_Exists.rec (List Nat) P (fn [_ :- (Exists P)] Q) f h)))

;; --- the contents of head redexes -------------------------------------------

;; caseLbl's branch lookup commutes with lift.  nthB recurses on two
;; arguments, so it is well-founded and read through its equation lemmas:
;; nthB.eq_k (label first, then the fields) for the k-th constructor of Exp
;; (k ≤ 24), eq_25 / eq_26 (fields, then label) for
;; bcons at label 0 / m + 1, and eq_(k+1) for the constructors after bcons.
;; (The label and hypothesis are introduced after `cases` on the label,
;; since cases does not rewrite hypotheses; their names lq bq hq avoid the
;; field names l, h, t.)
(prove-exp-lv! 'nthB_lift
  '(forall [kc Nat] (forall [lq Nat] (forall [bq Exp]
     (=> (= (nthB e lq) (Option.some Exp bq)) (= (nthB (lift 1 kc e) lq) (Option.some Exp (lift 1 kc bq)))))))
  '[kc lq]
  (let [index (into {} (map-indexed (fn [n [ctor _]] [ctor n]) exp-fields))]
    (fn [ctor fields]
      (let [n (index ctor)]
        (if (= ctor 'bcons)
          '[(cases lq)
            (intro bq hq)
            (exact (Eq.trans (nthB.eq_25 (lift 1 kc h) (lift 1 kc t))
                     (congrArg (fn [v :- Exp] (Option.some Exp (lift 1 kc v)))
                               (some_inj h bq (Eq.trans (Eq.symm (nthB.eq_25 h t)) hq)))))
            (intro bq hq)
            (exact (Eq.trans (nthB.eq_26 (lift 1 kc h) (lift 1 kc t) n)
                     (ih_t kc n bq (Eq.trans (Eq.symm (nthB.eq_26 h t n)) hq))))]
          (let [eqn (symbol (str "nthB.eq_" (if (< n 24) (inc n) (+ n 2))))]
            ['(intro bq hq)
             (list 'exact (list 'False.elim$0
               (list 'none_ne_someE 'bq
                     (list 'Eq.trans (list 'Eq.symm (apply list eqn (cons 'lq (map first fields)))) 'hq))))]))))))

;; A canonical code term (codeOf e ≠ none: sleaf/snode over labels) has no
;; variables, so every lift fixes it.  codeOf's snode clause, as a function
;; of the two sub-results (codeF), is none as soon as either is.
(kdef codeF (=> Nat (Option Code) (Option Code) (Option Code))
  (fn [l :- Nat, o1 :- (Option Code), o2 :- (Option Code)]
    (Option.rec$1$0 Code (fn [_ :- (Option Code)] (Option Code)) (Option.none Code)
      (fn [a0 :- Code] (Option.rec$1$0 Code (fn [_ :- (Option Code)] (Option Code)) (Option.none Code)
         (fn [b0 :- Code] (Option.some Code (Code.sn l a0 b0))) o2)) o1)))

(thm codeF_none_left [l :- Nat, o2 :- (Option Code)] (= (codeF l (Option.none Code) o2) (Option.none Code)) (rfl))
(thm codeF_none_right [l :- Nat, o1 :- (Option Code)] (= (codeF l o1 (Option.none Code)) (Option.none Code))
  (cases o1) (rfl) (rfl))

(def ^:private non-lbl-leaves
  "Tactics closing the goals of (cases x) for a code position x: the label
  case by `lbl-tac`, every other constructor by refuting codeOf ≠ none (the
  hypothesis hn is introduced after the split: cases does not rewrite it)."
  (fn [lbl-tac]
    (mapcat (fn [[ctor _]]
              (if (= ctor 'lbl) (cons '(intro hn) lbl-tac)
                  ['(intro hn) '(exact (False.elim$0 (hn (Eq.refl$1 (Option.none Code)))))]))
            exp-fields)))

;; The two code constructors, as lemmas with clean parameters (the label
;; position a is split by cases, which must not meet an induction
;; hypothesis mentioning a).
(a/prove-theorem 'code_sleaf '[a :- Exp, kq :- Nat, cq :- Nat]
  '(=> (=> (= (codeOf (Exp.sleaf a)) (Option.none Code)) False) (= (lift kq cq (Exp.sleaf a)) (Exp.sleaf a)))
  (lv (into ['(cases a)] (non-lbl-leaves ['(rfl)]))))

(a/prove-theorem 'code_snode
  '[a :- Exp, c1 :- Exp, c2 :- Exp, kq :- Nat, cq :- Nat,
    ih1 :- (=> (=> (= (codeOf c1) (Option.none Code)) False) (= (lift kq cq c1) c1)),
    ih2 :- (=> (=> (= (codeOf c2) (Option.none Code)) False) (= (lift kq cq c2) c2))]
  '(=> (=> (= (codeOf (Exp.snode a c1 c2)) (Option.none Code)) False) (= (lift kq cq (Exp.snode a c1 c2)) (Exp.snode a c1 c2)))
  (lv (into ['(cases a)]
            (non-lbl-leaves
             ['(exact (Eq.trans
                 (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) v (lift kq cq c2)))
                   (ih1 (fn [h1 :- (Eq (Option Code) (codeOf c1) (Option.none Code))]
                     (hn (Eq.trans (congrArg (fn [o :- (Option Code)] (codeF l o (codeOf c2))) h1) (codeF_none_left l (codeOf c2)))))))
                 (congrArg (fn [v :- Exp] (Exp.snode (Exp.lbl l) c1 v))
                   (ih2 (fn [h2 :- (Eq (Option Code) (codeOf c2) (Option.none Code))]
                     (hn (Eq.trans (congrArg (fn [o :- (Option Code)] (codeF l (codeOf c1) o)) h2) (codeF_none_right l (codeOf c1)))))))))]))))

(prove-exp-lv! 'code_lift
  '(forall [kq Nat] (forall [cq Nat] (=> (=> (= (codeOf e) (Option.none Code)) False) (= (lift kq cq e) e))))
  '[kq cq]
  (fn [ctor fields]
    (case ctor
      sleaf '[(exact (code_sleaf a kq cq))]
      snode '[(exact (code_snode a c1 c2 kq cq (ih_c1 kq cq) (ih_c2 kq cq)))]
      ['(intro hn) '(exact (False.elim$0 (hn (Eq.refl$1 (Option.none Code)))))])))

(thm none_ne_someC [cc :- Code, h :- (= (Option.none Code) (Option.some Code cc))] False (cases h))

(thm code_lift_some [e :- Exp, cc :- Code, h :- (= (codeOf e) (Option.some Code cc)), k :- Nat, c :- Nat] (= (lift k c e) e)
  (exact (code_lift e k c (fn [h0 :- (Eq (Option Code) (codeOf e) (Option.none Code))] (none_ne_someC cc (Eq.trans (Eq.symm h0) h))))))

(thm boolExp_lift [b :- Bool, k :- Nat, c :- Nat] (= (lift k c (boolExp b)) (boolExp b))
  (cases b) (rfl) (rfl))

;; --- head steps -------------------------------------------------------------

(thm hd_cast [chkf :- (=> Code Code Bool), r :- Exp, a0 :- Exp, b0 :- Exp, h :- (Hd chkf r a0), e :- (= a0 b0)] (Hd chkf r b0)
  (subst e) (exact h))

;; A head step lifts to a head step, at every cutoff.  The contractum's
;; substitutions commute with lift by §2 (β, let, recN, recSyn); caseLbl's
;; branch by nthB_lift; δ's codes are closed (code_lift_some), and so is its
;; Boolean result.
(defn- hd-expand [form]
  (walk/postwalk
   (fn [x]
     (if (and (seq? x) (#{'L 'L1 'L2 'L5} (first x)))
       (list 'lift 1 (case (first x) L 'kc L1 '(+ kc 1) L2 '(+ kc 2) L5 '(+ kc 5)) (second x))
       x))
   form))

(def ^:private hd-cases
  '[;; beta
    (hd_cast chkf (L (Exp.app (Exp.lam r A t) u)) (subst1 (L u) (L1 t)) (L (subst1 u t))
      (Hd.beta chkf r (L A) (L1 t) (L u)) (Eq.symm (lift_subst1 u t kc)))
    ;; betaLet
    (hd_cast chkf (L (Exp.letp C (Exp.pair S x y) t)) (substL (liftL kc (List.cons Exp y (List.cons Exp x (List.nil Exp)))) (L2 t))
      (L (substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t))
      (Hd.betaLet chkf (L C) (L S) (L x) (L y) (L2 t))
      (Eq.symm (lift_substL (List.cons Exp y (List.cons Exp x (List.nil Exp))) t kc)))
    (Hd.iteT chkf (L t) (L e))
    (Hd.iteF chkf (L t) (L e))
    (Hd.elimT chkf (L1 P) (L t) (L e))
    (Hd.elimF chkf (L1 P) (L t) (L e))
    (Hd.recNZ chkf (L1 P) (L z) (L2 s))
    ;; recNS
    (hd_cast chkf (L (Exp.recN P z s (Exp.succ n)))
      (substL (liftL kc (List.cons Exp (Exp.recN P z s n) (List.cons Exp n (List.nil Exp)))) (L2 s))
      (L (substL (List.cons Exp (Exp.recN P z s n) (List.cons Exp n (List.nil Exp))) s))
      (Hd.recNS chkf (L1 P) (L z) (L2 s) (L n))
      (Eq.symm (lift_substL (List.cons Exp (Exp.recN P z s n) (List.cons Exp n (List.nil Exp))) s kc)))
    ;; caseLb
    (Hd.caseLb chkf (L1 P) l (L bs) (L b) (nthB_lift bs kc l b h))
    ;; recSL
    (hd_cast chkf (L (Exp.recS P tl tn (Exp.sleaf x))) (subst1 (L x) (L1 tl)) (L (subst1 x tl))
      (Hd.recSL chkf (L1 P) (L1 tl) (L5 tn) (L x)) (Eq.symm (lift_subst1 x tl kc)))
    ;; recSN
    (hd_cast chkf (L (Exp.recS P tl tn (Exp.snode x c1 c2)))
      (substL (liftL kc (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
               (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp))))))) (L5 tn))
      (L (substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
               (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn))
      (Hd.recSN chkf (L1 P) (L1 tl) (L5 tn) (L x) (L c1) (L c2))
      (Eq.symm (lift_substL (List.cons Exp (Exp.recS P tl tn c2) (List.cons Exp (Exp.recS P tl tn c1)
               (List.cons Exp c2 (List.cons Exp c1 (List.cons Exp x (List.nil Exp)))))) tn kc)))
    (Hd.itRL chkf (L X) (L g) (L h) (L x))
    (Hd.itRN chkf (L X) (L g) (L h) (L d) (L x) (L r1) (L r2))
    (Hd.prnL chkf (L x))
    (Hd.prnN chkf (L d) (L x) (L r1) (L r2))
    ;; delta: the codes are closed
    (hd_cast chkf (L (Exp.chk c d)) (boolExp (chkf cc dc)) (L (boolExp (chkf cc dc)))
      (Hd.delta chkf (L c) (L d) cc dc
        (Eq.trans (congrArg codeOf (code_lift_some c cc hc 1 kc)) hc)
        (Eq.trans (congrArg codeOf (code_lift_some d dc hd 1 kc)) hd))
      (Eq.symm (boolExp_lift (chkf cc dc) 1 kc)))
    (Hd.tTT chkf)
    (Hd.tTF chkf)])

(a/prove-theorem 'hd_lift
  '[chkf :- (=> Code Code Bool), r0 :- Exp, s0 :- Exp, der :- (Hd chkf r0 s0)]
  '(forall [kc Nat] (Hd chkf (lift 1 kc r0) (lift 1 kc s0)))
  (lv (into ['(induction der)] (mapcat (fn [c] ['(intro kc) (list 'exact (hd-expand c))]) hd-cases))))

;; --- positions ------------------------------------------------------------------

;; ChL kc e i ch c2: child i of e is ch, and in lift 1 kc e it is ch lifted at
;; c2 (kc plus the binders the child lives under); writing child i there
;; commutes with the lift.
(kdef ChL (=> Nat Exp Nat Exp Nat Prop)
  (fn [kc :- Nat, e :- Exp, i :- Nat, ch :- Exp, c2 :- Nat]
    (And (Eq (Option Exp) (child (lift 1 kc e) i) (Option.some Exp (lift 1 c2 ch)))
         (forall [x Exp] (Eq Exp (setKid (lift 1 kc e) i (lift 1 c2 x)) (lift 1 kc (setKid e i x)))))))

;; One lemma per constructor, with clean parameter names p0, p1, … (fields
;; such as n would collide with the names `cases` gives the index).  The
;; index is split into 0, 1, …, arity − 1 (each an existing child, read off
;; by computation) and ≥ arity (no child: the hypothesis is none = some).
(defn- child-lift-ctor! [ctor fields]
  (let [ps (mapv (fn [k [_ ty _]] [(symbol (str "p" k)) ty]) (range) fields)
        E (if (seq ps) (apply list (symbol (str "Exp." ctor)) (map first ps)) (symbol (str "Exp." ctor)))
        kids (keep-indexed (fn [k [_ ty depth]] (when (= ty 'Exp) [(symbol (str "p" k)) depth])) fields)
        leaf (fn [j [F depth]]
               (let [D (under 'kc depth)]
                 ['(intro ch hch)
                  (list 'have 'heq (list '= 'ch F) (list 'Eq.symm (list 'some_inj F 'ch 'hch)))
                  '(subst heq)
                  (list 'apply (list 'Exists.intro D))
                  '(unfold ChL)
                  (list 'exact (list 'And.intro (list 'Eq.refl$1 (list 'Option.some 'Exp (list 'lift 1 D F)))
                                     (list 'fn '[x :- Exp] (list 'Eq.refl$1 (list 'lift 1 'kc (list 'setKid E j 'x))))))]))]
    (a/prove-theorem (symbol (str "child_lift_" ctor))
      (lv (into (vec (mapcat (fn [[p ty]] [p :- ty]) ps)) '[kc :- Nat, ix :- Nat]))
      (lv (list 'forall '[ch Exp] (list '=> (list '= (list 'child E 'ix) '(Option.some Exp ch))
                                        (list 'Exists (list 'fn '[c2 :- Nat] (list 'ChL 'kc E 'ix 'ch 'c2))))))
      (lv (vec (concat
                (mapcat (fn [j kid] (cons (if (zero? j) '(cases ix) '(cases n)) (leaf j kid))) (range) kids)
                ['(intro ch hch) '(exact (False.elim$0 (none_ne_someE ch hch)))]))))))

(doseq [[ctor fields] exp-fields] (child-lift-ctor! ctor fields))

(prove-exp! 'child_lift
  '(forall [kc Nat] (forall [ix Nat] (forall [ch Exp] (=> (= (child e ix) (Option.some Exp ch))
     (Exists (fn [c2 :- Nat] (ChL kc e ix ch c2)))))))
  '[kc ix]
  (fn [ctor fields]
    [(list 'exact (apply list (symbol (str "child_lift_" ctor)) (concat (map first fields) ['kc 'ix])))]))

;; Reading and writing along i :: q, once child i is known.
(thm optrec_get [q :- (List Nat), o :- (Option Exp), C :- Exp, h :- (= o (Option.some Exp C))]
  (= (Option.rec$1$0 Exp (fn [_ :- (Option Exp)] (Option Exp)) (Option.none Exp) (fn [c :- Exp] (getP q c)) o) (getP q C))
  (subst h) (rfl))
(thm optrec_set [i :- Nat, q :- (List Nat), E :- Exp, y :- Exp, o :- (Option Exp), C :- Exp, h :- (= o (Option.some Exp C))]
  (= (Option.rec$1$0 Exp (fn [_ :- (Option Exp)] Exp) E (fn [c :- Exp] (setKid E i (setP q c y))) o) (setKid E i (setP q C y)))
  (subst h) (rfl))
(thm getP_some [i :- Nat, q :- (List Nat), E :- Exp, C :- Exp, h :- (= (child E i) (Option.some Exp C))]
  (= (getP (List.cons Nat i q) E) (getP q C))
  (exact (optrec_get q (child E i) C h)))
(thm setP_some [i :- Nat, q :- (List Nat), E :- Exp, C :- Exp, y :- Exp, h :- (= (child E i) (Option.some Exp C))]
  (= (setP (List.cons Nat i q) E y) (setKid E i (setP q C y)))
  (exact (optrec_set i q E y (child E i) C h)))

;; PathL p e kc r c2: the subterm of e at path p is r, and at the same path of
;; lift 1 kc e sits r lifted at c2; writing there commutes with the lift.
(kdef PathL (=> (List Nat) Exp Nat Exp Nat Prop)
  (fn [p :- (List Nat), e :- Exp, kc :- Nat, r :- Exp, c2 :- Nat]
    (And (Eq (Option Exp) (getP p (lift 1 kc e)) (Option.some Exp (lift 1 c2 r)))
         (forall [x Exp] (Eq Exp (setP p (lift 1 kc e) (lift 1 c2 x)) (lift 1 kc (setP p e x)))))))

(thm path_cons [i :- Nat, q :- (List Nat), e :- Exp, kc :- Nat, r :- Exp,
                hg :- (= (getP (List.cons Nat i q) e) (Option.some Exp r)),
                ihq :- (forall [e2 Exp] (forall [k2 Nat] (forall [r2 Exp]
                         (=> (= (getP q e2) (Option.some Exp r2)) (Exists (fn [c2 :- Nat] (PathL q e2 k2 r2 c2)))))))]
  (Exists (fn [c2 :- Nat] (PathL (List.cons Nat i q) e kc r c2)))
  (refine' (exExp _ _ (getP_cons i q e e r hg) _))
  (intro ch hc)
  (refine' (exNat _ _ (child_lift e kc i ch (And.left hc)) _))
  (intro c1 hc1)
  (refine' (exNat _ _ (ihq ch c1 r (And.left (And.right hc))) _))
  (intro c2 hp2)
  (apply (Exists.intro c2))
  (unfold PathL)
  (exact (And.intro
    (Eq.trans (getP_some i q (lift 1 kc e) (lift 1 c1 ch) (And.left hc1)) (And.left hp2))
    (fn [x :- Exp]
      (Eq.trans (setP_some i q (lift 1 kc e) (lift 1 c1 ch) (lift 1 c2 x) (And.left hc1))
        (Eq.trans (congrArg (fn [v :- Exp] (setKid (lift 1 kc e) i v)) ((And.right hp2) x))
          (Eq.trans ((And.right hc1) (setP q ch x))
            (congrArg (fn [v :- Exp] (lift 1 kc v)) (Eq.symm (setP_some i q e ch x (And.left hc)))))))))))

(thm path_lift [p :- (List Nat)]
  (forall [e Exp] (forall [kc Nat] (forall [r Exp]
    (=> (= (getP p e) (Option.some Exp r)) (Exists (fn [c2 :- Nat] (PathL p e kc r c2)))))))
  (induction p)
  (intro e kc r hg)
  ;; the empty path: r is e itself (subst eliminates r, the left side)
  (have heq (= r e) (Eq.symm (some_inj e r hg)))
  (subst heq)
  (apply (Exists.intro kc))
  (unfold PathL)
  (exact (And.intro (Eq.refl$1 (Option.some Exp (lift 1 kc e))) (fn [x :- Exp] (Eq.refl$1 (lift 1 kc x)))))
  ;; i :: q
  (intro e kc r hg)
  (exact (path_cons head tail e kc r hg ih_tail)))

;; --- steps and conversion -----------------------------------------------------

(thm step_lift_core [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp, kc :- Nat, p :- (List Nat), r :- Exp, r2 :- Exp,
                     hg :- (= (getP p e) (Option.some Exp r)), hd :- (Hd chkf r r2), he :- (= e2 (setP p e r2))]
  (Step chkf (lift 1 kc e) (lift 1 kc e2))
  (refine' (exNat _ _ (path_lift p e kc r hg) _))
  (intro c2 hp)
  (unfold Step)
  (apply (Exists.intro p))
  (apply (Exists.intro (lift 1 c2 r)))
  (apply (Exists.intro (lift 1 c2 r2)))
  (exact (And.intro (And.left hp)
           (And.intro (hd_lift chkf r r2 hd c2)
             (Eq.trans (congrArg (fn [v :- Exp] (lift 1 kc v)) he) (Eq.symm ((And.right hp) r2)))))))

;; A step lifts to a step (the same path, the redex lifted under the
;; binders on the path).
(thm step_lift [chkf :- (=> Code Code Bool), e :- Exp, e2 :- Exp, hs :- (Step chkf e e2), kc :- Nat]
  (Step chkf (lift 1 kc e) (lift 1 kc e2))
  (exact (exists_elimL
    (fn [p :- (List Nat)] (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
      (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2)))))))))
    (Step chkf (lift 1 kc e) (lift 1 kc e2))
    hs
    (fn [p :- (List Nat),
         hp :- (Exists (fn [r :- Exp] (Exists (fn [r2 :- Exp]
                 (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2))))))))]
      (exists_elimE
        (fn [r :- Exp] (Exists (fn [r2 :- Exp]
          (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2)))))))
        (Step chkf (lift 1 kc e) (lift 1 kc e2))
        hp
        (fn [r :- Exp,
             hr :- (Exists (fn [r2 :- Exp]
                     (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2))))))]
          (exists_elimE
            (fn [r2 :- Exp] (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2)))))
            (Step chkf (lift 1 kc e) (lift 1 kc e2))
            hr
            (fn [r2 :- Exp,
                 h3 :- (And (Eq (Option Exp) (getP p e) (Option.some Exp r)) (And (Hd chkf r r2) (Eq Exp e2 (setP p e r2))))]
              (step_lift_core chkf e e2 kc p r r2 (And.left h3) (And.left (And.right h3)) (And.right (And.right h3)))))))))))

;; Conversion is preserved by weakening: the chain is lifted step by step,
;; and every element stays skeleton-well-formed in the extended skeleton
;; context (skj_weaken, lcert.formal.substitution).
(thm cv_lift [chkf :- (=> Code Code Bool), G :- (List Sk), c :- Nat, xs :- Sk, A0 :- Exp, B0 :- Exp, der :- (Cv chkf G A0 B0)]
  (Cv chkf (insS c xs G) (lift 1 c A0) (lift 1 c B0))
  (induction der)
  (exact (Cv.cvRefl chkf (insS c xs G) (lift 1 c A) (skj_weaken Bool.true G A Sk.unit h c xs)
                   (nbr_lift A 1 c hn)))
  (exact (Cv.cvFwd chkf (insS c xs G) (lift 1 c A) (lift 1 c B) (lift 1 c C) ih_hab (step_lift chkf B C hs c)
                   (skj_weaken Bool.true G C Sk.unit hc c xs) (nbr_lift C 1 c hn)))
  (exact (Cv.cvBwd chkf (insS c xs G) (lift 1 c A) (lift 1 c B) (lift 1 c C) ih_hab (step_lift chkf C B hs c)
                   (skj_weaken Bool.true G C Sk.unit hc c xs) (nbr_lift C 1 c hn))))

;; ===========================================================================
;; §4  Insertion into contexts and usage vectors
;; ===========================================================================

;; insD c X D: D with X inserted at position c (innermost first).  An entry
;; before the insertion point (position j < c) has X inserted c − j − 1
;; places into its tail, so it is lifted at that cutoff:
;;   insD 0 X D            = X :: D
;;   insD (k+1) X (A :: D) = lift 1 k A :: insD k X D
;;   insD (k+1) X []       = []      (past the end: nothing is inserted)
;; Recursion on c returning a function of D (as insS, substitution.clj), so
;; each equation holds definitionally.
(kdef insDF (=> Nat Exp (List Exp) (List Exp))
  (fn [c :- Nat, X :- Exp]
    (Nat.rec$1 (fn [_ :- Nat] (=> (List Exp) (List Exp)))
      (fn [D :- (List Exp)] (List.cons Exp X D))
      (fn [k :- Nat, ih :- (=> (List Exp) (List Exp))]
        (fn [D :- (List Exp)]
          (List.rec$1$0 Exp (fn [_ :- (List Exp)] (List Exp))
            (List.nil Exp)
            (fn [A :- Exp, rest :- (List Exp), _ :- (List Exp)] (List.cons Exp (lift 1 k A) (ih rest)))
            D)))
      c)))

(kdef insD (=> Nat Exp (List Exp) (List Exp))
  (fn [c :- Nat, X :- Exp, D :- (List Exp)] (insDF c X D)))

;; insU c r us: the usage r inserted at position c (no lifting).
(kdef insUF (=> Nat U (List U) (List U))
  (fn [c :- Nat, r :- U]
    (Nat.rec$1 (fn [_ :- Nat] (=> (List U) (List U)))
      (fn [us :- (List U)] (List.cons U r us))
      (fn [k :- Nat, ih :- (=> (List U) (List U))]
        (fn [us :- (List U)]
          (List.rec$1$0 U (fn [_ :- (List U)] (List U))
            (List.nil U)
            (fn [a :- U, rest :- (List U), _ :- (List U)] (List.cons U a (ih rest)))
            us)))
      c)))

(kdef insU (=> Nat U (List U) (List U))
  (fn [c :- Nat, r :- U, us :- (List U)] (insUF c r us)))

(thm insD_zero [X :- Exp, D :- (List Exp)] (= (insD 0 X D) (List.cons Exp X D)) (rfl))
(thm insD_succ_cons [c :- Nat, X :- Exp, A :- Exp, D :- (List Exp)]
  (= (insD (+ c 1) X (List.cons Exp A D)) (List.cons Exp (lift 1 c A) (insD c X D))) (rfl))

;; The skeletons of the extended context are the skeleton context extended
;; at the same position (lifting preserves skeletons, skel_lift).
(thm skels_insD [X :- Exp, c :- Nat]
  (forall [D (List Exp)] (= (skels (insD c X D)) (insS c (skel X) (skels D))))
  (induction c)
  (intro D)
  (rfl)
  (intro D)
  (cases D)
  (rfl)
  (exact (Eq.trans (congrArg (fn [v :- Sk] (List.cons Sk v (skels (insD n X tail)))) (skel_lift head 1 n))
                   (congrArg (fn [v :- (List Sk)] (List.cons Sk (skel head) v)) (ih_n tail)))))

;; Lookup at or above the insertion point: index m + c + 1 of the extended
;; context is index m + c of the original.  (nthE is well-founded: its
;; equation lemmas nthE.eq_1..3.)
(thm nthE_insD_above [X :- Exp, m :- Nat, c :- Nat]
  (forall [D (List Exp)] (= (nthE (insD c X D) (+ (+ m c) 1)) (nthE D (+ m c))))
  (induction c)
  (intro D)
  (exact (nthE.eq_3 X D (+ m 0)))
  (intro D)
  (cases D)
  (exact (Eq.trans (nthE.eq_1 (+ (+ m (+ n 1)) 1)) (Eq.symm (nthE.eq_1 (+ m (+ n 1))))))
  (exact (Eq.trans (nthE.eq_3 (lift 1 n head) (insD n X tail) (+ (+ m n) 1))
           (Eq.trans (ih_n tail) (Eq.symm (nthE.eq_3 head tail (+ m n)))))))

;; Lookup below the insertion point (index i < c = m + i + 1): the entry
;; lifted at m = c − i − 1.
(thm nthE_insD_below [X :- Exp, m :- Nat, i :- Nat]
  (forall [D (List Exp)] (forall [A Exp]
    (=> (= (nthE D i) (Option.some Exp A)) (= (nthE (insD (+ (+ m i) 1) X D) i) (Option.some Exp (lift 1 m A))))))
  (induction i)
  (intro D A)
  (cases D)
  (intro h)
  (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 0)) h))))
  (intro h)
  (exact (Eq.trans (nthE.eq_2 (lift 1 (+ m 0) head) (insD (+ m 0) X tail))
           (congrArg (fn [v :- Exp] (Option.some Exp (lift 1 m v))) (some_inj head A (Eq.trans (Eq.symm (nthE.eq_2 head tail)) h)))))
  (intro D A)
  (cases D)
  (intro h)
  (exact (False.elim$0 (none_ne_someE A (Eq.trans (Eq.symm (nthE.eq_1 (+ n 1))) h))))
  (intro h)
  (exact (Eq.trans (nthE.eq_3 (lift 1 (+ (+ m n) 1) head) (insD (+ (+ m n) 1) X tail) n)
           (ih_n tail A (Eq.trans (Eq.symm (nthE.eq_3 head tail n)) h)))))

;; The same for usage vectors (nthU.eq_1..3).
(thm nthU_insU_above [r :- U, m :- Nat, c :- Nat]
  (forall [us (List U)] (= (nthU (insU c r us) (+ (+ m c) 1)) (nthU us (+ m c))))
  (induction c)
  (intro us)
  (exact (nthU.eq_3 r us (+ m 0)))
  (intro us)
  (cases us)
  (exact (Eq.trans (nthU.eq_1 (+ (+ m (+ n 1)) 1)) (Eq.symm (nthU.eq_1 (+ m (+ n 1))))))
  (exact (Eq.trans (nthU.eq_3 head (insU n r tail) (+ (+ m n) 1))
           (Eq.trans (ih_n tail) (Eq.symm (nthU.eq_3 head tail (+ m n)))))))

(thm none_ne_someU [v :- U, h :- (= (Option.none U) (Option.some U v))] False (cases h))

(thm nthU_insU_below [r :- U, m :- Nat, i :- Nat]
  (forall [us (List U)] (forall [v U]
    (=> (= (nthU us i) (Option.some U v)) (= (nthU (insU (+ (+ m i) 1) r us) i) (Option.some U v)))))
  (induction i)
  (intro us v)
  (cases us)
  (intro h)
  (exact (False.elim$0 (none_ne_someU v (Eq.trans (Eq.symm (nthU.eq_1 0)) h))))
  (intro h)
  (exact (Eq.trans (nthU.eq_2 head (insU (+ m 0) r tail)) (Eq.trans (Eq.symm (nthU.eq_2 head tail)) h)))
  (intro us v)
  (cases us)
  (intro h)
  (exact (False.elim$0 (none_ne_someU v (Eq.trans (Eq.symm (nthU.eq_1 (+ n 1))) h))))
  (intro h)
  (exact (Eq.trans (nthU.eq_3 head (insU (+ (+ m n) 1) r tail) n)
           (ih_n tail v (Eq.trans (Eq.symm (nthU.eq_3 head tail n)) h)))))

;; Lengths stay equal (the axioms' side condition lenU us = lenE D).
(thm succ_inj_h [a0 :- Nat, b0 :- Nat, h :- (= (+ a0 1) (+ b0 1))] (= a0 b0) (omega))

;; (The cons/cons cases are separate lemmas with clean parameters: a second
;; `cases` on a list names its fields unstably; they are applied with _.)
(thm len_cons [X :- Exp, r :- U, c :- Nat, A :- Exp, D :- (List Exp), u :- U, us :- (List U),
               ih :- (forall [D2 (List Exp)] (forall [us2 (List U)]
                       (=> (= (lenU us2) (lenE D2)) (= (lenU (insU c r us2)) (lenE (insD c X D2)))))),
               h :- (= (lenU (List.cons U u us)) (lenE (List.cons Exp A D)))]
  (= (lenU (insU (+ c 1) r (List.cons U u us))) (lenE (insD (+ c 1) X (List.cons Exp A D))))
  (exact (congrArg (fn [v :- Nat] (+ v 1)) (ih D us (succ_inj_h (lenU us) (lenE D) h)))))

(thm len_ins [X :- Exp, r :- U, c :- Nat]
  (forall [D (List Exp)] (forall [us (List U)]
    (=> (= (lenU us) (lenE D)) (= (lenU (insU c r us)) (lenE (insD c X D))))))
  (induction c)
  (intro D us h)
  (exact (congrArg (fn [v :- Nat] (+ v 1)) h))
  (intro D us)
  (cases D)
  (cases us)
  (intro h)
  (rfl)
  (intro h)
  (exact (absurd h (Nat.succ_ne_zero (lenU tail))))
  (cases us)
  (intro h)
  (exact (absurd (Eq.symm h) (Nat.succ_ne_zero (lenE tail))))
  (intro h)
  (exact (len_cons X r n head tail _ _ ih_n h)))

;; Usage-vector arithmetic commutes with insertion.  vadd truncates to the
;; shorter vector; the identity holds at every position regardless.
(thm vadd_cons [c :- Nat, a :- U, x :- (List U), b :- U, y :- (List U), a0 :- U, b0 :- U,
                ih :- (forall [x2 (List U)] (forall [y2 (List U)] (forall [a1 U] (forall [b1 U]
                        (= (vadd (insU c a1 x2) (insU c b1 y2)) (insU c (uadd a1 b1) (vadd x2 y2)))))))]
  (= (vadd (insU (+ c 1) a0 (List.cons U a x)) (insU (+ c 1) b0 (List.cons U b y)))
     (insU (+ c 1) (uadd a0 b0) (vadd (List.cons U a x) (List.cons U b y))))
  (exact (congrArg (fn [v :- (List U)] (List.cons U (uadd a b) v)) (ih x y a0 b0))))

(thm vadd_insU [c :- Nat]
  (forall [x (List U)] (forall [y (List U)] (forall [a0 U] (forall [b0 U]
    (= (vadd (insU c a0 x) (insU c b0 y)) (insU c (uadd a0 b0) (vadd x y)))))))
  (induction c)
  (intro x y a0 b0)
  (rfl)
  (intro x y a0 b0)
  (cases x)
  (rfl)
  (cases y)
  (rfl)
  (exact (vadd_cons n head tail _ _ a0 b0 ih_n)))

(thm vscale_insU [r :- U, c :- Nat]
  (forall [x (List U)] (forall [a0 U] (= (vscale r (insU c a0 x)) (insU c (umul r a0) (vscale r x)))))
  (induction c)
  (intro x a0)
  (rfl)
  (intro x a0)
  (cases x)
  (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U (umul r head) v)) (ih_n tail a0))))

;; The instances at usage 0 (0 + 0 = 0, ρ · 0 = 0).
(thm vadd_ins0 [c :- Nat, x :- (List U), y :- (List U)]
  (= (vadd (insU c U.u0 x) (insU c U.u0 y)) (insU c U.u0 (vadd x y)))
  (exact (vadd_insU c x y U.u0 U.u0)))

(thm vscale_ins0 [r :- U, c :- Nat, x :- (List U)]
  (= (vscale r (insU c U.u0 x)) (insU c U.u0 (vscale r x)))
  (exact (Eq.trans (vscale_insU r c x U.u0)
                   (congrArg (fn [v :- U] (insU c v (vscale r x))) (umul_zero_right r)))))

;; ===========================================================================
;; §5  Lemma 2.1 (weakening)
;; ===========================================================================

;; --- side conditions of the axioms --------------------------------------------

;; Base types and ◇ (fBase), the constants' types (zConst, rConst), and
;; reflect's base type with its code (zRefl, rRefl) have no variables.
(a/prove-theorem 'baseDia_lift '[X :- Exp]
  '(=> (= (isBaseOrDia X) true) (forall [k Nat] (forall [c Nat] (= (lift k c X) X))))
  (into ['(cases X)]
        (mapcat (fn [[ctor _]]
                  (if ('#{tEmpty tUnit tBool tNat tLbl tSyn tDia tR} ctor) '[(intro h k c) (rfl)] '[(intro h) (cases h)]))
                exp-fields)))

;; constTyped t A = true only for a constant t at its base type; the split on
;; A tries `exact h` first (the matching base type, or a type whose clause
;; is false on both sides), and refutes h otherwise (a variable, whose lift
;; does not compute).
(def ^:private const-ctors '#{star tt ff zero lbl})
(a/prove-theorem 'constTyped_lift '[t :- Exp]
  '(forall [A Exp] (=> (= (constTyped t A) true) (forall [k Nat] (forall [c Nat] (= (constTyped (lift k c t) (lift k c A)) true)))))
  (into ['(cases t)]
        (mapcat (fn [[ctor _]]
                  (if (const-ctors ctor)
                    (into ['(intro A) '(cases A)]
                          (mapcat (fn [_] ['(intro h k c) '(first (exact h) (cases h))]) exp-fields))
                    ['(intro A h) '(cases h)]))
                exp-fields)))

;; baseCode X = some cd: X is a base data type and cd its (closed) code.
(def ^:private base-label '{tEmpty 15 tUnit 16 tBool 17 tNat 18 tLbl 19 tSyn 20 tR 22})
(a/prove-theorem 'baseCode_lift '[X :- Exp]
  '(forall [cd Exp] (=> (= (baseCode X) (Option.some Exp cd))
     (forall [k Nat] (forall [c Nat] (= (baseCode (lift k c X)) (Option.some Exp (lift k c cd)))))))
  (lv (into ['(cases X)]
            (mapcat (fn [[ctor _]]
                      (if-let [l (base-label ctor)]
                        ['(intro cd hb k c)
                         (list 'have 'heq (list '= 'cd (list 'cLeaf l)) (list 'Eq.symm (list 'some_inj (list 'cLeaf l) 'cd 'hb)))
                         '(subst heq)
                         '(rfl)]
                        ['(intro cd hb) '(exact (False.elim$0 (none_ne_someE cd hb)))]))
                    exp-fields))))

;; --- casts ------------------------------------------------------------------------

(thm tl_cast [chkf :- (=> Code Code Bool), w :- Bool, D :- (List Exp), t :- Exp, A :- Exp, B :- Exp,
              h :- (Tl chkf w D t A), e :- (= A B)]
  (Tl chkf w D t B)
  (subst e) (exact h))

(thm tl_cast3 [chkf :- (=> Code Code Bool), w :- Bool, D :- (List Exp), t :- Exp, t2 :- Exp, A :- Exp, B :- Exp,
               h :- (Tl chkf w D t A), et :- (= t t2), e :- (= A B)]
  (Tl chkf w D t2 B)
  (subst et) (subst e) (exact h))

(thm tl_ctx1 [chkf :- (=> Code Code Bool), w :- Bool, a0 :- Exp, a1 :- Exp, G :- (List Exp), t :- Exp, A :- Exp,
              h :- (Tl chkf w (List.cons Exp a0 G) t A), e :- (= a0 a1)]
  (Tl chkf w (List.cons Exp a1 G) t A)
  (subst e) (exact h))

(thm tl_ctx2 [chkf :- (=> Code Code Bool), w :- Bool, a0 :- Exp, a1 :- Exp, b0 :- Exp, b1 :- Exp, G :- (List Exp), t :- Exp, A :- Exp,
              h :- (Tl chkf w (List.cons Exp a0 (List.cons Exp b0 G)) t A), ea :- (= a0 a1), eb :- (= b0 b1)]
  (Tl chkf w (List.cons Exp a1 (List.cons Exp b1 G)) t A)
  (subst ea) (subst eb) (exact h))

;; --- the variable rule ---------------------------------------------------------------

;; Lookup at an index i ≥ c, as nthE_insD_above with m = i − c.
(thm nthE_ins_ge [X :- Exp, c :- Nat, i :- Nat, D :- (List Exp), hle :- (LE.le c i)]
  (= (nthE (insD c X D) (+ i 1)) (nthE D i))
  (exact (Eq.trans (congrArg (fn [j :- Nat] (nthE (insD c X D) (+ j 1))) (Eq.symm (Nat.sub_add_cancel hle)))
           (Eq.trans (nthE_insD_above X (- i c) c D)
             (congrArg (fn [j :- Nat] (nthE D j)) (Nat.sub_add_cancel hle))))))

(thm nthU_ins_ge [r :- U, c :- Nat, i :- Nat, us :- (List U), hle :- (LE.le c i)]
  (= (nthU (insU c r us) (+ i 1)) (nthU us i))
  (exact (Eq.trans (congrArg (fn [j :- Nat] (nthU (insU c r us) (+ j 1))) (Eq.symm (Nat.sub_add_cancel hle)))
           (Eq.trans (nthU_insU_above r (- i c) c us)
             (congrArg (fn [j :- Nat] (nthU us j)) (Nat.sub_add_cancel hle))))))

;; Lookup at an index i < c: c = (c − i − 1) + i + 1.
(thm cut_eq [i :- Nat, c :- Nat, h :- (LT.lt i c)] (= (+ (+ (- (- c i) 1) i) 1) c) (omega))
(thm cut_eq2 [i :- Nat, c :- Nat, h :- (LT.lt i c)] (= (+ (- (- c i) 1) (+ i 1)) c) (omega))
(thm le_succ_of_le [c :- Nat, i :- Nat, h :- (LE.le c i)] (LE.le c (+ i 1)) (omega))

(thm nthE_ins_lt [X :- Exp, c :- Nat, i :- Nat, D :- (List Exp), A :- Exp, h :- (LT.lt i c),
                  hA :- (= (nthE D i) (Option.some Exp A))]
  (= (nthE (insD c X D) i) (Option.some Exp (lift 1 (- (- c i) 1) A)))
  (exact (Eq.trans (congrArg (fn [j :- Nat] (nthE (insD j X D) i)) (Eq.symm (cut_eq i c h)))
                   (nthE_insD_below X (- (- c i) 1) i D A hA))))

(thm nthU_ins_lt [r :- U, c :- Nat, i :- Nat, us :- (List U), v :- U, h :- (LT.lt i c),
                  hv :- (= (nthU us i) (Option.some U v))]
  (= (nthU (insU c r us) i) (Option.some U v))
  (exact (Eq.trans (congrArg (fn [j :- Nat] (nthU (insU j r us) i)) (Eq.symm (cut_eq i c h)))
                   (nthU_insU_below r (- (- c i) 1) i us v hv))))

;; The variable's type lift (i+1) 0 A, after weakening: above the insertion
;; point the index moves up by one (lift_comp_ge), below it the stored
;; entry is lifted and the lifts commute (lift_lift_comm).
(thm var_ty_ge [A :- Exp, c :- Nat, i :- Nat, hle :- (LE.le c i)]
  (= (lift (+ (+ i 1) 1) 0 A) (lift 1 c (lift (+ i 1) 0 A)))
  (exact (Eq.symm (lift_comp_ge A 1 (+ i 1) c (le_succ_of_le c i hle) 0))))

(thm var_ty_lt [A :- Exp, c :- Nat, i :- Nat, h :- (LT.lt i c)]
  (= (lift (+ i 1) 0 (lift 1 (- (- c i) 1) A)) (lift 1 c (lift (+ i 1) 0 A)))
  (exact (Eq.trans (Eq.symm (lift_lift_comm A (+ i 1) (- (- c i) 1) 0))
                   (congrArg (fn [j :- Nat] (lift 1 j (lift (+ i 1) 0 A))) (cut_eq2 i c h)))))

(thm tl_var [chkf :- (=> Code Code Bool), D :- (List Exp), i :- Nat, A :- Exp, hA :- (= (nthE D i) (Option.some Exp A)),
             cc :- Nat, XX :- Exp]
  (Tl chkf Bool.false (insD cc XX D) (lift 1 cc (Exp.var i)) (lift 1 cc (lift (+ i 1) 0 A)))
  (have hc (Decidable (Nat.lt i cc)) (Nat.decLt i cc))
  (cases hc)
  (exact (tl_cast3 chkf Bool.false (insD cc XX D) (Exp.var (+ i 1)) (lift 1 cc (Exp.var i))
           (lift (+ (+ i 1) 1) 0 A) (lift 1 cc (lift (+ i 1) 0 A))
           (Tl.zVar chkf (insD cc XX D) (+ i 1) A (Eq.trans (nthE_ins_ge XX cc i D (Nat.le_of_not_lt h)) hA))
           (Eq.symm (lift_var_above 1 cc i (Nat.le_of_not_lt h)))
           (var_ty_ge A cc i (Nat.le_of_not_lt h))))
  (exact (tl_cast3 chkf Bool.false (insD cc XX D) (Exp.var i) (lift 1 cc (Exp.var i))
           (lift (+ i 1) 0 (lift 1 (- (- cc i) 1) A)) (lift 1 cc (lift (+ i 1) 0 A))
           (Tl.zVar chkf (insD cc XX D) i (lift 1 (- (- cc i) 1) A) (nthE_ins_lt XX cc i D A h hA))
           (Eq.symm (lift_var_below 1 cc i h))
           (var_ty_lt A cc i h))))

;; Conversion in the extended context (cv_lift, with skels_insD).
(thm cv_weaken [chkf :- (=> Code Code Bool), D :- (List Exp), A :- Exp, B :- Exp, hc :- (Cv chkf (skels D) A B),
                cc :- Nat, XX :- Exp]
  (Cv chkf (skels (insD cc XX D)) (lift 1 cc A) (lift 1 cc B))
  (rw [(skels_insD XX cc D)])
  (exact (cv_lift chkf (skels D) cc (skel XX) A B hc)))

;; --- Lemma 2.1 at type level ------------------------------------------------------
;; One explicit term per rule of Tl (judgment.clj), in the rule table's order,
;; written with the abbreviations
;;   DI              the extended context insD cc XX D
;;   (L k F)         F lifted at cc + k (a field under k binders)
;;   (IH ih k)       the hypothesis ih at cutoff cc + k (a premise under k
;;                   binders; its context insD (cc+k) XX (… :: D) reduces to
;;                   the rule's extended context over DI)
;;   (TC t A B h e)  tl_cast: h : Tl false DI t A and e : A = B
;; Substituted types are moved through the lift by §2, conversion by §3.
(defn- wk-expand [form]
  (let [cut (fn [k] (if (zero? k) 'cc (list '+ 'cc k)))]
    (walk/postwalk
     (fn [x]
       (cond
         (= x 'DI) '(insD cc XX D)
         (and (seq? x) (= (first x) 'L)) (let [[_ k F] x] (list 'lift 1 (cut k) F))
         (and (seq? x) (= (first x) 'IH)) (let [[_ h k] x] (list h (cut k) 'XX))
         (and (seq? x) (= (first x) 'TC)) (let [[_ t A B h e] x] (list 'tl_cast 'chkf 'Bool.false '(insD cc XX D) t A B h e))
         :else x))
     form)))

(def ^:private tl-wk-cases
  '[;; fBase
    (Tl.fBase chkf DI (L 0 X) (Eq.trans (congrArg isBaseOrDia (baseDia_lift X h 1 cc)) h))
    ;; fT, fPi, fSig
    (Tl.fT chkf DI (L 0 b) (IH ih_hb 0))
    (Tl.fPi chkf DI r (L 0 A) (L 1 B) (IH ih_hA 0) (IH ih_hB 1))
    (Tl.fSig chkf DI r (L 0 A) (L 1 B) (IH ih_hA 0) (IH ih_hB 1))
    ;; zVar
    (tl_var chkf D i A h cc XX)
    ;; zConst
    (Tl.zConst chkf DI (L 0 t) (L 0 A) (constTyped_lift t A h 1 cc))
    ;; zConv
    (Tl.zConv chkf DI (L 0 t) (L 0 A) (L 0 B) (IH ih_ht 0) (IH ih_hB 0) (cv_weaken chkf D A B hc cc XX))
    ;; zLam
    (Tl.zLam chkf DI r (L 0 A) (L 1 t) (L 1 B) (IH ih_hA 0) (IH ih_ht 1))
    ;; zApp
    (TC (Exp.app (L 0 f) (L 0 u)) (subst1 (L 0 u) (L 1 B)) (L 0 (subst1 u B))
      (Tl.zApp chkf DI r (L 0 f) (L 0 u) (L 0 A) (L 1 B) (IH ih_hf 0) (IH ih_hu 0) (IH ih_hA 0) (IH ih_hB 1))
      (Eq.symm (lift_subst1 u B cc)))
    ;; zPair
    (Tl.zPair chkf DI r (L 0 A) (L 1 B) (L 0 x) (L 0 y) (IH ih_hA 0) (IH ih_hB 1) (IH ih_hx 0)
      (TC (L 0 y) (L 0 (subst1 x B)) (subst1 (L 0 x) (L 1 B)) (IH ih_hy 0) (lift_subst1 x B cc)))
    ;; zLet
    (Tl.zLet chkf DI r (L 0 A) (L 1 B) (L 0 C) (L 0 p) (L 2 t) (IH ih_hp 0) (IH ih_hC 0) (IH ih_hA 0) (IH ih_hB 1)
      (tl_cast chkf Bool.false (List.cons Exp (L 1 B) (List.cons Exp (L 0 A) DI)) (L 2 t) (L 2 (lift 2 0 C)) (lift 2 0 (L 0 C))
        (IH ih_ht 2) (lift_lift2 C cc)))
    ;; zAbort, zIte
    (Tl.zAbort chkf DI (L 0 A) (L 0 t) (IH ih_ht 0) (IH ih_hA 0))
    (Tl.zIte chkf DI (L 0 b) (L 0 t) (L 0 e) (L 0 C) (IH ih_hb 0) (IH ih_ht 0) (IH ih_he 0))
    ;; zElimB
    (TC (Exp.elimB (L 1 P) (L 0 b) (L 0 t) (L 0 e)) (subst1 (L 0 b) (L 1 P)) (L 0 (subst1 b P))
      (Tl.zElimB chkf DI (L 1 P) (L 0 b) (L 0 t) (L 0 e) (IH ih_hb 0) (IH ih_hP 1)
        (TC (L 0 t) (L 0 (subst1 Exp.tt P)) (subst1 Exp.tt (L 1 P)) (IH ih_ht 0) (lift_subst1 Exp.tt P cc))
        (TC (L 0 e) (L 0 (subst1 Exp.ff P)) (subst1 Exp.ff (L 1 P)) (IH ih_he 0) (lift_subst1 Exp.ff P cc)))
      (Eq.symm (lift_subst1 b P cc)))
    ;; zSucc
    (Tl.zSucc chkf DI (L 0 n) (IH ih_h 0))
    ;; zRecN: the step under x : Nat, y : P, at cc + 2
    (TC (Exp.recN (L 1 P) (L 0 z) (L 2 s) (L 0 n)) (subst1 (L 0 n) (L 1 P)) (L 0 (subst1 n P))
      (Tl.zRecN chkf DI (L 1 P) (L 0 z) (L 2 s) (L 0 n) (IH ih_hn 0) (IH ih_hP 1)
        (TC (L 0 z) (L 0 (subst1 Exp.zero P)) (subst1 Exp.zero (L 1 P)) (IH ih_hz 0) (lift_subst1 Exp.zero P cc))
        (tl_cast chkf Bool.false (List.cons Exp (L 1 P) (List.cons Exp Exp.tNat DI)) (L 2 s) (L 2 (stepTy P)) (stepTy (L 1 P))
          (IH ih_hs 2) (lift_stepTy P cc)))
      (Eq.symm (lift_subst1 n P cc)))
    ;; zCaseL
    (TC (Exp.caseL (L 1 P) (L 0 x) (L 0 bs)) (subst1 (L 0 x) (L 1 P)) (L 0 (subst1 x P))
      (Tl.zCaseL chkf DI (L 1 P) (L 0 x) (L 0 bs) (IH ih_hx 0) (IH ih_hP 1) (IH ih_hb 0))
      (Eq.symm (lift_subst1 x P cc)))
    ;; zBnil, zBcons
    (Tl.zBnil chkf DI (L 1 P))
    (Tl.zBcons chkf DI (L 1 P) k (L 0 h) (L 0 t)
      (TC (L 0 h) (L 0 (subst1 (Exp.lbl k) P)) (subst1 (Exp.lbl k) (L 1 P)) (IH ih_hh 0) (lift_subst1 (Exp.lbl k) P cc))
      (IH ih_ht 0))
    ;; zSleaf, zSnode
    (Tl.zSleaf chkf DI (L 0 x) (IH ih_h 0))
    (Tl.zSnode chkf DI (L 0 x) (L 0 c1) (L 0 c2) (IH ih_hx 0) (IH ih_h1 0) (IH ih_h2 0))
    ;; zRecS: the leaf step under a (cc + 1), the node step under a c1 c2 y1 y2 (cc + 5)
    (TC (Exp.recS (L 1 P) (L 1 tl) (L 5 tn) (L 0 c)) (subst1 (L 0 c) (L 1 P)) (L 0 (subst1 c P))
      (Tl.zRecS chkf DI (L 1 P) (L 1 tl) (L 5 tn) (L 0 c) (IH ih_hc 0) (IH ih_hP 1)
        (tl_cast chkf Bool.false (List.cons Exp Exp.tLbl DI) (L 1 tl) (L 1 (leafTy P)) (leafTy (L 1 P))
          (IH ih_hl 1) (lift_leafTy P cc))
        (tl_ctx2 chkf Bool.false (L 4 (y2Ty P)) (y2Ty (L 1 P)) (L 3 (y1Ty P)) (y1Ty (L 1 P))
          (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI))) (L 5 tn) (nodeTy (L 1 P))
          (tl_cast chkf Bool.false
            (List.cons Exp (L 4 (y2Ty P)) (List.cons Exp (L 3 (y1Ty P))
              (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI)))))
            (L 5 tn) (L 5 (nodeTy P)) (nodeTy (L 1 P)) (IH ih_hn 5) (lift_nodeTy P cc))
          (lift_y2Ty P cc) (lift_y1Ty P cc)))
      (Eq.symm (lift_subst1 c P cc)))
    ;; zLeaf, zNode
    (Tl.zLeaf chkf DI (L 0 x) (IH ih_h 0))
    (Tl.zNode chkf DI (L 0 d) (L 0 x) (L 0 r1) (L 0 r2) (IH ih_hd 0) (IH ih_hx 0) (IH ih_h1 0) (IH ih_h2 0))
    ;; zItR
    (Tl.zItR chkf DI (L 0 X) (L 0 g) (L 0 h) (L 0 r) (IH ih_hX 0)
      (TC (L 0 g) (L 0 (gTy X)) (gTy (L 0 X)) (IH ih_hg 0) (lift_gTy X cc))
      (TC (L 0 h) (L 0 (hTy X)) (hTy (L 0 X)) (IH ih_hh 0) (lift_hTy X cc))
      (IH ih_hr 0))
    ;; zPrn, zChk, zH1
    (Tl.zPrn chkf DI (L 0 r) (IH ih_h 0))
    (Tl.zChk chkf DI (L 0 c) (L 0 d) (IH ih_hc 0) (IH ih_hd 0))
    (Tl.zH1 chkf DI (L 0 r) (L 0 s) (L 0 c) (L 0 e1) (L 0 e2) (IH ih_hr 0) (IH ih_hs 0) (IH ih_hc 0) (IH ih_h1 0) (IH ih_h2 0))
    ;; zRefl: X and its code are closed
    (Tl.zRefl chkf DI (L 0 X) (L 0 cd) (L 0 r) (L 0 e) (baseCode_lift X cd hb 1 cc) (IH ih_hr 0) (IH ih_he 0))
    ;; zInsp: the branches under r : R and a T(…) proof (cc + 2); the
    ;; context entries mention c lifted past r (lift_lift_comm)
    (Tl.zInsp chkf DI (L 0 X) (L 0 r) (L 0 c) (L 2 t1) (L 2 t2) (IH ih_hr 0) (IH ih_hc 0) (IH ih_hX 0)
      (tl_ctx1 chkf Bool.false (L 1 (chkT (Exp.var 0) (lift 1 0 c))) (chkT (Exp.var 0) (lift 1 0 (L 0 c)))
        (List.cons Exp Exp.tR DI) (L 2 t1) (lift 2 0 (L 0 X))
        (tl_cast chkf Bool.false (List.cons Exp (L 1 (chkT (Exp.var 0) (lift 1 0 c))) (List.cons Exp Exp.tR DI))
          (L 2 t1) (L 2 (lift 2 0 X)) (lift 2 0 (L 0 X)) (IH ih_h1 2) (lift_lift2 X cc))
        (congrArg (fn [v :- Exp] (chkT (Exp.var 0) v)) (lift_lift_comm c 1 cc 0)))
      (tl_ctx1 chkf Bool.false (L 1 (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))))
        (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 (L 0 c)))))
        (List.cons Exp Exp.tR DI) (L 2 t2) (lift 2 0 (L 0 X))
        (tl_cast chkf Bool.false (List.cons Exp (L 1 (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))) (List.cons Exp Exp.tR DI))
          (L 2 t2) (L 2 (lift 2 0 X)) (lift 2 0 (L 0 X)) (IH ih_h2 2) (lift_lift2 X cc))
        (congrArg (fn [v :- Exp] (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) v)))) (lift_lift_comm c 1 cc 0))))])

;; Lemma 2.1 (weakening), type level: formation and :⁰ typing are preserved
;; by inserting any entry XX at any position cc, the subject and its type
;; lifted at cc.
(a/prove-theorem 'tl_weaken
  '[chkf :- (=> Code Code Bool), w0 :- Bool, D0 :- (List Exp), t0 :- Exp, A0 :- Exp, der :- (Tl chkf w0 D0 t0 A0)]
  '(forall [cc Nat] (forall [XX Exp] (Tl chkf w0 (insD cc XX D0) (lift 1 cc t0) (lift 1 cc A0))))
  (lv (into ['(induction der)] (mapcat (fn [c] ['(intro cc XX) (list 'exact (wk-expand c))]) tl-wk-cases))))

;; --- Lemma 2.1 at runtime ------------------------------------------------------------
;; The new entry has usage 0 (insU cc U.u0).  Sums and scalings of usage
;; vectors commute with inserting 0 (vadd_ins0, vscale_ins0), so a rule's
;; conclusion over the premises' extended vectors is its conclusion over the
;; original vectors, extended.

(thm rt_cast [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), us2 :- (List U), t :- Exp, A :- Exp, B :- Exp,
              h :- (Rt chkf D us t A), eu :- (= us us2), eA :- (= A B)]
  (Rt chkf D us2 t B)
  (subst eu) (subst eA) (exact h))

(thm rt_cast_t [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), t :- Exp, t2 :- Exp, A :- Exp, B :- Exp,
                h :- (Rt chkf D us t A), et :- (= t t2), eA :- (= A B)]
  (Rt chkf D us t2 B)
  (subst et) (subst eA) (exact h))

(thm rt_ctx1 [chkf :- (=> Code Code Bool), a0 :- Exp, a1 :- Exp, G :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
              h :- (Rt chkf (List.cons Exp a0 G) us t A), e :- (= a0 a1)]
  (Rt chkf (List.cons Exp a1 G) us t A)
  (subst e) (exact h))

(thm rt_ctx2 [chkf :- (=> Code Code Bool), a0 :- Exp, a1 :- Exp, b0 :- Exp, b1 :- Exp, G :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
              h :- (Rt chkf (List.cons Exp a0 (List.cons Exp b0 G)) us t A), ea :- (= a0 a1), eb :- (= b0 b1)]
  (Rt chkf (List.cons Exp a1 (List.cons Exp b1 G)) us t A)
  (subst ea) (subst eb) (exact h))

;; The variable rule: as tl_var, with the usage read at the same index
;; (nthU_ins_ge / nthU_ins_lt) and the lengths kept equal (len_ins).
(thm rt_var [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), i :- Nat, A :- Exp, r :- U,
             hl :- (= (lenU us) (lenE D)), hA :- (= (nthE D i) (Option.some Exp A)), hu :- (= (nthU us i) (Option.some U r)),
             hr :- (= (nonzero r) true), cc :- Nat, XX :- Exp]
  (Rt chkf (insD cc XX D) (insU cc U.u0 us) (lift 1 cc (Exp.var i)) (lift 1 cc (lift (+ i 1) 0 A)))
  (have hc (Decidable (Nat.lt i cc)) (Nat.decLt i cc))
  (cases hc)
  (exact (rt_cast_t chkf (insD cc XX D) (insU cc U.u0 us) (Exp.var (+ i 1)) (lift 1 cc (Exp.var i))
           (lift (+ (+ i 1) 1) 0 A) (lift 1 cc (lift (+ i 1) 0 A))
           (Rt.rVar chkf (insD cc XX D) (insU cc U.u0 us) (+ i 1) A r (len_ins XX U.u0 cc D us hl)
             (Eq.trans (nthE_ins_ge XX cc i D (Nat.le_of_not_lt h)) hA)
             (Eq.trans (nthU_ins_ge U.u0 cc i us (Nat.le_of_not_lt h)) hu) hr)
           (Eq.symm (lift_var_above 1 cc i (Nat.le_of_not_lt h)))
           (var_ty_ge A cc i (Nat.le_of_not_lt h))))
  (exact (rt_cast_t chkf (insD cc XX D) (insU cc U.u0 us) (Exp.var i) (lift 1 cc (Exp.var i))
           (lift (+ i 1) 0 (lift 1 (- (- cc i) 1) A)) (lift 1 cc (lift (+ i 1) 0 A))
           (Rt.rVar chkf (insD cc XX D) (insU cc U.u0 us) i (lift 1 (- (- cc i) 1) A) r (len_ins XX U.u0 cc D us hl)
             (nthE_ins_lt XX cc i D A h hA) (nthU_ins_lt U.u0 cc i us r h hu) hr)
           (Eq.symm (lift_var_below 1 cc i h))
           (var_ty_lt A cc i h))))

;; Usage-vector expressions of the rule table, built from vadd and vscale
;; over the premises' vectors.  (ueq E) = [l p]: l is E with every vector x
;; replaced by its extension insU cc 0 x (the form a rule applied to the
;; extended premises concludes with), and p : l = insU cc 0 E.
(defn- UI [x] (list 'insU 'cc 'U.u0 x))
(defn- ueq [E]
  (let [lu '(List U)]
    (cond
      (symbol? E) [(UI E) (list 'Eq.refl$1 (UI E))]
      (= (first E) 'vscale)
      (let [[_ r X] E [l p] (ueq X)]
        [(list 'vscale r l)
         (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vscale r 'v)) p) (list 'vscale_ins0 r 'cc X))])
      (= (first E) 'vadd)
      (let [[_ X Y] E [l1 p1] (ueq X) [l2 p2] (ueq Y)]
        [(list 'vadd l1 l2)
         (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd 'v l2)) p1)
               (list 'Eq.trans (list 'congrArg (list 'fn ['v :- lu] (list 'vadd (UI X) 'v)) p2)
                     (list 'vadd_ins0 'cc X Y)))]))))

;; Abbreviations of the runtime cases (besides those of the type level):
;;   (UI x)               insU cc U.u0 x
;;   (TF D A h k)         tl_weaken of a formation premise D ⊢ A type
;;   (T0 D t A h k)       tl_weaken of a :⁰ premise D ⊢ t :⁰ A
;;   (RC E t A B h eA)    h concludes at the vector (ueq E)'s l and type A:
;;                        cast to insU cc 0 E and type B
;;   (PU E)               the equation l = insU cc 0 E of (ueq E)
(defn- rwk-expand [form]
  (let [cut (fn [k] (if (zero? k) 'cc (list '+ 'cc k)))]
    (wk-expand
     (walk/prewalk
      (fn [x]
        (cond
          (and (seq? x) (= (first x) 'UI)) (UI (second x))
          (and (seq? x) (= (first x) 'TF)) (let [[_ D A h k] x] (list 'tl_weaken 'chkf 'Bool.true D A 'Exp.tUnit h (cut k) 'XX))
          (and (seq? x) (= (first x) 'T0)) (let [[_ D t A h k] x] (list 'tl_weaken 'chkf 'Bool.false D t A h (cut k) 'XX))
          (and (seq? x) (= (first x) 'RC))
          (let [[_ E t A B h eA] x [l p] (ueq E)]
            (list 'rt_cast 'chkf 'DI l (UI E) t A B h p eA))
          (and (seq? x) (= (first x) 'PU)) (second (ueq (second x)))
          :else x))
      form))))

(def ^:private rt-wk-cases
  '[;; rVar, rConst
    (rt_var chkf D us i A r hl hA hu hr cc XX)
    (Rt.rConst chkf DI (UI us) (L 0 t) (L 0 A) (len_ins XX U.u0 cc D us hl) (constTyped_lift t A h 1 cc))
    ;; rLam
    (Rt.rLam chkf DI (UI us) r (L 0 A) (L 1 t) (L 1 B) (TF D A hA 0) (IH ih_ht 1))
    ;; rApp0
    (rt_cast chkf DI (UI us) (UI us) (Exp.app (L 0 f) (L 0 u)) (subst1 (L 0 u) (L 1 B)) (L 0 (subst1 u B))
      (Rt.rApp0 chkf DI (UI us) (L 0 f) (L 0 u) (L 0 A) (L 1 B) (IH ih_hf 0) (T0 D u A hu 0) (TF D A hA 0)
        (TF (List.cons Exp A D) B hB 1))
      (Eq.refl$1 (UI us)) (Eq.symm (lift_subst1 u B cc)))
    ;; rApp
    (RC (vadd us1 (vscale r us2)) (Exp.app (L 0 f) (L 0 u)) (subst1 (L 0 u) (L 1 B)) (L 0 (subst1 u B))
      (Rt.rApp chkf DI (UI us1) (UI us2) r (L 0 f) (L 0 u) (L 0 A) (L 1 B) hr (IH ih_hf 0) (IH ih_hu 0) (TF D A hA 0)
        (TF (List.cons Exp A D) B hB 1))
      (Eq.symm (lift_subst1 u B cc)))
    ;; rPair0
    (Rt.rPair0 chkf DI (UI us) (L 0 A) (L 1 B) (L 0 x) (L 0 y) (TF D A hA 0) (TF (List.cons Exp A D) B hB 1) (T0 D x A hx 0)
      (rt_cast chkf DI (UI us) (UI us) (L 0 y) (L 0 (subst1 x B)) (subst1 (L 0 x) (L 1 B)) (IH ih_hy 0)
        (Eq.refl$1 (UI us)) (lift_subst1 x B cc)))
    ;; rPair
    (RC (vadd (vscale r us1) us2) (Exp.pair (Exp.tSig r (L 0 A) (L 1 B)) (L 0 x) (L 0 y)) (Exp.tSig r (L 0 A) (L 1 B)) (Exp.tSig r (L 0 A) (L 1 B))
      (Rt.rPair chkf DI (UI us1) (UI us2) r (L 0 A) (L 1 B) (L 0 x) (L 0 y) hr (TF D A hA 0) (TF (List.cons Exp A D) B hB 1)
        (IH ih_hx 0)
        (rt_cast chkf DI (UI us2) (UI us2) (L 0 y) (L 0 (subst1 x B)) (subst1 (L 0 x) (L 1 B)) (IH ih_hy 0)
          (Eq.refl$1 (UI us2)) (lift_subst1 x B cc)))
      (Eq.refl$1 (Exp.tSig r (L 0 A) (L 1 B))))
    ;; rLet
    (RC (vadd us1 us2) (Exp.letp (L 0 C) (L 0 p) (L 2 t)) (L 0 C) (L 0 C)
      (Rt.rLet chkf DI (UI us1) (UI us2) r (L 0 A) (L 1 B) (L 0 C) (L 0 p) (L 2 t) (IH ih_hp 0) (TF D C hC 0) (TF D A hA 0)
        (TF (List.cons Exp A D) B hB 1)
        (rt_cast chkf (List.cons Exp (L 1 B) (List.cons Exp (L 0 A) DI))
          (List.cons U U.u1 (List.cons U r (UI us2))) (List.cons U U.u1 (List.cons U r (UI us2)))
          (L 2 t) (L 2 (lift 2 0 C)) (lift 2 0 (L 0 C)) (IH ih_ht 2)
          (Eq.refl$1 (List.cons U U.u1 (List.cons U r (UI us2)))) (lift_lift2 C cc)))
      (Eq.refl$1 (L 0 C)))
    ;; rAbort
    (Rt.rAbort chkf DI (UI us) (L 0 A) (L 0 t) (IH ih_ht 0) (TF D A hA 0))
    ;; rConv
    (Rt.rConv chkf DI (UI us) (L 0 t) (L 0 A) (L 0 B) (IH ih_ht 0) (TF D B hB 0) (cv_weaken chkf D A B hc cc XX))
    ;; rIte
    (RC (vadd us1 us2) (Exp.ite (L 0 b) (L 0 t) (L 0 e)) (L 0 C) (L 0 C)
      (Rt.rIte chkf DI (UI us1) (UI us2) (L 0 b) (L 0 t) (L 0 e) (L 0 C) (IH ih_hb 0) (IH ih_ht 0) (IH ih_he 0))
      (Eq.refl$1 (L 0 C)))
    ;; rElimB
    (RC (vadd us1 us2) (Exp.elimB (L 1 P) (L 0 b) (L 0 t) (L 0 e)) (subst1 (L 0 b) (L 1 P)) (L 0 (subst1 b P))
      (Rt.rElimB chkf DI (UI us1) (UI us2) (L 1 P) (L 0 b) (L 0 t) (L 0 e) (IH ih_hb 0) (TF (consE Exp.tBool D) P hP 1)
        (rt_cast chkf DI (UI us2) (UI us2) (L 0 t) (L 0 (subst1 Exp.tt P)) (subst1 Exp.tt (L 1 P)) (IH ih_ht 0)
          (Eq.refl$1 (UI us2)) (lift_subst1 Exp.tt P cc))
        (rt_cast chkf DI (UI us2) (UI us2) (L 0 e) (L 0 (subst1 Exp.ff P)) (subst1 Exp.ff (L 1 P)) (IH ih_he 0)
          (Eq.refl$1 (UI us2)) (lift_subst1 Exp.ff P cc)))
      (Eq.symm (lift_subst1 b P cc)))
    ;; rSucc
    (Rt.rSucc chkf DI (UI us) (L 0 n) (IH ih_h 0))
    ;; rRecN: the step at usages 1 (y), ω (x) and ω·us3
    (RC (vadd us1 (vadd us2 (vscale U.uw us3))) (Exp.recN (L 1 P) (L 0 z) (L 2 s) (L 0 n)) (subst1 (L 0 n) (L 1 P)) (L 0 (subst1 n P))
      (Rt.rRecN chkf DI (UI us1) (UI us2) (UI us3) (L 1 P) (L 0 z) (L 2 s) (L 0 n) (IH ih_hn 0) (TF (consE Exp.tNat D) P hP 1)
        (rt_cast chkf DI (UI us2) (UI us2) (L 0 z) (L 0 (subst1 Exp.zero P)) (subst1 Exp.zero (L 1 P)) (IH ih_hz 0)
          (Eq.refl$1 (UI us2)) (lift_subst1 Exp.zero P cc))
        (rt_cast chkf (List.cons Exp (L 1 P) (List.cons Exp Exp.tNat DI))
          (List.cons U U.u1 (List.cons U U.uw (UI (vscale U.uw us3))))
          (List.cons U U.u1 (List.cons U U.uw (vscale U.uw (UI us3))))
          (L 2 s) (L 2 (stepTy P)) (stepTy (L 1 P)) (IH ih_hs 2)
          (congrArg (fn [v :- (List U)] (List.cons U U.u1 (List.cons U U.uw v))) (Eq.symm (PU (vscale U.uw us3))))
          (lift_stepTy P cc)))
      (Eq.symm (lift_subst1 n P cc)))
    ;; rCaseL
    (RC (vadd us1 us2) (Exp.caseL (L 1 P) (L 0 x) (L 0 bs)) (subst1 (L 0 x) (L 1 P)) (L 0 (subst1 x P))
      (Rt.rCaseL chkf DI (UI us1) (UI us2) (L 1 P) (L 0 x) (L 0 bs) (IH ih_hx 0) (TF (consE Exp.tLbl D) P hP 1) (IH ih_hb 0))
      (Eq.symm (lift_subst1 x P cc)))
    ;; rBnil, rBcons
    (Rt.rBnil chkf DI (UI us) (L 1 P) (len_ins XX U.u0 cc D us hl))
    (Rt.rBcons chkf DI (UI us) (L 1 P) k (L 0 h) (L 0 t)
      (rt_cast chkf DI (UI us) (UI us) (L 0 h) (L 0 (subst1 (Exp.lbl k) P)) (subst1 (Exp.lbl k) (L 1 P)) (IH ih_hh 0)
        (Eq.refl$1 (UI us)) (lift_subst1 (Exp.lbl k) P cc))
      (IH ih_ht 0) (TF (consE Exp.tLbl D) P hP 1))
    ;; rSleaf, rSnode
    (Rt.rSleaf chkf DI (UI us) (L 0 x) (IH ih_h 0))
    (RC (vadd us1 (vadd us2 us3)) (Exp.snode (L 0 x) (L 0 c1) (L 0 c2)) Exp.tSyn Exp.tSyn
      (Rt.rSnode chkf DI (UI us1) (UI us2) (UI us3) (L 0 x) (L 0 c1) (L 0 c2) (IH ih_hx 0) (IH ih_h1 0) (IH ih_h2 0))
      (Eq.refl$1 Exp.tSyn))
    ;; rRecS
    (RC (vadd us1 (vadd (vscale U.uw us2) (vscale U.uw us3))) (Exp.recS (L 1 P) (L 1 tl) (L 5 tn) (L 0 c))
        (subst1 (L 0 c) (L 1 P)) (L 0 (subst1 c P))
      (Rt.rRecS chkf DI (UI us1) (UI us2) (UI us3) (L 1 P) (L 1 tl) (L 5 tn) (L 0 c) (IH ih_hc 0) (TF (consE Exp.tSyn D) P hP 1)
        (rt_cast chkf (List.cons Exp Exp.tLbl DI)
          (List.cons U U.uw (UI (vscale U.uw us2))) (List.cons U U.uw (vscale U.uw (UI us2)))
          (L 1 tl) (L 1 (leafTy P)) (leafTy (L 1 P)) (IH ih_hl 1)
          (congrArg (fn [v :- (List U)] (List.cons U U.uw v)) (Eq.symm (PU (vscale U.uw us2))))
          (lift_leafTy P cc))
        (rt_ctx2 chkf (L 4 (y2Ty P)) (y2Ty (L 1 P)) (L 3 (y1Ty P)) (y1Ty (L 1 P))
          (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI)))
          (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw (vscale U.uw (UI us3)))))))
          (L 5 tn) (nodeTy (L 1 P))
          (rt_cast chkf
            (List.cons Exp (L 4 (y2Ty P)) (List.cons Exp (L 3 (y1Ty P))
              (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI)))))
            (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw (UI (vscale U.uw us3)))))))
            (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw (vscale U.uw (UI us3)))))))
            (L 5 tn) (L 5 (nodeTy P)) (nodeTy (L 1 P)) (IH ih_hn 5)
            (congrArg (fn [v :- (List U)] (List.cons U U.u1 (List.cons U U.u1 (List.cons U U.uw (List.cons U U.uw (List.cons U U.uw v))))))
                      (Eq.symm (PU (vscale U.uw us3))))
            (lift_nodeTy P cc))
          (lift_y2Ty P cc) (lift_y1Ty P cc))
        ;; hY1, hY2: weakened under the node branch's binders, then cast as
        ;; the branch's context is (lift_y1Ty, lift_y2Ty)
        (tl_cast3 chkf Bool.true (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI)))
          (L 3 (y1Ty P)) (y1Ty (L 1 P)) Exp.tUnit Exp.tUnit
          (TF (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D))) (y1Ty P) hY1 3) (lift_y1Ty P cc) (Eq.refl$1 Exp.tUnit))
        (tl_ctx1 chkf Bool.true (L 3 (y1Ty P)) (y1Ty (L 1 P)) (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI)))
          (y2Ty (L 1 P)) Exp.tUnit
          (tl_cast3 chkf Bool.true (List.cons Exp (L 3 (y1Ty P)) (List.cons Exp Exp.tSyn (List.cons Exp Exp.tSyn (List.cons Exp Exp.tLbl DI))))
            (L 4 (y2Ty P)) (y2Ty (L 1 P)) Exp.tUnit Exp.tUnit
            (TF (consE (y1Ty P) (consE Exp.tSyn (consE Exp.tSyn (consE Exp.tLbl D)))) (y2Ty P) hY2 4) (lift_y2Ty P cc) (Eq.refl$1 Exp.tUnit))
          (lift_y1Ty P cc)))
      (Eq.symm (lift_subst1 c P cc)))
    ;; rLeaf, rNode
    (Rt.rLeaf chkf DI (UI us) (L 0 x) (IH ih_h 0))
    (RC (vadd us1 (vadd us2 (vadd us3 us4))) (Exp.node (L 0 d) (L 0 x) (L 0 r1) (L 0 r2)) Exp.tR Exp.tR
      (Rt.rNode chkf DI (UI us1) (UI us2) (UI us3) (UI us4) (L 0 d) (L 0 x) (L 0 r1) (L 0 r2)
        (IH ih_hd 0) (IH ih_hx 0) (IH ih_h1 0) (IH ih_h2 0))
      (Eq.refl$1 Exp.tR))
    ;; rItR: the steps at ω·us1, ω·us2
    (RC (vadd (vscale U.uw us1) (vadd (vscale U.uw us2) us3)) (Exp.itR (L 0 X) (L 0 g) (L 0 h) (L 0 r)) (L 0 X) (L 0 X)
      (Rt.rItR chkf DI (UI us1) (UI us2) (UI us3) (L 0 X) (L 0 g) (L 0 h) (L 0 r) (TF D X hX 0)
        (rt_cast chkf DI (UI (vscale U.uw us1)) (vscale U.uw (UI us1)) (L 0 g) (L 0 (gTy X)) (gTy (L 0 X)) (IH ih_hg 0)
          (Eq.symm (PU (vscale U.uw us1))) (lift_gTy X cc))
        (rt_cast chkf DI (UI (vscale U.uw us2)) (vscale U.uw (UI us2)) (L 0 h) (L 0 (hTy X)) (hTy (L 0 X)) (IH ih_hh 0)
          (Eq.symm (PU (vscale U.uw us2))) (lift_hTy X cc))
        (IH ih_hr 0))
      (Eq.refl$1 (L 0 X)))
    ;; rPrn, rChk
    (Rt.rPrn chkf DI (UI us) (L 0 r) (IH ih_h 0))
    (RC (vadd us1 us2) (Exp.chk (L 0 c) (L 0 d)) Exp.tBool Exp.tBool
      (Rt.rChk chkf DI (UI us1) (UI us2) (L 0 c) (L 0 d) (IH ih_hc 0) (IH ih_hd 0))
      (Eq.refl$1 Exp.tBool))
    ;; rH1: the code c at ω·us3
    (RC (vadd us1 (vadd us2 (vadd (vscale U.uw us3) (vadd us4 us5)))) (Exp.h1 (L 0 r) (L 0 s) (L 0 c) (L 0 e1) (L 0 e2)) Exp.tEmpty Exp.tEmpty
      (Rt.rH1 chkf DI (UI us1) (UI us2) (UI us3) (UI us4) (UI us5) (L 0 r) (L 0 s) (L 0 c) (L 0 e1) (L 0 e2)
        (IH ih_hr 0) (IH ih_hs 0)
        (rt_cast chkf DI (UI (vscale U.uw us3)) (vscale U.uw (UI us3)) (L 0 c) Exp.tSyn Exp.tSyn (IH ih_hc 0)
          (Eq.symm (PU (vscale U.uw us3))) (Eq.refl$1 Exp.tSyn))
        (IH ih_h1 0) (IH ih_h2 0))
      (Eq.refl$1 Exp.tEmpty))
    ;; rRefl
    (RC (vadd us1 us2) (Exp.refl (L 0 X) (L 0 r) (L 0 e)) (L 0 X) (L 0 X)
      (Rt.rRefl chkf DI (UI us1) (UI us2) (L 0 X) (L 0 cd) (L 0 r) (L 0 e) (baseCode_lift X cd hb 1 cc) (IH ih_hr 0) (IH ih_he 0))
      (Eq.refl$1 (L 0 X)))
    ;; rInsp: c at ω·us0; the branches as at type level
    (RC (vadd us1 (vadd (vscale U.uw us0) us2)) (Exp.insp (L 0 X) (L 0 r) (L 0 c) (L 2 t1) (L 2 t2)) (L 0 X) (L 0 X)
      (Rt.rInsp chkf DI (UI us1) (UI us0) (UI us2) (L 0 X) (L 0 r) (L 0 c) (L 2 t1) (L 2 t2) (IH ih_hr 0)
        (rt_cast chkf DI (UI (vscale U.uw us0)) (vscale U.uw (UI us0)) (L 0 c) Exp.tSyn Exp.tSyn (IH ih_hc 0)
          (Eq.symm (PU (vscale U.uw us0))) (Eq.refl$1 Exp.tSyn))
        (TF D X hX 0)
        ;; the formation premises of the branches' added entries, weakened
        ;; under the certificate binder and cast as the branch contexts are
        (tl_cast3 chkf Bool.true (List.cons Exp Exp.tR DI) (L 1 (chkT (Exp.var 0) (lift 1 0 c))) (chkT (Exp.var 0) (lift 1 0 (L 0 c)))
          Exp.tUnit Exp.tUnit (TF (List.cons Exp Exp.tR D) (chkT (Exp.var 0) (lift 1 0 c)) hF1 1)
          (congrArg (fn [v :- Exp] (chkT (Exp.var 0) v)) (lift_lift_comm c 1 cc 0)) (Eq.refl$1 Exp.tUnit))
        (tl_cast3 chkf Bool.true (List.cons Exp Exp.tR DI) (L 1 (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))))
          (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 (L 0 c)))))
          Exp.tUnit Exp.tUnit (TF (List.cons Exp Exp.tR D) (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))) hF2 1)
          (congrArg (fn [v :- Exp] (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) v)))) (lift_lift_comm c 1 cc 0)) (Eq.refl$1 Exp.tUnit))
        (rt_ctx1 chkf (L 1 (chkT (Exp.var 0) (lift 1 0 c))) (chkT (Exp.var 0) (lift 1 0 (L 0 c)))
          (List.cons Exp Exp.tR DI) (List.cons U U.u1 (List.cons U U.u1 (UI us2))) (L 2 t1) (lift 2 0 (L 0 X))
          (rt_cast chkf (List.cons Exp (L 1 (chkT (Exp.var 0) (lift 1 0 c))) (List.cons Exp Exp.tR DI))
            (List.cons U U.u1 (List.cons U U.u1 (UI us2))) (List.cons U U.u1 (List.cons U U.u1 (UI us2)))
            (L 2 t1) (L 2 (lift 2 0 X)) (lift 2 0 (L 0 X)) (IH ih_h1 2)
            (Eq.refl$1 (List.cons U U.u1 (List.cons U U.u1 (UI us2)))) (lift_lift2 X cc))
          (congrArg (fn [v :- Exp] (chkT (Exp.var 0) v)) (lift_lift_comm c 1 cc 0)))
        (rt_ctx1 chkf (L 1 (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c)))))
          (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 (L 0 c)))))
          (List.cons Exp Exp.tR DI) (List.cons U U.u1 (List.cons U U.u1 (UI us2))) (L 2 t2) (lift 2 0 (L 0 X))
          (rt_cast chkf (List.cons Exp (L 1 (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) (lift 1 0 c))))) (List.cons Exp Exp.tR DI))
            (List.cons U U.u1 (List.cons U U.u1 (UI us2))) (List.cons U U.u1 (List.cons U U.u1 (UI us2)))
            (L 2 t2) (L 2 (lift 2 0 X)) (lift 2 0 (L 0 X)) (IH ih_h2 2)
            (Eq.refl$1 (List.cons U U.u1 (List.cons U U.u1 (UI us2)))) (lift_lift2 X cc))
          (congrArg (fn [v :- Exp] (Exp.tT (notE (Exp.chk (Exp.prn (Exp.var 0)) v)))) (lift_lift_comm c 1 cc 0))))
      (Eq.refl$1 (L 0 X)))])

;; Lemma 2.1 (weakening), runtime: Γ ⊢ t :¹ A gives the same judgment with an
;; entry XX at usage 0 inserted at any position cc, t and A lifted at cc.
(a/prove-theorem 'rt_weaken
  '[chkf :- (=> Code Code Bool), D0 :- (List Exp), us0 :- (List U), t0 :- Exp, A0 :- Exp, der :- (Rt chkf D0 us0 t0 A0)]
  '(forall [cc Nat] (forall [XX Exp] (Rt chkf (insD cc XX D0) (insU cc U.u0 us0) (lift 1 cc t0) (lift 1 cc A0))))
  (lv (into ['(induction der)] (mapcat (fn [c] ['(intro cc XX) (list 'exact (rwk-expand c))]) rt-wk-cases))))

;; ===========================================================================
;; §6  Lemma 2.4: disjoint token blocks and composition
;; ===========================================================================

;; A uniform usage prefix. The tail remains an arbitrary usage vector.
(a/defn prefixU [n :- Nat, r :- U, us :- (List U)] (List U)
  (match n [zero us] [(succ k) (List.cons U r (prefixU k r us))]))

(thm prefixU_one [n :- Nat]
  (= (prefixU n U.u1 (List.nil U)) (thetaU n))
  (induction n)
  (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n)))

(thm insU_prefix [n :- Nat, r :- U, s :- U, us :- (List U)]
  (= (insU n s (prefixU n r us)) (prefixU n r (List.cons U s us)))
  (induction n)
  (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U r v)) ih_n)))

;; Every inserted type is Dia, hence no telescope entry changes under lift.
(thm insD_theta [c :- Nat, m :- Nat]
  (= (insD c Exp.tDia (thetaD (+ c m))) (thetaD (+ (+ c m) 1)))
  (induction c)
  (rw [(Nat.zero_add m)])
  (rw [(Nat.succ_add n m)])
  (exact (congrArg (fn [v :- (List Exp)] (List.cons Exp Exp.tDia v)) ih_n)))

(thm vscale_one [us :- (List U)] (= (vscale U.u1 us) us)
  (induction us)
  (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U head v)) ih_tail)))

(thm vadd_theta_zero [m :- Nat]
  (= (vadd (thetaU m) (vzero m)) (thetaU m))
  (induction m)
  (rfl)
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n)))

(thm vadd_theta_blocks [m1 :- Nat, m2 :- Nat]
  (= (vadd (prefixU m1 U.u0 (thetaU m2))
           (vscale U.u1 (prefixU m1 U.u1 (vzero m2))))
     (thetaU (+ m1 m2)))
  (rw [(vscale_one (prefixU m1 U.u1 (vzero m2)))])
  (induction m1)
  (exact (Eq.trans (vadd_theta_zero m2) (Eq.symm (congrArg thetaU (Nat.zero_add m2)))))
  (rw [(Nat.succ_add n m2)])
  (exact (congrArg (fn [v :- (List U)] (List.cons U U.u1 v)) ih_n)))

;; Transport all four indices of Rt. This only transports equalities,
;; and does not add a conversion or subusaging rule to the object calculus.
(thm rt_reindex [chkf :- (=> Code Code Bool), D :- (List Exp), us :- (List U), t :- Exp, A :- Exp,
                 D2 :- (List Exp), us2 :- (List U), t2 :- Exp, A2 :- Exp,
                 h :- (Rt chkf D us t A), eD :- (= D D2), eu :- (= us us2), et :- (= t t2), eA :- (= A A2)]
  (Rt chkf D2 us2 t2 A2)
  (subst eD) (subst eu) (subst et) (subst eA) (exact h))

;; Formation at the empty context extends to any token context. The closed
;; hypothesis removes each lift of A; the formation judgment is independent
;; of runtime usages.
(thm tl_theta_closed [chkf :- (=> Code Code Bool), A :- Exp, hc :- (= (closedTy A) true),
                      hA :- (Tl chkf Bool.true (List.nil Exp) A Exp.tUnit), m :- Nat]
  (Tl chkf Bool.true (thetaD m) A Exp.tUnit)
  (induction m)
  (exact hA)
  (exact (tl_cast3 chkf Bool.true (thetaD (+ n 1)) (lift 1 0 A) A Exp.tUnit Exp.tUnit
           (tl_weaken chkf Bool.true (thetaD n) A Exp.tUnit ih_n 0 Exp.tDia)
           (closedTy_lift A hc 1 0) (Eq.refl$1 Exp.tUnit))))

;; Prepending n unused tokens: the original block moves up by n indices.
(thm rt_theta_prepend [chkf :- (=> Code Code Bool), m :- Nat, t :- Exp, A :- Exp,
                       hc :- (= (closedTy A) true), h :- (Rt chkf (thetaD m) (thetaU m) t A), n :- Nat]
  (Rt chkf (thetaD (+ n m)) (prefixU n U.u0 (thetaU m)) (lift n 0 t) A)
  (induction n)
  (exact (rt_reindex chkf _ _ _ _ _ _ _ _ h
           (Eq.symm (congrArg thetaD (Nat.zero_add m))) rfl (Eq.symm (lift_zero t 0)) rfl))
  (rw [(Nat.succ_add n m)])
  (exact (rt_reindex chkf _ _ _ _ _ _ _ _
           (rt_weaken chkf (thetaD (+ n m)) (prefixU n U.u0 (thetaU m)) (lift n 0 t) A ih_n 0 Exp.tDia)
           rfl rfl (lift_comp t 1 n 0) (closedTy_lift A hc 1 0))))

;; Appending n unused tokens at the original block's outer edge. Writing
;; lift n m t explicitly avoids needing a separate scopedness/strengthening
;; theorem even though a well-scoped t has no free index >= m.
(thm rt_theta_append [chkf :- (=> Code Code Bool), m :- Nat, t :- Exp, A :- Exp,
                      hc :- (= (closedTy A) true), h :- (Rt chkf (thetaD m) (thetaU m) t A), n :- Nat]
  (Rt chkf (thetaD (+ m n)) (prefixU m U.u1 (vzero n)) (lift n m t) A)
  (induction n)
  (exact (rt_reindex chkf _ _ _ _ _ _ _ _ h
           rfl (Eq.symm (prefixU_one m)) (Eq.symm (lift_zero t m)) rfl))
  (exact (rt_reindex chkf _ _ _ _ _ _ _ _
           (rt_weaken chkf (thetaD (+ m n)) (prefixU m U.u1 (vzero n)) (lift n m t) A ih_n m Exp.tDia)
           (insD_theta m n) (insU_prefix m U.u1 U.u0 (vzero n))
           (lift_comp t 1 n m) (closedTy_lift A hc 1 m))))

;; Lemma 2.4 (composition). The stored telescope lists t1's m1 tokens first
;; and t2's m2 tokens second. De Bruijn renaming is explicit in the result:
;; t1' = lift m2 m1 t1 and t2' = lift m1 0 t2. The extra formation premise
;; records regularity that Rt alone does not currently provide; closedTy
;; only checks free variables and is not itself a formation judgment.
(thm lemma24 [chkf :- (=> Code Code Bool), m1 :- Nat, m2 :- Nat, t1 :- Exp, t2 :- Exp, A :- Exp,
              hc :- (= (closedTy A) true), hA :- (Tl chkf Bool.true (List.nil Exp) A Exp.tUnit),
              h1 :- (Rt chkf (thetaD m1) (thetaU m1) t1 A),
              h2 :- (Rt chkf (thetaD m2) (thetaU m2) t2 (Exp.tPi U.u1 A Exp.tEmpty))]
  (Rt chkf (thetaD (+ m1 m2)) (thetaU (+ m1 m2))
      (Exp.app (lift m1 0 t2) (lift m2 m1 t1)) Exp.tEmpty)
  (have hneg (= (closedTy (Exp.tPi U.u1 A Exp.tEmpty)) true)
    (andb_intro (closedTy A) true hc (Eq.refl$1 Bool.true)))
  (exact (rt_cast chkf (thetaD (+ m1 m2))
           (vadd (prefixU m1 U.u0 (thetaU m2)) (vscale U.u1 (prefixU m1 U.u1 (vzero m2))))
           (thetaU (+ m1 m2)) (Exp.app (lift m1 0 t2) (lift m2 m1 t1)) Exp.tEmpty Exp.tEmpty
           (Rt.rApp chkf (thetaD (+ m1 m2))
             (prefixU m1 U.u0 (thetaU m2)) (prefixU m1 U.u1 (vzero m2)) U.u1
             (lift m1 0 t2) (lift m2 m1 t1) A Exp.tEmpty (Eq.refl$1 Bool.true)
             (rt_theta_prepend chkf m2 t2 (Exp.tPi U.u1 A Exp.tEmpty) hneg h2 m1)
             (rt_theta_append chkf m1 t1 A hc h1 m2)
             (tl_theta_closed chkf A hc hA (+ m1 m2))
             (Tl.fBase chkf (List.cons Exp A (thetaD (+ m1 m2))) Exp.tEmpty (Eq.refl$1 Bool.true)))
           (vadd_theta_blocks m1 m2) (Eq.refl$1 Exp.tEmpty))))
