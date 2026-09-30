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
  (exact (Cv.cvRefl chkf (insS c xs G) (lift 1 c A) (skj_weaken Bool.true G A Sk.unit h c xs)))
  (exact (Cv.cvFwd chkf (insS c xs G) (lift 1 c A) (lift 1 c B) (lift 1 c C) ih_hab (step_lift chkf B C hs c)
                   (skj_weaken Bool.true G C Sk.unit hc c xs)))
  (exact (Cv.cvBwd chkf (insS c xs G) (lift 1 c A) (lift 1 c B) (lift 1 c C) ih_hab (step_lift chkf C B hs c)
                   (skj_weaken Bool.true G C Sk.unit hc c xs))))

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
