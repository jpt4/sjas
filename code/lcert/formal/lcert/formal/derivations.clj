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
