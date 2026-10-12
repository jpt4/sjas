(ns lcert.formal.paprop
  "F6.3 — schematic axiom templates A1, A2 and A4 (R4 §6.2)."
  (:require [ansatz.core :as a]
            [lcert.formal.base :as b :refer [thm kdef lv]]
            [lcert.formal.painst]))

;; Variable lookup at all-omega, with an explicit equation accounting for
;; the telescope lift. This is the ordinary rVar rule, not subsumption.
(thm paHyp_allW [chkf :- (=> Code Code Bool), D :- (List Exp), i :- Nat,
                 A :- Exp, B :- Exp,
                 hi :- (Eq (Option Exp) (nthE D i) (Option.some Exp A)),
                 hB :- (Eq Exp (lift (+ i 1) 0 A) B)]
  (Rt chkf D (allW D) (Exp.var i) B)
  (exact (rt_cast chkf D (allW D) (allW D) (Exp.var i) (lift (+ i 1) 0 A) B
    (Rt.rVar chkf D (allW D) i A U.uw (paAllW_len D)
      hi (paAllW_nth D i A hi) (Eq.refl Bool.true)) rfl hB)))

(thm trF_lift3 [φ :- PF, ρ :- (=> Nat Nat)]
  (Eq Exp (lift 3 0 (trF φ ρ)) (trF φ (trSucR (trSucR (trSucR ρ)))))
  (exact (Eq.trans (Eq.symm (lift_comp (trF φ ρ) 1 2 0))
    (Eq.trans (congrArg (lift 1 0) (trF_lift2 φ ρ))
      (trF_lift1 φ (trSucR (trSucR ρ)))))))

;; The following small emitter writes explicit applications of rLam,
;; paHyp_allW, pa_mp and pa_instantiate. Formulas remain schematic kernel
;; parameters: this is not bounded testing or reflection on sample formulas.
;; Every generated theorem is elaborated and kernel-checked at load time.
(defn- imp [p q] (list 'PF.pimp p q))
(defn- tr [p rho] (list 'trF p rho))
(defn- cons-e [A D] (list 'List.cons 'Exp A D))
(defn- aw [D] (list 'allW D))
(defn- form [{:keys [D rho good]} p] (list 'trF_form 'chkf p D rho good))
(defn- extend-proof [{:keys [D rho good stack]} p]
  (let [A (tr p rho)]
    {:D (cons-e A D) :rho (list 'trSucR rho)
     :good (list 'paGood_suc D rho good A)
     :stack (into [{:p p :A A :rho rho}] stack)}))
(defn- lookup-proof [D stack i]
  (let [{:keys [A]} (first stack), tail (nth D 3)]
    (if (zero? i) (list 'nthE.eq_2 A tail)
      (list 'Eq.trans (list 'nthE.eq_3 A tail (dec i))
        (lookup-proof tail (rest stack) (dec i))))))
(defn- hyp [{:keys [D rho stack]} i]
  (let [{p :p A :A born :rho} (nth stack i)
        e (list 'Exp.var i)
        lift-eq (list (nth '[trF_lift1 trF_lift2 trF_lift3] i) p born)]
    {:p p :e e :proof (list 'paHyp_allW 'chkf D i A (tr p rho)
                       (lookup-proof D stack i) lift-eq)}))
(defn- lam [{:keys [D rho] :as ctx} p body]
  (let [{q :p e :e proof :proof} (body (extend-proof ctx p))]
    {:p (imp p q) :e (list 'Exp.lam 'U.uw (tr p rho) e)
     :proof (list 'Rt.rLam 'chkf D (aw D) 'U.uw (tr p rho) e
              (tr q (list 'trSucR rho)) (form ctx p) proof)}))
(defn- app [{:keys [D rho good]} f u]
  (let [[tag p q] (:p f)]
    (assert (= tag 'PF.pimp))
    (assert (= p (:p u)))
    {:p q :e (list 'Exp.app (:e f) (:e u))
     :proof (list 'pa_mp 'chkf p q D rho good (:e f) (:e u) (:proof f) (:proof u))}))
(defn- instantiate [{:keys [D rho good]} f p t]
  (assert (= (:p f) (list 'PF.pall p)))
  {:p (list 'paSbF (list 'paInst t) p)
   :e (list 'Exp.app (:e f) (list 'trT t rho))
   :proof (list 'pa_instantiate 'chkf p t D rho good (:e f) (:proof f))})

(def ^:private initial {:D 'D :rho 'ρ :good 'hg :stack []})
(defn- prove-template! [name params result]
  (a/prove-theorem name
    (into (into '[chkf :- (=> Code Code Bool)] params)
      '[D :- (List Exp), ρ :- (=> Nat Nat), hg :- (Good D ρ)])
    (list 'Rt 'chkf 'D '(allW D) (:e result) (tr (:p result) 'ρ))
    [(list 'exact (:proof result))]))

;; A1: lambda a b. a; the outer hypothesis is lifted across both binders.
(prove-template! 'pa_ax_a1 '[a :- PF, b :- PF]
  (lam initial 'a (fn [ca] (lam ca 'b (fn [cab] (hyp cab 1))))))

;; A2: lambda p q a. p a (q a). Each application uses pa_mp; all-omega
;; addition permits the shared argument and all three proof hypotheses.
(prove-template! 'pa_ax_a2 '[a :- PF, b :- PF, c :- PF]
  (lam initial (imp 'a (imp 'b 'c))
    (fn [cp] (lam cp (imp 'a 'b)
      (fn [cpq] (lam cpq 'a
        (fn [ctx] (app ctx (app ctx (hyp ctx 2) (hyp ctx 0))
                          (app ctx (hyp ctx 1) (hyp ctx 0))))))))))

;; A4: lambda p. p <t>. Good supplies every variable in t, even when t
;; contains variables absent from the instance. Thus this open-environment
;; theorem needs none of the closed-template variable pruning in R4.
(prove-template! 'pa_ax_a4 '[p :- PF, t :- PT]
  (lam initial '(PF.pall p)
    (fn [ctx] (instantiate ctx (hyp ctx 0) 'p 't))))
