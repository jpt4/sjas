(ns lcert.formal.syntactic
  "F2d — syntactic lemmas for R4-metatheory.md §2 (ADR-0006).

  Contexts are telescopes, innermost entry first; a stored entry's type is
  relative to its tail. All statements use the existing lift/subst and
  judgments, without extending the object calculus or the kernel's axioms.

  The lift laws below are structural inductions over every Exp constructor.
  Their repetitive congruence proofs are assembled as Clojure data, then
  elaborated and checked by Ansatz. This avoids Clojure's 64 KiB method limit
  for a single large quoted tactic block. The data generator is not an
  additional trust assumption: an incorrect case still fails kernel checking.
  Every recursive field is listed with its binder depth, including the
  internal branch-list pseudo-type tBrs introduced by F2c.

  Ansatz detail: use explicit congruence rather than unfolding liftF inside
  simp_all; unfolding can expand its recursor before matching a quantified
  induction hypothesis. Keep changing cutoffs quantified in the motive."
  (:require [ansatz.core :as a]
            [lcert.formal.base :refer [thm kdef]]
            [lcert.formal.judgment]))

;; [constructor [[field type number-of-binders] ...]]. This is a proof
;; generation table, not a second definition of substitution or lifting.
(def ^:private exp-fields
  '[[tEmpty []] [tUnit []] [tBool []] [tNat []] [tLbl []] [tSyn []]
    [tDia []] [tR []] [tT [[b Exp 0]]]
    [tPi [[r U 0] [A Exp 0] [B Exp 1]]]
    [tSig [[r U 0] [A Exp 0] [B Exp 1]]]
    [var [[i Nat 0]]] [star []] [abort [[A Exp 0] [t Exp 0]]]
    [tt []] [ff []] [ite [[b Exp 0] [t Exp 0] [e Exp 0]]]
    [elimB [[P Exp 1] [b Exp 0] [t Exp 0] [e Exp 0]]]
    [zero []] [succ [[n Exp 0]]]
    [recN [[P Exp 1] [z Exp 0] [s Exp 2] [n Exp 0]]]
    [lbl [[l Nat 0]]] [caseL [[P Exp 1] [a Exp 0] [bs Exp 0]]]
    [bnil []] [bcons [[h Exp 0] [t Exp 0]]] [sleaf [[a Exp 0]]]
    [snode [[a Exp 0] [c1 Exp 0] [c2 Exp 0]]]
    [recS [[P Exp 1] [tl Exp 1] [tn Exp 5] [c Exp 0]]]
    [leaf [[a Exp 0]]] [node [[d Exp 0] [a Exp 0] [r1 Exp 0] [r2 Exp 0]]]
    [itR [[X Exp 0] [g Exp 0] [h Exp 0] [r Exp 0]]] [prn [[r Exp 0]]]
    [lam [[r U 0] [A Exp 0] [t Exp 1]]] [app [[f Exp 0] [u Exp 0]]]
    [pair [[S Exp 0] [a Exp 0] [b Exp 0]]] [letp [[C Exp 0] [p Exp 0] [t Exp 2]]]
    [chk [[c Exp 0] [d Exp 0]]] [h1 [[r Exp 0] [s Exp 0] [c Exp 0] [e1 Exp 0] [e2 Exp 0]]]
    [refl [[D Exp 0] [r Exp 0] [e Exp 0]]]
    [insp [[X Exp 0] [r Exp 0] [c Exp 0] [t1 Exp 2] [t2 Exp 2]]]
    [tBrs [[P Exp 1] [k Nat 0]]]])

(defn- under [cut depth] (if (zero? depth) cut (list '+ cut depth)))
(defn- ih [field] (symbol (str "ih_" field)))
(defn- application [f xs] (if (seq xs) (apply list f xs) f))

(defn- congruence
  "Congruence for a constructor, replacing one field at a time. Each field
  is [type left right equality-proof], with nil for an unchanged field.
  The resulting Eq.trans/congrArg term is checked by the kernel."
  [ctor fields]
  (let [left (mapv second fields), right (mapv #(nth % 2) fields)
        proofs (keep-indexed
                (fn [i [ty _ _ proof]]
                  (when proof
                    (list 'congrArg
                          (list 'fn ['v ':- ty]
                                (application ctor (concat (subvec right 0 i) ['v] (subvec left (inc i)))))
                          proof))) fields)]
    (if (seq proofs)
      (reduce #(list 'Eq.trans %2 %1) (last proofs) (reverse (butlast proofs)))
      'rfl)))

(defn- prove-exp!
  "Prove a quantified property of Exp by its 41 constructor cases. A case
  supplies ordinary Ansatz tactics; nothing is installed unless checked."
  [nm prop introductions clause]
  (a/prove-theorem nm '[e :- Exp] prop
    (into ['(induction e)]
          (mapcat (fn [[ctor fields]]
                    (cons (apply list 'intro introductions) (clause ctor fields)))
                  exp-fields))))

;; Lift/substitution algebra used by Lemma 2.1. Zero displacement is the
;; identity, including beneath all one-, two-, and five-variable binders.
(prove-exp! 'lift_zero '(forall [cut Nat] (= (lift 0 cut e) e)) '[cut]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(simp [lift liftF])]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty depth]]
                   (if (= ty 'Exp)
                     [ty (list 'lift 0 (under 'cut depth) f) f (list (ih f) (under 'cut depth))]
                     [ty f f nil])) fields)))])))

;; Variable cases split the decidable index comparison. Explicit equality
;; chains avoid proof-goal ordering changes from rewrite/have under cases.
(thm skel_lift_var [i :- Nat, k :- Nat, cut :- Nat]
  (= (skel (lift k cut (Exp.var i))) Sk.unit)
  (have hc (Decidable (Nat.lt i cut)) (Nat.decLt i cut)) (cases hc)
  (exact (Eq.trans (congrArg skel (lift_var_above k cut i (Nat.le_of_not_lt h))) rfl))
  (exact (Eq.trans (congrArg skel (lift_var_below k cut i h)) rfl)))

(thm lift_comp_var [i :- Nat, k :- Nat, j :- Nat, cut :- Nat]
  (= (lift k cut (lift j cut (Exp.var i))) (lift (+ j k) cut (Exp.var i)))
  (have hc (Decidable (Nat.lt i cut)) (Nat.decLt i cut)) (cases hc)
  (exact (Eq.trans
    (congrArg (fn [v :- Exp] (lift k cut v)) (lift_var_above j cut i (Nat.le_of_not_lt h)))
    (Eq.trans (lift_var_above k cut (+ i j) (Nat.le_trans (Nat.le_of_not_lt h) (Nat.le_add_right i j)))
      (Eq.trans (congrArg Exp.var (Nat.add_assoc i j k))
        (Eq.symm (lift_var_above (+ j k) cut i (Nat.le_of_not_lt h)))))))
  (exact (Eq.trans
    (congrArg (fn [v :- Exp] (lift k cut v)) (lift_var_below j cut i h))
    (Eq.trans (lift_var_below k cut i h) (Eq.symm (lift_var_below (+ j k) cut i h))))))

;; Lemma 2.5's lift fact holds for every Exp, without formation hypotheses.
;; tBrs is treated as Lbl → skel(P), as in the corrected F2 base.
(prove-exp! 'skel_lift
  '(forall [amt Nat] (forall [cut Nat] (= (skel (lift amt cut e)) (skel e))))
  '[amt cut]
  (fn [ctor fields]
    (cond
      (= ctor 'var) '[(exact (skel_lift_var i amt cut))]
      (#{'tPi 'tSig 'tBrs} ctor)
      [(list 'exact
         (congruence (if (= ctor 'tSig) 'Sk.prod 'Sk.arr)
           (into (if (= ctor 'tBrs) [['Sk 'Sk.lbl 'Sk.lbl nil]] [])
             (for [[f ty depth] fields :when (= ty 'Exp)]
               ['Sk (list 'skel (list 'lift 'amt (under 'cut depth) f))
                (list 'skel f) (list (ih f) 'amt (under 'cut depth))]))))]
      :else '[(rfl)])))

;; Composition at one cutoff: applying shifts j then k adds j+k. The
;; theorem quantifies over the cutoff so the same induction works in binders.
(prove-exp! 'lift_comp
  '(forall [amt Nat] (forall [other Nat] (forall [cut Nat]
     (= (lift amt cut (lift other cut e)) (lift (+ other amt) cut e)))))
  '[amt other cut]
  (fn [ctor fields]
    (if (= ctor 'var)
      '[(exact (lift_comp_var i amt other cut))]
      [(list 'exact
         (congruence (symbol (str "Exp." ctor))
           (mapv (fn [[f ty depth]]
                   (let [cut (under 'cut depth)]
                     (if (= ty 'Exp)
                       [ty (list 'lift 'amt cut (list 'lift 'other cut f))
                        (list 'lift '(+ other amt) cut f) (list (ih f) 'amt 'other cut)]
                       [ty f f nil]))) fields)))])))
